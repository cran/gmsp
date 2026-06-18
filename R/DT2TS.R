#' Convert displacement time series into AT/VT/DT bundles
#'
#' @description
#' End-to-end workflow that takes displacement time histories and produces a
#' consistent set of velocity and acceleration, along with the displacement
#' processed outputs. It regularizes sampling if needed, converts units (for raw
#' data), chooses STFT parameters/resampling, applies robust edge tapering,
#' performs spectral/time derivatives, and applies post-tapering/optional trimming.
#'
#' @param .x data.table. Input displacement records with a time column and one or
#'   more signal columns.
#' @param units.source character. Source units for the input displacement when
#'   `isRaw = TRUE`. Same set as `AT2TS` / `VT2TS`: `"mm"`, `"cm"`,
#'   `"m"`, plus `"gal"` (treated as cm scale) and `"g"` (multiplied by
#'   `g_mms2`). Practical displacement records are virtually always in
#'   `"mm"`, `"cm"` or `"m"`; the acceleration-flavoured entries are
#'   accepted for symmetry with `.getSF()` but are unusual here. If
#'   different from `units.target`, a scale factor is applied per
#'   channel.
#' @param time character. Name of the time column in the input (default `"t"`).
#'   Internally and in `TSL` output, time is canonicalized to `t`.
#' @param Fmax numeric. Maximum frequency of interest (Hz). Guides STFT strategy
#'   and low-pass regularization during integration.
#' @param kNyq numeric. Target Nyquist multiplier (`Fs_target ~= kNyq * Fmax`)
#'   when forced by the user. Otherwise an automatic grid is searched.
#' @param resample logical. Kept for compatibility; decision is made by the
#'   internal STFT strategy.
#' @param derivate character. Derivative method for `DT -> VT / AT`
#'   (`"time"` or `"freq"`).
#' @param units.target character. Target units for acceleration-related outputs.
#' @param NW integer. Nominal STFT window length (samples). May be adjusted.
#' @param OVLP numeric. Window overlap percent.
#' @param flatZeros logical. Apply edge tapering; if `isRaw = TRUE`, tapering is
#'   applied regardless.
#' @param Astop0,Apass0 numeric. Normalized thresholds `0..1` for taper/flatten; relative to the per-channel max amplitude.
#' @param AstopLP,ApassLP numeric. Anti-alias LP specs for resampling.
#' @param trimZeros logical. If `TRUE`, trims leading/trailing zeros by the final window.
#' @param detrend logical. Remove mean before/after stages.
#' @param regularize logical. Force time regularization of input if needed.
#' @param output character. Early/short-circuit outputs (default: "TSL"): "DTo", "AT", "VT",
#'   "DT", "TSW", "TSL".
#' @param verbose logical. Print diagnostic logs.
#' @param isRaw logical. If `TRUE`, handle unit conversion and default tapering.
#' @param audit logical. If `TRUE`, runs `auditSTFT()` to validate STFT/resampling
#'   strategy and emit warnings for risky configurations. Default: `TRUE`.
#' @param lowPass logical. If `TRUE`, multiply the spectral derivative kernel by
#'   an additional Butterworth-like low-pass at `Fmax` to suppress high-frequency
#'   amplification of the numerical derivative. Default: `TRUE`.
#'
#' @return Returns the requested object based on `output`.
#'
#' @examples
#' t <- seq(0, 2, by = 0.02)
#' x <- data.table::data.table(
#'   t = t,
#'   H1 = sin(2 * pi * t),
#'   H2 = 0.5 * cos(2 * pi * t),
#'   UP = 0.25 * sin(4 * pi * t)
#' )
#' tsl <- DT2TS(x, units.source = "mm", Fmax = 4, NW = 16,
#'              audit = FALSE, isRaw = FALSE)
#' head(tsl)
#'
#' @export


DT2TS <- function(
  .x, units.source,
  time = "t",
  Fmax = 16,
  kNyq = 3.125, #>2.5
  resample = TRUE,
  derivate = "freq",  # "time" or "freq" for derivatives
  units.target = "mm",
  NW = 128,
  OVLP = 75,
  flatZeros = FALSE,
  Astop0 = 1e-4,
  Apass0 = 1e-3,
  AstopLP = 1e-3,
  ApassLP = 0.98,
  trimZeros = FALSE,
  detrend = FALSE,
  regularize = FALSE,
  output = "TSL",
  verbose = FALSE,
  audit = TRUE,
  isRaw = TRUE,
  lowPass = TRUE) {
  . <- NULL
  # Set package-scoped verbose flag for helpers
  .verbose.set(verbose)
  stopifnot(is.data.table(.x))
  stopifnot(nrow(.x) > 2L)

  X <- copy(as.data.table(.x))
  X <- .canonicalizeTimeColumn(X, time = time)

  NP <- nrow(X)
  if (NW > (NP / 2)) {
    NW.lower <- 2^floor(log2(NP / 2))
    NW.upper <- 2^ceiling(log2(NP / 2))
    if (abs(NW.lower - (NP / 2)) < abs(NW.upper - (NP / 2))) {
      NW <- min(as.integer(NW.lower), NP)
    } else {
      NW <- min(as.integer(NW.upper), NP)
    }
  }


  t <- X[["t"]]
  if (!is.numeric(t) || any(!is.finite(t))) {
    stop("DT2TS requires a finite numeric time column.", call. = FALSE)
  }
  SIGNAL <- setdiff(names(X), "t")
  BAD <- vapply(X[, SIGNAL, with = FALSE], function(z) any(!is.finite(z)),
                logical(1L))
  if (any(BAD) && !identical(output, "DTo")) {
    stop(sprintf(
      "DT2TS requires finite signal columns; non-finite columns: %s.",
      paste(names(BAD)[BAD], collapse = ", ")
    ), call. = FALSE)
  }
  dt <- unique(diff(t))
  if ((isRaw || regularize) ||
    length(dt) > 1 ||
    (length(dt) == 1 && !isTRUE(all.equal(dt, .rationalize(dt))))) {
    if (verbose) cat("Regularizing time grid...\n")
    X <- .regularize(X, time = "t")
    t <- X[["t"]]
    dt <- t[2] - t[1]
  }

  # Remove time column to keep only signal columns
  X[, t := NULL]

  t0 <- min(t)
  t1 <- max(t)
  n <- floor((t1 - t0) / dt + 1e-12)
  ts <- (0:n) * dt
  Fs <- 1 / dt
  stopifnot(isTRUE(all.equal(Fs, round(Fs))))

  ## Scale Units  ----
  SFU <- 1

  # Unit conversion only for raw data.
  # Canonical Units only (see R/validateUnits.R). Verbose forms rejected;
  # "g"/"gal" rejected because DT is displacement (use AT2TS for acceleration).
  if (isRaw == TRUE) {
    units.source <- .validateUnits(units.source, kind = "DT")
    if (units.source != units.target) {
      SFU <- .getSF(SourceUnits = units.source, TargetUnits = units.target)
      OCID <- names(X)
      X <- X[, .(sapply(.SD, function(x) {
        x * SFU
      }))]
      names(X) <- OCID
    }
  }

  # For processed data, assume units are already correct
  if (isRaw == FALSE) {
    units.source <- units.target  # Assume already in target units
  }

  OCID <- names(X)

  # Create initial record
  DTo <- data.table(ts = ts, Units = units.target, X)
  if (!is.null(output) && output == "DTo") {
    return(DTo)
  }

  # Detrend DT initially
  if (detrend == TRUE) {
    OCID <- names(X)
    X <- X[, .(sapply(.SD, function(x) {
      x - mean(x)
    }))]
    names(X) <- OCID
  }

  ## STFT Strategy and Resample ----
  # Get optimal STFT parameters
  HasKNyq <- "kNyq" %in% names(as.list(match.call()))
  if (HasKNyq) {
    STFTParams <- .setSTFT(NP = NP, Fs = Fs, Fmax = Fmax, kNyq = kNyq,
      NW.min = NW, MW.min = 16, OVLP = OVLP)
  } else {
    STFTParams <- .setSTFT(NP = NP, Fs = Fs, Fmax = Fmax,
      NW.min = NW, MW.min = 16, OVLP = OVLP)
  }

  # Extract optimal parameters
  NW <- STFTParams$NW
  OVLP <- STFTParams$OVLP
  TargetFs <- as.integer(round(STFTParams$Fs))

  # Audit STFT and parameters (pre-resample)
  if (audit) {
    AuditSignal <- as.numeric(X[[1]])
    DT_audit <- data.table(t = t, s = AuditSignal)
    HasKNyq <- "kNyq" %in% names(as.list(match.call()))
    aud <- try(
      if (HasKNyq) {
        auditSTFT(DT_audit, Fmax = Fmax, kNyq = kNyq, NW.min = NW, MW.min = 16, OVLP = OVLP,
          ApassLP = ApassLP, AstopLP = AstopLP)
      } else {
        auditSTFT(DT_audit, Fmax = Fmax, NW.min = NW, MW.min = 16, OVLP = OVLP,
          ApassLP = ApassLP, AstopLP = AstopLP)
      }, silent = TRUE)
    if (!inherits(aud, "try-error") && length(aud$warnings)) {
      warning(paste(aud$warnings, collapse = "\n"))
      if (verbose) {
        cat("STFT audit warnings:\n")
        cat(paste0(" - ", aud$warnings, collapse = "\n"), "\n")
      }
    }
  }

  # Apply resampling if strategy indicates it's needed
  if (STFTParams$Resample) {
    OCID <- names(X)
    # Use original dt for anti-alias filtering
    X <- .resample(X, dt = (1 / Fs), TargetFs = TargetFs, Fmax = Fmax,
      NW = NW, OVLP = OVLP, Apass = ApassLP, Astop = AstopLP)
    names(X) <- OCID
    Fs <- TargetFs
    dt <- 1 / Fs
  }


  ## Initial Taper for DT ----
  # Generate initial taper window
  if (isRaw || flatZeros) {
    # When `flatZeros` (or `isRaw`) is on, the edge window is a
    # `.taperA` amplitude-threshold mask using `(Astop0, Apass0)`. A
    # separate FlattenZeros helper was planned earlier but never
    # implemented; the taper is the active edge stage.
    Wo <- X[, .(sapply(.SD, function(ts) {
      .taperA(ts, Astop = Astop0, Apass = Apass0)
    }))]
  } else {
    Wo <- X[, .(sapply(.SD, function(ts) {
      rep(1, length(ts))
    }))]
  }

  OCID <- names(X)
  DT <- X[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]
  names(DT) <- OCID

  if (detrend == TRUE) {
    DT <- DT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(DT) <- OCID
  }

  ## Derivate DT -> VT -> AT ----
  # DT -> VT Derivation
  VT <- DT[, lapply(.SD, function(v) {
    .derivate(.x = v, dt = dt, method = derivate, NW = NW, OVLP = OVLP,
              Fmax = Fmax, ApassLP = ApassLP, AstopLP = AstopLP, LowPass = lowPass)
  })]

  if (detrend == TRUE) {
    VT <- VT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(VT) <- OCID
  } else {
    names(VT) <- OCID
  }

  # VT -> AT Derivation
  AT <- VT[, lapply(.SD, function(v) {
    .derivate(.x = v, dt = dt, method = derivate, NW = NW, OVLP = OVLP,
              Fmax = Fmax, ApassLP = ApassLP, AstopLP = AstopLP, LowPass = lowPass)
  })]

  if (detrend == TRUE) {
    AT <- AT[, .(sapply(.SD, function(ts) { ts - mean(ts) }))]
    names(AT) <- OCID
  } else {
    names(AT) <- OCID
  }

  ## Final Taper Zeros (based on final AT) ----
  # Generate final taper window based on AT
  if (isRaw || flatZeros) {
    # Final-stage edge mask: same `.taperA` shape applied to the AT
    # output after differentiation/integration. `FlattenZeros` was a
    # planned helper that never landed; the taper is the active
    # implementation.
    Wo <- AT[, .(sapply(.SD, function(ts) {
      .taperA(ts, Astop = Astop0, Apass = Apass0)
    }))]
  } else {
    Wo <- AT[, .(sapply(.SD, function(ts) {
      rep(1, length(ts))
    }))]
  }

  AT <- AT[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]
  names(AT) <- OCID

  VT <- VT[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]
  names(VT) <- OCID

  DT <- DT[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]
  names(DT) <- OCID

  # Trim Zeros
  if (trimZeros) {
    idx <- apply(Wo != 0, MARGIN = 1, function(x) {
      all(x)
    })
    AT <- AT[idx]
    VT <- VT[idx]
    DT <- DT[idx]
  }

  # Fix trend
  if (detrend == TRUE) {
    AT <- AT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    VT <- VT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    DT <- DT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]

    names(AT) <- OCID
    names(VT) <- OCID
    names(DT) <- OCID
  }


  names(AT) <- OCID
  names(VT) <- OCID
  names(DT) <- OCID

  if (identical(output, "AT")) {
    return(AT)
  }
  if (identical(output, "VT")) {
    return(VT)
  }
  if (identical(output, "DT")) {
    return(DT)
  }

  ## Summary ----
  NP <- nrow(AT)
  ts <- seq(0, dt * (NP - 1), dt)
  Fs <- 1 / dt
  df <- Fs / NW
  fs <- seq(from = 0, by = df, length.out = NW / 2)

  ## Pack Time Series  ----
  TSW <- data.table(ts = ts, AT = AT, VT = VT, DT = DT)
  if (identical(output, "TSW")) {
    return(TSW)
  }

  id.vars <- c("ts")
  measure.vars <- colnames(TSW[, -c("ts")])
  AUX <- data.table::melt(TSW, id.vars = id.vars, measure.vars = measure.vars)
  BAD <- !is.finite(AUX[["value"]])
  if (any(BAD)) {
    COUNT <- AUX[BAD, .N, by = "variable"]
    MSG <- paste(
      sprintf("%s=%d", COUNT[["variable"]], COUNT[["N"]]),
      collapse = ", "
    )
    stop(sprintf(
      "DT2TS TSL output requires finite samples; non-finite values by variable: %s.",
      MSG
    ), call. = FALSE)
  }
  TSL <- AUX[, .(t = ts, s = value, ID = gsub("\\..*$", "", variable), OCID = gsub("^[^.]*\\.", "", variable))]
  if (identical(output, "TSL")) {
    return(TSL)
  }

  ## Return ----
  stop("Invalid output. Must be one of 'DTo','AT','VT','DT','TSW','TSL'.")
}
