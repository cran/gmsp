#' Convert velocity time series into AT/VT/DT bundles
#'
#' @description
#' End-to-end workflow that takes velocity time histories and produces a
#' consistent set of acceleration, velocity, and displacement time series.
#' It optionally regularizes sampling, converts units (for raw data), selects
#' optimal STFT parameters and resampling strategy, applies robust edge tapering,
#' performs spectral-domain derivation and integration, yields post-tapering and
#' optional trimming.
#'
#' @param .x data.table. Input velocity records with a time column and one or
#'   more signal columns.
#' @param units.source character. Source units for input velocity when `isRaw = TRUE`.
#'   Supported: "mm", "cm", "m", "gal", "g" (interpreted for acceleration scaling
#'   on derivative/integral paths). If different from `units.target`, a scale factor
#'   is applied per channel.
#' @param time character. Name of the time column in the input (default `"t"`).
#'   Internally and in `TSL` output, time is canonicalized to `t`.
#' @param Fmax numeric. Maximum frequency of interest (Hz). Guides STFT strategy.
#' @param kNyq numeric. Target Nyquist multiplier (`Fs_target ~= kNyq * Fmax`)
#'   if the user forces it; otherwise an automatic grid is searched.
#' @param resample logical. Kept for compatibility; the actual decision is made by
#'   the internal STFT strategy based on `Fmax` and constraints.
#' @param derivate character. Derivative method for `VT -> AT`
#'   (`"time"` or `"freq"`).
#' @param units.target character. Target units for output acceleration. Default: "mm".
#' @param NW integer. Nominal STFT window length (samples). May be adjusted.
#' @param OVLP numeric. Window overlap percent.
#' @param flatZeros logical. Apply edge tapering. If `isRaw = TRUE`, tapering is
#'   applied regardless.
#' @param Astop0,Apass0 numeric. Normalized thresholds `0..1` for taper/flatten; relative to the per-channel max amplitude.
#' @param AstopLP,ApassLP numeric. Anti-alias LP specifications for resampling.
#' @param trimZeros logical. If `TRUE`, trims leading/trailing zeros by the final window.
#' @param detrend logical. Remove mean before/after stages.
#' @param regularize logical. Force time regularization of input if needed.
#' @param verbose logical. Print diagnostic logs.
#' @param output character. Early/short-circuit outputs (default: "TSL"): "VTo", "AT", "VT",
#'   "DT", "TSW", "TSL".
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
#' tsl <- VT2TS(x, units.source = "mm", Fmax = 4, NW = 16,
#'              audit = FALSE, isRaw = FALSE)
#' head(tsl)
#'
#' @export


VT2TS <- function(
  .x, units.source,
  time = "t",
  Fmax = 16,
  kNyq = 3.125, #>2.5
  resample = TRUE,
  derivate = "freq",  # "time" or "freq"
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
  verbose = FALSE,
  audit = TRUE,
  output = "TSL",
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

  # === LOGGING: Initial state ===
  if (verbose) {
    cat("\n=== VT2TS PROCESSING START ===\n")
    cat(sprintf("Initial: NP=%d\n", NP))
    cat(sprintf("FLAGS: isRaw=%s resample=%s flatZeros=%s detrend=%s\n",
      isRaw, resample, flatZeros, detrend))
    cat(sprintf("PARAMS: Fmax=%.0f NW=%d OVLP=%.0f\n", Fmax, NW, OVLP))
  }

  t <- X[["t"]]
  if (!is.numeric(t) || any(!is.finite(t))) {
    stop("VT2TS requires a finite numeric time column.", call. = FALSE)
  }
  SIGNAL <- setdiff(names(X), "t")
  BAD <- vapply(X[, SIGNAL, with = FALSE], function(z) any(!is.finite(z)),
                logical(1L))
  if (any(BAD) && !identical(output, "VTo")) {
    stop(sprintf(
      "VT2TS requires finite signal columns; non-finite columns: %s.",
      paste(names(BAD)[BAD], collapse = ", ")
    ), call. = FALSE)
  }
  dt <- unique(diff(t))

  # === LOGGING: Original sampling ===
  if (verbose) cat(sprintf("Original: dt=%.2e Fs=%.0f\n", dt[1], 1 / dt[1]))

  if ((isRaw || regularize) ||
    length(dt) > 1 ||
    (length(dt) == 1 && !isTRUE(all.equal(dt, .rationalize(dt))))) {
    if (verbose) cat("Regularizing time grid...\n")
    X <- .regularize(X, time = "t")
    t <- X[["t"]]
    dt <- t[2] - t[1]
    if (verbose) cat(sprintf("Regularized: dt=%.2e Fs=%.0f\n", dt, 1 / dt))
  }

  # Remove time column to keep only signal columns
  X[, t := NULL]

  # === LOGGING: Initial amplitude ===
  if (verbose) cat(sprintf("Initial Vo=%.3e\n", max(abs(as.matrix(X)), na.rm = TRUE)))

  # build Time vector
  t0 <- min(t)
  t1 <- max(t)
  n <- floor((t1 - t0) / dt + 1e-12)
  ts <- (0:n) * dt
  Fs <- 1 / dt
  stopifnot(isTRUE(all.equal(Fs, round(Fs))))

  ## Scale Units  ----
  SFU <- 1

  # Unit conversion only for raw data.
  # Canonical Units only (see R/validateUnits.R). Verbose forms like
  # "mm/s" are rejected; "g"/"gal" are rejected because VT is velocity
  # (use AT2TS for acceleration sources).
  if (isRaw == TRUE) {
    units.source <- .validateUnits(units.source, kind = "VT")
    SFU <- .getSF(SourceUnits = units.source, TargetUnits = units.target)
    if (verbose) cat(sprintf("Unit conversion: %s->%s (SFU=%.1e)\n", units.source, units.target, SFU))

    if (units.source != units.target) {
      OCID <- names(X)
      X <- X[, .(sapply(.SD, function(x) {
        x * SFU
      }))]
      names(X) <- OCID
      if (verbose) cat(sprintf("After units: Vo=%.3e\n",
        max(abs(as.matrix(X)), na.rm = TRUE)))
    }
  }

  # For processed data, assume units are already correct
  if (isRaw == FALSE) {
    units.source <- units.target  # Assume already in target units
  }

  OCID <- names(X)

  # Create initial record
  VTo <- data.table(ts = ts, Units = units.target, X)
  if (!is.null(output) && output == "VTo") {
    return(VTo)
  }

  # Detrend VT initially
  if (detrend == TRUE) {
    OCID <- names(X)
    X <- X[, .(sapply(.SD, function(x) {
      x - mean(x)
    }))]
    names(X) <- OCID
  }

  ## STFT Strategy and Resample ----
  # Get optimal STFT parameters
  if (verbose) cat(sprintf("\n=== STFT STRATEGY (resample=%s) ===\n", resample))
  if (missing(kNyq)) {
    STFTParams <- .setSTFT(NP = NP, Fs = Fs, Fmax = Fmax,
      NW.min = NW, MW.min = 16, OVLP = OVLP)
  } else {
    STFTParams <- .setSTFT(NP = NP, Fs = Fs, Fmax = Fmax, kNyq = kNyq,
      NW.min = NW, MW.min = 16, OVLP = OVLP)
  }

  # Extract optimal parameters
  if (verbose) {
    cat(sprintf("Strategy: %s\n", STFTParams$strategy))
    cat(sprintf("Before: NW=%d OVLP=%.0f Fs=%.0f\n", NW, OVLP, Fs))
  }
  NW <- STFTParams$NW
  OVLP <- STFTParams$OVLP
  TargetFs <- as.integer(round(STFTParams$Fs))

  # Audit STFT and parameters (pre-resample)
  if (audit) {
    aud <- try(
      if (missing(kNyq)) {
        auditSTFT(data.table(t = t, s = as.numeric(X[[1]])), Fmax = Fmax,
          NW.min = NW, MW.min = 16, OVLP = OVLP, ApassLP = ApassLP,
          AstopLP = AstopLP)
      } else {
        auditSTFT(data.table(t = t, s = as.numeric(X[[1]])), Fmax = Fmax,
          kNyq = kNyq, NW.min = NW, MW.min = 16, OVLP = OVLP,
          ApassLP = ApassLP, AstopLP = AstopLP)
      },
      silent = TRUE)
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
    if (verbose) cat(sprintf("Resampling %.0f->%.0f Hz (factor=%.2f)\n", Fs, TargetFs, Fs / TargetFs))
    OCID <- names(X)
    # Use original dt for anti-alias filter
    X <- .resample(X = X, dt = (1 / Fs), TargetFs = TargetFs, Fmax = Fmax,
      NW = NW, OVLP = OVLP, Apass = ApassLP, Astop = AstopLP)
    names(X) <- OCID
    Fs <- TargetFs
    dt <- 1 / Fs
    if (verbose) cat(sprintf("After resample: Vo=%.3e\n",
      max(abs(as.matrix(X)), na.rm = TRUE)))
  } else {
    if (verbose) cat("No resampling needed\n")
  }

  ## Case #2. VT -> AT Derivation ----
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

  VT <- X[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]
  names(VT) <- OCID

  # VT -> AT Derivation
  if (verbose) {
    cat(sprintf("\n=== VT->AT DERIVATION ===\n"))
    cat(sprintf("Using method=%s dt=%.2e\n", derivate, dt))
    Vo <- max(abs(as.matrix(VT)), na.rm = TRUE)
    cat(sprintf("Before derivation Vo=%.3e\n", Vo))
  }

  AT <- VT[, lapply(.SD, function(v) {
    .derivate(.x = v, dt = dt, method = derivate, NW = NW, OVLP = OVLP,
              Fmax = Fmax, ApassLP = ApassLP, AstopLP = AstopLP, LowPass = lowPass)
  })]
  names(AT) <- OCID

  if (verbose) {
    Ao <- max(abs(as.matrix(AT)), na.rm = TRUE)
    cat(sprintf("After derivation Ao=%.3e\n", Ao))
  }

  if (detrend == TRUE) {
    AT <- AT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(AT) <- OCID
  }

  # VT -> DT Integration
  if (verbose) {
    cat(sprintf("\n=== VT->DT INTEGRATION ===\n"))
    cat(sprintf("Using dt=%.2e NW=%d OVLP=%.0f\n", dt, NW, OVLP))
    Vo <- max(abs(as.matrix(VT)), na.rm = TRUE)
    cat(sprintf("Before integration Vo=%.3e\n", Vo))
  }

  DT <- VT[, lapply(.SD, function(d) {
    .integrate(.x = d, dt = dt, NW = NW, OVLP = OVLP, Fmax = Fmax)
  })]

  if (nrow(DT) > nrow(Wo)) {
    AUX <- data.table(sapply(Wo, function(w) rep(0, nrow(DT) - nrow(Wo))))
    Wo <- rbind(Wo, AUX)
  }

  DT <- DT[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]
  names(DT) <- OCID

  if (verbose) {
    Do <- max(abs(as.matrix(DT)), na.rm = TRUE)
    cat(sprintf("After integration Do=%.3e\n", Do))
  }

  if (detrend == TRUE) {
    DT <- DT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(DT) <- OCID
  }

  ## Final Taper Zeros ----
  # Generate final taper window based on AT
  if (isRaw || flatZeros) {
    # Final-stage edge mask: same `.taperA` shape applied to the AT
    # output after derivation. `FlattenZeros` was a planned helper
    # that never landed; the taper is the active implementation.
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

  # === LOGGING: Final amplitude ===
  Ao <- max(abs(as.matrix(AT)), na.rm = TRUE)
  if (verbose) cat(sprintf("\n=== FINAL RESULTS ===\n"))
  if (verbose) cat(sprintf("Final Ao=%.3e\n", Ao))

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

  ## Pack Time Series  ----
  ts <- seq(0, dt * (nrow(AT) - 1), dt)
  TSW <- data.table(ts = ts, AT = AT, VT = VT, DT = DT)
  if (identical(output, "TSW")) {
    return(TSW)
  }

  AUX <- data.table::melt(TSW, id.vars = "ts",
                          measure.vars = setdiff(names(TSW), "ts"))
  if (nrow(AUX[!is.finite(value)])) {
    stop(sprintf(
      "VT2TS TSL output requires finite samples; non-finite values by variable: %s.",
      AUX[!is.finite(value), .N, by = variable][
        , paste(sprintf("%s=%d", variable, N), collapse = ", ")]
    ), call. = FALSE)
  }
  TSL <- AUX[, .(t = ts, s = value, ID = gsub("\\..*$", "", variable), OCID = gsub("^[^.]*\\.", "", variable))]

  if (identical(output, "TSL")) {
    return(TSL)
  }

  ## Return ----
  stop("Invalid output. Must be one of 'VTo','AT','VT','DT','TSW','TSL'.")
}
