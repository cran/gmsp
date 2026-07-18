#' Convert acceleration time series into AT/VT/DT bundles
#'
#' @description
#' End-to-end workflow that takes acceleration time histories and produces a
#' consistent set of acceleration, velocity, and displacement time series.
#' It optionally regularizes sampling, converts units (for raw data), selects
#' optimal STFT parameters and resampling strategy, applies robust edge tapering,
#' performs spectral-domain integration, and provides post-tapering/optional
#' trimming.
#'
#' The function is designed for seismic/structural records but is agnostic to
#' the physical origin provided `Fmax` reflects the analysis band of interest.
#'
#' @param .x data.table. Input acceleration records with a time column and one or
#'   more signal columns (e.g., H1, H2, V).
#' @param units.source character. Source units for input acceleration when
#'   `isRaw = TRUE`. Supported: "mm", "cm", "m", "gal", "g". If different
#'   from `units.target`, a scale factor is applied per channel.
#' @param time character. Name of the time column in the input (default `"t"`).
#'   Internally and in `TSL` output, time is canonicalized to `t`.
#' @param Fmax numeric. Maximum frequency of interest (Hz). Used to set STFT
#'   strategy and low-pass regularization in integration. Default: `16`.
#' @param kNyq numeric. Target Nyquist multiplier (`Fs_target ~= kNyq * Fmax`)
#'   if the user forces it. If not provided, an automatic grid is searched.
#'   Default: `3.125`.
#' @param resample logical. Kept for compatibility; the actual decision is made by
#'   the internal STFT strategy based on `Fmax` and constraints. Default:
#'   `TRUE`.
#' @param units.target character. Output target units for acceleration records.
#'   Default: "mm".
#' @param NW integer. Nominal STFT window length (samples); may be adjusted by the
#'   strategy. Default: `128`.
#' @param OVLP numeric. Window overlap percent. Default: `75`.
#' @param flatZeros logical. If `TRUE`, flatten external low-amplitude tails to
#'   exact zeros using per-channel normalized acceleration amplitude before
#'   optional trimming. If `isRaw = TRUE`, the historical smooth taper is still
#'   applied even when `flatZeros = FALSE`. Default: `FALSE`.
#' @param Astop0 numeric. Normalized stop threshold `0..1` for taper/flatten;
#'   relative to per-channel max amplitude. `Astop0 = 1e-3` is equivalent to
#'   threshold `10` after normalizing PGA to `10000`. Default: `1e-3`.
#' @param Apass0 numeric. Normalized pass threshold `0..1` for taper/flatten; relative to per-channel max amplitude. Default: `1e-3`.
#' @param AstopLP numeric. Stopband attenuation for anti-alias LP (resampling).
#'   Default: `1e-3`.
#' @param ApassLP numeric. Passband for anti-alias LP (resampling). Default: `0.98`.
#' @param trimZeros logical. If `TRUE`, trims rows outside the final nonzero
#'   window support after taper/flatten. Multi-component records use the union
#'   of component supports. Default: `FALSE`.
#' @param detrend logical. Remove mean before/after each main stage. Default: `FALSE`.
#' @param regularize logical. Force time regularization of input if needed.
#'   Default: `FALSE`.
#' @param output character. Short-circuit outputs (default: "TSL"):
#'   "ATo": early wide-frame after units; "AT"/"VT"/"DT": final wide;
#'   "TSW": combined wide; "TSL": long table.
#' @param verbose logical. Print diagnostic logs. Default: `FALSE`.
#' @param isRaw logical. If `TRUE`, perform unit conversion and
#'   robust pre/post tapering by default. If `FALSE`, the input is
#'   assumed to be already in `units.target` and the `units.source` argument is
#'   silently overridden to match; no scale factor is applied. Default:
#'   `TRUE`.
#' @param audit logical. If `TRUE`, runs `auditSTFT()` to validate STFT/resampling
#'   strategy and emit warnings for risky configurations. Default: `TRUE`.
#'
#' @return Returns the requested object based on `output` (no other
#'   element is returned alongside it):
#'   - `"ATo"`: wide table with `ts` (time starting at 0), `Units` and
#'     channels, before any tapering or integration.
#'   - `"AT"` / `"VT"` / `"DT"`: wide table with the channels only (no
#'     `ts` column).
#'   - `"TSW"`: wide table with columns `ts`, `AT.<OCID>`, `VT.<OCID>`,
#'     `DT.<OCID>`.
#'   - `"TSL"` (default): long table with columns `t`, `s`, `ID`
#'     (one of `"AT"`, `"VT"`, `"DT"`), and `OCID`.
#'   Sampling interval and row count are not returned separately; recover
#'   them from the output via `1 / diff(ts)[1]` and `nrow(.)`.
#'
#' @examples
#' t <- seq(0, 2, by = 0.02)
#' x <- data.table::data.table(
#'   t = t,
#'   H1 = sin(2 * pi * t),
#'   H2 = 0.5 * cos(2 * pi * t),
#'   UP = 0.25 * sin(4 * pi * t)
#' )
#' tsl <- AT2TS(x, units.source = "mm", Fmax = 4, NW = 16,
#'              audit = FALSE, isRaw = FALSE)
#' head(tsl)
#'
#' @export


AT2TS <- function(
  .x, units.source,
  time = "t",
  Fmax = 16,
  kNyq = 3.125, #>2.5
  resample = TRUE,
  units.target = "mm",
  NW = 128,
  OVLP = 75,
  flatZeros = FALSE,
  Astop0 = 1e-3,
  Apass0 = 1e-3,
  AstopLP = 1e-3,
  ApassLP = 0.98,
  trimZeros = FALSE,
  detrend = FALSE,
  regularize = FALSE,
  output = "TSL",
  verbose = FALSE,
  audit = TRUE,
  isRaw = TRUE)   {
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
    stop("AT2TS requires a finite numeric time column.", call. = FALSE)
  }
  SIGNAL <- setdiff(names(X), "t")
  BAD <- vapply(X[, SIGNAL, with = FALSE], function(z) any(!is.finite(z)),
                logical(1L))
  if (any(BAD) && !identical(output, "ATo")) {
    stop(sprintf(
      "AT2TS requires finite signal columns; non-finite columns: %s.",
      paste(names(BAD)[BAD], collapse = ", ")
    ), call. = FALSE)
  }
  dt <- unique(diff(t))
  if ((isRaw || regularize) ||
    length(dt) > 1 ||
    (length(dt) == 1 && !isTRUE(all.equal(dt, .rationalize(dt))))) {
    X <- .regularize(X, time = "t")
    t <- X[["t"]]
    dt <- t[2] - t[1]
  }

  # Remove time column to keep only signal columns
  X[, t := NULL]
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
  # "mm/s2" or "m/s2" are rejected; KIND is implicit in the function (AT).
  if (isRaw == TRUE) {
    units.source <- .validateUnits(units.source, kind = "AT")
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
  ATo <- data.table(ts = ts, Units = units.target, X)
  if (!is.null(output) && output == "ATo") {
    return(ATo)
  }

  BAD <- vapply(X, function(z) any(!is.finite(z)), logical(1L))
  if (any(BAD)) {
    stop(sprintf(
      "AT2TS requires finite signal columns; non-finite columns: %s.",
      paste(names(BAD)[BAD], collapse = ", ")
    ), call. = FALSE)
  }

  # Detrend
  if (detrend == TRUE) {
    OCID <- names(X)
    X <- X[, .(sapply(.SD, function(x) {
      x - mean(x)
    }))]
    names(X) <- OCID
  }

  ## STFT Strategy and Resample ----
  # Get optimal STFT parameters
  if (missing(kNyq)) {
    STFTParams <- .setSTFT(NP = NP, Fs = Fs, Fmax = Fmax,
      NW.min = NW, MW.min = 16, OVLP = OVLP)
  } else {
    STFTParams <- .setSTFT(NP = NP, Fs = Fs, Fmax = Fmax, kNyq = kNyq,
      NW.min = NW, MW.min = 16, OVLP = OVLP)
  }

  # Extract optimal parameters
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
    OCID <- names(X)
    # Use original dt for anti-alias filter (1/Fs before resampling)
    X <- .resample(X = X, dt = (1 / Fs), TargetFs = TargetFs, Fmax = Fmax,
      NW = NW, OVLP = OVLP, Apass = ApassLP, Astop = AstopLP)
    names(X) <- OCID
    Fs <- TargetFs
    dt <- 1 / Fs
  }


  ## Case #2. Acceleration Time Histories ----
  # Generate initial taper window
  if (isRaw || flatZeros) {
    # `isRaw` keeps the historical smooth taper. `flatZeros` adds an exact
    # support mask based on amplitude normalized by each component peak.
    Wo <- X[, .(sapply(.SD, function(ts) {
      w <- .taperA(ts, Astop = Astop0, Apass = Apass0)
      if (flatZeros) w <- w * .flattenA(ts, Astop = Astop0)
      w
    }))]
  } else {
    Wo <- X[, .(sapply(.SD, function(ts) {
      rep(1, length(ts))
    }))]
  }

  AT <- X[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]

  names(AT) <- OCID

  # AT -> VT Integration
  VT <- AT[, lapply(.SD, function(v) {
    .integrate(.x = v, dt = dt, NW = NW, OVLP = OVLP, Fmax = Fmax)
  })]

  if (nrow(VT) > nrow(Wo)) {
    AUX <- data.table(sapply(Wo, function(w) rep(0, nrow(VT) - nrow(Wo))))
    Wo <- rbind(Wo, AUX)
  }

  VT <- VT[, lapply(seq_along(.SD), function(i) {
    .SD[[i]] * Wo[[i]]
  })]
  names(VT) <- OCID

  if (detrend == TRUE) {
    VT <- VT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(VT) <- OCID
  }

  # VT -> DT Integration
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

  if (detrend == TRUE) {
    DT <- DT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(DT) <- OCID
  }

  # Final detrending if requested
  if (detrend == TRUE) {
    AT <- AT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(AT) <- OCID

    VT <- VT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(VT) <- OCID

    DT <- DT[, .(sapply(.SD, function(ts) {
      ts - mean(ts)
    }))]
    names(DT) <- OCID
  }

  ## Final Taper Zeros ----
  # Generate final taper window based on AT
  if (isRaw || flatZeros) {
    # Final-stage edge mask: keep raw taper behavior, and when requested
    # flatten the normalized-amplitude tails to exact zeros before trimming.
    Wo <- AT[, .(sapply(.SD, function(ts) {
      w <- .taperA(ts, Astop = Astop0, Apass = Apass0)
      if (flatZeros) w <- w * .flattenA(ts, Astop = Astop0)
      w
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
      any(x)
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
      "AT2TS TSL output requires finite samples; non-finite values by variable: %s.",
      AUX[!is.finite(value), .N, by = variable][
        , paste(sprintf("%s=%d", variable, N), collapse = ", ")]
    ), call. = FALSE)
  }
  TSL <- AUX[, .(t = ts, s = value, ID = gsub("\\..*$", "", variable), OCID = gsub("^[^.]*\\.", "", variable))]

  if (identical(output, "TSL")) {
    return(TSL)
  }

  ## Return ----
  stop("Invalid output. Must be one of 'ATo','AT','VT','DT','TSW','TSL'.")
}
