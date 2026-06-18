.derivate <- function(.x, dt, method = "time", NW, OVLP,
                       Fmax = NULL, ApassLP = NULL, AstopLP = NULL,
                       LowPass = TRUE) {
  # Vector column helper - processes single time series vector
  # method: "time" for temporal differences, "freq" for frequency domain
  stopifnot(is.vector(.x))

  # Common scaling setup (RMS-based)
  n <- length(.x)
  stopifnot(n > 4)
  stopifnot(tolower(method) %in% c("time", "freq"))

  Xo <- max(.getRMS(.x), .Machine$double.eps)

  if (method == "time") {
    # ========================================================================
    # TEMPORAL DERIVATION (finite differences) with RMS normalization
    # ========================================================================
    xw <- .x / Xo
    X <- numeric(n)
    # Four-point central difference for the bulk. (dt constant)
    for (i in 3:(n - 2)) {
      X[i] <- (-xw[i + 2] + 8 * xw[i + 1] - 8 * xw[i - 1] + xw[i - 2]) / (12 * dt)
    }

    # Three-point differences for the edges. (dt constant)
    X[1] <- (-3 * xw[1] + 4 * xw[2] - xw[3]) / (2 * dt)
    X[2] <- (-xw[4] + 4 * xw[3] - 3 * xw[2]) / (2 * dt)
    X[n - 1] <- (3 * xw[n - 1] - 4 * xw[n - 2] + xw[n - 3]) / (2 * dt)
    X[n] <- (3 * xw[n] - 4 * xw[n - 1] + xw[n - 2]) / (2 * dt)

  }
  if (method == "freq") {
    # ========================================================================
    # FREQUENCY DOMAIN DERIVATION (with HD filters) with RMS normalization
    # ========================================================================
    # === LOGGING: Derivation setup ===

if (.verbose()) cat(sprintf("  .derivate: Xo=%.3e dt=%.2e NW=%d method=%s\n", Xo, dt, NW, method))

    # Build filter
    Fs <- 1 / dt
    stopifnot(isTRUE(all.equal(Fs, round(Fs))))
    df <- Fs / NW
    fs <- seq(from = 0, by = df, length.out = ceiling(NW / 2))
if (.verbose()) cat(sprintf("  Filter: Fs=%.0f df=%.3f freq_range=[%.3f-%.3f]\n", Fs, df, min(fs), max(fs)))

    HD <- .buildDerivateFilter(f = fs) ## Derivate Filter
if (.verbose()) cat(sprintf("  DerivateFilter: max=%.3e\n", max(abs(HD), na.rm = TRUE)))

    # Optional LP for derivation (HD * LP)
    custom <- HD
    if (isTRUE(LowPass) && !is.null(Fmax)) {
      # Design LP as in .resample
      Fnyq <- Fs / 2
      dfT <- max(0.1 * Fmax, 0.05 * Fnyq, 1.0)
      Fpass <- min(Fmax, Fnyq - dfT)
      Fstop <- Fpass + dfT
      # quantize
      dfq <- Fs / NW
      Fpass <- floor(Fpass / dfq) * dfq; if (Fpass <= 0) Fpass <- dfq
      Fstop <- ceiling(Fstop / dfq) * dfq; if (Fstop <= Fpass) Fstop <- Fpass + dfq
      Fnyq2 <- Fnyq - dfq; if (Fstop > Fnyq2) Fstop <- Fnyq2
      if ((Fstop - Fpass) < (8 * dfq)) { Fstop <- min(Fpass + 8 * dfq, Fnyq2); if (Fstop <= Fpass) Fstop <- Fpass + dfq }
      LP <- .buildLowPassButtterworth(f = fs, Fstop = Fstop, Fpass = Fpass,
                                      Astop = if (is.null(AstopLP)) 1e-3 else AstopLP,
                                      Apass = if (is.null(ApassLP)) 0.98 else ApassLP)
      custom <- HD * LP
if (.verbose()) cat(sprintf("  Derivate+LP: Fpass=%.2f Fstop=%.2f Apass=%.3f Astop=%.1e\n", Fpass, Fstop, if (is.null(ApassLP)) 0.98 else ApassLP, if (is.null(AstopLP)) 1e-3 else AstopLP))
    }

    # Pad Zeros
    xw <- .x / Xo
    N0 <- length(xw)
    X <- .padZeros(X = xw, NW = NW, OVLP = OVLP)

if (.verbose()) cat(sprintf("  After padding: max=%.3e (length=%d)\n", max(abs(X), na.rm = TRUE), length(X)))

    # Derivate with HD (and optional LP)
    X <- .ffilter(.x = X, f = Fs, NW = NW, OVLP = OVLP, custom = custom) * NW
if (.verbose()) cat(sprintf("  After derivation: max=%.3e (length=%d)\n", max(abs(X), na.rm = TRUE), length(X)))

    # Trim back to original length
    if (length(X) > N0) X <- X[1:N0]
  }
  # Common rescaling (RMS-based)
  X <- X * Xo
if (.verbose()) cat(sprintf("  After rescale rms=%.3e (length=%d)\n", .getRMS(X), length(X)))

  return(X)
}
