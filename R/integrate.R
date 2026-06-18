.integrate <- function(.x, dt, NW, OVLP, Fmax) {
  # Vector column helper - processes single time series vector
  stopifnot(is.vector(.x))

  # === LOGGING: Integration setup ===
  Xo <- max(.getRMS(.x), .Machine$double.eps)
if (.verbose()) cat(sprintf("  .integrate: Xo=%.3e dt=%.2e NW=%d\n", Xo, dt, NW))
  # Common scaling setup (RMS-based)
  .x <- .x / Xo
  N0 <- length(.x)
  # Build filter
  Fs <- 1 / dt
  stopifnot(isTRUE(all.equal(Fs, round(Fs))))
  df <- Fs / NW
  fs <- seq(from = 0, by = df, length.out = ceiling(NW / 2))
if (.verbose()) cat(sprintf("  Filter: Fs=%.0f df=%.3f freq_range=[%.3f-%.3f]\n", Fs, df, min(fs), max(fs)))

  HI <- .buildIntegrateFilter(f = fs) ## Integrate Filter

if (.verbose()) cat(sprintf("  IntegrateFilter: max=%.3e\n", max(abs(HI), na.rm = TRUE)))


  # Pad Zeros
  X <- .padZeros(X = .x, NW = NW, OVLP = OVLP)

if (.verbose()) cat(sprintf("  After padding: max=%.3e (length=%d)\n", max(abs(X), na.rm = TRUE), length(X)))

  # Integrate with HI filter only (like original build_TS)
  X <- .ffilter(.x = X, f = Fs, NW = NW, OVLP = OVLP, custom = HI) * NW

if (.verbose()) cat(sprintf("  After integration: max=%.3e (length=%d)\n", max(abs(X), na.rm = TRUE), length(X)))

  # Trim back to original length
  if (length(X) > N0) X <- X[1:N0]

  # Common rescaling (RMS-based)
  X <- X * Xo

if (.verbose()) cat(sprintf("  After rescale rms=%.3e (length=%d)\n", .getRMS(X), length(X)))

  return(X)
}


.padZeros <- function(X, nz = 0, OVLP, NW) {
  # Mixed helper - can process both data.table and vector

  if (is.data.table(X)) {
    NP <- nrow(X)
  } else if (is.vector(X)) {
    NP <- length(X)
  } else {
    stop("X must be either data.table or vector")
  }

  NO <- OVLP * NW / 100
  NB <- ceiling((NP - NO) / (NW - NO))
  TargetNP <- NB * (NW - NO) + NO
  NZ <- max(nz, TargetNP - NP)

  if (is.data.table(X) && NZ > 0) {
    O <- data.table(sapply(X, function(col) rep(0, NZ)))
    X <- rbind(X, O)
  } else if (is.vector(X) && NZ > 0) {
    O <- rep(0, NZ)
    X <- c(X, O)
  }
  return(X)


}
