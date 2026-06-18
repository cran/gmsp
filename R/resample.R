.resample <- function(X, dt, TargetFs, Fmax, NW, OVLP, Apass = 0.98, Astop = 0.001) {
  . <- NULL
  OCID <- names(X)
  Fs <- 1 / dt
  NP <- nrow(X)

  # === LOGGING: Resample setup ===
  Xmax <- max(sapply(X, function(col) max(abs(as.numeric(unlist(col))), na.rm = TRUE)), na.rm = TRUE)
if (.verbose()) cat(sprintf("  .resample: input_max=%.3e Fs=%.0f->%.0f Fmax=%.0f\n", Xmax, Fs, TargetFs, Fmax))

  # Reference peak (per-column) for later restoration
  Xp.in <- sapply(X, .getPeak)

  # --- Anti-alias filter design (adaptive transition) ---
  # New Nyquist after resampling
  Fnyq <- TargetFs / 2
  # Transition width (Hz)
  df.T <- max(0.1 * Fmax, 0.05 * Fnyq, 1.0)
  # Passband/stopband before quantization
  Fpass <- min(Fmax, Fnyq - df.T)
  Fstop <- Fpass + df.T

  # Choose NW so that df is sufficiently fine (df <= df.T/8)
  df <- max(1, df.T / 8)
  NW <- ceiling(Fs / df)
  NW <- as.integer(2^floor(log2(max(64, min(NP, NW)))))
  if (NW > NP) NW <- as.integer(2^floor(log2(max(64, NP))))
  if (NW < 64L) NW <- 64L

  # Frequency bin spacing and axis
  df <- Fs / NW
  fs <- seq(from = 0, by = df, length.out = NW %/% 2)

  # Quantize Fpass/Fstop to df grid and ensure margins below new Nyquist
  Fpass <- floor(Fpass / df) * df
  if (Fpass <= 0) Fpass <- df
  Fstop <- ceiling(Fstop / df) * df
  if (Fstop <= Fpass) Fstop <- Fpass + df
  Fnyq <- Fnyq - df
  if (Fstop > Fnyq) Fstop <- Fnyq
  # Ensure at least 8 bins in transition for short signals
  if ((Fstop - Fpass) < (8 * df)) {
    Fstop <- min(Fpass + 8 * df, Fnyq)
    if (Fstop <= Fpass) Fstop <- Fpass + df
  }
if (.verbose()) cat(sprintf("  AntiAlias filter: NW=%d df=%.3f Fpass=%.1f Fstop=%.1f Hz (Fnyq=%.1f)\n", NW, df, Fpass, Fstop, Fnyq))

  LP <- .buildLowPassButtterworth(f = fs, Fstop = Fstop, Fpass = Fpass, Astop = Astop, Apass = Apass)
if (.verbose()) cat(sprintf("  LowPass filter: max=%.3e\n", max(abs(LP), na.rm = TRUE)))
  
  # Golden rules logging
if (.verbose()) {
    cat(sprintf(
      "  Rules: Fs.int=%s NW.pow2=%s df<=df.T/8=%s bins.tr=%d ipass.int=%s istop.int=%s nyq.margin=%.3f (>=df=%s) Apass=%.3f Astop=%.1e\n",
      as.character(TargetFs == as.integer(TargetFs)),
      as.character(as.integer(2^round(log(NW, 2))) == NW),
      as.character(df <= (df.T / 8 + 1e-9)),
      as.integer(round((Fstop - Fpass) / df)),
      as.character(abs((Fpass / df) - round(Fpass / df)) < 1e-9),
      as.character(abs((Fstop / df) - round(Fstop / df)) < 1e-9),
      (TargetFs / 2) - Fstop,
      as.character(((TargetFs / 2) - Fstop) >= (df - 1e-9)),
      Apass, Astop
    ))
  }

  # Anti-alias filtering using STFT
  X <- X[, lapply(.SD, function(z) {
    .ffilter(.x = as.numeric(z), f = Fs, custom = LP, OVLP = OVLP, NW = NW)
  })]
if (.verbose()) cat(sprintf("  After anti-alias: max=%.3e\n", max(abs(as.matrix(X)), na.rm = TRUE)))

  # Resample to integer TargetFs
  TargetFs <- as.integer(round(TargetFs))
  X <- X[, lapply(.SD, function(z) signal::resample(z, TargetFs, Fs))]
  LOG <- max(abs(as.matrix(X)), na.rm = TRUE)
if (.verbose()) cat(sprintf("  After signal::resample: max=%.3e\n", LOG))

  # Remove DC
  X <- X[, .(sapply(.SD, function(x) x - mean(x)))]
  X <- data.table::as.data.table(as.data.frame(X))
if (.verbose()) cat(sprintf("  After remove DC: max=%.3e\n", max(sapply(X, function(col) max(abs(as.numeric(unlist(col))), na.rm = TRUE)), na.rm = TRUE)))

  # Peak-based amplitude restoration per column
  Xp.out <- sapply(X, .getPeak)
  eps <- .Machine$double.eps
  X <- data.table::as.data.table(as.data.frame(mapply(function(col, xp_in, xp_out) {
    col <- as.numeric(col)
    if (xp_out > eps && xp_in > 0) col * (xp_in / xp_out) else col
  }, X, Xp.in, Xp.out, SIMPLIFY = FALSE)))
  names(X) <- OCID
  LOG <- max(sapply(X, function(col) max(abs(as.numeric(unlist(col))), na.rm = TRUE)), na.rm = TRUE)
if (.verbose()) cat(sprintf("  After peak rescale: max=%.3e\n", LOG))

  return(X)
}
