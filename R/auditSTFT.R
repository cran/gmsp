auditSTFT <- function(.x, t = NULL, Fmax, kNyq = NULL,
                      NW.min = 128, MW.min = 16, OVLP = 75,
                      ApassLP = 0.98, AstopLP = 1e-3) {
  stopifnot(!is.null(Fmax), is.finite(Fmax), Fmax > 0)
  if (data.table::is.data.table(.x)) {
    DT <- data.table::copy(.x)
    if (is.null(t)) t <- as.numeric(DT[[if ("t" %chin% names(DT)) "t" else names(DT)[1]]])
    s <- as.numeric(DT[[if ("s" %chin% names(DT)) "s" else names(DT)[2]]])
  } else if (is.vector(.x)) {
    s <- as.numeric(.x)
    stopifnot(!is.null(t))
    t <- as.numeric(t)
  } else {
    stop(".x must be data.table or vector")
  }
  stopifnot(length(t) == length(s), length(s) > 8)
  t <- t[is.finite(t)]; s <- s[is.finite(s)]
  NP <- length(s)
  Fs.in <- 1 / mean(diff(t))

  # Internal STFT decision
  if (is.null(kNyq)) {
    ST <- .setSTFT(NP = NP, Fs = Fs.in, Fmax = Fmax, NW.min = NW.min, MW.min = MW.min, OVLP = OVLP)
  } else {
    ST <- .setSTFT(NP = NP, Fs = Fs.in, Fmax = Fmax, kNyq = kNyq, NW.min = NW.min, MW.min = MW.min, OVLP = OVLP)
  }
  Fs.out <- as.numeric(ST$Fs)
  NW <- as.integer(ST$NW)
  MW <- as.integer(ST$MW)
  df <- Fs.out / NW
  Fnyq <- Fs.out / 2

  # Content estimate: peak freq and energy above Fmax
  spec <- buildFFT(s = s, t = t, zp = 0, kf = 1.0)
  Fpeak <- spec$fs[which.max(spec$A)]
  E.total <- sum(spec$A, na.rm = TRUE) + .Machine$double.eps
  E.above <- sum(spec$A[spec$fs > Fmax], na.rm = TRUE)
  E.above.frac <- E.above / E.total

  # Anti-alias LP expected bands (replica of resample heuristics)
  df.T <- max(0.1 * Fmax, 0.05 * Fnyq, 1.0)
  FpassPre <- min(Fmax, Fnyq - df.T)
  FstopPre <- FpassPre + df.T
  Fpass <- floor(FpassPre / df) * df; if (Fpass <= 0) Fpass <- df
  Fstop <- ceiling(FstopPre / df) * df; if (Fstop <= Fpass) Fstop <- Fpass + df
  Fstop <- min(Fstop, Fnyq - df)
  if ((Fstop - Fpass) < (8 * df)) {
    Fstop <- min(Fpass + 8 * df, Fnyq - df)
    if (Fstop <= Fpass) Fstop <- Fpass + df
  }
  bins.tr <- max(0L, as.integer(round((Fstop - Fpass) / df)))
  nyq.margin <- Fnyq - Fstop

  warnings <- character(0)

  # 1) Fmax << content
  if (E.above.frac > 0.05) {
    warnings <- c(warnings, sprintf("Fmax=%.1f Hz captures only %.1f%% of spectral amplitude; consider higher Fmax.", Fmax, 100 * (1 - E.above.frac)))
  }
  if (Fpeak > (Fmax * 1.10)) {
    warnings <- c(warnings, sprintf("Peak content at %.1f Hz is above Fmax=%.1f Hz (by >10%%).", Fpeak, Fmax))
  }

  # 2) kNyq target feasibility
  if (!is.null(kNyq)) {
    Fs.target <- round(kNyq * Fmax)
    if (abs(Fs.out - Fs.target) > max(1, 0.05 * Fs.target)) {
      warnings <- c(warnings, sprintf("Requested kNyq=%.3f -> Fs_target=%d Hz but chosen Fs_out=%d Hz (constraints/optimization overrode target).", kNyq, Fs.target, Fs.out))
    }
  }

  # 3) Transition and guard checks
  if (bins.tr < 8) warnings <- c(warnings, sprintf("Anti-alias LP transition too short: %d bins (< 8).", bins.tr))
  if (nyq.margin < df) warnings <- c(warnings, sprintf("Stopband too close to Nyquist: nyq.margin=%.3f Hz (< df=%.3f).", nyq.margin, df))

  # 4) STFT coverage
  if (MW < MW.min) warnings <- c(warnings, sprintf("Few STFT windows: MW=%d (< MW.min=%d).", MW, MW.min))

  info <- list(
    Fs.in = Fs.in, Fs.out = Fs.out, NW = NW, MW = MW, df = df,
    Fnyq = Fnyq, Fmax = Fmax, kNyq = kNyq,
    Fpeak = Fpeak, E.above.frac = E.above.frac,
    Fpass = Fpass, Fstop = Fstop, bins.tr = bins.tr, nyq.margin = nyq.margin,
    ApassLP = ApassLP, AstopLP = AstopLP,
    Resample = ST$Resample, strategy = ST$strategy
  )
  return(list(warnings = warnings, info = info, stft = ST))
}
