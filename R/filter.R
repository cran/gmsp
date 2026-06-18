.ffilter <- function(.x, f, custom, OVLP, NW) {
  # Vector column helper - processes single time series vector
  stopifnot(is.vector(.x))

  NP <- length(.x)

  # Validate lengths and hop
  if (NP < NW) {
    stop(sprintf(".ffilter: vector length (%d) is shorter than window size (%d). Consider padding.", NP, NW))
  }
  hop <- as.integer(round(NW * (1 - OVLP / 100)))
  if (hop < 1L) hop <- 1L
  StepEnd <- NP + 1L - NW
  if (StepEnd < 1L) {
    stop(".ffilter: invalid STEP range; check NP and NW")
  }
  STEP <- seq.int(1L, StepEnd, by = hop)
if (.verbose()) cat(sprintf("    STFT windows: %d steps OVLP=%.0f\n", length(STEP), OVLP))

  z <- seewave::stdft(
    wave = matrix(.x, ncol = 1), f = f, wl = NW, zp = 0, step = STEP,
    wn = "hanning", fftw = FALSE, complex = TRUE)
  LOG <- max(abs(z), na.rm = TRUE)
if (.verbose()) cat(sprintf("    After STDFT: max=%.3e\n", LOG))

  # Ensure custom length matches frequency bins
  if (length(custom) != nrow(z)) {
    stop(sprintf(".ffilter: custom length mismatch: %d (custom) vs %d (STFT bins)", length(custom), nrow(z)))
  }
  # Safe per-row multiplication to avoid recycling
  z <- sweep(z, 1L, custom, `*`)
  LOG <- max(abs(z), na.rm = TRUE)
if (.verbose()) cat(sprintf("    After filter*custom: max=%.3e\n", LOG))

  x <- (seewave::istft(z, wl = NW, ovlp = OVLP, wn = "hanning", output = "matrix", f = f)) |> as.vector()
  LOG <- max(abs(x), na.rm = TRUE)
if (.verbose()) cat(sprintf("    After ISTFT: max=%.3e\n", LOG))

  return(x)
}

.buildLowPassButtterworth <- function(f = NULL, Fstop = NULL, Fpass = NULL, Astop = NULL, Apass = NULL) {
  OK <- !is.null(f) &&
    is.vector(f) &&
    !is.null(Fstop) &&
    !is.null(Fpass) &&
    Fpass < Fstop &&
    !is.null(Astop) &&
    !is.null(Apass) &&
    Astop < Apass
  stopifnot(OK)
  TOL <- Astop
  N <- .NminLP(fS = Fstop, fP = Fpass, aS = Astop, aP = Apass)
  LP <- 1 / sqrt(1 - (Astop^2 - 1) * ((f / Fstop)^(2 * N)) / (Astop^2))
  LP[LP < TOL] <- 0
  # NminHP[fs, fp, As, Ap] == NminLP[fs, fp, 1 - As, 1 - Ap]
  return(LP)
}

.buildHighPassButtterworth <- function(f = NULL, Fstop = NULL, Fpass = NULL, Astop = NULL, Apass = NULL) {

  OK <- !is.null(f) &&
    is.vector(f) &&
    !is.null(Fstop) &&
    !is.null(Fpass) &&
    Fpass > Fstop &&
    !is.null(Astop) &&
    !is.null(Apass) &&
    Astop < Apass
  stopifnot(OK)
  TOL <- Astop
  n <- .NminLP(fS = Fstop, fP = Fpass, aS = 1 - Astop, aP = 1 - Apass)
  HP <- 1 - 1 / sqrt(1 + (-1 + (-1 + Astop)^(-2)) * (f / Fstop)^(2 * n))
  HP[HP < TOL] <- 0
  # NminHP[fs, fp, As, Ap] == NminLP[fs, fp, 1 - As, 1 - Ap]
  return(HP)
}

.NminLP <- function(fS = 0, fP = 0, aS = NULL, aP = NULL) {
  OK <- fS > 0 && fP > 0 && !is.null(aS) && !is.null(aP)
  stopifnot(OK)
  n <- log((aS * sqrt((-1 + aP^2) / (-1 + aS^2))) / aP) / log(fP / fS)

  return(n)
}

.buildIntegrateFilter <- function(f) {
  ws <- 2 * pi * f
  HI <- c(0, 1 / (1i * ws[2:length(ws)]))
  # HI <- purrr::prepend(values=0,1/(1i*ws[2:length(fs)]))
  return(HI)
}

.buildDerivateFilter <- function(f) {
  ws <- 2 * pi * f
  HD <- c(0, 1i * ws[2:length(ws)])
  # HD <- purrr::prepend(values=0,(1i*ws[2:length(fs)]))
  return(HD)
}
