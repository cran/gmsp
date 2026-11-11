# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.computeButterworth <- function(f, stopFrequency, passFrequency,
                                astop0, apass0, highPass) {
  stopifnot(is.numeric(f), all(is.finite(f)), all(f >= 0),
            stopFrequency > 0, passFrequency > 0,
            astop0 > 0, astop0 < apass0, apass0 < 1)
  if (highPass) {
    stopifnot(passFrequency > stopFrequency)
    a <- 1 - astop0
    b <- 1 - apass0
    Order <- log((a * sqrt((-1 + b^2) / (-1 + a^2))) / b) /
      log(passFrequency / stopFrequency)
    OUT <- 1 - 1 / sqrt(1 + (-1 + (-1 + astop0)^(-2)) *
                         (f / stopFrequency)^(2 * Order))
  }
  if (!highPass) {
    stopifnot(passFrequency < stopFrequency)
    Order <- log((astop0 * sqrt((-1 + apass0^2) /
                 (-1 + astop0^2))) / apass0) /
      log(passFrequency / stopFrequency)
    OUT <- 1 / sqrt(1 - (astop0^2 - 1) *
                    ((f / stopFrequency)^(2 * Order)) / (astop0^2))
  }
  OUT[OUT < astop0] <- 0
  OUT
}

.filterSTFT <- function(s, samplingInterval, transfer, windowLength,
                        overlap) {
  if (windowLength %% 2 != 0) {
    stop("STFT requires an even windowLength (seewave::istft).", call. = FALSE)
  }
  if (is.matrix(s)) {
    LIST <- lapply(seq_len(ncol(s)), function(j) {
      .filterSTFT(s = s[, j], samplingInterval = samplingInterval,
                  transfer = transfer, windowLength = windowLength,
                  overlap = overlap)
    })
    OUT <- do.call(cbind, LIST)
    colnames(OUT) <- colnames(s)
    return(OUT)
  }
  if (length(s) < windowLength) stop("Signal is shorter than windowLength.", call. = FALSE)
  Hop <- max(1L, as.integer(round(windowLength * (1 - overlap / 100))))
  IDX <- seq.int(1L, length(s) + 1L - windowLength, by = Hop)
  DATA <- seewave::stdft(wave = matrix(s, ncol = 1L), f = 1 / samplingInterval,
                        wl = windowLength, zp = 0, step = IDX, wn = "hanning",
                        fftw = FALSE, complex = TRUE)
  if (length(transfer) != nrow(DATA)) {
    stop("transfer must contain one value per STFT bin.", call. = FALSE)
  }
  DATA <- sweep(DATA, 1L, transfer, `*`)
  as.vector(seewave::istft(stft = DATA, f = 1 / samplingInterval,
                          wl = windowLength, ovlp = 100 * (1 - Hop / windowLength),
                          wn = "hanning", output = "matrix"))
}

.padSeries <- function(s, windowLength, overlap) {
  SampleCount <- if (is.matrix(s)) nrow(s) else length(s)
  Hop <- max(1L, as.integer(round(windowLength * (1 - overlap / 100))))
  Padding <- max(0L, windowLength + max(0L, ceiling((SampleCount - windowLength) /
                 Hop)) * Hop - SampleCount)
  if (Padding == 0) return(s)
  if (is.matrix(s)) {
    return(rbind(s, matrix(0, nrow = Padding, ncol = ncol(s),
                           dimnames = list(NULL, colnames(s)))))
  }
  c(s, numeric(Padding))
}

.integrateSeries <- function(s, samplingInterval, windowLength, overlap) {
  if (is.matrix(s)) {
    OUT <- vapply(seq_len(ncol(s)), function(j) {
      .integrateSeries(s = s[, j], samplingInterval = samplingInterval,
                       windowLength = windowLength, overlap = overlap)
    }, numeric(nrow(s)))
    colnames(OUT) <- colnames(s)
    return(OUT)
  }
  Scale <- max(sqrt(1 / length(s) * as.numeric(s %*% s)), .Machine$double.eps)
  Frequency <- 1 / samplingInterval
  stopifnot(isTRUE(all.equal(Frequency, round(Frequency))))
  f <- seq(from = 0, by = Frequency / windowLength,
           length.out = windowLength %/% 2)
  Transfer <- c(0, 1 / (1i * (2 * pi * f)[-1L]))
  OUT <- .padSeries(s = s / Scale, windowLength = windowLength,
                    overlap = overlap)
  OUT <- .filterSTFT(s = OUT, samplingInterval = samplingInterval, transfer = Transfer,
                     windowLength = windowLength, overlap = overlap) * windowLength
  OUT[seq_along(s)] * Scale
}

.differentiateSeries <- function(s, samplingInterval, method, windowLength,
                                 overlap, fmax, lowPass, astop0,
                                 apass0) {
  if (is.matrix(s)) {
    OUT <- vapply(seq_len(ncol(s)), function(j) {
      .differentiateSeries(s = s[, j], samplingInterval = samplingInterval,
                           method = method, windowLength = windowLength,
                           overlap = overlap, fmax = fmax,
                           lowPass = lowPass, astop0 = astop0,
                           apass0 = apass0)
    }, numeric(nrow(s)))
    colnames(OUT) <- colnames(s)
    return(OUT)
  }
  n <- length(s)
  stopifnot(n > 4L)
  method <- tolower(method)
  if (!method %in% c("time", "freq")) stop("method must be time or freq.", call. = FALSE)
  Scale <- max(sqrt(1 / n * as.numeric(s %*% s)), .Machine$double.eps)
  DATA <- s / Scale
  if (method == "time") {
    OUT <- numeric(n)
    for (i in 3:(n - 2L)) {
      OUT[i] <- (-DATA[i + 2L] + 8 * DATA[i + 1L] - 8 * DATA[i - 1L] +
                 DATA[i - 2L]) / (12 * samplingInterval)
    }
    OUT[1L] <- (-3 * DATA[1L] + 4 * DATA[2L] - DATA[3L]) / (2 * samplingInterval)
    OUT[2L] <- (-DATA[4L] + 4 * DATA[3L] - 3 * DATA[2L]) / (2 * samplingInterval)
    OUT[n - 1L] <- (3 * DATA[n - 1L] - 4 * DATA[n - 2L] + DATA[n - 3L]) /
      (2 * samplingInterval)
    OUT[n] <- (3 * DATA[n] - 4 * DATA[n - 1L] + DATA[n - 2L]) / (2 * samplingInterval)
    return(OUT * Scale)
  }
  Frequency <- 1 / samplingInterval
  stopifnot(isTRUE(all.equal(Frequency, round(Frequency))))
  BinWidth <- Frequency / windowLength
  f <- seq(from = 0, by = BinWidth, length.out = windowLength %/% 2)
  Transfer <- c(0, 1i * (2 * pi * f)[-1L])
  if (lowPass) {
    Nyquist <- Frequency / 2
    Transition <- max(0.1 * fmax, 0.05 * Nyquist, 1.0)
    Pass <- min(fmax, Nyquist - Transition)
    Stop <- Pass + Transition
    Pass <- floor(Pass / BinWidth) * BinWidth
    if (Pass <= 0) Pass <- BinWidth
    Stop <- ceiling(Stop / BinWidth) * BinWidth
    if (Stop <= Pass) Stop <- Pass + BinWidth
    Nyquist <- Nyquist - BinWidth
    if (Stop > Nyquist) Stop <- Nyquist
    if (Stop - Pass < 8 * BinWidth) {
      Stop <- min(Pass + 8 * BinWidth, Nyquist)
      if (Stop <= Pass) Stop <- Pass + BinWidth
    }
    Transfer <- Transfer * .computeButterworth(f = f, stopFrequency = Stop,
                  passFrequency = Pass, astop0 = astop0,
                  apass0 = apass0, highPass = FALSE)
  }
  OUT <- .padSeries(s = DATA, windowLength = windowLength, overlap = overlap)
  OUT <- .filterSTFT(s = OUT, samplingInterval = samplingInterval, transfer = Transfer,
                     windowLength = windowLength, overlap = overlap) * windowLength
  OUT[seq_along(s)] * Scale
}
