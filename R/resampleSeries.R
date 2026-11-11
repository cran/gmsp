# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.resampleSeries <- function(s, samplingInterval, frequency, fmax, overlap,
                            astop0, apass0) {
  SampleCount <- if (is.matrix(s)) nrow(s) else length(s)
  Frequency <- 1 / samplingInterval
  Nyquist <- frequency / 2
  Transition <- max(0.1 * fmax, 0.05 * Nyquist, 1.0)
  Pass <- min(fmax, Nyquist - Transition)
  Stop <- Pass + Transition
  Window <- ceiling(Frequency / max(1, Transition / 8))
  Window <- as.integer(2^floor(log2(max(64, min(SampleCount, Window)))))
  if (Window > SampleCount) Window <- as.integer(2^floor(log2(max(64, SampleCount))))
  Window <- max(64L, Window)
  BinWidth <- Frequency / Window
  f <- seq(from = 0, by = BinWidth, length.out = Window %/% 2)
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
  Transfer <- .computeButterworth(f = f, stopFrequency = Stop, passFrequency = Pass,
                                  astop0 = astop0,
                                  apass0 = apass0, highPass = FALSE)
  applyChannel <- function(x) {
    Peak <- max(abs(x))
    OUT <- .filterSTFT(s = x, samplingInterval = samplingInterval,
                       transfer = Transfer, windowLength = Window,
                       overlap = overlap)
    OUT <- signal::resample(x = OUT, p = as.integer(round(frequency)), q = Frequency)
    OUT <- OUT - mean(OUT)
    Crest <- max(abs(OUT))
    if (Crest > .Machine$double.eps && Peak > 0) OUT <- OUT * (Peak / Crest)
    OUT
  }
  if (is.matrix(s)) {
    OUT <- do.call(cbind, lapply(seq_len(ncol(s)), function(j) applyChannel(s[, j])))
    colnames(OUT) <- colnames(s)
    return(OUT)
  }
  applyChannel(s)
}
