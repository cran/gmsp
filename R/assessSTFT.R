# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.assessSTFT <- function(t, s, samplingInterval, fmax, kNyq, windowLength,
                        overlap, astopLP,
                        apassLP, includeWarnings) {
  SampleCount <- length(s)
  Frequency <- 1 / samplingInterval
  getCount <- function(n, w) {
    Hop <- w * (100 - overlap) / 100
    as.integer(floor((n - w) / Hop) + 1L)
  }
  Grid <- unique(as.integer(round(c(Frequency, c(3.125, 4, 5, 6, 8, 10) * fmax))))
  if (!is.null(kNyq)) Grid <- unique(as.integer(round(kNyq * fmax)))
  Grid <- Grid[Grid <= as.integer(Frequency * 16) & Grid > 0]
  Windows <- sort(unique(pmax(64L, c(windowLength, 2^floor(log2(windowLength)),
                    2^ceiling(log2(windowLength)), 2L * windowLength,
                    4L * windowLength))))
  Best <- NULL
  Cost <- Inf
  for (f in Grid) {
    Estimate <- max(8L, floor(SampleCount * (f / Frequency)))
    Limit <- max(64L, as.integer(floor(Estimate / 4)))
    for (w in Windows) {
      if (w > Limit) next
      Count <- getCount(Estimate, w)
      if (Count < 16L) next
      Alignment <- abs(fmax / (f / w) - round(fmax / (f / w)))
      Penalty <- 0
      if (is.null(kNyq)) Penalty <- if (f > Frequency) 0.08 else 0.02
      j <- Alignment + Penalty * abs(f - Frequency) / max(Frequency, 1)
      if (j < Cost - 1e-12) {
        Cost <- j
        Best <- list(frequency = as.integer(f), windowLength = as.integer(w),
                     windowCount = as.integer(Count))
      }
    }
  }
  if (is.null(Best) && is.null(kNyq)) {
    Best <- list(frequency = Frequency, windowLength = windowLength,
                 windowCount = max(3L, getCount(SampleCount, windowLength)))
    Strategy <- "fallback_no_candidate"
  }
  if (is.null(Best)) {
    f <- unique(as.integer(round(kNyq * fmax)))
    Estimate <- max(8L, floor(SampleCount * (f / Frequency)))
    Limit <- max(16L, as.integer(floor(Estimate / 3)))
    w <- max(16L, min(windowLength, Limit))
    w <- as.integer(2^floor(log2(max(16L, min(w, max(16L, Estimate - 1))))))
    Best <- list(frequency = f, windowLength = max(16L, w),
                 windowCount = max(1L, getCount(Estimate, w)))
    Strategy <- "forced_knyq_fallback"
  }
  if (is.finite(Cost)) {
    Strategy <- "optimized_min_leakage_no_resample"
    if (Best$frequency < as.integer(Frequency)) Strategy <- "optimized_min_leakage_downsample"
    if (Best$frequency > as.integer(Frequency)) Strategy <- "optimized_min_leakage_upsample"
  }
  Up <- Best$frequency > Frequency
  Down <- Best$frequency < Frequency
  UpsamplingFactor <- 1
  if (Up) UpsamplingFactor <- ceiling(Best$frequency / max(1, Frequency))
  if (is.finite(Cost)) UpsamplingFactor <- min(16, UpsamplingFactor)
  OUT <- c(Best, list(overlap = overlap,
    samplingInterval = 1 / Best$frequency,
    upsamplingFactor = UpsamplingFactor,
    downsamplingFactor = if (Down) Frequency / max(1, Best$frequency) else 1,
    resample = Best$frequency != Frequency, upsample = Up, downsample = Down,
    strategy = Strategy, warnings = character(), diagnostics = list(available = FALSE)))
  if (!includeWarnings) return(OUT)
  if (SampleCount < 9L) {
    OUT$warnings <- "Spectral diagnostics require at least nine original samples."
    return(OUT)
  }
  # The diagnostic transform uses its actual length and removes the mean.
  f <- (0:floor(SampleCount / 2)) / (SampleCount * mean(diff(t)))
  Amplitude <- Mod(stats::fft(s - mean(s)))[seq_along(f)] / SampleCount
  IDX <- seq.int(2L, length(Amplitude))
  if (SampleCount %% 2 == 0) IDX <- IDX[-length(IDX)]
  Amplitude[IDX] <- 2 * Amplitude[IDX]
  Peak <- f[which.max(Amplitude)]
  Above <- sum(Amplitude[f > fmax]) / (sum(Amplitude) + .Machine$double.eps)
  OUT$diagnostics <- list(available = TRUE, peakFrequency = Peak,
                         amplitudeAbove = Above, antialias = NULL)
  if (Above > 0.05) OUT$warnings <- c(OUT$warnings,
    paste0("fmax=", fmax, " Hz retains ", 100 * (1 - Above), "% of spectral amplitude."))
  if (Peak > fmax * 1.10) OUT$warnings <- c(OUT$warnings,
    paste0("Peak frequency ", Peak, " Hz exceeds fmax by more than 10%."))
  if (OUT$resample) {
    # Use the effective input grid and the same antialias window as resampling.
    Estimate <- floor((max(t) - min(t)) / samplingInterval + 1e-12) + 1L
    Nyquist <- Best$frequency / 2
    Transition <- max(0.1 * fmax, 0.05 * Nyquist, 1.0)
    Pass <- min(fmax, Nyquist - Transition)
    Stop <- Pass + Transition
    Window <- ceiling(Frequency / max(1, Transition / 8))
    Window <- as.integer(2^floor(log2(max(64, min(Estimate, Window)))))
    if (Window > Estimate) Window <- as.integer(2^floor(log2(max(64, Estimate))))
    Window <- max(64L, Window)
    BinWidth <- Frequency / Window
    Pass <- floor(Pass / BinWidth) * BinWidth
    if (Pass <= 0) Pass <- BinWidth
    Stop <- ceiling(Stop / BinWidth) * BinWidth
    if (Stop <= Pass) Stop <- Pass + BinWidth
    if (Stop > Nyquist - BinWidth) Stop <- Nyquist - BinWidth
    if (Stop - Pass < 8 * BinWidth) {
      Stop <- min(Pass + 8 * BinWidth, Nyquist - BinWidth)
      if (Stop <= Pass) Stop <- Pass + BinWidth
    }
    Bins <- max(0L, as.integer(round((Stop - Pass) / BinWidth)))
    Margin <- Nyquist - Stop
    OUT$diagnostics$antialias <- list(windowLength = Window, binWidth = BinWidth,
      passFrequency = Pass, stopFrequency = Stop, transitionBins = Bins,
      nyquistMargin = Margin, astop0 = astopLP,
      apass0 = apassLP)
    if (Bins < 8) OUT$warnings <- c(OUT$warnings,
      paste0("Antialias transition has ", Bins, " bins (fewer than eight)."))
    if (Margin < BinWidth) OUT$warnings <- c(OUT$warnings,
      "Antialias stopband has less than one bin of Nyquist margin.")
  }
  if (Best$windowCount < 16L) OUT$warnings <- c(OUT$warnings,
    paste0("Only ", Best$windowCount, " STFT windows (fewer than sixteen)."))
  OUT
}
