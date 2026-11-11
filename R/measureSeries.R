# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.measureSeries <- function(s, t, ID, unitsTarget) {
  SamplingInterval <- mean(diff(t))
  SampleCount <- length(s)
  Peak <- max(abs(s))
  RootMeanSquare <- sqrt(1 / SampleCount * as.numeric(s %*% s))
  ZeroCrossings <- 0
  if (SampleCount > 2L) {
    ZeroCrossings <- sum(sign(s[2:SampleCount]) == -sign(s[1:(SampleCount - 1L)]))
  }
  MeanPeriod <- .measureMeanPeriod(s, samplingInterval = SamplingInterval)
  if (ID == "AT") {
    Gravity <- .getGravity(unitsTarget)
    Arias <- as.numeric(s %*% s) * SamplingInterval * pi / (2 * Gravity)
    x <- pmax(s, 0)
    AriasPositive <- as.numeric(x %*% x) * SamplingInterval * pi / (2 * Gravity)
    x <- pmin(s, 0)
    AriasNegative <- as.numeric(x %*% x) * SamplingInterval * pi / (2 * Gravity)
    Duration <- .measureDuration(s, t = t, lower = 0.05, upper = 0.95,
                                 gravity = Gravity)
    Value <- c(AI = Arias, AIu = AriasPositive, AId = AriasNegative,
      PGA = Peak, ARMS = RootMeanSquare, AZC = ZeroCrossings,
      ATo = s[1L], ATn = s[SampleCount], D0595 = Duration,
      D2080 = .measureDuration(s, t = t, lower = 0.20, upper = 0.80,
                               gravity = Gravity),
      D0575 = .measureDuration(s, t = t, lower = 0.05, upper = 0.75,
                               gravity = Gravity),
      TmA = MeanPeriod, CAV = sum(abs(s)) * SamplingInterval,
      CAV5 = sum(abs(s) * (abs(s) >= 0.05 * Gravity)) * SamplingInterval,
      NP = SampleCount, dt = SamplingInterval, Fs = 1 / SamplingInterval,
      Dmax = t[SampleCount], EPI = 0.9 / pi * Arias * 2 * Gravity * Duration,
      PDI = Arias * (t[SampleCount] / ZeroCrossings)^2)
    Units <- c(rep(paste(unitsTarget, "/s"), 3L),
      rep(paste(unitsTarget, "/s2"), 2L), "-",
      rep(paste(unitsTarget, "/s2"), 2L), rep("s", 4L),
      rep(paste(unitsTarget, "/s"), 2L), "samples", "s", "Hz", "s",
      paste0(unitsTarget, "^2/s^2"), paste0(unitsTarget, ".s"))
    return(list(value = Value, units = stats::setNames(Units, names(Value))))
  }
  if (ID == "VT") {
    Value <- c(PGV = Peak, VRMS = RootMeanSquare, VZC = ZeroCrossings,
               VTo = s[1L], VTn = s[SampleCount], TmV = MeanPeriod)
    Units <- c(rep(paste(unitsTarget, "/s"), 2L), "-",
               rep(paste(unitsTarget, "/s"), 2L), "s")
    return(list(value = Value, units = stats::setNames(Units, names(Value))))
  }
  if (ID == "DT") {
    Value <- c(PGD = Peak, DRMS = RootMeanSquare, DZC = ZeroCrossings,
               DTo = s[1L], DTn = s[SampleCount], TmD = MeanPeriod)
    Units <- c(rep(unitsTarget, 2L), "-", rep(unitsTarget, 2L), "s")
    return(list(value = Value, units = stats::setNames(Units, names(Value))))
  }
  stop("ID must be AT, VT or DT.", call. = FALSE)
}

.measureDuration <- function(s, t, lower, upper, gravity) {
  SamplingInterval <- mean(diff(t))
  Arias <- SamplingInterval * (s %*% s) * pi / (2 * gravity)
  Cumulative <- pi * SamplingInterval * cumsum(s^2)
  Lower <- lower * 2 * gravity * Arias
  Upper <- upper * 2 * gravity * Arias
  i <- 1L
  while (Cumulative[i] < Lower) i <- i + 1L
  Start <- t[i]
  while (Cumulative[i] < Upper) i <- i + 1L
  t[i] - Start
}

.measureMeanPeriod <- function(s, samplingInterval) {
  if (max(abs(s)) == 0) return(0)
  SampleCount <- length(s)
  Frequency <- 1 / samplingInterval
  Spectrum <- 1 / SampleCount * stats::fft(s, inverse = FALSE)
  FrequencyStep <- Frequency / SampleCount
  UpperIndex <- ceiling(SampleCount / 2) + 1
  Frequencies <- seq(from = 1, to = UpperIndex) * FrequencyStep
  Lower <- max(0.1, min(Frequencies))
  Upper <- min(25, max(Frequencies))
  MASK <- Frequencies >= Lower & Frequencies <= Upper
  Amplitudes <- sqrt(Re(Spectrum[1:UpperIndex] * Conj(Spectrum[1:UpperIndex])))
  sum(Amplitudes[MASK]^2 / Frequencies[MASK]) / sum(Amplitudes[MASK]^2)
}

.accumulateSeries <- function(s, samplingInterval, ID, gravity) {
  if (ID == "AI") OUT <- cumsum(s^2) * samplingInterval * pi / (2 * gravity)
  if (ID == "CAV") OUT <- cumsum(abs(s)) * samplingInterval
  if (ID == "CAV5") {
    OUT <- cumsum(abs(s) * (abs(s) >= 0.05 * gravity)) * samplingInterval
  }
  if (!ID %in% c("AI", "CAV", "CAV5")) {
    stop("ID must be AI, CAV or CAV5.", call. = FALSE)
  }
  Maximum <- max(OUT, na.rm = TRUE)
  if (is.finite(Maximum) && Maximum != 0) return(OUT / Maximum)
  rep(NA_real_, length(s))
}
