# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.buildTaper <- function(s, astop0, apass0, flatZeros) {
  n <- length(s)
  Peak <- max(abs(s))
  OUT <- list(weights = rep(1, n), mask = rep(1, n))
  if (Peak <= .Machine$double.eps) return(OUT)
  if (flatZeros) {
    IDX <- which(abs(s) / Peak >= astop0)
    if (length(IDX) >= 2L) {
      OUT$mask <- numeric(n)
      OUT$mask[min(IDX):max(IDX)] <- 1
    }
  }
  s <- s / Peak
  Amplitude <- abs(s)
  if (apass0 >= max(Amplitude)) apass0 <- max(Amplitude) * 0.8
  if (astop0 >= apass0) astop0 <- apass0 * 0.5
  IDX <- which(Amplitude > astop0)
  if (!length(IDX)) return(OUT)
  StartStop <- IDX[1L]
  EndStop <- IDX[length(IDX)]
  IDX <- which(Amplitude > apass0)
  StartPass <- if (length(IDX)) IDX[1L] else min(n, StartStop + 2L)
  StartPass <- max(StartPass, StartStop + 1L)
  if (StartPass >= n) StartPass <- n - 1L
  if (StartPass <= StartStop) StartPass <- min(n - 1L, StartStop + 1L)
  EndPass <- if (length(IDX)) IDX[length(IDX)] else max(1, EndStop - 2L)
  EndPass <- min(EndPass, EndStop - 1L)
  if (EndPass <= 1) EndPass <- 2L
  if (EndPass >= EndStop) EndPass <- max(2L, EndStop - 1L)
  if (StartPass <= StartStop || EndPass >= EndStop || StartPass >= EndPass) return(OUT)
  HighPass <- .computeButterworth(f = seq_len(n), stopFrequency = StartStop,
                                  passFrequency = StartPass, astop0 = 0.01,
                                  apass0 = 0.95, highPass = TRUE)
  LowPass <- .computeButterworth(f = seq_len(n), stopFrequency = EndStop,
                                 passFrequency = EndPass, astop0 = 0.01,
                                 apass0 = 0.95, highPass = FALSE)
  OUT$weights <- LowPass * HighPass
  OUT
}
