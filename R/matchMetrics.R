# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.matchInside <- function(x, low, high) {
  Tolerance <- 1e-6 * pmax(1, abs(high))
  x >= low - Tolerance & x <= high + Tolerance
}

.matchRMSE <- function(x, y) {
  OK <- is.finite(x) & is.finite(y) & y != 0
  sqrt(mean(((x[OK] - y[OK]) / y[OK]) ^ 2))
}

.matchBandMetrics <- function(PSA, target) {
  DT <- target[PSA, on = "Tn", nomatch = 0L]
  if (!nrow(DT)) .stopUsage("No matching target periods.", operation = ".matchBandMetrics")
  DT[, inside := .matchInside(Sa, Sa.low, Sa.high)]
  data.table::data.table(
    RMSE = .matchRMSE(DT$Sa, DT$Sa.mean), inside = mean(DT$inside),
    bandLoss = mean(pmax((DT$Sa.low - DT$Sa) / DT$Sa.low,
                         (DT$Sa - DT$Sa.high) / DT$Sa.high, 0) ^ 2),
    below = sum(DT$Sa < DT$Sa.low), above = sum(DT$Sa > DT$Sa.high),
    maxViolation = max(pmax((DT$Sa.low - DT$Sa) / DT$Sa.low,
                            (DT$Sa - DT$Sa.high) / DT$Sa.high, 0), na.rm = TRUE),
    medianRatio = stats::median(DT$Sa / DT$Sa.mean),
    medianBandPos = stats::median((DT$Sa - DT$Sa.low) / (DT$Sa.mean - DT$Sa.low)))
}

.matchEnergyFactor <- function(source, rebuilt) {
  if (length(source) != length(rebuilt)) {
    .stopUsage("Energy restoration requires aligned signals.", operation = ".matchEnergyFactor")
  }
  OK <- is.finite(source) & is.finite(rebuilt)
  SourceEnergy <- sum(source[OK] ^ 2)
  RebuiltEnergy <- sum(rebuilt[OK] ^ 2)
  if (!is.finite(SourceEnergy) || SourceEnergy <= 0 ||
      !is.finite(RebuiltEnergy) || RebuiltEnergy <= 0) {
    .stopUsage("Energy restoration requires positive finite energy.", operation = ".matchEnergyFactor")
  }
  sqrt(SourceEnergy / RebuiltEnergy)
}
