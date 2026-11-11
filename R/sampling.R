# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.rationalizeInterval <- function(samplingInterval) {
  stopifnot(is.numeric(samplingInterval), length(samplingInterval) == 1L,
            is.finite(samplingInterval), samplingInterval > 0)
  Frequency <- 1 / samplingInterval
  Frequency <- Frequency - 16 * .Machine$double.eps * max(1, Frequency)
  b <- 0L:max(0L, ceiling(log(Frequency, 5)) + 6L)
  a <- pmax(0L, ceiling(log(Frequency, 2) - b * log(5, 2)))
  AUX <- (2^a) * (5^b)
  AUX <- AUX[AUX >= Frequency]
  if (!length(AUX)) stop("No admissible sampling frequency.", call. = FALSE)
  Frequency <- AUX[which.min(AUX)]
  if (Frequency <= .Machine$integer.max) Frequency <- as.integer(round(Frequency))
  1 / Frequency
}

.regularizeSeries <- function(t, s) {
  IDX <- order(t)
  t <- t[IDX]
  Interval <- .rationalizeInterval(min(diff(t)))
  Time <- (0:floor((max(t) - min(t)) / Interval + 1e-12)) * Interval
  if (is.matrix(s)) {
    OUT <- vapply(seq_len(ncol(s)), function(j) {
      stats::splinefun(t - min(t), s[IDX, j], method = "monoH.FC")(Time)
    }, numeric(length(Time)))
    colnames(OUT) <- colnames(s)
  }
  if (is.null(dim(s))) {
    OUT <- stats::splinefun(t - min(t), s[IDX], method = "monoH.FC")(Time)
  }
  list(t = Time, s = OUT, samplingInterval = Interval)
}

.resolveUnitFactor <- function(unitsSource, unitsTarget, ID) {
  if (!is.character(ID) || length(ID) != 1L || is.na(ID) ||
      !ID %in% c("AT", "VT", "DT")) {
    .stopUsage("ID must be AT, VT or DT.", operation = ".resolveUnitFactor")
  }
  Units <- c(mm = 1, cm = 10, m = 1000)
  if (!is.character(unitsTarget) || length(unitsTarget) != 1L ||
      is.na(unitsTarget) || !tolower(unitsTarget) %in% names(Units)) {
    .stopUsage("unitsTarget must be mm, cm or m.", operation = ".resolveUnitFactor")
  }
  if (ID == "AT") Units <- c(Units, g = 9806.650, gal = 10)
  if (!is.character(unitsSource) || length(unitsSource) != 1L ||
      is.na(unitsSource) || !tolower(unitsSource) %in% names(Units)) {
    .stopUsage("unitsSource is not a canonical unit for ", ID, ".", operation = ".resolveUnitFactor")
  }
  unname(Units[tolower(unitsSource)] / Units[tolower(unitsTarget)])
}

.getGravity <- function(unitsTarget) {
  9806.650 * .resolveUnitFactor(unitsSource = "mm", unitsTarget = unitsTarget,
                              ID = "AT")
}
