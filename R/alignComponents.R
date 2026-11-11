# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Equalize component sample counts
#'
#' Pads shorter components with trailing zeros for max alignment, or truncates
#' longer components for min alignment. Alignment operates by row count without
#' interpolating time.
#'
#' @param data A long data.table with \code{t, OCID, s, DIR} for one record.
#' @param align Either \code{max} or \code{min}.
#' @return A list with \code{DT}, the aligned table, and \code{NP}, the final
#'   sample count per component. The input table remains unchanged.
#' @details Padding uses the interval between the first two samples of the
#'   first OCID. Empty input, unknown alignment and a nonfinite or nonpositive
#'   interval needed for padding raise errors.
#' @examples
#' Record <- data.table::data.table(
#'   t = c(0, 0.01, 0.02, 0, 0.01), OCID = c("H1", "H1", "H1", "H2", "H2"),
#'   DIR = c("H1", "H1", "H1", "H2", "H2"), s = c(1, 2, 3, 4, 5))
#' Padded <- alignComponents(Record, align = "max")
#' Padded$NP
#' print(Padded$DT)
#' alignComponents(Record, align = "min")$NP
#' @export
alignComponents <- function(data, align = "max") {
  if (!is.character(align) || length(align) != 1L || is.na(align) ||
      !align %in% c("max", "min")) {
    .stopUsage("align must be max or min.", operation = "alignComponents")
  }
  if (!is.data.table(data) || !all(c("t", "OCID", "s", "DIR") %in% names(data)) || !nrow(data)) {
    .stopUsage("data requires nonempty t, OCID, s and DIR columns.", operation = "alignComponents")
  }
  DT <- copy(data)
  AUX <- DT[, .N, by = OCID]
  SampleCount <- max(AUX$N)
  if (align == "min") SampleCount <- min(AUX$N)
  if (all(AUX$N == SampleCount)) return(list(DT = DT, NP = SampleCount))
  SamplingInterval <- NULL
  if (align == "max") {
    DATA <- DT[OCID == DT$OCID[1L], t]
    SamplingInterval <- DATA[2L] - DATA[1L]
    if (length(SamplingInterval) != 1L || !is.finite(SamplingInterval) || SamplingInterval <= 0) {
      .stopUsage("Padding requires a finite positive first-channel interval.", operation = "alignComponents")
    }
  }
  OUT <- rbindlist(lapply(AUX$OCID, function(ocid) {
    AUX <- DT[OCID == ocid]
    n <- nrow(AUX)
    if (n == SampleCount) return(AUX)
    if (n < SampleCount) {
      return(rbind(AUX, data.table(
        t = AUX[n, t] + SamplingInterval * seq_len(SampleCount - n),
        OCID = ocid, s = 0, DIR = AUX[1L, DIR])))
    }
    AUX[seq_len(SampleCount)]
  }))
  list(DT = OUT, NP = SampleCount)
}
