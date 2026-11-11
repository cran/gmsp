# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Accumulate normalized intensity histories
#'
#' Computes cumulative prefixes from acceleration rows and divides each
#' record/component history by its own maximum.
#'
#' @param TSL A table with columns \code{OCID,ID,t,s} and the metadata
#'   selected by \code{by}. Rows with ID equal to AT supply acceleration
#'   samples; other signal domains are excluded from the result. Time t is
#'   in seconds and s is numeric acceleration in the source length basis
#'   per second squared.
#' @param by Character vector of record metadata columns; \code{character()}
#'   identifies one record without metadata. A retained \code{Units} column
#'   declares the target length basis and must agree with \code{unitsTarget}.
#' @param ID Cumulative quantity: exactly \code{"AI"}, \code{"CAV"} or
#'   \code{"CAV5"}.
#' @param unitsSource,unitsTarget Required source and target length bases:
#'   \code{"mm"}, \code{"cm"} or \code{"m"}, matched case-insensitively.
#'   Acceleration units g and gal, and forms such as mm/s, are rejected.
#'
#' @return A new data.table with columns \code{by,OCID,ID,t,s}, ordered by
#'   metadata, component and time. ID contains the requested quantity and s
#'   contains dimensionless ratios. A zero or nonfinite cumulative maximum
#'   produces NA_real_ throughout that history, including arithmetic overflow
#'   from finite samples.
#'
#' @details
#' At least one AT history is required. Each history requires at least two
#' finite numeric samples and unique times; rows are sorted by t before
#' calculation. Missing columns, duplicate keys and invalid units cause errors.
#'
#' Prefix sums use the mean time step and include the first sample. AI sums
#' squared acceleration with the Arias scale; CAV sums absolute acceleration.
#' CAV5 includes absolute acceleration at least 0.05 times standard gravity.
#' This CAV5 cutoff is the inherited gmsp definition.
#'
#' The input is copied. The function reads or writes no files, uses no random
#' numbers and changes no session options.
#'
#' @seealso [measureIntensitiesTSL()]
#' @examples
#' TSL <- data.table::data.table(
#'   RecordID = "R1", OCID = "H1", ID = "AT",
#'   t = seq(0, 1, by = 0.25), s = c(0, 400, 500, 400, 0))
#' accumulateIntensityTSL(TSL, by = "RecordID", ID = "CAV5",
#'                        unitsSource = "mm", unitsTarget = "mm")
#' @export
accumulateIntensityTSL <- function(TSL, by, ID, unitsSource, unitsTarget) {
  DT <- copy(as.data.table(TSL))
  .checkTableKeys(DT, by = by, columns = c("OCID", "ID", "t", "s"),
                  keys = c(by, "OCID", "ID", "t"),
                  operation = "accumulateIntensityTSL")
  if (!is.character(ID) || length(ID) != 1L || is.na(ID) ||
      !ID %in% c("AI", "CAV", "CAV5")) {
    .stopUsage("ID must be AI, CAV or CAV5.", operation = "accumulateIntensityTSL")
  }
  UnitFactor <- tryCatch(
    .resolveUnitFactor(unitsSource = unitsSource, unitsTarget = unitsTarget, ID = "DT"),
    error = function(e) .stopUsage(conditionMessage(e), operation = "accumulateIntensityTSL"))
  unitsTarget <- tolower(unitsTarget)
  DT <- DT[ID == "AT", c(by, "OCID", "t", "s"), with = FALSE]
  if (!nrow(DT)) {
    .stopUsage("TSL requires AT rows.", operation = "accumulateIntensityTSL")
  }
  for (j in c("t", "s")) {
    if (!is.numeric(DT[[j]]) || any(!is.finite(DT[[j]]))) {
      .stopUsage(j, " must contain finite numeric values.",
                 operation = "accumulateIntensityTSL")
    }
  }
  if ("Units" %in% by &&
      (!is.character(DT[["Units"]]) || anyNA(DT[["Units"]]) ||
       any(tolower(DT[["Units"]]) != unitsTarget))) {
    .stopUsage("Units metadata must agree with unitsTarget.",
               operation = "accumulateIntensityTSL")
  }
  DT[, s := s * UnitFactor]
  setorderv(DT, c(by, "OCID", "t"), na.last = TRUE)
  Gravity <- .getGravity(unitsTarget)
  OUT <- DT[, {
    if (.N < 2L || any(diff(t) <= 0)) {
      .stopUsage("At least two increasing times are required for ",
                 paste(paste0(names(.BY), "=", .BY), collapse = ", "), ".",
                 operation = "accumulateIntensityTSL")
    }
    list(t = t, s = .accumulateSeries(s, samplingInterval = mean(diff(t)),
                                     ID = ID, gravity = Gravity))
  }, by = c(by, "OCID")]
  OUT[, ID := ID]
  setcolorder(OUT, c(by, "OCID", "ID", "t", "s"))
  OUT[]
}
