# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Assess compatibility of a scaled suite
#'
#' Calculates record diagnostics and a suite summary against a target band,
#' preserving the input tables and performing no file access.
#'
#' @param PSW A data.table of scaled spectra with \code{RecordID}, \code{Tn}
#'   in seconds and \code{PSA.<OCID>} in acceleration units.
#'   Available \code{OwnerID}, \code{EventID}, \code{StationID} and
#'   \code{ProcessID} keys are retained.
#' @param target A table with finite, unique, non-negative \code{Tn} and
#'   finite positive \code{Sa.low}, \code{Sa.mean}, \code{Sa.high} in the
#'   same acceleration units as \code{PSW}, ordered from low through mean to high.
#'   Exactly one row satisfies \code{abs(Tn) <= 1e-12}, the PGA anchor.
#' @param scale A data.table with unique \code{RecordID} and numeric
#'   \code{scaleFactor}; every evaluated record requires a finite factor.
#' @param OCID Component identifier: \code{"H1"}, \code{"H2"} or \code{"UP"}.
#' @param factorMin Finite non-negative lower record-factor bound.
#' @param factorMax Upper record-factor bound, at least \code{factorMin};
#'   \code{Inf} permits an unbounded upper factor.
#'
#' @return A list of data.tables: \code{record} contains record keys,
#'   \code{nTn}, PGA values and target ratios, relative PSA RMSE, band
#'   containment and violations, \code{scaleFactor}, \code{scaleAtMin},
#'   \code{scaleAtMax} and PGA \code{status}, ordered by decreasing \code{PGA.final}.
#'   \code{summary} contains record counts by PGA status and factor bounds,
#'   and median or extreme values of record RMSE, containment and violation.
#'
#' @details Duplicate \code{RecordID/Tn} keys in \code{PSW} are errors.
#'   Calculations use matching periods without interpolation; an empty
#'   intersection is an error, and \code{nTn} reports its size for each record.
#'   When a record lacks the PGA period, its PGA value, ratios and status
#'   remain \code{NA}.
#'   Band containment uses \code{1e-6 * pmax(1, abs(Sa.high))}; relative
#'   violations use the band limits without that tolerance.
#'   Factor-bound flags use an absolute difference below \code{1e-8}.
#' @seealso [fitRecordFactor()]
#' @examples
#' Target <- data.table::data.table(Tn = c(0, 1), Sa.low = 0.50 * 9806.65,
#'   Sa.mean = 0.84 * 9806.65, Sa.high = 0.95 * 9806.65)
#' PSW <- data.table::data.table(RecordID = rep(c("R1", "R2"), each = 2),
#'   Tn = c(0, 1, 0, 1), PSA.H1 = c(7000, 8500, 4000, 9800))
#' Scale <- data.table::data.table(RecordID = c("R1", "R2"), scaleFactor = c(1, 1.6))
#' Compatibility <- assessCompatibility(PSW, target = Target, scale = Scale,
#'   OCID = "H1", factorMin = 0.5, factorMax = 1.6)
#' print(Compatibility$record[, c("RecordID", "PGA.final", "PSA.inside",
#'                                "scaleAtMax", "status")])
#' print(Compatibility$summary[, c("records", "nPGA.inside", "nPGA.below",
#'                                 "nScaleAtMax")])
#' @importFrom data.table fifelse
#' @export
assessCompatibility <- function(PSW, target, scale, OCID, factorMin, factorMax) {
  if (!inherits(PSW, "data.table") || !inherits(scale, "data.table")) {
    .stopUsage("PSW and scale must be data.tables.", operation = "assessCompatibility")
  }
  if (!is.character(OCID) || length(OCID) != 1L || is.na(OCID) || !OCID %in% c("H1", "H2", "UP")) {
    .stopUsage("OCID must be H1, H2 or UP.", operation = "assessCompatibility")
  }
  if (!is.numeric(factorMin) || length(factorMin) != 1L || !is.finite(factorMin) || factorMin < 0 ||
      !is.numeric(factorMax) || length(factorMax) != 1L || is.na(factorMax) || factorMax < factorMin) {
    .stopUsage("factorMin/factorMax must define a non-negative interval with finite minimum.",
               operation = "assessCompatibility")
  }
  COL <- paste("PSA", OCID, sep = ".")
  COLS.key <- intersect(c("RecordID", "OwnerID", "EventID", "StationID", "ProcessID"), names(PSW))
  .checkTableKeys(PSW, by = character(), columns = c("RecordID", "Tn", COL),
                  keys = c("RecordID", "Tn"), operation = "assessCompatibility")
  .checkTableKeys(scale, by = character(), columns = c("RecordID", "scaleFactor"),
                  keys = "RecordID", operation = "assessCompatibility")
  if (!is.character(PSW$RecordID) || anyNA(PSW$RecordID) || any(!nzchar(PSW$RecordID)) ||
      !is.character(scale$RecordID) || anyNA(scale$RecordID) || any(!nzchar(scale$RecordID))) {
    .stopUsage("RecordID must contain nonempty character values.", operation = "assessCompatibility")
  }
  if (!is.numeric(PSW$Tn) || !is.numeric(PSW[[COL]]) || !is.numeric(scale$scaleFactor)) {
    .stopUsage("Periods, spectra and scale factors must be numeric.", operation = "assessCompatibility")
  }
  Target <- .checkTargetBand(target, operation = "assessCompatibility")
  TargetPGA <- Target[abs(Tn) <= 1e-12]
  if (nrow(TargetPGA) != 1L) {
    .stopUsage("Compatibility requires exactly one target PGA row.", operation = "assessCompatibility")
  }
  DT <- PSW[, c(COLS.key, "Tn", COL), with = FALSE]
  setnames(DT, COL, "Sa")
  DT <- Target[DT, on = "Tn", nomatch = 0L]
  if (!nrow(DT)) {
    .stopUsage("Compatibility lost all PSW rows after target alignment.", operation = "assessCompatibility")
  }
  DT[, `:=`(isPGA = abs(Tn) <= 1e-12,
             inside = .matchInside(Sa, Sa.low, Sa.high),
             violation = pmax((Sa.low - Sa) / Sa.low, (Sa - Sa.high) / Sa.high, 0))]
  Record <- DT[, {
    PGA <- NA_real_
    if (any(isPGA)) PGA <- Sa[which(isPGA)[1L]]
    .(nTn = .N, PGA.final = PGA,
      PGA.target.low = TargetPGA$Sa.low, PGA.target.mean = TargetPGA$Sa.mean,
      PGA.target.high = TargetPGA$Sa.high,
      PGA.ratio.mean = PGA / TargetPGA$Sa.mean, PGA.ratio.high = PGA / TargetPGA$Sa.high,
      PSA.RMSE = .matchRMSE(Sa, Sa.mean), PSA.inside = mean(inside, na.rm = TRUE),
      PSA.below = sum(Sa < Sa.low, na.rm = TRUE), PSA.above = sum(Sa > Sa.high, na.rm = TRUE),
      PSA.maxViolation = max(violation, na.rm = TRUE))
  }, by = COLS.key]
  Record[scale, scaleFactor := i.scaleFactor, on = "RecordID"]
  if (any(!is.finite(Record$scaleFactor))) {
    .stopUsage("Compatibility requires finite scaleFactor for records: ",
               paste(Record$RecordID[!is.finite(Record$scaleFactor)], collapse = ", "),
               operation = "assessCompatibility")
  }
  Record[, `:=`(scaleAtMin = abs(scaleFactor - factorMin) < 1e-8,
                 scaleAtMax = abs(scaleFactor - factorMax) < 1e-8,
                 status = fifelse(PGA.final < PGA.target.low, "below",
                                   fifelse(PGA.final > PGA.target.high, "above", "inside")))]
  setorderv(Record, "PGA.final", order = -1L)
  Summary <- data.table(
    records = nrow(Record), nPGA.inside = sum(Record$status == "inside", na.rm = TRUE),
    nPGA.below = sum(Record$status == "below", na.rm = TRUE),
    nPGA.above = sum(Record$status == "above", na.rm = TRUE),
    nScaleAtMin = sum(Record$scaleAtMin, na.rm = TRUE),
    nScaleAtMax = sum(Record$scaleAtMax, na.rm = TRUE),
    medianRecordRMSE = stats::median(Record$PSA.RMSE, na.rm = TRUE),
    maxRecordRMSE = max(Record$PSA.RMSE, na.rm = TRUE),
    medianInside = stats::median(Record$PSA.inside, na.rm = TRUE),
    minInside = min(Record$PSA.inside, na.rm = TRUE),
    medianMaxViolation = stats::median(Record$PSA.maxViolation, na.rm = TRUE),
    maxViolation = max(Record$PSA.maxViolation, na.rm = TRUE))
  list(record = Record[], summary = Summary)
}
