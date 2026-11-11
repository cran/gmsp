# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Assess coordinate and distance metadata
#'
#' Returns flagged rows with the first applicable reason, preserving the input.
#'
#' @param data Table with numeric EventLatitude, EventLongitude,
#'   StationLatitude, StationLongitude, EventDepth, Repi and Rhyp.
#'   Repi and Rhyp use kilometres. Additional columns are retained.
#' @param repiOutlier Finite scalar cutoff in kilometres for Repi.
#' @return A data.table containing flagged rows in input order, with Reason.
#' @details Reasons follow this precedence: latNA, lonNA, eventLatOOR,
#'   stationLatOOR, eventLonOOR, stationLonOOR, depthNeg, repiOut, geomBad.
#'   Absolute coordinate limits are 90 for latitude and 180 for longitude.
#'   geomBad uses Rhyp less than Repi minus 0.01 kilometres; this is the
#'   diagnostic rule. Missing coordinates are flagged, while missing depth
#'   or distances do not by themselves produce a reason.
#'   Missing required columns, nonnumeric columns and an invalid cutoff are
#'   input errors. The operation performs no file access.
#' @seealso [assessSite()]
#' @examples
#' Records <- data.frame(RecordID = c("R1", "R2", "R3", "R4"),
#'   EventLatitude = c(10, NA, 10, 10), EventLongitude = 20,
#'   StationLatitude = 10.4, StationLongitude = 20.1, EventDepth = 30,
#'   Repi = c(45, 45, 450, 45), Rhyp = c(54, 54, 451, 40))
#' assessDistances(Records, repiOutlier = 300)
#' @export
assessDistances <- function(data, repiOutlier) {
  DT <- copy(as.data.table(data))
  COLS <- c("EventLatitude", "EventLongitude", "StationLatitude",
             "StationLongitude", "EventDepth", "Repi", "Rhyp")
  if (anyDuplicated(names(DT)) || length(setdiff(COLS, names(DT))) ||
      any(!vapply(DT[, COLS, with = FALSE], is.numeric, logical(1L)))) {
    .stopUsage("Distance metadata requires numeric coordinate, depth, Repi and Rhyp columns.",
               operation = "assessDistances")
  }
  if (!is.numeric(repiOutlier) || length(repiOutlier) != 1L || !is.finite(repiOutlier)) {
    .stopUsage("repiOutlier must be one finite number.", operation = "assessDistances")
  }
  DT[, Reason := data.table::fcase(
    is.na(EventLatitude) | is.na(StationLatitude), "latNA",
    is.na(EventLongitude) | is.na(StationLongitude), "lonNA",
    abs(EventLatitude) > 90, "eventLatOOR",
    abs(StationLatitude) > 90, "stationLatOOR",
    abs(EventLongitude) > 180, "eventLonOOR",
    abs(StationLongitude) > 180, "stationLonOOR",
    !is.na(EventDepth) & EventDepth < 0, "depthNeg",
    !is.na(Repi) & Repi > repiOutlier, "repiOut",
    !is.na(Rhyp) & Rhyp < Repi - 0.01, "geomBad",
    default = NA_character_)]
  DT[!is.na(Reason)]
}
