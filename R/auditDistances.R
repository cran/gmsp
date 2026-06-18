#' Audit distances in the master table -- sanity v1.
#'
#' Flags each row with the FIRST applicable reason from a fixed
#' precedence: lat/lon NA, lat/lon out of range, depth negative,
#' Repi above outlier threshold, Rhyp < Repi (geometric impossibility).
#' Rows with no issue are dropped from the output.
#'
#' Does NOT read `record.json` or provider flatfiles. Comparison against
#' raw/flatfile distances is deferred to v2.
#'
#' @param DT           master `data.table`.
#' @param repiOutlier  threshold in km above which `Repi` is
#'                     flagged (default 5000).
#' @return `data.table` of flagged rows with column `Reason`.
#' @examples
#' x <- data.table::data.table(
#'   EventLatitude = c(0, 95),
#'   EventLongitude = c(0, 0),
#'   StationLatitude = c(0.1, 0.1),
#'   StationLongitude = c(0.1, 0.1),
#'   EventDepth = c(10, 10),
#'   Repi = c(15, 20),
#'   Rhyp = c(18, 25)
#' )
#' auditDistances(x)
#'
#' @importFrom data.table copy fcase
#' @export
auditDistances <- function(DT, repiOutlier = 5000) {
  AUX <- copy(DT)
  AUX[, Reason := fcase(
    is.na(EventLatitude)    | is.na(StationLatitude),     "latNA",
    is.na(EventLongitude)   | is.na(StationLongitude),    "lonNA",
    abs(EventLatitude)   > 90,                            "eventLatOOR",
    abs(StationLatitude) > 90,                            "stationLatOOR",
    abs(EventLongitude)   > 180,                          "eventLonOOR",
    abs(StationLongitude) > 180,                          "stationLonOOR",
    !is.na(EventDepth) & EventDepth < 0,                  "depthNeg",
    !is.na(Repi) & Repi > repiOutlier,  "repiOut",
    !is.na(Rhyp) & Rhyp < Repi - 0.01, "geomBad",
    default = NA_character_
  )]
  AUX[!is.na(Reason)]
}
