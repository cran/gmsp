#' Audit site / station information in a record metadata table.
#'
#' Flags rows with the FIRST applicable reason: `StationVs30` NA, below
#' low cutoff, or above high cutoff. Coord checks live in `auditDistances`.
#'
#' @param DT       record metadata `data.table`.
#' @param vs30Low  lower physical cutoff for `StationVs30` (m/s).
#' @param vs30High upper physical cutoff (m/s).
#' @return `data.table` of flagged rows with column `Reason`.
#' @examples
#' x <- data.table::data.table(StationVs30 = c(30, 760, NA))
#' auditSite(x)
#'
#' @importFrom data.table copy fcase
#' @export
auditSite <- function(DT, vs30Low = 50, vs30High = 3000) {
  AUX <- copy(DT)
  AUX[, Reason := fcase(
    is.na(StationVs30),       "vs30NA",
    StationVs30 < vs30Low,    "vs30Low",
    StationVs30 > vs30High,   "vs30High",
    default = NA_character_
  )]
  AUX[!is.na(Reason)]
}
