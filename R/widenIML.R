# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Widen long intensity measures
#'
#' Projects intensity measures to one row per record and component, with
#' one column for each measure. Values and units are preserved without
#' conversion.
#'
#' @param IML A table with columns \code{OCID,IM,value}, optional \code{ID}
#'   and \code{units}, and the metadata selected by \code{by}.
#' @param by Character vector of record metadata columns; \code{character()}
#'   retains OCID as the row key. Data columns cannot be metadata keys.
#'
#' @return A new data.table with columns \code{by,OCID,<IM>}, ordered by
#'   metadata and component. Measure columns follow their first occurrence
#'   in IML; missing combinations and supplied NA values remain missing.
#'   The tabular attribute \code{measureMetadata} retains
#'   \code{by,OCID,IM} and the available \code{ID,units} fields.
#'   A correctly shaped empty input returns the row-key columns with zero
#'   rows and no measure columns.
#'
#' @details
#' Keys \code{by,OCID,IM} must be unique; duplicate or contradictory measure
#' rows cause an error without aggregation. Absent ID or units fields are
#' omitted from the attribute. Metadata excluded by by are dropped.
#' CSV serialization contains the value columns; the measureMetadata
#' attribute remains an in-memory representation of domains and units.
#'
#' The input is copied. The function reads or writes no files, uses no random
#' numbers and changes no session options.
#'
#' @seealso [measureIntensitiesTSL()]
#' @examples
#' IML <- data.table::data.table(
#'   RecordID = "R1", OCID = "H1", ID = "AT", IM = c("PGA", "CAV"),
#'   value = c(500, 120), units = c("mm /s2", "mm /s"))
#' IMW <- widenIML(IML, by = "RecordID")
#' attr(IMW, "measureMetadata")
#' @export
widenIML <- function(IML, by) {
  DT <- copy(as.data.table(IML))
  .checkTableKeys(DT, by = by, columns = c("OCID", "IM", "value"),
                  keys = c(by, "OCID", "IM"), operation = "widenIML")
  if (length(intersect(by, c("ID", "units")))) {
    .stopUsage("by cannot contain ID or units.", operation = "widenIML")
  }
  COLS <- c(by, "OCID", "IM", intersect(c("ID", "units"), names(DT)))
  Metadata <- DT[, COLS, with = FALSE]
  if (!nrow(DT)) {
    OUT <- DT[0, c(by, "OCID"), with = FALSE]
    data.table::setattr(OUT, "measureMetadata", Metadata)
    return(OUT[])
  }
  Formula <- stats::as.formula(call(
    "~", Reduce(function(x, y) call("+", x, y), lapply(c(by, "OCID"), as.name)),
    as.name("IM")))
  OUT <- dcast(DT, Formula, value.var = "value")
  setcolorder(OUT, c(by, "OCID", intersect(unique(as.character(DT[["IM"]])), names(OUT))))
  setorderv(OUT, c(by, "OCID"), na.last = TRUE)
  data.table::setattr(OUT, "measureMetadata", Metadata)
  OUT[]
}
