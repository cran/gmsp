# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.checkTableKeys <- function(data, by, columns, keys, operation) {
  if (!is.character(by) || anyNA(by) || any(!nzchar(by)) ||
      anyDuplicated(by) || length(intersect(by, columns))) {
    .stopUsage("by must contain distinct metadata column names.",
               operation = operation)
  }
  COLS <- setdiff(unique(c(by, columns, keys)), names(data))
  if (length(COLS)) {
    .stopUsage("Missing columns: ", paste(COLS, collapse = ", "),
               operation = operation)
  }
  if (anyDuplicated(names(data))) {
    .stopUsage("Column names must be unique.", operation = operation)
  }
  if (anyDuplicated(data, by = keys)) {
    .stopUsage("Duplicated table keys.", operation = operation)
  }
  invisible(NULL)
}

.preferValues <- function(values, preferred) {
  values <- as.character(values)
  preferred <- as.character(preferred)
  c(intersect(preferred, values), setdiff(values, preferred))
}

#' Widen a canonical long time-series table
#'
#' Projects samples to columns named \code{<ID>.<OCID>}. It performs no
#' signal processing or unit conversion and does not modify its input.
#'
#' @param TSL A table with \code{OCID}, \code{ID}, \code{t} and \code{s},
#'   and the metadata columns selected by \code{by}. Time is in seconds.
#' @param by Character vector of metadata columns identifying a record.
#'   Use \code{character()} for one record without metadata. Names must be
#'   unique, present, and distinct from the data columns.
#' @param ids Preferred order of quantity identifiers. Other identifiers
#'   remain after these; this argument never filters quantities.
#' @return A new data.table ordered by \code{by,t}, with those columns first
#'   and one column per quantity/component. Missing combinations become NA.
#'   Values and unit-bearing metadata selected in \code{by} are preserved.
#' @details Duplicate \code{by,t,ID,OCID} keys are errors. A dense input with
#'   nonempty identifiers and no dots in ID supports a lossless value round
#'   trip. The first dot separates ID from OCID; further dots belong to OCID.
#'   Sparse input acquires explicit NA rows on conversion back.
#' @seealso [lengthenTSW()]
#' @examples
#' TSL <- data.table::data.table(
#'   RecordID = "R1", OCID = "H1", ID = rep(c("AT", "VT"), each = 2),
#'   t = rep(c(0, 0.01), 2), s = c(1, 2, 0.1, 0.2))
#' widenTSL(TSL, by = "RecordID")
#' @importFrom data.table as.data.table copy dcast melt measurev setcolorder
#'   setorderv setnames :=
#' @export
widenTSL <- function(TSL, by, ids = c("AT", "VT", "DT")) {
  DT <- copy(as.data.table(TSL))
  .checkTableKeys(DT, by = by, columns = c("t", "s", "ID", "OCID"),
                  keys = c(by, "t", "ID", "OCID"), operation = "widenTSL")
  Formula <- stats::as.formula(call(
    "~", Reduce(function(x, y) call("+", x, y), lapply(c(by, "t"), as.name)),
    quote(ID + OCID)))
  OUT <- dcast(DT, Formula, value.var = "s", sep = ".")
  COLS <- unlist(lapply(.preferValues(unique(DT[["ID"]]), ids), function(x) {
    paste(x, unique(DT[["OCID"]]), sep = ".")
  }), use.names = FALSE)
  setcolorder(OUT, c(by, "t", intersect(COLS, names(OUT))))
  setorderv(OUT, c(by, "t"), na.last = TRUE)
  OUT[]
}

#' Lengthen wide time-series columns
#'
#' Converts columns named \code{<ID>.<OCID>} into canonical samples without
#' signal processing, unit conversion, or mutation of the supplied table.
#'
#' @param TSW A table with \code{t} (or \code{ts} when t is absent), the
#'   selected metadata and one or more \code{<ID>.<OCID>} columns.
#' @param by Character vector of metadata columns to retain; use
#'   \code{character()} for one record without metadata. All remaining
#'   non-time columns must be signal columns.
#' @param ids Preferred quantity order; quantities not listed are retained.
#' @return A new data.table with columns \code{by,OCID,ID,t,s}, ordered by
#'   metadata, component, preferred quantity and time. NA values remain NA.
#'   Metadata and amplitudes retain their supplied units.
#' @details The first dot separates ID and OCID; additional dots in OCID
#'   remain intact. Duplicate metadata/time keys are errors. Tables carrying
#'   both t and ts are rejected. Sparse wide cells become explicit NA rows.
#' @seealso [widenTSL()]
#' @examples
#' TSW <- data.table::data.table(RecordID = "R1", t = c(0, 0.01),
#'                               AT.H1 = c(1, 2), VT.H1 = c(0.1, 0.2))
#' lengthenTSW(TSW, by = "RecordID")
#' @export
lengthenTSW <- function(TSW, by, ids = c("AT", "VT", "DT")) {
  DT <- copy(as.data.table(TSW))
  if (all(c("t", "ts") %in% names(DT))) {
    .stopUsage("Supply t or ts, not both.", operation = "lengthenTSW")
  }
  if (!"t" %in% names(DT) && "ts" %in% names(DT)) setnames(DT, "ts", "t")
  .checkTableKeys(DT, by = by, columns = "t", keys = c(by, "t"),
                  operation = "lengthenTSW")
  COLS <- setdiff(names(DT), c(by, "t"))
  if (!length(COLS) || any(!grepl("^[^.]+[.].+$", COLS))) {
    .stopUsage("Signal columns must be named <ID>.<OCID>.",
               operation = "lengthenTSW")
  }
  OUT <- melt(DT, id.vars = c(by, "t"), value.name = "s",
              measure.vars = measurev(list(ID = NULL, OCID = NULL),
                                      pattern = "^([^.]+)[.](.+)$", cols = COLS))
  OUT[, ID := factor(ID, levels = .preferValues(unique(ID), ids))]
  setorderv(OUT, c(by, "OCID", "ID", "t"), na.last = TRUE)
  OUT[, ID := as.character(ID)]
  setcolorder(OUT, c(by, "OCID", "ID", "t", "s"))
  OUT[]
}
