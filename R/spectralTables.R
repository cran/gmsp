# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Widen canonical response spectra
#'
#' Projects spectra to columns named \code{<ID>.<OCID>} without changing
#' values or units. The input table is preserved.
#'
#' @param PSL Long table with \code{OCID,Tn,ID,S} and selected metadata.
#' @param by Character vector of metadata columns forming row keys, or
#'   \code{character()} for one record without metadata. A damping column
#'   \code{xi} is retained when included in \code{by}.
#' @return A new data.table ordered by \code{by,Tn}. These key columns precede
#'   the spectra columns, ordered by component with H1, H2, UP, D50 and D100
#'   first, and by ID with PSA, PSV and SD first within each component.
#'   Other components and IDs remain after the preferred values.
#' @details Duplicate \code{by,Tn,ID,OCID} keys are errors; values are not
#'   aggregated. Missing combinations become NA. A correctly shaped empty
#'   input returns its key columns with no spectra columns. The projection
#'   uses no random numbers, files or session-option changes.
#' @seealso [lengthenPSW()], [computeSpectraTSL()]
#' @examples
#' PSL <- data.table::data.table(RecordID = "R1", OCID = "H1",
#'                                Tn = 0.1, ID = "PSA", S = 1)
#' widenPSL(PSL, by = "RecordID")
#' @importFrom data.table as.data.table copy dcast melt measurev setcolorder
#'   setorderv :=
#' @export
widenPSL <- function(PSL, by) {
  DT <- copy(as.data.table(PSL))
  .checkTableKeys(DT, by = by, columns = c("OCID", "Tn", "ID", "S"),
                  keys = c(by, "Tn", "ID", "OCID"), operation = "widenPSL")
  if (!nrow(DT)) return(DT[, c(by, "Tn"), with = FALSE])
  Formula <- stats::as.formula(call(
    "~", Reduce(function(x, y) call("+", x, y), lapply(c(by, "Tn"), as.name)),
    quote(ID + OCID)))
  OUT <- dcast(DT, Formula, value.var = "S", sep = ".")
  Components <- .preferValues(unique(DT[["OCID"]]), c("H1", "H2", "UP", "D50", "D100"))
  Quantities <- .preferValues(unique(DT[["ID"]]), c("PSA", "PSV", "SD"))
  COLS <- unlist(lapply(Components, function(x) paste(Quantities, x, sep = ".")),
                 use.names = FALSE)
  setcolorder(OUT, c(by, "Tn", intersect(COLS, names(OUT))))
  setorderv(OUT, c(by, "Tn"), na.last = TRUE)
  OUT[]
}

#' Lengthen wide response spectra
#'
#' Converts \code{<ID>.<OCID>} columns to canonical long spectra without
#' changing values, units or the supplied table.
#'
#' @param PSW Wide table with \code{Tn}, selected metadata and spectra columns
#'   named \code{<ID>.<OCID>}.
#' @param by Character vector of metadata columns to retain, or
#'   \code{character()} for one record without metadata. Remaining columns
#'   other than Tn are spectra columns.
#' @param ids Preferred order of spectral IDs. Other IDs remain after these;
#'   the argument orders rows without filtering quantities.
#' @return A new data.table with \code{by,OCID,Tn,ID,S}, ordered by metadata,
#'   component, preferred ID and period. NA values remain explicit rows.
#' @details The first dot separates ID from OCID; additional dots remain
#'   part of OCID. Duplicate metadata/period keys are errors. A correctly
#'   shaped empty input retains the key columns and has no spectral rows.
#'   The projection uses no random numbers, files or session-option changes.
#' @seealso [widenPSL()], [computeSpectraTSL()]
#' @examples
#' PSW <- data.table::data.table(RecordID = "R1", Tn = 0.1, PSA.H1 = 1)
#' lengthenPSW(PSW, by = "RecordID")
#' @export
lengthenPSW <- function(PSW, by, ids = c("PSA", "PSV", "SD")) {
  DT <- copy(as.data.table(PSW))
  .checkTableKeys(DT, by = by, columns = "Tn", keys = c(by, "Tn"),
                  operation = "lengthenPSW")
  COLS <- setdiff(names(DT), c(by, "Tn"))
  if (any(!grepl("^[^.]+[.].+$", COLS)) || (!length(COLS) && nrow(DT))) {
    .stopUsage("Spectra columns must be named <ID>.<OCID>.", operation = "lengthenPSW")
  }
  if (!length(COLS)) {
    OUT <- DT[, c(by, "Tn"), with = FALSE]
    OUT[, `:=`(OCID = character(), ID = character(), S = numeric())]
    setcolorder(OUT, c(by, "OCID", "Tn", "ID", "S"))
    return(OUT[])
  }
  OUT <- melt(DT, id.vars = c(by, "Tn"), value.name = "S",
              measure.vars = measurev(list(ID = NULL, OCID = NULL),
                                      pattern = "^([^.]+)[.](.+)$", cols = COLS))
  OUT[, ID := factor(ID, levels = .preferValues(unique(ID), ids))]
  setorderv(OUT, c(by, "OCID", "ID", "Tn"), na.last = TRUE)
  OUT[, ID := as.character(ID)]
  setcolorder(OUT, c(by, "OCID", "Tn", "ID", "S"))
  OUT[]
}
