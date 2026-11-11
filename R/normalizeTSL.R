# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Normalize all quantities by a reference peak
#'
#' Applies one factor per record and component, based on the peak of AT,
#' VT or DT. The input table is preserved, including its row order.
#'
#' @param TSL Canonical long table with \code{OCID,ID,t,s} and metadata.
#' @param by Character vector of record metadata columns, or
#'   \code{character()} for a single record without metadata.
#' @param reference Reference measure: exactly \code{"PGA"}, \code{"PGV"}
#'   or \code{"PGD"}, corresponding to AT, VT and DT.
#' @return A new data.table with the same columns and rows, whose s values
#'   are divided by the reference peak in each \code{by,OCID} group.
#'   No metadata unit is reinterpreted. Other quantities use the same factor;
#'   their own peaks need not equal one.
#' @details A missing reference, nonfinite reference sample or nonpositive
#'   reference peak is an error. Samples are not omitted to compute a peak.
#'   All quantities in the group are scaled, including additional IDs.
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' TSL <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, s = 2 * sin(2 * pi * t), ID = "AT", OCID = "H1"),
#'   data.table::data.table(t = t, s = cos(2 * pi * t), ID = "VT", OCID = "H1"),
#'   data.table::data.table(t = t, s = sin(pi * t), ID = "DT", OCID = "H1")))
#' normalizeTSL(TSL, by = character(), reference = "PGA")
#' @export
normalizeTSL <- function(TSL, by, reference) {
  DT <- copy(as.data.table(TSL))
  .checkTableKeys(DT, by = by, columns = c("OCID", "ID", "t", "s"),
                  keys = c(by, "OCID", "ID", "t"), operation = "normalizeTSL")
  if (!is.character(reference) || length(reference) != 1L ||
      is.na(reference) || !reference %in% c("PGA", "PGV", "PGD")) {
    .stopUsage("reference must be PGA, PGV or PGD.", operation = "normalizeTSL")
  }
  Reference <- switch(reference, PGA = "AT", PGV = "VT", PGD = "DT")
  DT[, s := {
    x <- s[ID == Reference]
    if (!length(x) || any(!is.finite(x)) || max(abs(x)) <= 0) {
      .stopUsage("Reference peak is missing, nonfinite or zero.",
                 operation = "normalizeTSL")
    }
    s * (1 / max(abs(x)))
  }, by = c(by, "OCID")]
  DT[]
}
