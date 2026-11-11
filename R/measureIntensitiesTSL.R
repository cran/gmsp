# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Measure intensities from time histories
#'
#' Computes scalar intensity measures for each record, component and signal
#' domain after conversion to the requested length basis.
#'
#' @param TSL A table with columns \code{OCID,ID,t,s} and the metadata
#'   selected by \code{by}. Time t is in seconds and s contains finite
#'   numeric samples. Each record and component contains AT alone or the
#'   complete AT, VT and DT set.
#' @param by Character vector of record metadata columns; \code{character()}
#'   identifies one record without metadata. A retained \code{Units} column
#'   declares the target length basis and must agree with \code{unitsTarget}.
#' @param unitsSource,unitsTarget Required source and target length bases:
#'   \code{"mm"}, \code{"cm"} or \code{"m"}, matched case-insensitively.
#'   Acceleration units g and gal, and forms such as mm/s, are rejected.
#'
#' @return A new data.table with columns \code{by,OCID,ID,IM,value,units}.
#'   Rows follow quantity order AT, VT, DT and the measure order below;
#'   record/component groups retain their first-occurrence order within each
#'   measure. Dimensional values use \code{unitsTarget}; the units column
#'   identifies the units of each measure.
#'
#' @section Measures:
#' AT returns AI, AIu, AId, PGA, ARMS, AZC, ATo, ATn, D0595, D2080,
#' D0575, TmA, CAV, CAV5, NP, dt, Fs, Dmax, EPI and PDI.
#' VT returns PGV, VRMS, VZC, VTo, VTn and TmV.
#' DT returns PGD, DRMS, DZC, DTo, DTn and TmD.
#'
#' @details
#' Each series is ordered by t and requires at least two finite numeric
#' samples with unique times. Duplicate \code{by,OCID,ID,t} keys, missing AT,
#' incomplete domain sets and unsupported units are errors.
#'
#' Arias intensity and cumulative absolute velocity use mean-step rectangular
#' sums including the first sample. CAV5 includes samples with absolute
#' acceleration at least 0.05 times standard gravity. Durations use the first
#' crossings without interpolation: D0595 uses 0.05 and 0.95, D2080 uses
#' 0.20 and 0.80, and D0575 uses 0.05 and 0.75. Zero signals return zero
#' duration. Zero-crossing counts include adjacent zero
#' samples when the series has more than two samples. Dmax is the last time,
#' including a nonzero time origin. The first FFT coefficient is assigned
#' one frequency step, and mean periods use the 0.1--25 Hz band; zero signals
#' return zero mean period.
#' PDI retains Inf or NaN when its zero-crossing denominator is zero.
#'
#' The input is copied. The function reads or writes no files, uses no random
#' numbers and changes no session options.
#'
#' @seealso [accumulateIntensityTSL()], [widenIML()]
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' TSL <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, s = sin(2 * pi * t), ID = "AT", OCID = "H1"),
#'   data.table::data.table(t = t, s = cos(2 * pi * t), ID = "VT", OCID = "H1"),
#'   data.table::data.table(t = t, s = sin(pi * t), ID = "DT", OCID = "H1")))
#' measureIntensitiesTSL(TSL, by = character(),
#'                       unitsSource = "mm", unitsTarget = "mm")
#' @export
measureIntensitiesTSL <- function(TSL, by, unitsSource, unitsTarget) {
  DT <- copy(as.data.table(TSL))
  .checkTableKeys(DT, by = by, columns = c("OCID", "ID", "t", "s"),
                  keys = c(by, "OCID", "ID", "t"),
                  operation = "measureIntensitiesTSL")
  if (length(intersect(by, c("IM", "value", "units")))) {
    .stopUsage("by cannot contain IM, value or units.",
               operation = "measureIntensitiesTSL")
  }
  if (!nrow(DT)) {
    .stopUsage("TSL has no samples.", operation = "measureIntensitiesTSL")
  }
  for (j in c("t", "s")) {
    if (!is.numeric(DT[[j]]) || any(!is.finite(DT[[j]]))) {
      .stopUsage(j, " must contain finite numeric values.",
                 operation = "measureIntensitiesTSL")
    }
  }
  UnitFactor <- tryCatch(
    .resolveUnitFactor(unitsSource = unitsSource, unitsTarget = unitsTarget, ID = "DT"),
    error = function(e) .stopUsage(conditionMessage(e), operation = "measureIntensitiesTSL"))
  unitsTarget <- tolower(unitsTarget)
  if ("Units" %in% by &&
      (!is.character(DT[["Units"]]) || anyNA(DT[["Units"]]) ||
       any(tolower(DT[["Units"]]) != unitsTarget))) {
    .stopUsage("Units metadata must agree with unitsTarget.",
               operation = "measureIntensitiesTSL")
  }
  DT[, {
    Quantity <- sort(unique(as.character(ID)))
    if (anyNA(ID) || !(identical(Quantity, "AT") ||
                      identical(Quantity, c("AT", "DT", "VT")))) {
      .stopUsage("Expected AT only or complete AT/VT/DT for ",
                 paste(paste0(names(.BY), "=", .BY), collapse = ", "), ".",
                 operation = "measureIntensitiesTSL")
    }
    NULL
  }, by = c(by, "OCID")]
  DT[, s := s * UnitFactor]
  OUT <- DT[, {
    IDX <- order(t)
    if (.N < 2L || any(diff(t[IDX]) <= 0)) {
      .stopUsage("At least two increasing times are required for ",
                 paste(paste0(names(.BY), "=", .BY), collapse = ", "), ".",
                 operation = "measureIntensitiesTSL")
    }
    AUX <- .measureSeries(s[IDX], t = t[IDX], ID = as.character(.BY[["ID"]]),
                           unitsTarget = unitsTarget)
    list(IM = names(AUX$value), value = unname(AUX$value), units = unname(AUX$units))
  }, by = c(by, "OCID", "ID")]
  OUT[, `:=`(ID = factor(ID, levels = c("AT", "VT", "DT")),
              IM = factor(IM, levels = unique(IM)))]
  setorderv(OUT, c("ID", "IM"), na.last = TRUE)
  OUT[, `:=`(ID = as.character(ID), IM = as.character(IM))]
  setcolorder(OUT, c(by, "OCID", "ID", "IM", "value", "units"))
  OUT[]
}
