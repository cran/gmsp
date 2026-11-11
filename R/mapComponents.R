# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Map provider channels to canonical directions
#'
#' Classifies one three-component record as H1, H2 and UP while preserving
#' provider channel identities in OCID. Optional rotation aligns the horizontal
#' signals with their principal axes.
#'
#' @param data A long data.table with \code{t, OCID, s}, or a wide data.table
#'   with \code{t} and three provider signal columns.
#' @param rotate Logical scalar selecting horizontal rotation.
#' @param output Either \code{long} or \code{wide}.
#' @return A long data.table with \code{t, OCID, DIR, s}, or a wide data.table
#'   with \code{t, H1, H2, UP}. Both carry \code{componentMap} and \code{rotate}
#'   attributes; rotated results also carry \code{theta} in radians. Returns
#'   NULL when the record cannot be classified or rotated.
#' @details Classification requires three OCID values and one recognized
#'   vertical channel. Finite horizontal peaks provide the classification
#'   fallback when covariance is unavailable. Rotation requires at least two
#'   finite horizontal pairs. The input table remains unchanged.
#' @examples
#' Time <- seq(0, 1, by = 0.01)
#' Record <- data.table::data.table(
#'   t = rep(Time, 3), OCID = rep(c("HNE", "HNN", "HNZ"), each = length(Time)),
#'   s = c(sin(2 * pi * 2 * Time), 0.5 * sin(2 * pi * 2 * Time + 0.3),
#'         0.2 * sin(2 * pi * 5 * Time)))
#' Wide <- mapComponents(Record, rotate = FALSE, output = "wide")
#' head(Wide)
#' attr(Wide, "componentMap")
#' Rotated <- mapComponents(Record, rotate = TRUE)
#' attr(Rotated, "theta")
#' head(Rotated)
#' @importFrom data.table copy dcast fifelse fcase is.data.table setattr setcolorder uniqueN
#' @export
mapComponents <- function(data, rotate, output = "long") {
  if (!is.data.table(data)) .stopUsage("data must be a data.table.", operation = "mapComponents")
  if (!is.logical(rotate) || length(rotate) != 1L || is.na(rotate)) {
    .stopUsage("rotate must be one logical value.", operation = "mapComponents")
  }
  if (!is.character(output) || length(output) != 1L || is.na(output) ||
      !output %in% c("long", "wide")) {
    .stopUsage("output must be long or wide.", operation = "mapComponents")
  }
  if (!"t" %in% names(data)) return(NULL)
  DT <- data
  if (!all(c("OCID", "s") %in% names(DT))) {
    COLS <- setdiff(names(DT), "t")
    if (length(COLS) != 3L) return(NULL)
    DT <- melt(DT, id.vars = "t", measure.vars = COLS,
                variable.name = "OCID", value.name = "s", variable.factor = FALSE)
  }
  OUT <- .classifyComponents(DT)
  if (is.null(OUT)) return(NULL)
  MAP <- unique(OUT[, .(OCID, DIR)])
  if (nrow(MAP) != 3L || !identical(sort(MAP$DIR), c("H1", "H2", "UP"))) return(NULL)
  if (rotate) {
    AUX <- dcast(OUT[DIR %in% c("H1", "H2")], t ~ DIR, value.var = "s")
    if (!all(c("H1", "H2") %in% names(AUX))) return(NULL)
    OK <- is.finite(AUX[["H1"]]) & is.finite(AUX[["H2"]])
    if (sum(OK) < 2L) return(NULL)
    Covariance <- stats::cov(cbind(AUX[["H1"]][OK], AUX[["H2"]][OK]))
    if (!all(is.finite(Covariance))) return(NULL)
    Axes <- eigen(Covariance, symmetric = TRUE)
    Theta <- atan2(Axes$vectors[2L, 1L], Axes$vectors[1L, 1L])
    AUX <- data.table(t = AUX$t,
      H1 = AUX[["H1"]] * cos(Theta) + AUX[["H2"]] * sin(Theta),
      H2 = -AUX[["H1"]] * sin(Theta) + AUX[["H2"]] * cos(Theta))
    OUT[AUX, on = "t", s := fcase(DIR == "H1", i.H1, DIR == "H2", i.H2, default = s)]
    setattr(OUT, "theta", Theta)
  }
  setattr(OUT, "componentMap", MAP)
  setattr(OUT, "rotate", rotate)
  setcolorder(OUT, c("t", "OCID", "DIR", "s", setdiff(names(OUT), c("t", "OCID", "DIR", "s"))))
  if (output == "long") return(OUT[])
  AUX <- OUT[, .N, by = .(t, DIR)]
  if (any(AUX[["N"]] > 1L)) return(NULL)
  DT <- dcast(OUT[, .(t, DIR, s)], t ~ DIR, value.var = "s")
  if (!all(c("H1", "H2", "UP") %in% names(DT))) return(NULL)
  DT <- DT[, .(t, H1, H2, UP)]
  for (j in c("componentMap", "rotate", "theta")) {
    AUX <- attr(OUT, j, exact = TRUE)
    if (!is.null(AUX)) setattr(DT, j, AUX)
  }
  DT[]
}

.classifyComponents <- function(data) {
  OUT <- copy(data)
  AUX <- unique(OUT$OCID)
  if (length(AUX) != 3L || anyNA(AUX)) return(NULL)
  if (sum(.isVerticalComponent(AUX)) != 1L) return(NULL)
  OUT[, DIR := fifelse(.isVerticalComponent(OCID), "UP", "H2")]
  COLS <- OUT[DIR == "H2", unique(OCID)]
  if (length(COLS) != 2L) return(NULL)
  AUX <- dcast(OUT[DIR == "H2"], t ~ OCID, value.var = "s")
  DATA <- as.matrix(AUX[, COLS, with = FALSE])
  OK <- is.finite(DATA[, 1L]) & is.finite(DATA[, 2L])
  if (sum(OK) >= 2L) {
    Covariance <- stats::cov(DATA[OK, , drop = FALSE])
    if (all(is.finite(Covariance))) {
      Loading <- abs(eigen(Covariance, symmetric = TRUE)$vectors[, 1L])
      OUT[OCID == COLS[which.max(Loading)], DIR := "H1"]
      return(OUT)
    }
  }
  Peak <- vapply(COLS, function(ocid) {
    s <- OUT[OCID == ocid, s]
    s <- s[is.finite(s)]
    if (!length(s)) return(-Inf)
    max(abs(s))
  }, numeric(1L))
  if (!any(is.finite(Peak))) return(NULL)
  OUT[OCID == COLS[which.max(Peak)], DIR := "H1"]
  OUT
}

.isVerticalComponent <- function(x) {
  AUX <- toupper(x)
  AUX %in% c("U", "UD", "UD2", "UP", "V", "HLZ", "HNZ", "DWN", "DN", "HHZ", "BHZ",
              "Z", "DOWN", "VER", "VERT", "VERTICAL", "VRT", "HGZ", "UPDO") |
    grepl("Z$", AUX, perl = TRUE)
}
