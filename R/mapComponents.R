#' Map provider components to canonical processed components.
#'
#' Classifies a single three-component record, preserves provider channel names
#' in `OCID`, and records canonical processed directions in `DIR`
#' (`H1`, `H2`, `UP`). This helper is for processed products; raw extraction
#' continues to preserve provider `OCID` values.
#'
#' @param DT LONG `data.table(t, OCID, s)` or WIDE
#'   `data.table(t, <OCID1>, <OCID2>, <OCID3>)` for ONE record.
#' @param rotate Logical scalar. If `TRUE`, rotate the two horizontal
#'   components to their principal axes and store `attr(out, "theta")`.
#' @param output `"long"` (default) returns provider `OCID`, canonical `DIR`,
#'   and signal `s`; `"wide"` returns `data.table(t, H1, H2, UP)`.
#' @return A `data.table` in the requested `output` shape, or `NULL` when the
#'   record cannot be mapped. Both shapes carry `attr(out, "componentMap")`
#'   and `attr(out, "rotate")`; rotated outputs also carry
#'   `attr(out, "theta")`.
#' @seealso [extractRecord()]
#' @examples
#' t <- seq(0, 1, by = 0.1)
#' x <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, OCID = "N", s = sin(2 * pi * t)),
#'   data.table::data.table(t = t, OCID = "E", s = 0.5 * cos(2 * pi * t)),
#'   data.table::data.table(t = t, OCID = "Z", s = 0.1 * sin(2 * pi * t))
#' ))
#' mapped <- mapComponents(x, rotate = FALSE)
#' head(mapped)
#'
#' @importFrom data.table copy data.table dcast fcase fifelse is.data.table melt setattr setcolorder
#' @importFrom stats cov
#' @export
mapComponents <- function(DT, rotate = TRUE, output = c("long", "wide")) {
  stopifnot(is.data.table(DT))
  stopifnot(is.logical(rotate), length(rotate) == 1L, !is.na(rotate))
  output <- match.arg(output)

  DT <- .componentInput(DT)
  if (is.null(DT)) return(NULL)

  OUT <- .classifyComponents(DT)
  if (is.null(OUT)) return(NULL)

  MAP <- unique(OUT[, .(OCID, DIR)])
  if (nrow(MAP) != 3L) return(NULL)
  if (!identical(sort(MAP$DIR), c("H1", "H2", "UP"))) return(NULL)

  if (rotate) {
    AUX <- dcast(OUT[DIR %chin% c("H1", "H2")], t ~ DIR, value.var = "s")
    if (!all(c("H1", "H2") %chin% names(AUX))) return(NULL)

    KEEP <- is.finite(AUX[["H1"]]) & is.finite(AUX[["H2"]])
    if (sum(KEEP) < 2L) return(NULL)

    COV <- cov(cbind(AUX[["H1"]][KEEP], AUX[["H2"]][KEEP]))
    if (!all(is.finite(COV))) return(NULL)

    E <- eigen(COV, symmetric = TRUE)
    theta <- atan2(E$vectors[2L, 1L], E$vectors[1L, 1L])

    H1 <- AUX[["H1"]] * cos(theta) + AUX[["H2"]] * sin(theta)
    H2 <- -AUX[["H1"]] * sin(theta) + AUX[["H2"]] * cos(theta)
    AUX <- data.table(t = AUX$t, H1 = H1, H2 = H2)

    OUT[AUX, on = "t",
        s := fcase(DIR == "H1", i.H1,
                   DIR == "H2", i.H2,
                   default     = s)]
    setattr(OUT, "theta", theta)
  }

  setattr(OUT, "componentMap", MAP)
  setattr(OUT, "rotate", rotate)

  FRONT <- c("t", "OCID", "DIR", "s")
  setcolorder(OUT, c(FRONT, setdiff(names(OUT), FRONT)))
  if (output == "wide") return(.wideComponents(OUT))
  OUT[]
}

#' @noRd
.componentInput <- function(DT) {
  if (!("t" %chin% names(DT))) return(NULL)
  if (all(c("OCID", "s") %chin% names(DT))) return(copy(DT))

  COLS <- setdiff(names(DT), "t")
  if (length(COLS) != 3L) return(NULL)
  OUT <- melt(
    copy(DT),
    id.vars = "t",
    measure.vars = COLS,
    variable.name = "OCID",
    value.name = "s",
    variable.factor = FALSE
  )
  OUT[, OCID := as.character(OCID)]
  OUT[]
}

#' @noRd
.wideComponents <- function(DT) {
  AUX <- DT[, .N, by = .(t, DIR)]
  if (any(AUX[["N"]] > 1L)) return(NULL)

  OUT <- dcast(DT[, .(t, DIR, s)], t ~ DIR, value.var = "s")
  if (!all(c("H1", "H2", "UP") %chin% names(OUT))) return(NULL)
  OUT <- OUT[, .(t, H1, H2, UP)]

  MAP <- attr(DT, "componentMap", exact = TRUE)
  if (!is.null(MAP)) setattr(OUT, "componentMap", MAP)
  Rotate <- attr(DT, "rotate", exact = TRUE)
  if (!is.null(Rotate)) setattr(OUT, "rotate", Rotate)
  Theta <- attr(DT, "theta", exact = TRUE)
  if (!is.null(Theta)) setattr(OUT, "theta", Theta)
  OUT[]
}

#' @noRd
.classifyComponents <- function(DT) {
  OUT <- copy(DT)
  AUX <- unique(OUT$OCID)
  if (length(AUX) != 3L) return(NULL)
  if (sum(.isVerticalComponent(AUX)) != 1L) return(NULL)
  OUT[, DIR := fifelse(.isVerticalComponent(OCID), "UP", "H2")]

  COLS <- OUT[DIR == "H2", unique(OCID)]
  if (length(COLS) != 2L) return(NULL)

  AUX <- dcast(OUT[DIR == "H2"], t ~ OCID, value.var = "s")
  MAT <- as.matrix(AUX[, COLS, with = FALSE])
  OK  <- is.finite(MAT[, 1L]) & is.finite(MAT[, 2L])

  if (sum(OK) >= 2L) {
    COV <- cov(MAT[OK, , drop = FALSE])
    if (all(is.finite(COV))) {
      LOAD <- abs(eigen(COV, symmetric = TRUE)$vectors[, 1L])
      OUT[OCID == COLS[which.max(LOAD)], DIR := "H1"]
      return(OUT)
    }
  }

  PEAK <- vapply(COLS, function(ocid) {
    X <- OUT[OCID == ocid, s]
    X <- X[is.finite(X)]
    if (!length(X)) return(-Inf)
    max(abs(X))
  }, numeric(1L))
  if (!any(is.finite(PEAK))) return(NULL)
  OUT[OCID == COLS[which.max(PEAK)], DIR := "H1"]
  OUT
}

#' @noRd
.isVerticalComponent <- function(x) {
  X <- toupper(x)
  X %in% .verticalComponents | grepl("Z$", X, perl = TRUE)
}

#' @noRd
.verticalComponents <- toupper(c(
  "U", "UD", "UD2", "UP", "V", "HLZ", "HNZ", "DWN", "DN", "HHZ", "BHZ",
  "Z", "DOWN", "VER", "VERT", "VERTICAL", "VRT", "HGZ", "UPDO"
))
