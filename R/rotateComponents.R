#' Rotate horizontal components to principal axes.
#'
#' Applies a 2-D PCA rotation to the two horizontal signal components
#' (`DIR == "H1"` and `"H2"`) so that H1 aligns with the direction of
#' maximum variance and H2 is orthogonal. The vertical component
#' (`DIR == "UP"`) is left untouched.
#'
#' This step requires a LONG table already classified with `DIR` values
#' `H1`, `H2`, and `UP`. New processing code should usually call
#' [mapComponents()] with `rotate = TRUE` instead of calling this helper
#' directly.
#'
#' The rotation angle theta is stored as `attr(DT, "theta")` in radians so
#' that [extractRecord()] can persist it in the JSON sidecar.
#'
#' @param DT LONG `data.table(t, OCID, s, DIR)` for ONE record.
#'   `DIR` must already be assigned.
#' @return `DT` with `s` values updated in-place for `DIR %in%
#'   c("H1","H2")` and `attr(DT, "theta")` set.  Returns `DT` unchanged
#'   when the record does not have exactly two horizontal components.
#' @seealso [mapComponents()]
#' @examples
#' t <- seq(0, 1, by = 0.1)
#' x <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, OCID = "N", DIR = "H1",
#'                          s = sin(2 * pi * t)),
#'   data.table::data.table(t = t, OCID = "E", DIR = "H2",
#'                          s = 0.5 * cos(2 * pi * t)),
#'   data.table::data.table(t = t, OCID = "Z", DIR = "UP",
#'                          s = 0.1 * sin(2 * pi * t))
#' ))
#' rotated <- rotateComponents(x)
#' attr(rotated, "theta")
#'
#' @importFrom data.table dcast fcase setattr
#' @importFrom stats cov
#' @export
rotateComponents <- function(DT) {
  stopifnot(is.data.table(DT))
  stopifnot("DIR" %chin% names(DT))
  stopifnot(all(c("t", "s") %chin% names(DT)))

  DIRS <- intersect(c("H1", "H2"), unique(DT$DIR))
  if (length(DIRS) != 2L) return(DT)          # skip: not exactly 2 horizontals

  # Wide form of the two horizontals keyed on t
  W  <- dcast(DT[DIR %chin% DIRS], t ~ DIR, value.var = "s")
  h1 <- W[["H1"]]
  h2 <- W[["H2"]]

  # Eigendecomposition of the 2x2 horizontal covariance matrix
  C     <- cov(cbind(h1, h2))
  E     <- eigen(C, symmetric = TRUE)
  theta <- atan2(E$vectors[2L, 1L], E$vectors[1L, 1L])

  # Rotated signals: H1 -> max-variance axis, H2 -> orthogonal
  H1.ROT <- h1 * cos(theta) + h2 * sin(theta)
  H2.ROT <- -h1 * sin(theta) + h2 * cos(theta)

  # Update s in-place (join on t; UP rows are untouched via default = s)
  ROT <- data.table(t = W$t, H1 = H1.ROT, H2 = H2.ROT)
  DT[ROT, on = "t",
     s := fcase(DIR == "H1", i.H1,
                DIR == "H2", i.H2,
                default     = s)]

  setattr(DT, "theta", theta)
  DT[]
}
