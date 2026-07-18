#' Normalize a long time-series table to unit amplitude per channel.
#'
#' @description
#' Divides the signal column `s` by the peak amplitude of a reference quantity
#' (`norm`) for every `(metadata, OCID)` group. The same scale factor is applied
#' to all `ID` values within each group so that the physical relationship
#' between AT, VT, and DT is preserved.
#'
#' The default `norm = "PGA"` scales by `1 / max(abs(s))` computed from
#' `ID == "AT"` rows, making `max(abs(AT)) = 1` for every channel. The same
#' `SF` is then applied to the corresponding VT and DT rows.
#'
#' Modifies `.x` in place; returns `.x[]`.
#'
#' @param .x Canonical `TSL` `data.table` with columns `t`, `s`, `ID`, and
#'   `OCID`, plus optional metadata columns.
#' @param norm character. Reference quantity used to derive the scale factor.
#'   `"PGA"` (default) uses `max(abs(AT))`, `"PPV"` uses `max(abs(VT))`,
#'   `"PGD"` uses `max(abs(DT))`.
#'
#' @return `.x` with `s` scaled in place.
#' @seealso [VT2TS()], [mapComponents()]
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' tsl <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, s = 2 * sin(2 * pi * t),
#'                          ID = "AT", OCID = "H1"),
#'   data.table::data.table(t = t, s = cos(2 * pi * t),
#'                          ID = "VT", OCID = "H1"),
#'   data.table::data.table(t = t, s = sin(pi * t),
#'                          ID = "DT", OCID = "H1")
#' ))
#' normalizeTS(tsl)
#' max(abs(tsl[ID == "AT", s]))
#'
#' @importFrom data.table is.data.table
#' @export
normalizeTS <- function(.x, norm = "PGA") {

  stopifnot(is.data.table(.x))
  REQUIRED <- c("t", "s", "ID", "OCID")
  MISSING <- setdiff(REQUIRED, names(.x))
  if (length(MISSING)) {
    stop(sprintf("normalizeTS requires canonical TSL columns; missing: %s.",
                 paste(MISSING, collapse = ", ")), call. = FALSE)
  }
  norm <- match.arg(norm, c("PGA", "PPV", "PGD"))

  META <- setdiff(names(.x), REQUIRED)
  RefID <- switch(norm, PGA = "AT", PPV = "VT", PGD = "DT")

  GROUP <- c(META, "OCID")
  Norms <- .x[ID == RefID, .(SF = 1 / max(abs(s), na.rm = TRUE)),
              by = GROUP]

  .x[Norms, on = GROUP, s := s * i.SF]
  .x[]
}
