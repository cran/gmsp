#' Derive the canonical KIND of a raw record from its `Units` string.
#'
#' Returns one of `"AT"` (acceleration), `"VT"` (velocity), `"DT"`
#' (displacement), or `NA_character_` (unrecognised). Sibling of
#' `.parseUnits()`: same input string, orthogonal output channel.
#'
#' Rules (lowercase suffix on the `Units` string):
#' - `g`, `gal`, `*/s2`, `*/sec2`, `*/s/s`, `*/sec/sec` -> `"AT"`
#' - `*/s`, `*/sec` (when not already AT) -> `"VT"`
#' - bare length (`mm`, `cm`, `m`) -> `"DT"`
#'
#' @param s provider units string (scalar, possibly NA).
#' @return one of `"AT"`, `"VT"`, `"DT"`, or `NA_character_`.
#' @importFrom data.table fcase
#' @noRd
.parseKind <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  AUX <- tolower(s)
  fcase(
    grepl("/s2$|/sec2$|/s/s$|/sec/sec$", AUX), "AT",
    grepl("^g('s)?$|^gal",                AUX), "AT",
    grepl("/s$|/sec$",                    AUX), "VT",
    grepl("^(mm|cm|m)$",                  AUX), "DT",
    default = NA_character_
  )
}
