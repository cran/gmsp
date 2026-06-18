#' Parse a provider units string to a canonical base unit.
#'
#' Maps the heterogeneous unit strings that providers use
#' (`"cm/s2"`, `"cm/sec2"`, `"m/s2"`, `"mm/s/s"`, `"G"`, `"GALS"`, ...)
#' to one of `"mm"`, `"cm"`, `"m"`, `"g"`. Returns `NA_character_` for
#' missing / empty / unrecognized strings.
#'
#' Note: `gal` collapses to `"cm"` because 1 gal = 1 cm/s^2
#' (numerically identical scale factor to mm).
#'
#' @param s provider units string (scalar, possibly NA).
#' @return one of `"mm"`, `"cm"`, `"m"`, `"g"`, or `NA_character_`.
#' @importFrom data.table fcase
#' @noRd
.parseUnits <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  AUX <- tolower(s)
  fcase(
    grepl("^mm",      AUX),                "mm",
    grepl("^cm|^gal", AUX),                "cm",
    AUX == "m" | grepl("^m/", AUX),        "m",
    grepl("^g('s)?$|^g/", AUX),            "g",
    default = NA_character_
  )
}
