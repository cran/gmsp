# validateUnits.R — canonical Units validator for public API boundary.
#
# Public-facing entry points (`AT2TS`, `VT2TS`, `DT2TS`, `getIntensity`)
# accept Units only in the canonical base form:
#   - length basis:  "mm", "cm", "m"           (valid for AT, VT, DT)
#   - acceleration:  "g", "gal"                (valid for AT ONLY)
# Verbose strings such as `"mm/s"`, `"mm/s2"`, `"m/s2"`, `"cm/s"` are
# rejected with a clear error. KIND (acceleration / velocity / displacement)
# is determined by the function called, not by a suffix of the Units string.
#
# The provider-string parsers `.parseUnits()` and `.parseKind()` continue to
# accept heterogeneous raw provider strings (e.g. `"cm/sec2"`, `"G"`,
# `"GALS"`) — they live at the data-ingestion layer and pre-normalize to
# canonical before flowing into the public API.

.UNITS.LENGTH <- c("mm", "cm", "m")
.UNITS.ACCEL  <- c("g", "gal")
.UNITS.AT     <- c(.UNITS.LENGTH, .UNITS.ACCEL)
.UNITS.VT     <- .UNITS.LENGTH
.UNITS.DT     <- .UNITS.LENGTH

#' @noRd
.validateUnits <- function(Units, kind) {
  if (length(Units) != 1L || is.na(Units) || !nzchar(Units))
    stop(sprintf("Units must be a non-empty single string; got %s.",
                 utils::capture.output(print(Units))[1L]))
  ALLOWED <- switch(kind,
                    AT = .UNITS.AT,
                    VT = .UNITS.VT,
                    DT = .UNITS.DT,
                    stop(sprintf(".validateUnits: kind must be 'AT'/'VT'/'DT'; got '%s'.", kind)))
  U <- tolower(Units)
  if (!(U %in% ALLOWED))
    stop(sprintf(
      "Units = '%s' is not a canonical %s unit. Allowed for %s: {%s}. %s",
      Units, kind, kind, paste(ALLOWED, collapse = ", "),
      if (grepl("/", Units, fixed = TRUE))
        "Pass the length base only (e.g. 'mm' not 'mm/s'); KIND is determined by the function called."
      else if (kind != "AT" && U %in% .UNITS.ACCEL)
        sprintf("'%s' is an acceleration unit; use AT2TS() for acceleration series.", Units)
      else ""))
  U
}
