#' Equalize NP across components of a parsed record.
#'
#' Pads shorter OCIDs with trailing zeros (align = "max") or truncates
#' longer OCIDs (align = "min") so all components share the same NP.
#' Operates on a classified LONG table with columns `t`, `OCID`, `s`, and `DIR`.
#'
#' @param DT   LONG `data.table(t, OCID, s, DIR)` for ONE record.
#' @param align `"max"` (default) or `"min"`.
#' @return Named list with two elements:
#'   - `DT` : aligned LONG `data.table` with the same columns as the
#'     input.
#'   - `NP` : integer, final number of samples per component after
#'     alignment (the same for every OCID).
#'   When every OCID already shares the same NP the function returns
#'   without padding or truncation; the input is passed through as
#'   `DT` unchanged.
#' @examples
#' x <- data.table::rbindlist(list(
#'   data.table::data.table(t = c(0, 0.01, 0.02), OCID = "H1",
#'                          DIR = "H1", s = c(1, 2, 3)),
#'   data.table::data.table(t = c(0, 0.01), OCID = "H2",
#'                          DIR = "H2", s = c(1, 2)),
#'   data.table::data.table(t = c(0, 0.01, 0.02), OCID = "UP",
#'                          DIR = "UP", s = c(0, 1, 0))
#' ))
#' aligned <- alignComponents(x, align = "max")
#' aligned$NP
#'
#' @export
alignComponents <- function(DT, align = "max") {
  AUX      <- DT[, .N, by = OCID]
  NpTarget <- if (align == "max") max(AUX$N) else min(AUX$N)
  if (all(AUX$N == NpTarget)) return(list(DT = DT, NP = NpTarget))
  Dt  <- DT[OCID == DT$OCID[1L]][2L, t] - DT[OCID == DT$OCID[1L]][1L, t]
  OUT <- rbindlist(lapply(AUX$OCID, function(ocid) {
    AUX <- DT[OCID == ocid]
    N   <- nrow(AUX)
    if (N == NpTarget) return(AUX)
    if (N <  NpTarget)
      rbind(AUX, data.table(t = AUX[N, t] + Dt * seq_len(NpTarget - N),
                             OCID = ocid, s = 0, DIR = AUX[1L, DIR]))
    else
      AUX[seq_len(NpTarget)]
  }))
  list(DT = OUT, NP = NpTarget)
}
