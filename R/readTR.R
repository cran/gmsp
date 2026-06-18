#' Read a TRA/TRZ/TRB/TRC acceleration record (GSC and SGC families).
#'
#' TRA/TRZ have a multi-column body after `END_HEADER`; the last column
#' is the corrected acceleration. TRB/TRC have a single-column body
#' after the `Unidades:` (TRB) or `USER5` (TRC) line. OCID lives in the
#' header: `Component:` (TRA/TRZ), `Componente:` (TRB), `STREAM:` (TRC).
#'
#' @param file Path to the file.
#' @param type One of `"TRA"`, `"TRZ"`, `"TRB"`, `"TRC"`.
#' @return LONG `data.table(t, OCID, s)`.
#' @examples
#' file <- tempfile()
#' writeLines(c(
#'   "Component: HNZ",
#'   "rate: 100",
#'   "END_HEADER",
#'   "skip1",
#'   "skip2",
#'   "1 2 3",
#'   "4 5 6"
#' ), file)
#' readTR(file, type = "TRA")
#'
#' @importFrom data.table fread data.table
#' @export
readTR <- function(file, type) {
  DATA <- .readClean(file)
  AUX  <- switch(type,
    TRA = list(end = "END_HEADER", skip = 3L, col = "last",
               pat  = "(?<=rate:)\\s*[0-9.]+",                inv = TRUE,
               ocid = "(?<=Component:)\\s*\\S+"),
    TRZ = list(end = "END_HEADER", skip = 3L, col = "last",
               pat  = "(?<=rate:)\\s*[0-9.]+",                inv = TRUE,
               ocid = "(?<=Component:)\\s*\\S+"),
    TRB = list(end = "Unidades",   skip = 1L, col = "first",
               pat  = "(?<=muestreo:)\\s*[0-9.]+",            inv = TRUE,
               ocid = "(?<=Componente:)\\s*\\S+"),
    TRC = list(end = "USER5",      skip = 1L, col = "first",
               pat  = "(?<=SAMPLING_INTERVAL_S:)\\s*[0-9.]+", inv = FALSE,
               ocid = "(?<=STREAM:)\\s*\\S+"))
  i  <- grep(AUX$end, DATA)
  stopifnot(length(i) == 1L)
  dt <- as.numeric(regmatches(DATA[1L:i],
                              regexpr(AUX$pat, DATA[1L:i], perl = TRUE)))
  stopifnot(length(dt) == 1L, dt > 0)
  if (AUX$inv) dt <- 1 / dt
  OCID <- gsub("\\s", "",
               regmatches(DATA[1L:i],
                          regexpr(AUX$ocid, DATA[1L:i], perl = TRUE))[1L])
  stopifnot(nzchar(OCID))
  DT <- suppressWarnings(fread(text = DATA[(i + AUX$skip):length(DATA)]))
  s  <- as.vector(if (AUX$col == "last") DT[[ncol(DT)]] else DT[[1L]])
  .toLong(dt, OCID, s)
}
