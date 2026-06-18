#' Read a PEER NGA-West2 AT2 acceleration record.
#'
#' AT2 has a 4-line header ending with `NPTS=`/`DT=`. Line 2 holds the
#' direction as the last comma-separated token (e.g.,
#' `Helena Montana-01, 10/31/1935, Carroll College, 180`). Body has up
#' to 8 values per row in scientific notation; "stuck" negatives
#' (`1.234-5.678`) are split before parsing. Truncated at NPTS.
#'
#' @param file Path to the .AT2 file.
#' @return LONG `data.table(t, OCID, s)`.
#' @examples
#' file <- tempfile(fileext = ".AT2")
#' writeLines(c(
#'   "header",
#'   "Event, date, station, H1",
#'   "units",
#'   "NPTS= 4, DT= 0.01 SEC",
#'   "1.0 2.0 3.0 4.0"
#' ), file)
#' readAT2(file)
#'
#' @importFrom data.table data.table
#' @export
readAT2 <- function(file) {
  DATA <- .readClean(file)
  i    <- grep("NPTS", DATA)
  stopifnot(length(i) == 1L)
  dt   <- as.numeric(sub(".*DT=\\s*([0-9.]+).*",  "\\1", DATA[i], perl = TRUE))
  n    <- as.integer(sub(".*NPTS=\\s*([0-9]+).*", "\\1", DATA[i], perl = TRUE))
  TOK  <- strsplit(DATA[i - 2L], ",", fixed = TRUE)[[1L]]
  TOK  <- gsub("\\s", "", TOK, perl = TRUE)
  if (length(TOK) > 1L && grepl("\\.AT2$", TOK[length(TOK)], ignore.case = TRUE)) {
    OCID <- TOK[length(TOK) - 1L]
  } else {
    OCID <- TOK[length(TOK)]
  }
  s    <- .scanN(DATA[-(1L:i)], n)
  if (is.null(s)) return(NULL)
  .guard(dt, n, OCID, s)
  .toLong(dt, OCID, s)
}
