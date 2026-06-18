#' Read a 2-column whitespace-delimited ASCII record.
#'
#' Time in column 1 (seconds), signal in column 2. No header. Separator
#' (space or tab) is auto-detected by `fread`. Trailing all-NA columns
#' (some providers leave a trailing tab -> phantom column) are dropped.
#'
#' OCID extracted from filename. Three known patterns:
#'   - SEED-like:  `NET.STA.LOC.CHA__...`            -> CHA
#'   - CENA:       `<date>_<time>_NET.STA.CHA_AccTH` -> CHA
#'   - CLSMD:      `<X>_acc.txt`                     -> X
#'
#' @param file Path to the file.
#' @return LONG `data.table(t, OCID, s)`.
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' file <- file.path(dir, "N_acc.txt")
#' writeLines(c("0 1", "0.01 2"), file)
#' readTwoCol(file)
#'
#' @importFrom data.table fread data.table
#' @export
readTwoCol <- function(file) {
  DT <- fread(file, header = FALSE)
  DT <- DT[, .SD, .SDcols = !vapply(DT,
                                    function(x) is.logical(x) && all(is.na(x)),
                                    logical(1L))]
  stopifnot(ncol(DT) == 2L)
  setnames(DT, c("t", "s"))
  AUX  <- basename(file)
  OCID <- if      (grepl("^[^.]+\\.[^.]+\\.[^.]*\\.[^.]+?__", AUX, perl = TRUE))
               sub("^[^.]+\\.[^.]+\\.[^.]*\\.([^.]+?)__.*",   "\\1", AUX, perl = TRUE)
          else if (grepl("\\.[A-Z0-9]+_AccTH\\.txt$",          AUX, perl = TRUE))
               sub(".*\\.([A-Z0-9]+)_AccTH\\.txt$",            "\\1", AUX, perl = TRUE)
          else if (grepl("^[NEZ]_acc\\.",                      AUX, perl = TRUE))
               sub("^([NEZ])_acc\\..*",                        "\\1", AUX, perl = TRUE)
          else stop("readTwoCol: unknown filename pattern: ", AUX)
  stopifnot(nzchar(OCID))
  DT[, OCID := OCID][, .(t, OCID, s)]
}
