#' Read a NWZ V2A acceleration record (3D-BLOCK, 1 file = 3 components).
#'
#' Each component is a sequential block opened by `Corrected accelerogram`
#' (case-insensitive). Within a block, header ends 10 lines after
#' `Displacement:`. Body has 10 values per row in fixed-width form;
#' "stuck" negatives are split before parsing. Each component vector is
#' truncated at its own `Number of points` value.
#'
#' OCIDs come from `Component <X>` lines in each block.
#'
#' @param file Path to the .V2A file.
#' @return LONG `data.table(t, OCID, s)`.
#' @examples
#' file <- tempfile(fileext = ".V2A")
#' writeLines(c(
#'   "Corrected Accelerogram", "Component H1", "at 0.01 sec intervals",
#'   "Number of points 4", "Displacement:", rep("header", 10), "1 2 3 4",
#'   "Corrected Accelerogram", "Component H2",
#'   "Number of points 4", "Displacement:", rep("header", 10), "2 3 4 5",
#'   "Corrected Accelerogram", "Component UP",
#'   "Number of points 4", "Displacement:", rep("header", 10), "3 4 5 6"
#' ), file)
#' readV2A(file)
#'
#' @importFrom data.table data.table rbindlist
#' @export
readV2A <- function(file) {
  DATA <- .readClean(file)
  IDX  <- grep("^Corrected [Aa]ccelerogram", DATA)
  stopifnot(length(IDX) >= 3L)
  IDX  <- c(IDX[1L:3L], length(DATA) + 1L)
  dt   <- as.numeric(sub(".*at\\s+([0-9.]+)\\s*sec intervals.*", "\\1",
                         grep("sec intervals", DATA[IDX[1L]:(IDX[2L] - 1L)],
                              value = TRUE, perl = TRUE)[1L],
                         perl = TRUE))
  stopifnot(!is.na(dt), dt > 0)

  rbindlist(lapply(1L:3L, function(k) {
    AUX  <- DATA[IDX[k]:(IDX[k + 1L] - 1L)]
    OCID <- sub(".*Component\\s+(\\S+).*", "\\1",
                grep("^Component\\s", AUX, value = TRUE, perl = TRUE)[1L],
                perl = TRUE)
    n    <- as.integer(sub(".*Number of points\\s+([0-9]+).*", "\\1",
                           grep("Number of points", AUX, value = TRUE, perl = TRUE)[1L],
                           perl = TRUE))
    i    <- grep("Displacement", AUX)[1L]
    s    <- .scanN(AUX[(i + 11L):length(AUX)], n)
    if (is.null(s)) return(NULL)
    .guard(dt, n, OCID, s)
    .toLong(dt, OCID, s)
  }))
}
