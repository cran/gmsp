#' Read a CESMD V2 acceleration record (multi-channel V2 or single-channel V2c).
#'
#' Two CESMD variants:
#' - Multi-channel V2 (`.v2`): blocks marked `^Corrected accelerogram`,
#'   `8f10.6` body, ends at next `points of veloc data`. 1+ blocks.
#' - Single-channel V2c (`.V2c`): line 1 = `Corrected acceleration`,
#'   `1E15.6` body (1 col/row). Marker `acceleration pts` carries NPTS.
#'   `samples/sec` (last occurrence -- DECIMATE > RESAMPLE) carries dt.
#'   Body ends at `End-of-data` or EOF. OCID from `Sta Chan ...:` line.
#'
#' @param file Path to the .v2 / .V2c file.
#' @return LONG `data.table(t, OCID, s)`.
#' @examples
#' file <- tempfile(fileext = ".V2c")
#' writeLines(c(
#'   "Corrected acceleration",
#'   "Sta Chan 1: HNZ",
#'   "100 samples/sec",
#'   "4 acceleration pts approx 0.04 secs",
#'   "1", "2", "3", "4",
#'   "End-of-data"
#' ), file)
#' readV2(file)
#'
#' @importFrom data.table data.table rbindlist
#' @export
readV2 <- function(file) {
  RAW <- .readClean(file)
  if (grepl("^Corrected accelerogram", RAW[1L])) return(.readV2multi(RAW))
  if (grepl("^Corrected acceleration",  RAW[1L])) return(.readV2c(RAW))
  stop("V2: unknown marker: ", substr(RAW[1L], 1L, 60L))
}

#' @noRd
.readV2multi <- function(RAW) {
  IDX <- grep("^Corrected accelerogram", RAW)
  stopifnot(length(IDX) >= 1L)
  IDX <- c(IDX, length(RAW) + 1L)
  rbindlist(lapply(seq_len(length(IDX) - 1L), function(k) {
    AUX  <- RAW[IDX[k]:(IDX[k + 1L] - 1L)]
    OCID <- sub("^Chan[[:space:]]+[[:digit:]]*:[[:space:]]*([^[:space:]]+).*", "\\1",
                grep("^Chan[[:space:]]+[[:digit:]]*:", AUX, value = TRUE)[1L])
    i  <- grep("points of accel data", AUX)[1L]
    j  <- grep("points of veloc data", AUX)[1L]
    if (is.na(j)) j <- length(AUX) + 1L
    n  <- as.integer(sub("\\s*([0-9]+)\\s+points.*",         "\\1", AUX[i], perl = TRUE))
    dt <- as.numeric(sub(".*spaced at\\s+([0-9.]+)\\s+sec.*", "\\1", AUX[i], perl = TRUE))
    s  <- .scanN(AUX[(i + 1L):(j - 1L)], n)
    if (is.null(s)) return(NULL)
    .guard(dt, n, OCID, s)
    .toLong(dt, OCID, s)
  }))
}

#' @noRd
.readV2c <- function(RAW) {
  i    <- grep("acceleration pts", RAW)[1L]
  stopifnot(!is.na(i))
  n    <- as.integer(sub("\\s*([0-9]+)\\s+acceleration pts.*", "\\1", RAW[i], perl = TRUE))
  AUX  <- grep("samples/sec", RAW, value = TRUE)
  dt   <- if (length(AUX) >= 1L)
            1 / as.numeric(sub(".*?([0-9.]+)\\s+samples/sec.*", "\\1", tail(AUX, 1L), perl = TRUE))
          else
            as.numeric(sub(".*approx\\s+([0-9.]+)\\s+secs.*", "\\1", RAW[i], perl = TRUE)) / n
  AUX  <- grep("^Sta Chan", RAW, value = TRUE)
  stopifnot(length(AUX) >= 1L)
  OCID <- sub(".*Chan[[:space:]]+[^:]+:[[:space:]]*([^[:space:]]+).*", "\\1", AUX[1L])
  j    <- grep("End-of-data", RAW)
  j    <- if (length(j) > 0L) j[1L] - 1L else length(RAW)
  s    <- .scanN(RAW[(i + 1L):j], n)
  if (is.null(s)) return(NULL)
  .guard(dt, n, OCID, s)
  .toLong(dt, OCID, s)
}
