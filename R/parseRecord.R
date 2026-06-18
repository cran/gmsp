#' Parse one record (event x station x owner) into a LONG time-series table.
#'
#' Reads `<path>/<OwnerID>/<EventID>/<StationID>/raw.owner/` according
#' to the owner's format and returns LONG `(t, OCID, s)`. NPTS divergence
#' between components is not enforced.
#'
#' Quantity (`ID = "AT"|"VT"|"DT"`) is NOT set here.
#'
#' @param .x `data.table` subset of master for ONE record (same `OwnerID`,
#'   `EventID`, `StationID`).
#' @param path Absolute path to the records root. The function expects
#'   per-station files under
#'   `<path>/<OwnerID>/<EventID>/<StationID>/raw.owner/`. Required -- no
#'   default.
#' @return LONG `data.table(t, OCID, s)`.
#' @examples
#' root <- file.path(tempdir(), "gmsp-parse-example")
#' unlink(root, recursive = TRUE)
#' raw <- file.path(root, "ESM", "E1", "S1", "raw.owner")
#' dir.create(raw, recursive = TRUE)
#' writeLines(c("0 1", "0.01 2", "0.02 3"), file.path(raw, "N_acc.txt"))
#' writeLines(c("0 2", "0.01 3", "0.02 4"), file.path(raw, "E_acc.txt"))
#' writeLines(c("0 0", "0.01 1", "0.02 0"), file.path(raw, "Z_acc.txt"))
#' rows <- data.table::data.table(
#'   OwnerID = "ESM", EventID = "E1", StationID = "S1",
#'   FileID = c("N_acc.txt", "E_acc.txt", "Z_acc.txt")
#' )
#' parseRecord(rows, path = root)
#'
#' @importFrom data.table data.table rbindlist
#' @export
parseRecord <- function(.x, path) {
  PATH.records <- path.expand(path)
  AUX <- .x[1L]
  fmt <- .OWNER_FORMAT[[AUX$OwnerID]]
  stopifnot(!is.null(fmt))

  DIR   <- file.path(PATH.records, AUX$OwnerID, AUX$EventID, AUX$StationID, "raw.owner")
  FILES <- unique(.x$FileID)

  rbindlist(lapply(FILES, function(s) {
    FILE <- file.path(DIR, s)
    if      (fmt == "AT2")                           readAT2(FILE)
    else if (fmt == "TWO_COL")                       readTwoCol(FILE)
    else if (fmt %in% c("TRA","TRZ","TRB","TRC"))    readTR(FILE, fmt)
    else if (fmt %in% c("ACA","LIS","ACB"))          readAC(FILE, fmt)
    else if (fmt == "V2A")                           readV2A(FILE)
    else if (fmt == "V2")                            readV2(FILE)
    else if (fmt == "ISEE")                          readISEE(FILE)
    else stop("unknown format: ", fmt)
  }))
}

#' @noRd
.OWNER_FORMAT <- c(
  CAL   = "TWO_COL", CENA = "TWO_COL", CLSMD = "TWO_COL", ESM   = "TWO_COL",
  NCEDC = "TWO_COL", NN   = "TWO_COL", PNSN  = "TWO_COL", SCEDC = "TWO_COL",
  CESMD = "V2",      GSC  = "TRA",     IGP   = "ACA",     NGAW  = "AT2",
  NWZ   = "V2A",     UCR  = "LIS",     ISEE  = "ISEE"
)
