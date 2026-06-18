#' Audit parsers: dry-run `parseRecord` on every record of an owner.
#'
#' Iterates the master subset for `owner`, calls `parseRecord` per
#' unique `(EventID, StationID)`, catches errors, and returns a status
#' table.
#'
#' @param .x master `data.table` from `buildMaster()`.
#' @param owner one OwnerID string.
#' @param path Absolute path to the records root passed through to
#'   `parseRecord()`. Required -- no default.
#' @return `data.table(OwnerID, EventID, StationID, status, reason)`.
#' @examples
#' root <- file.path(tempdir(), "gmsp-auditparsers-example")
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
#' auditParsers(rows, owner = "ESM", path = root)
#'
#' @importFrom data.table data.table rbindlist
#' @export
auditParsers <- function(.x, owner, path) {
  PATH.records <- path.expand(path)
  Owner <- owner
  DT  <- .x[OwnerID == Owner]
  IDX <- unique(DT[, .(EventID, StationID)])

  rbindlist(lapply(seq_len(nrow(IDX)), function(k) {
    AUX <- IDX[k]
    Rs  <- DT[EventID   == AUX$EventID &
              StationID == AUX$StationID]
    OUT <- tryCatch(
      { OUT <- parseRecord(.x = Rs, path = PATH.records)
        list(status = "OK",
             reason = sprintf("rows=%d cols=%d", nrow(OUT), ncol(OUT))) },
      error   = function(e) list(status = "FAIL", reason = conditionMessage(e)),
      warning = function(w) list(status = "WARN", reason = conditionMessage(w))
    )
    data.table(OwnerID   = Owner,
               EventID   = AUX$EventID,
               StationID = AUX$StationID,
               status    = OUT$status,
               reason    = OUT$reason)
  }))
}
