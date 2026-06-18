#' Build the canonical RawRecordTable for one or more owners.
#'
#' Scans `<path.records>/<OwnerID>/<EventID>/<StationID>/raw/AT.*.json` and
#' emits one row per `RecordID` to `<path.index>/RawRecordTable.<OwnerID>.csv`.
#'
#' Schema:
#' ```
#' RecordID, EventID, StationID, OwnerID, NP, Fs, pad
#' ```
#' where `NP = max(json$NP)` (post-align) and
#' `pad = max(json$NP) - min(json$NP)`.
#'
#' This table carries zero per-direction metadata: directional detail
#' (PGA, AI, ARMS, ...) lives in `RawIntensityTable.<Owner>.csv`. The
#' per-layer `Raw*` prefix leaves room for downstream processors to
#' emit their own `<ProcessID>RecordTable.<Owner>.csv`.
#'
#' @param path.records Absolute path to the records root. Required -- no default.
#' @param path.index Absolute path to the index root where per-owner CSVs are
#'   written. Required -- no default.
#' @param owners     Character vector of `OwnerID`s. `NULL` = scan all
#'                   subdirs of `path.records`.
#' @return Invisibly, the per-owner row counts.
#' @examples
#' root <- file.path(tempdir(), "gmsp-raw-record-example")
#' index <- file.path(tempdir(), "gmsp-raw-record-index")
#' unlink(c(root, index), recursive = TRUE)
#' raw <- file.path(root, "AAA", "E1", "S1", "raw")
#' dir.create(raw, recursive = TRUE)
#' dir.create(index)
#' jsonlite::write_json(
#'   list(RecordID = "R1", OwnerID = "AAA", EventID = "E1",
#'        StationID = "S1", NP = c(4, 4, 4), Fs = 100),
#'   file.path(raw, "AT.R1.json"), auto_unbox = TRUE
#' )
#' suppressMessages(buildRawRecordTable(root, index, owners = "AAA"))
#' data.table::fread(file.path(index, "RawRecordTable.AAA.csv"))
#'
#' @importFrom data.table data.table rbindlist fwrite setorder
#' @importFrom jsonlite read_json
#' @export
buildRawRecordTable <- function(path.records, path.index, owners = NULL) {
  path.records <- path.expand(path.records)
  path.index   <- path.expand(path.index)
  if (is.null(owners))
    owners <- basename(list.dirs(path.records, recursive = FALSE))

  OUT <- list()
  for (O in owners) {
    FILES <- list.files(file.path(path.records, O),
                        pattern = "^(AT|VT|DT)\\..+\\.json$",
                        recursive = TRUE, full.names = TRUE)
    if (length(FILES) == 0L) next
    DT <- rbindlist(lapply(FILES, function(F) {
      J <- jsonlite::read_json(F, simplifyVector = TRUE)
      data.table(RecordID  = J$RecordID,
                 EventID   = J$EventID,
                 StationID = J$StationID,
                 OwnerID   = J$OwnerID,
                 NP        = max(J$NP),
                 Fs        = J$Fs,
                 pad       = max(J$NP) - min(J$NP))
    }))
    setorder(DT, EventID, StationID)
    fwrite(DT, file.path(path.index, sprintf("RawRecordTable.%s.csv", O)))
    OUT[[O]] <- nrow(DT)
    message(sprintf("[buildRawRecordTable] %s: %d records -> RawRecordTable.%s.csv",
                    O, nrow(DT), O))
  }
  invisible(OUT)
}
