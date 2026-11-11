# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Build raw record indexes
#'
#' Reads AT, VT and DT raw JSON sidecars and writes
#' `RawRecordTable.<OwnerID>.csv` for each owner with records.
#'
#' @param pathRecords Records root containing owner, event and station folders.
#' @param pathIndex Existing destination folder for the index files.
#' @param owners Owner identifiers; NULL scans immediate directories of
#'   pathRecords.
#' @return An invisible named list of row counts for the owners written.
#' @details Columns are RecordID, EventID, StationID, OwnerID, NP, Fs and pad,
#'   ordered by event and station. NP is the maximum pre-alignment count from
#'   the sidecar, and pad is the difference between maximum and minimum
#'   counts. Under truncation, NP does not describe the CSV row count.
#'
#' A station failure retains its cause and path. Valid rows are published;
#' an error of class modulePartialPublication then carries failures, the
#' partial result and destination path.
#' @examples
#' Root <- tempfile("buildRawRecordTable")
#' Raw <- file.path(Root, "records", "NGAW", "E1", "S1", "raw")
#' Index <- file.path(Root, "index")
#' dir.create(Raw, recursive = TRUE)
#' dir.create(Index)
#' jsonlite::write_json(list(RecordID = "R1", OwnerID = "NGAW", EventID = "E1",
#'   StationID = "S1", NP = c(4, 4, 3), Fs = 100), file.path(Raw, "AT.R1.json"),
#'   auto_unbox = TRUE)
#' Counts <- buildRawRecordTable(file.path(Root, "records"), pathIndex = Index)
#' str(Counts)
#' data.table::fread(file.path(Index, "RawRecordTable.NGAW.csv"))
#' unlink(Root, recursive = TRUE)
#' @export
buildRawRecordTable <- function(pathRecords, pathIndex, owners = NULL) {
  pathRecords <- path.expand(pathRecords)
  pathIndex <- path.expand(pathIndex)
  if (is.null(owners)) owners <- basename(list.dirs(pathRecords, recursive = FALSE))
  OUT <- list()
  Failures <- list()
  for (Owner in owners) {
    FILES <- list.files(file.path(pathRecords, Owner), pattern = "^(AT|VT|DT)\\..+\\.json$",
                         recursive = TRUE, full.names = TRUE)
    LIST <- lapply(FILES, function(file) {
      tryCatch({
        Metadata <- jsonlite::read_json(file, simplifyVector = TRUE)
        COLS <- c("RecordID", "EventID", "StationID", "OwnerID", "NP", "Fs")
        if (!all(COLS %in% names(Metadata)) || !length(Metadata$NP)) {
          stop("Raw sidecar lacks record index fields.", call. = FALSE)
        }
        if (any(lengths(Metadata[setdiff(COLS, "NP")]) != 1L)) {
          stop("Raw record index keys and Fs must be scalar.", call. = FALSE)
        }
        data.table(RecordID = Metadata$RecordID, EventID = Metadata$EventID,
          StationID = Metadata$StationID, OwnerID = Metadata$OwnerID,
          NP = max(Metadata$NP), Fs = Metadata$Fs,
          pad = max(Metadata$NP) - min(Metadata$NP))
      }, error = function(e) {
        Failures[[file]] <<- list(operation = "buildRawRecordTable",
                                  unit = basename(dirname(dirname(file))), path = file, cause = e)
        NULL
      })
    })
    LIST <- Filter(Negate(is.null), LIST)
    if (!length(LIST)) next
    DT <- rbindlist(LIST, use.names = TRUE)
    setorder(DT, EventID, StationID)
    FILE <- file.path(pathIndex, paste0("RawRecordTable.", Owner, ".csv"))
    tryCatch({
      fwrite(DT, FILE, eol = "\n")
      OUT[[Owner]] <- nrow(DT)
    }, error = function(e) {
      Failures[[FILE]] <<- list(operation = "buildRawRecordTable", unit = Owner,
                                path = FILE, cause = e)
    })
  }
  if (length(Failures)) .stopPartialPublication(
    "Raw record index has failures at: ", paste(names(Failures), collapse = ", "),
    operation = "buildRawRecordTable", failures = Failures, result = OUT, path = pathIndex)
  invisible(OUT)
}
