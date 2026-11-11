# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Build provider file indexes
#'
#' Reads record.json from each station's raw.owner directory or its
#' raw.owner.tar.gz archive and writes one RawFileTable CSV per owner.
#'
#' @param pathRecords Records root containing owner, event and station folders.
#' @param pathIndex Existing destination folder for `RawFileTable.<OwnerID>.csv`.
#' @param owners Owner identifiers; NULL scans immediate directories of
#'   pathRecords.
#' @return An invisible named list of row counts for the owners written.
#' @details Columns are OwnerID, EventID, StationID, ComponentID, FileID, NP,
#'   dt, Fs, Units, HP, LP and isArray, ordered by event, station and component.
#'   Missing metadata become typed NA and extra fields are dropped. isArray
#'   indicates more than three component rows in the provider metadata.
#'
#' A station failure retains its cause and path. Valid rows are published;
#' an error of class modulePartialPublication then carries failures, the
#' partial result and destination path. Counts are not returned as complete
#' after such a failure. Archive metadata are read without extracting files.
#' @examples
#' Root <- tempfile("buildRawFileTable")
#' Station <- file.path(Root, "records", "NGAW", "E1", "S1", "raw.owner")
#' Index <- file.path(Root, "index")
#' dir.create(Station, recursive = TRUE)
#' dir.create(Index)
#' Metadata <- list(Event = list(EventID = "E1"), Station = list(StationID = "S1"),
#'   Record = data.frame(ComponentID = c("HNE", "HNN", "HNZ"),
#'                       FileID = c("HNE.AT2", "HNN.AT2", "HNZ.AT2"),
#'                       NP = 4L, dt = 0.01, Fs = 100, Units = "g"))
#' jsonlite::write_json(Metadata, file.path(Station, "record.json"), auto_unbox = TRUE)
#' Counts <- buildRawFileTable(file.path(Root, "records"), pathIndex = Index)
#' str(Counts)
#' data.table::fread(file.path(Index, "RawFileTable.NGAW.csv"))
#' unlink(Root, recursive = TRUE)
#' @export
buildRawFileTable <- function(pathRecords, pathIndex, owners = NULL) {
  pathRecords <- path.expand(pathRecords)
  pathIndex <- path.expand(pathIndex)
  if (is.null(owners)) owners <- basename(list.dirs(pathRecords, recursive = FALSE))
  OUT <- list()
  Failures <- list()
  for (Owner in owners) {
    DIR <- list.dirs(file.path(pathRecords, Owner), recursive = FALSE)
    DIR <- unlist(lapply(DIR, list.dirs, recursive = FALSE), use.names = FALSE)
    LIST <- lapply(DIR, function(path) {
      tryCatch({
        FILE <- file.path(path, "raw.owner", "record.json")
        TAR <- file.path(path, "raw.owner.tar.gz")
        Metadata <- NULL
        if (file.exists(FILE)) Metadata <- jsonlite::read_json(FILE, simplifyVector = TRUE)
        if (!file.exists(FILE) && file.exists(TAR)) {
          AUX <- system2("tar", args = c("-xzOf", shQuote(TAR), "raw.owner/record.json"),
                          stdout = TRUE, stderr = TRUE)
          if (!is.null(attr(AUX, "status")) && attr(AUX, "status") != 0L) {
            stop("Cannot read archived metadata: ", paste(AUX, collapse = "\n"), call. = FALSE)
          }
          Metadata <- jsonlite::fromJSON(paste(AUX, collapse = "\n"), simplifyVector = TRUE)
        }
        if (is.null(Metadata) || !length(Metadata$Record)) return(NULL)
        DT <- as.data.table(Metadata$Record)
        DT[, `:=`(OwnerID = Owner,
          EventID = if (is.null(Metadata$Event$EventID)) NA_character_ else Metadata$Event$EventID,
          StationID = if (is.null(Metadata$Station$StationID)) NA_character_ else Metadata$Station$StationID,
          isArray = .N > 3L)]
        .canonicalRawFileTable(DT)
      }, error = function(e) {
        Failures[[path]] <<- list(operation = "buildRawFileTable", unit = basename(path),
                                  path = path, cause = e)
        NULL
      })
    })
    LIST <- Filter(Negate(is.null), LIST)
    if (!length(LIST)) next
    DT <- rbindlist(LIST, use.names = TRUE)
    if (!nrow(DT)) next
    setorder(DT, EventID, StationID, ComponentID)
    FILE <- file.path(pathIndex, paste0("RawFileTable.", Owner, ".csv"))
    tryCatch({
      fwrite(DT, FILE, eol = "\n")
      OUT[[Owner]] <- nrow(DT)
    }, error = function(e) {
      Failures[[FILE]] <<- list(operation = "buildRawFileTable", unit = Owner,
                                path = FILE, cause = e)
    })
  }
  if (length(Failures)) .stopPartialPublication(
    "Provider file index has failures at: ", paste(names(Failures), collapse = ", "),
    operation = "buildRawFileTable", failures = Failures, result = OUT, path = pathIndex)
  invisible(OUT)
}

.canonicalRawFileTable <- function(data) {
  COLS <- c("OwnerID", "EventID", "StationID", "ComponentID", "FileID",
            "NP", "dt", "Fs", "Units", "HP", "LP", "isArray")
  for (j in setdiff(COLS, names(data))) set(data, j = j, value = NA)
  for (j in setdiff(names(data), COLS)) set(data, j = j, value = NULL)
  setcolorder(data, COLS)
  COLS <- c("OwnerID", "EventID", "StationID", "ComponentID", "FileID", "Units")
  data[, (COLS) := lapply(.SD, as.character), .SDcols = COLS]
  COLS <- c("dt", "Fs", "HP", "LP")
  data[, (COLS) := lapply(.SD, as.numeric), .SDcols = COLS]
  data[, `:=`(NP = as.integer(NP), isArray = as.logical(isArray))]
  data[]
}
