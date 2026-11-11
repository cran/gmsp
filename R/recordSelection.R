# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
RecordKeys <- c("RecordID", "OwnerID", "EventID", "StationID")
ProcessKeys <- c("ProcessID", RecordKeys)
# EventID.USGS names one earthquake across owners; legacy processed indexes call it usgsEventId.
USGSEventColumns <- c("EventID.USGS", "usgsEventId")

.eventIdentity <- function(data) {
  OUT <- as.character(data[["EventID"]])
  COLS <- intersect(USGSEventColumns, names(data))
  if (!length(COLS)) return(OUT)
  AUX <- as.character(data[[COLS[1L]]])
  OK <- !is.na(AUX) & nzchar(AUX)
  OUT[OK] <- AUX[OK]
  OUT
}

.recordPaths <- function(paths, need = character()) {
  if (!is.list(paths)) .stopUsage("paths must be a list.", operation = ".recordPaths")
  if (!is.null(paths$database)) {
    DIR <- path.expand(as.character(paths$database)[1L])
    if (is.null(paths$records)) paths$records <- file.path(DIR, "records")
    if (is.null(paths$index)) paths$index <- file.path(DIR, "index")
    if (is.null(paths$process)) paths$process <- file.path(DIR, "process")
  }
  for (COL in need) {
    if (is.null(paths[[COL]]) || !nzchar(as.character(paths[[COL]])[1L])) {
      .stopUsage("paths$", COL, " is required.", operation = ".recordPaths")
    }
  }
  OUT <- paths
  COLS <- intersect(c("database", "records", "index", "process"), names(OUT))
  for (COL in COLS) OUT[[COL]] <- path.expand(as.character(OUT[[COL]])[1L])
  OUT
}

.recordNormalizeKeys <- function(keys) {
  if (is.null(keys)) .stopUsage("keys is required.", operation = ".recordNormalizeKeys")
  if (data.table::is.data.table(keys) || is.data.frame(keys)) {
    OUT <- data.table::copy(data.table::as.data.table(keys))
  } else if (is.list(keys)) {
    COLS <- setdiff(names(keys), ProcessKeys)
    if (length(COLS)) {
      .stopUsage("Unknown key fields: ", paste(COLS, collapse = ", "),
                 operation = ".recordNormalizeKeys")
    }
    OUT <- data.table::as.data.table(keys)
  } else {
    .stopUsage("keys must be a named list or data.table.", operation = ".recordNormalizeKeys")
  }
  if (!nrow(OUT)) .stopUsage("keys produced zero rows.", operation = ".recordNormalizeKeys")
  COLS <- intersect(ProcessKeys, names(OUT))
  if (!length(COLS)) .stopUsage("keys has no canonical key fields.", operation = ".recordNormalizeKeys")
  for (COL in COLS) data.table::set(OUT, j = COL, value = as.character(OUT[[COL]]))
  OUT[]
}

.recordIsWildcard <- function(x) {
  is.na(x) || !nzchar(x) || identical(x, "*")
}

.recordProcessID <- function(keys) {
  if (!"ProcessID" %chin% names(keys)) {
    .stopUsage("keys must include explicit ProcessID.", operation = ".recordProcessID")
  }
  OUT <- unique(keys$ProcessID[!vapply(keys$ProcessID, .recordIsWildcard,
                                       logical(1L))])
  if (length(OUT) != 1L) {
    .stopUsage("keys must resolve exactly one non-wildcard ProcessID.", operation = ".recordProcessID")
  }
  OUT
}

.recordBindTables <- function(tables, label) {
  if (!length(tables)) stop(label, " has no tables to bind.", call. = FALSE)
  COLS <- names(tables[[1L]])
  OK <- vapply(tables, function(x) setequal(names(x), COLS), logical(1L))
  if (!all(OK)) {
    stop(label, " schema mismatch before binding.", call. = FALSE)
  }
  data.table::rbindlist(lapply(tables, function(x) x[, ..COLS]),
                        use.names = TRUE)
}

.recordReadMany <- function(files, label) {
  FILES <- as.character(files)
  if (!length(FILES)) stop(label, " has no files.", call. = FALSE)
  OK <- file.exists(FILES)
  if (!all(OK)) {
    stop("Missing ", label, " files: ", paste(FILES[!OK], collapse = ", "),
         call. = FALSE)
  }
  .recordBindTables(lapply(FILES, .recordFread),
                    label = label)
}

.recordFread <- function(file) {
  HEADER <- names(data.table::fread(file, nrows = 0L, showProgress = FALSE))
  COLS <- intersect(ProcessKeys, HEADER)
  if (!length(COLS)) return(data.table::fread(file, showProgress = FALSE))
  data.table::fread(file, showProgress = FALSE,
                    colClasses = stats::setNames(rep("character",
                                                     length(COLS)), COLS))
}

.recordReadSearchTable <- function(processID, keys, paths) {
  if (identical(processID, "raw")) {
    paths <- .recordPaths(paths, need = "index")
    FILES <- paths$index
    if (dir.exists(paths$index)) {
      AUX <- if ("OwnerID" %chin% names(keys)) {
        unique(keys$OwnerID[!vapply(keys$OwnerID, .recordIsWildcard, logical(1L))])
      } else {
        character()
      }
      FILES <- if (length(AUX)) {
        file.path(paths$index, paste0("MasterIndex.", AUX, ".csv"))
      } else if (file.exists(file.path(paths$index, "RawMasterIndex.csv"))) {
        file.path(paths$index, "RawMasterIndex.csv")
      } else {
        file.path(paths$index, "MasterIndex.csv")
      }
      if (!all(file.exists(FILES)) && file.exists(file.path(paths$index, "MasterIndex.csv"))) {
        FILES <- file.path(paths$index, "MasterIndex.csv")
      }
    }
    DT <- .recordReadMany(FILES, label = "raw index")
  } else {
    FILES <- file.path(.recordProcessDir(processID = processID, paths = paths),
                       "MasterIndex.csv")
    DT <- .recordReadMany(FILES, label = paste(processID, "MasterIndex"))
  }
  if (!"ProcessID" %chin% names(DT)) DT[, ProcessID := processID]
  .requireColumns(DT, ProcessKeys, "search table")
  for (COL in ProcessKeys) data.table::set(DT, j = COL, value = as.character(DT[[COL]]))
  DT[]
}

.recordUniqueRows <- function(data) {
  .requireColumns(data, ProcessKeys, "record set")
  COLS <- c(ProcessKeys, intersect(c("DIR", "OCID"), names(data)))
  data.table::setorderv(data, COLS, na.last = TRUE)
  unique(data, by = ProcessKeys)
}

.recordSelect <- function(keys,
                          criteria = list(),
                          paths) {
  Keys <- .recordNormalizeKeys(keys)
  ProcessID <- .recordProcessID(Keys)
  DT <- .recordReadSearchTable(processID = ProcessID, keys = Keys, paths = paths)
  .recordCandidates(data = DT, keys = Keys, criteria = criteria)
}

.recordCandidates <- function(data, keys, criteria) {
  DT <- .recordApplyKeys(data = data, keys = keys)
  DT <- .recordApplyCriteria(data = DT, criteria = criteria)
  if (!nrow(DT)) stop("Selection produced zero records.", call. = FALSE)
  OUT <- .recordUniqueRows(DT)
  if (!nrow(OUT)) stop("Selection produced zero records.", call. = FALSE)
  data.table::setcolorder(OUT, c(intersect(ProcessKeys, names(OUT)),
                                 setdiff(names(OUT), ProcessKeys)))
  OUT[]
}

.recordProcessDir <- function(processID, paths) {
  paths <- .recordPaths(paths, need = "process")
  file.path(paths$process, processID)
}
