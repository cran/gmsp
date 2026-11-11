# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.recordNull <- function(x, default) {
  if (is.null(x)) default else x
}

.recordSourceDataFile <- function(path, file) {
  FILE <- file.path(path, "data", file)
  if (file.exists(FILE)) return(FILE)
  file.path(path, file)
}

.recordSourceMetadataFile <- function(path, file) {
  FILE <- file.path(path, "metadata", file)
  if (file.exists(FILE)) return(FILE)
  file.path(path, file)
}

.recordReadSelectionKeys <- function(path = NULL,
                                     processID,
                                     recordID = NULL,
                                     label = "selection",
                                     allowAll = FALSE) {
  ID.process <- as.character(processID)
  if (length(ID.process) != 1L || is.na(ID.process) || !nzchar(ID.process)) {
    stop("processID must be one non-empty string.", call. = FALSE)
  }
  Records <- unique(as.character(recordID))
  Records <- Records[nzchar(Records) & !is.na(Records)]
  if (is.null(path)) {
    if (!length(Records) && !isTRUE(allowAll)) {
      stop(label, " or recordID is required.", call. = FALSE)
    }
    OUT <- if (length(Records)) {
      data.table::data.table(ProcessID = ID.process, RecordID = Records)
    } else {
      data.table::data.table(ProcessID = ID.process)
    }
    return(OUT[])
  }
  FILE <- if (dir.exists(path)) file.path(path, "selection.csv") else path
  OUT <- .recordFread(FILE)
  .requireColumns(OUT, "RecordID", label)
  if (!"ProcessID" %chin% names(OUT)) {
    OUT[, ProcessID := ID.process]
  } else {
    OUT[is.na(ProcessID) | !nzchar(ProcessID), ProcessID := ID.process]
    if (data.table::uniqueN(OUT$ProcessID) != 1L) {
      stop(label, " has inconsistent ProcessID values.",
           call. = FALSE)
    }
    if (any(OUT$ProcessID != ID.process)) {
      stop(label, " ProcessID does not match ", ID.process, ".",
           call. = FALSE)
    }
  }
  if (length(Records)) OUT <- OUT[RecordID %chin% Records]
  if (!nrow(OUT)) stop(label, " has zero rows.", call. = FALSE)
  OUT[]
}

.recordCompleteKeys <- function(keys) {
  all(ProcessKeys %chin% names(keys)) &&
    all(vapply(ProcessKeys, function(COL) {
      !any(vapply(keys[[COL]], .recordIsWildcard, logical(1L)))
    }, logical(1L)))
}

.recordResolveKeys <- function(keys, paths) {
  Keys <- .recordNormalizeKeys(keys)
  ProcessID <- .recordProcessID(Keys)
  if (.recordCompleteKeys(keys = Keys)) {
    CanIndex <- !is.null(paths$index) &&
      (identical(ProcessID, "raw") || !is.null(paths$process))
    OUT <- if (isTRUE(CanIndex)) {
      .recordSelect(keys = Keys, criteria = list(),
                    paths = paths)
    } else {
      .recordUniqueRows(Keys)
    }
    return(list(keys = OUT[, ProcessKeys, with = FALSE], index = OUT))
  }
  OUT <- .recordSelect(keys = Keys, criteria = list(),
                       paths = paths)
  list(keys = OUT[, ProcessKeys, with = FALSE], index = OUT)
}

.recordTableFile <- function(processID, paths, output) {
  file.path(.recordProcessDir(processID = processID, paths = paths),
            paste0(output, ".csv"))
}

.recordProcessContractFile <- function(processID, paths) {
  file.path(.recordProcessDir(processID = processID, paths = paths),
            "process.json")
}

.recordReadProcessJSON <- function(processID, paths, required = TRUE) {
  FILE <- .recordProcessContractFile(processID = processID, paths = paths)
  if (!file.exists(FILE)) {
    if (isTRUE(required)) {
      stop("Missing process contract: ", FILE, call. = FALSE)
    }
    return(NULL)
  }
  AUX <- jsonlite::read_json(FILE, simplifyVector = TRUE)
  ID <- .recordNull(AUX$processID, .recordNull(AUX$ProcessID, NULL))
  if (!identical(as.character(ID), processID)) {
    stop("process.json ProcessID does not match ", processID, ".",
         call. = FALSE)
  }
  AUX
}

.recordReadProcessContract <- function(processID, paths, required = TRUE) {
  if (identical(processID, "raw")) {
    return(list(record = "AT", process = character()))
  }
  AUX <- .recordReadProcessJSON(processID = processID, paths = paths,
                                required = required)
  if (is.null(AUX)) return(NULL)
  Output <- .recordNull(AUX$output, list())
  Record <- unique(as.character(.recordNull(Output$record, character())))
  Process <- unique(as.character(.recordNull(Output$table,
                                             .recordNull(Output$process,
                                                         character()))))
  Record <- Record[nzchar(Record) & !is.na(Record)]
  Process <- Process[nzchar(Process) & !is.na(Process)]
  if (!length(Record) && !length(Process)) {
    stop("process.json has empty output inventory for ", processID, ".",
         call. = FALSE)
  }
  list(record = Record, process = Process)
}

.recordDeclaredOutputs <- function(processID, paths) {
  unique(.recordReadProcessContract(processID = processID, paths = paths,
                                    required = TRUE)$record)
}

.recordOutputPlan <- function(output, processID, paths) {
  if (is.null(output) || !length(output)) {
    output <- character()
  }
  output <- unique(as.character(output))
  if (length(output) && any(is.na(output) | !nzchar(output))) {
    stop("output must be a non-empty character vector.", call. = FALSE)
  }
  if (!length(output)) {
    CONTRACT <- .recordReadProcessContract(processID, paths = paths,
                                           required = TRUE)
    OUT <- data.table::rbindlist(list(
      data.table::data.table(output = CONTRACT$record, kind = "local"),
      data.table::data.table(output = CONTRACT$process, kind = "table")
    ), use.names = TRUE)
    OUT[, required := TRUE]
    return(unique(OUT, by = c("output", "kind"))[])
  }
  if ("all" %chin% output) {
    stop("output = \"all\" is not supported; omit output to read the process inventory.",
         call. = FALSE)
  }

  if (identical(processID, "raw")) {
    AUX <- setdiff(output, "AT")
    if (length(AUX)) {
      stop("Unsupported output for ProcessID raw: ", paste(AUX, collapse = ", "),
           call. = FALSE)
    }
    return(data.table::data.table(output = output, kind = "local",
                                  required = TRUE))
  }

  CONTRACT <- .recordReadProcessContract(processID, paths = paths,
                                         required = TRUE)
  Table <- CONTRACT$process
  Local <- CONTRACT$record
  Bad <- setdiff(output, c(Local, Table))
  if (length(Bad)) {
    stop("Output not declared for ProcessID ", processID, ": ",
         paste(Bad, collapse = ", "),
         call. = FALSE)
  }
  OUT <- data.table::data.table(output = output, kind = "local",
                                required = TRUE)
  OUT[output %chin% Table, kind := "table"]
  OUT[]
}

.recordAddKeyColumns <- function(data, row) {
  for (COL in ProcessKeys) {
    AUX <- as.character(row[[COL]][1L])
    if (!COL %chin% names(data)) {
      data.table::set(data, j = COL, value = AUX)
    } else if (anyNA(data[[COL]]) || any(as.character(data[[COL]]) != AUX)) {
      stop(COL, " column does not match requested key.",
           call. = FALSE)
    }
  }
  COLS <- c(ProcessKeys, intersect(c("t", "ts", "OCID", "ID", "IMF", "Tn"),
                                       names(data)))
  data.table::setcolorder(data, c(COLS, setdiff(names(data), COLS)))
  data[]
}

.recordLocalFile <- function(row, pathRecords, output) {
  if (identical(output, "AT")) {
    return(file.path(pathRecords, row$OwnerID, row$EventID, row$StationID,
                     "raw", sprintf("AT.%s.csv", row$RecordID)))
  }
  file.path(pathRecords, row$OwnerID, row$EventID, row$StationID,
            row$ProcessID, sprintf("%s.%s.csv", output, row$RecordID))
}

.recordFileTable <- function(keys, output, physical, files) {
  OUT <- data.table::copy(keys[, ProcessKeys, with = FALSE])
  OUT[, `:=`(output = output,
             physical = physical,
             FILE = as.character(files))]
  OUT[]
}

.recordReadRawAT <- function(row, pathRecords) {
  FILE <- .recordLocalFile(row, pathRecords = pathRecords, output = "AT")
  AUX <- .readRawPair(FILE, key = as.list(row[1L, RecordKeys, with = FALSE]))
  DT <- AUX$data
  TimeStep <- AUX$metadata$dt
  DT[, t := seq(0, by = TimeStep, length.out = .N)]
  .recordAddKeyColumns(data = DT, row = row)
}

.recordReadLocalOutput <- function(keys, pathRecords, output, required = TRUE) {
  LIST <- lapply(seq_len(nrow(keys)), function(i) {
    Row <- keys[i]
    FILE <- .recordLocalFile(Row, pathRecords = pathRecords, output = output)
    Physical <- sub(sprintf("[.]%s[.]csv$", Row$RecordID), "", basename(FILE))
    if (identical(Physical, "AT")) {
      return(list(data = .recordReadRawAT(Row, pathRecords = pathRecords),
                  keys = Row[, ProcessKeys, with = FALSE],
                  file = FILE,
                  physical = Physical))
    }
    if (!file.exists(FILE)) {
      if (isTRUE(required)) {
        stop("Missing ", output, " file for ", Row$RecordID, ": ", FILE,
             call. = FALSE)
      }
      return(NULL)
    }
    DT <- .recordFread(FILE)
    list(data = .recordAddKeyColumns(data = DT, row = Row),
         keys = Row[, ProcessKeys, with = FALSE],
         file = FILE,
         physical = Physical)
  })
  LIST <- LIST[!vapply(LIST, is.null, logical(1L))]
  if (!length(LIST)) return(NULL)
  Keys <- data.table::rbindlist(lapply(LIST, `[[`, "keys"), use.names = TRUE)
  FILES <- vapply(LIST, `[[`, character(1L), "file")
  Physical <- vapply(LIST, `[[`, character(1L), "physical")
  list(data = .recordBindTables(lapply(LIST, `[[`, "data"), label = output),
       files = .recordFileTable(Keys, output = output, physical = Physical,
                                files = FILES))
}

.recordReadProcessTable <- function(processID, paths, output, keys,
                                    required = TRUE) {
  FILE <- .recordTableFile(processID, paths, output)
  if (!file.exists(FILE)) {
    if (isTRUE(required)) {
      stop("Missing ", output, " table: ", FILE, call. = FALSE)
    }
    return(NULL)
  }
  OUT <- .recordFread(FILE)
  .requireColumns(OUT, "RecordID", output)
  OUT <- OUT[as.character(RecordID) %chin% keys$RecordID]
  if (!nrow(OUT)) {
    if (isTRUE(required)) {
      stop(output, " has no rows for requested records.",
           call. = FALSE)
    }
    return(NULL)
  }
  if (!"ProcessID" %chin% names(OUT)) OUT[, ProcessID := processID]
  list(data = OUT[],
       files = .recordFileTable(keys, output = output, physical = output,
                                files = FILE))
}

.readRecordProducts <- function(keys,
                       paths,
                       output = NULL) {
  paths <- .recordPaths(paths, need = "records")
  RES <- .recordResolveKeys(keys, paths = paths)
  Keys <- RES$keys
  ProcessID <- .recordProcessID(Keys)
  Output <- .recordOutputPlan(output, processID = ProcessID,
                              paths = paths)
  LIST <- lapply(seq_len(nrow(Output)), function(i) {
    COL <- Output$output[i]
    if (identical(Output$kind[i], "local")) {
      AUX <- .recordReadLocalOutput(keys = Keys, pathRecords = paths$records,
                                    output = COL,
                                    required = Output$required[i])
    } else {
      AUX <- .recordReadProcessTable(processID = ProcessID, paths = paths,
                                     output = COL, keys = Keys,
                                     required = Output$required[i])
    }
    if (is.null(AUX)) return(NULL)
    list(output = COL, data = AUX$data, files = AUX$files)
  })
  LIST <- LIST[!vapply(LIST, is.null, logical(1L))]
  OUT <- lapply(LIST, `[[`, "data")
  names(OUT) <- vapply(LIST, `[[`, character(1L), "output")
  FILES <- lapply(LIST, `[[`, "files")
  if (!length(OUT)) stop("No output tables were read.", call. = FALSE)
  attr(OUT, "keys") <- Keys[]
  attr(OUT, "index") <- RES$index
  attr(OUT, "files") <- data.table::rbindlist(FILES, use.names = TRUE)
  attr(OUT, "ProcessID") <- ProcessID
  OUT
}
