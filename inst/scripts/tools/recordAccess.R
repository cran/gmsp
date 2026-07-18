.record.KEY <- c("RecordID", "OwnerID", "EventID", "StationID")
.record.KEY.ALL <- c("ProcessID", .record.KEY)
.record.OUTPUT.LOCAL <- c("AT", "TS", "TSW", "IMW", "IMF", "IMFF", "PSW")
.record.OUTPUT.TABLE <- c("MasterIndex", "IntensityTable",
                          "ComponentMapTable", "TrimWindowTable")

.recordNull <- function(x, default) {
  if (is.null(x)) default else x
}

.recordRequireColumns <- function(data, cols, label) {
  COLS <- setdiff(cols, names(data))
  if (length(COLS)) {
    stop(sprintf("%s missing columns: %s", label, paste(COLS, collapse = ", ")),
         call. = FALSE)
  }
  invisible(NULL)
}

.recordPaths <- function(paths, need = character()) {
  if (!is.list(paths)) stop("paths must be a list.", call. = FALSE)
  if (!is.null(paths$database)) {
    DIR <- path.expand(as.character(paths$database)[1L])
    if (is.null(paths$records)) paths$records <- file.path(DIR, "records")
    if (is.null(paths$index)) paths$index <- file.path(DIR, "index")
    if (is.null(paths$process)) paths$process <- file.path(DIR, "process")
  }
  for (COL in need) {
    if (is.null(paths[[COL]]) || !nzchar(as.character(paths[[COL]])[1L])) {
      stop(sprintf("paths$%s is required.", COL), call. = FALSE)
    }
  }
  OUT <- paths
  COLS <- intersect(c("database", "records", "index", "process"), names(OUT))
  for (COL in COLS) OUT[[COL]] <- path.expand(as.character(OUT[[COL]])[1L])
  OUT
}

.recordNormalizeKeys <- function(keys) {
  if (is.null(keys)) stop("keys is required.", call. = FALSE)
  if (data.table::is.data.table(keys) || is.data.frame(keys)) {
    OUT <- data.table::copy(data.table::as.data.table(keys))
  } else if (is.list(keys)) {
    COLS <- setdiff(names(keys), .record.KEY.ALL)
    if (length(COLS)) {
      stop(sprintf("Unknown key fields: %s", paste(COLS, collapse = ", ")),
           call. = FALSE)
    }
    OUT <- data.table::as.data.table(keys)
  } else {
    stop("keys must be a named list or data.table.", call. = FALSE)
  }
  if (!nrow(OUT)) stop("keys produced zero rows.", call. = FALSE)
  COLS <- intersect(.record.KEY.ALL, names(OUT))
  if (!length(COLS)) stop("keys has no canonical key fields.", call. = FALSE)
  for (COL in COLS) data.table::set(OUT, j = COL, value = as.character(OUT[[COL]]))
  OUT[]
}

.recordIsWildcard <- function(x) {
  is.na(x) || !nzchar(x) || identical(x, "*")
}

.recordProcessID <- function(keys) {
  if (!"ProcessID" %chin% names(keys)) {
    stop("keys must include explicit ProcessID.", call. = FALSE)
  }
  OUT <- unique(keys$ProcessID[!vapply(keys$ProcessID, .recordIsWildcard,
                                       logical(1L))])
  if (length(OUT) != 1L) {
    stop("keys must resolve exactly one non-wildcard ProcessID.", call. = FALSE)
  }
  OUT
}

.recordApplyKeys <- function(data, keys) {
  for (COL in intersect(names(keys), .record.KEY.ALL)) {
    AUX <- keys[[COL]]
    AUX <- AUX[!vapply(AUX, .recordIsWildcard, logical(1L))]
    if (length(AUX) && !COL %chin% names(data)) {
      stop(sprintf("Index table missing key column: %s", COL), call. = FALSE)
    }
  }
  LIST <- lapply(seq_len(nrow(keys)), function(i) {
    OK <- rep(TRUE, nrow(data))
    for (COL in intersect(.record.KEY.ALL, names(keys))) {
      if (!COL %chin% names(data)) next
      AUX <- keys[[COL]][i]
      if (.recordIsWildcard(AUX)) next
      OK <- OK & as.character(data[[COL]]) == AUX
    }
    OK
  })
  MASK <- Reduce(`|`, LIST)
  data[MASK]
}

.recordRange <- function(x, label) {
  OUT <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(OUT) != 2L) {
    stop(sprintf("%s range must have length 2.", label), call. = FALSE)
  }
  if (is.na(OUT[1L])) OUT[1L] <- -Inf
  if (is.na(OUT[2L])) OUT[2L] <- Inf
  if (OUT[1L] > OUT[2L]) OUT <- rev(OUT)
  OUT
}

.recordApplyCriteria <- function(data, criteria) {
  if (is.null(criteria) || !length(criteria)) return(data)
  if (!is.list(criteria) || is.null(names(criteria))) {
    stop("criteria must be a named list.", call. = FALSE)
  }
  for (COL in names(criteria)) {
    if (!nzchar(COL)) stop("criteria names must be non-empty.", call. = FALSE)
    if (!COL %chin% names(data)) {
      stop(sprintf("Selection field not found in index table: %s", COL),
           call. = FALSE)
    }
    AUX <- unlist(criteria[[COL]], use.names = FALSE)
    if (is.numeric(data[[COL]]) || is.integer(data[[COL]])) {
      if (length(AUX) == 2L) {
        OUT <- .recordRange(AUX, COL)
        data <- data[!is.na(get(COL)) & get(COL) >= OUT[1L] & get(COL) <= OUT[2L]]
      } else {
        AUX <- suppressWarnings(as.numeric(AUX))
        data <- data[get(COL) %in% AUX]
      }
    } else {
      data <- data[as.character(get(COL)) %chin% as.character(AUX)]
    }
  }
  data
}

.recordBindTables <- function(tables, label) {
  if (!length(tables)) stop(sprintf("%s has no tables to bind.", label), call. = FALSE)
  COLS <- names(tables[[1L]])
  OK <- vapply(tables, function(x) setequal(names(x), COLS), logical(1L))
  if (!all(OK)) {
    stop(sprintf("%s schema mismatch before binding.", label), call. = FALSE)
  }
  data.table::rbindlist(lapply(tables, function(x) x[, ..COLS]),
                        use.names = TRUE)
}

.recordReadMany <- function(files, label) {
  FILES <- as.character(files)
  if (!length(FILES)) stop(sprintf("%s has no files.", label), call. = FALSE)
  OK <- file.exists(FILES)
  if (!all(OK)) {
    stop(sprintf("Missing %s files: %s",
                 label, paste(FILES[!OK], collapse = ", ")),
         call. = FALSE)
  }
  .recordBindTables(lapply(FILES, .recordFread),
                    label = label)
}

.recordFread <- function(file) {
  HEADER <- names(data.table::fread(file, nrows = 0L, showProgress = FALSE))
  COLS <- intersect(.record.KEY.ALL, HEADER)
  if (!length(COLS)) return(data.table::fread(file, showProgress = FALSE))
  data.table::fread(file, showProgress = FALSE,
                    colClasses = stats::setNames(rep("character",
                                                     length(COLS)), COLS))
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
      stop(sprintf("%s or recordID is required.", label), call. = FALSE)
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
  .recordRequireColumns(OUT, "RecordID", label)
  if (!"ProcessID" %chin% names(OUT)) {
    OUT[, ProcessID := ID.process]
  } else {
    OUT[is.na(ProcessID) | !nzchar(ProcessID), ProcessID := ID.process]
    if (data.table::uniqueN(OUT$ProcessID) != 1L) {
      stop(sprintf("%s has inconsistent ProcessID values.", label),
           call. = FALSE)
    }
    if (any(OUT$ProcessID != ID.process)) {
      stop(sprintf("%s ProcessID does not match %s.", label, ID.process),
           call. = FALSE)
    }
  }
  if (length(Records)) OUT <- OUT[RecordID %chin% Records]
  if (!nrow(OUT)) stop(sprintf("%s has zero rows.", label), call. = FALSE)
  OUT[]
}

.recordReadSearchTable <- function(processID, keys, paths) {
  if (identical(processID, "raw")) {
    paths <- .recordPaths(paths, need = "index")
    AUX <- if ("OwnerID" %chin% names(keys)) {
      unique(keys$OwnerID[!vapply(keys$OwnerID, .recordIsWildcard, logical(1L))])
    } else {
      character()
    }
    FILES <- if (length(AUX)) {
      file.path(paths$index, sprintf("MasterIndex.%s.csv", AUX))
    } else if (file.exists(file.path(paths$index, "RawMasterIndex.csv"))) {
      file.path(paths$index, "RawMasterIndex.csv")
    } else {
      file.path(paths$index, "MasterIndex.csv")
    }
    if (!all(file.exists(FILES)) && file.exists(file.path(paths$index, "MasterIndex.csv"))) {
      FILES <- file.path(paths$index, "MasterIndex.csv")
    }
    DT <- .recordReadMany(FILES, label = "raw index")
  } else {
    FILES <- file.path(.recordProcessDir(processID = processID, paths = paths),
                       "MasterIndex.csv")
    DT <- .recordReadMany(FILES, label = sprintf("%s MasterIndex", processID))
  }
  if (!"ProcessID" %chin% names(DT)) DT[, ProcessID := processID]
  .recordRequireColumns(DT, .record.KEY.ALL, "search table")
  for (COL in .record.KEY.ALL) data.table::set(DT, j = COL, value = as.character(DT[[COL]]))
  DT[]
}

.recordUniqueRows <- function(data) {
  .recordRequireColumns(data, .record.KEY.ALL, "record set")
  COLS <- c(.record.KEY.ALL, intersect(c("DIR", "OCID"), names(data)))
  data.table::setorderv(data, COLS, na.last = TRUE)
  unique(data, by = .record.KEY.ALL)
}

.recordSingleRecord <- function(data, singleRecord) {
  if (identical(singleRecord, FALSE)) return(.recordUniqueRows(data))
  if (identical(singleRecord, TRUE)) {
    stop("singleRecord = TRUE is ambiguous; use an explicit policy.",
         call. = FALSE)
  }
  if (!is.character(singleRecord) || length(singleRecord) != 1L ||
      is.na(singleRecord) || !nzchar(singleRecord)) {
    stop("singleRecord must be FALSE or an approved policy string.",
         call. = FALSE)
  }
  if (!identical(singleRecord, "max.PGA")) {
    stop(sprintf("Unsupported singleRecord policy: %s", singleRecord),
         call. = FALSE)
  }
  .recordRequireColumns(data, c(.record.KEY.ALL, "PGA"), "singleRecord max.PGA")
  AUX <- data.table::copy(data)
  data.table::setorder(AUX, EventID, RecordID, -PGA, OwnerID, StationID)
  AUX <- AUX[, .SD[1L], by = .record.KEY.ALL]
  data.table::setorder(AUX, EventID, -PGA, RecordID, OwnerID, StationID)
  .recordUniqueRows(AUX[, .SD[1L], by = EventID])
}

selectRecords <- function(keys,
                          criteria = list(),
                          singleRecord = FALSE,
                          paths) {
  Keys <- .recordNormalizeKeys(keys)
  ProcessID <- .recordProcessID(Keys)
  DT <- .recordReadSearchTable(processID = ProcessID, keys = Keys, paths = paths)
  DT <- .recordApplyKeys(data = DT, keys = Keys)
  DT <- .recordApplyCriteria(data = DT, criteria = criteria)
  if (!nrow(DT)) stop("Selection produced zero records.", call. = FALSE)
  OUT <- .recordSingleRecord(DT, singleRecord = singleRecord)
  if (!nrow(OUT)) stop("Selection produced zero records.", call. = FALSE)
  data.table::setcolorder(OUT, c(intersect(.record.KEY.ALL, names(OUT)),
                                 setdiff(names(OUT), .record.KEY.ALL)))
  OUT[]
}

.recordCompleteKeys <- function(keys) {
  all(.record.KEY.ALL %chin% names(keys)) &&
    all(vapply(.record.KEY.ALL, function(COL) {
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
      selectRecords(keys = Keys, criteria = list(), singleRecord = FALSE,
                    paths = paths)
    } else {
      .recordUniqueRows(Keys)
    }
    return(list(keys = OUT[, .record.KEY.ALL, with = FALSE], index = OUT))
  }
  OUT <- selectRecords(keys = Keys, criteria = list(), singleRecord = FALSE,
                       paths = paths)
  list(keys = OUT[, .record.KEY.ALL, with = FALSE], index = OUT)
}

.recordProcessDir <- function(processID, paths) {
  paths <- .recordPaths(paths, need = "process")
  file.path(paths$process, processID)
}

.recordTableDir <- function(processID, paths) {
  .recordProcessDir(processID = processID, paths = paths)
}

.recordTableFile <- function(processID, paths, output) {
  file.path(.recordTableDir(processID, paths), sprintf("%s.csv", output))
}

.recordProcessContractFile <- function(processID, paths) {
  file.path(.recordProcessDir(processID = processID, paths = paths),
            "process.json")
}

.recordReadProcessJSON <- function(processID, paths, required = TRUE) {
  FILE <- .recordProcessContractFile(processID = processID, paths = paths)
  if (!file.exists(FILE)) {
    if (isTRUE(required)) {
      stop(sprintf("Missing process contract: %s", FILE), call. = FALSE)
    }
    return(NULL)
  }
  AUX <- jsonlite::read_json(FILE, simplifyVector = TRUE)
  ID <- .recordNull(AUX$processID, .recordNull(AUX$ProcessID, NULL))
  if (!identical(as.character(ID), processID)) {
    stop(sprintf("process.json ProcessID does not match %s.", processID),
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
    stop(sprintf("process.json has empty output inventory for %s.", processID),
         call. = FALSE)
  }
  list(record = Record, process = Process)
}

.recordDeclaredOutputs <- function(processID, paths) {
  unique(.recordReadProcessContract(processID = processID, paths = paths,
                                    required = TRUE)$record)
}

.recordOutputPlan <- function(output, processID, keys, paths) {
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
      stop(sprintf("Unsupported output for ProcessID raw: %s",
                   paste(AUX, collapse = ", ")),
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
    stop(sprintf("Output not declared for ProcessID %s: %s",
                 processID, paste(Bad, collapse = ", ")),
         call. = FALSE)
  }
  OUT <- data.table::data.table(output = output, kind = "local",
                                required = TRUE)
  OUT[output %chin% Table, kind := "table"]
  OUT[]
}

.recordAddKeyColumns <- function(data, row) {
  for (COL in .record.KEY.ALL) {
    AUX <- as.character(row[[COL]][1L])
    if (!COL %chin% names(data)) {
      data.table::set(data, j = COL, value = AUX)
    } else if (any(as.character(data[[COL]]) != AUX, na.rm = TRUE)) {
      stop(sprintf("%s column does not match requested key.", COL),
           call. = FALSE)
    }
  }
  COLS <- c(.record.KEY.ALL, intersect(c("t", "ts", "OCID", "ID", "IMF", "Tn"),
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
  OUT <- data.table::copy(keys[, .record.KEY.ALL, with = FALSE])
  OUT[, `:=`(output = output,
             physical = physical,
             FILE = as.character(files))]
  OUT[]
}

.recordReadRawAT <- function(row, pathRecords) {
  FILE <- .recordLocalFile(row, pathRecords = pathRecords, output = "AT")
  JSON <- sub("[.]csv$", ".json", FILE)
  if (!file.exists(FILE) || !file.exists(JSON)) {
    stop(sprintf("Missing raw AT files for %s.", row$RecordID), call. = FALSE)
  }
  DT <- .recordFread(FILE)
  AUX <- jsonlite::read_json(JSON, simplifyVector = TRUE)
  dt <- as.numeric(AUX$dt)
  if (length(dt) != 1L || !is.finite(dt) || dt <= 0) {
    stop(sprintf("Invalid dt in %s.", JSON), call. = FALSE)
  }
  DT[, t := seq(0, by = dt, length.out = .N)]
  .recordAddKeyColumns(data = DT, row = row)
}

.recordReadLocalOutput <- function(keys, pathRecords, output, required = TRUE) {
  LIST <- lapply(seq_len(nrow(keys)), function(i) {
    Row <- keys[i]
    FILE <- .recordLocalFile(Row, pathRecords = pathRecords, output = output)
    Physical <- sub(sprintf("[.]%s[.]csv$", Row$RecordID), "", basename(FILE))
    if (identical(Physical, "AT")) {
      return(list(data = .recordReadRawAT(Row, pathRecords = pathRecords),
                  keys = Row[, .record.KEY.ALL, with = FALSE],
                  file = FILE,
                  physical = Physical))
    }
    if (!file.exists(FILE)) {
      if (isTRUE(required)) {
        stop(sprintf("Missing %s file for %s: %s",
                     output, Row$RecordID, FILE),
             call. = FALSE)
      }
      return(NULL)
    }
    DT <- .recordFread(FILE)
    list(data = .recordAddKeyColumns(data = DT, row = Row),
         keys = Row[, .record.KEY.ALL, with = FALSE],
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
      stop(sprintf("Missing %s table: %s", output, FILE), call. = FALSE)
    }
    return(NULL)
  }
  OUT <- .recordFread(FILE)
  .recordRequireColumns(OUT, "RecordID", output)
  OUT <- OUT[as.character(RecordID) %chin% keys$RecordID]
  if (!nrow(OUT)) {
    if (isTRUE(required)) {
      stop(sprintf("%s has no rows for requested records.", output),
           call. = FALSE)
    }
    return(NULL)
  }
  if (!"ProcessID" %chin% names(OUT)) OUT[, ProcessID := processID]
  list(data = OUT[],
       files = .recordFileTable(keys, output = output, physical = output,
                                files = FILE))
}

getRecords <- function(keys,
                       paths,
                       output = NULL) {
  paths <- .recordPaths(paths, need = "records")
  RES <- .recordResolveKeys(keys, paths = paths)
  Keys <- RES$keys
  ProcessID <- .recordProcessID(Keys)
  Output <- .recordOutputPlan(output, processID = ProcessID, keys = Keys,
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

getRecord <- function(keys, paths, output = NULL) {
  OUT <- getRecords(keys = keys, paths = paths, output = output)
  KEYS <- attr(OUT, "keys")
  if (nrow(unique(KEYS, by = .record.KEY.ALL)) != 1L) {
    stop("getRecord() resolved more than one record.", call. = FALSE)
  }
  OUT
}

.recordEnsureDir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

.recordWriteAtomic <- function(data, file, override = FALSE) {
  DIR <- dirname(file)
  .recordEnsureDir(DIR)
  if (file.exists(file) && !isTRUE(override)) {
    stop(sprintf("Refusing to overwrite existing file: %s", file),
         call. = FALSE)
  }
  FILE <- tempfile(pattern = paste0(".", basename(file), "."),
                   tmpdir = DIR, fileext = ".tmp")
  data.table::fwrite(data, FILE)
  if (file.exists(file)) file.remove(file)
  if (!file.rename(FILE, file)) {
    stop(sprintf("Could not write file: %s", file), call. = FALSE)
  }
  invisible(file)
}

.recordSubsetOutput <- function(data, row, output) {
  .recordRequireColumns(data, .record.KEY, output)
  OUT <- data.table::copy(data)
  for (COL in .record.KEY) {
    OUT <- OUT[as.character(get(COL)) == as.character(row[[COL]][1L])]
  }
  if (!nrow(OUT)) {
    stop(sprintf("%s has zero rows for %s.", output, row$RecordID),
         call. = FALSE)
  }
  .recordAddKeyColumns(data = OUT, row = row)
}

saveRecordTables <- function(row,
                             paths,
                             tables,
                             override = FALSE) {
  paths <- .recordPaths(paths, need = "records")
  Row <- data.table::as.data.table(row)
  .recordRequireColumns(Row, .record.KEY.ALL, "record row")
  if (nrow(Row) != 1L) stop("row must describe one record.", call. = FALSE)
  if (identical(as.character(Row$ProcessID[1L]), "raw")) {
    stop("saveRecordTables() does not write raw records.", call. = FALSE)
  }
  if (!is.list(tables) || is.null(names(tables)) || any(!nzchar(names(tables)))) {
    stop("tables must be a named list.", call. = FALSE)
  }
  Bad <- setdiff(names(tables), .record.OUTPUT.LOCAL)
  if (length(Bad)) {
    stop(sprintf("Unsupported record output: %s", paste(Bad, collapse = ", ")),
         call. = FALSE)
  }
  OUT <- vector("list", length(tables))
  names(OUT) <- names(tables)
  for (COL in names(tables)) {
    DT <- .recordSubsetOutput(data.table::as.data.table(tables[[COL]]),
                              row = Row, output = COL)
    FILE <- .recordLocalFile(Row, pathRecords = paths$records, output = COL)
    .recordWriteAtomic(data = DT, file = FILE, override = override)
    OUT[[COL]] <- data.table::data.table(
      ProcessID = as.character(Row$ProcessID[1L]),
      RecordID = as.character(Row$RecordID[1L]),
      OwnerID = as.character(Row$OwnerID[1L]),
      EventID = as.character(Row$EventID[1L]),
      StationID = as.character(Row$StationID[1L]),
      output = COL,
      FILE = FILE)
  }
  data.table::rbindlist(OUT, use.names = TRUE)
}

.recordWriteProcessContract <- function(processID, paths, record, table,
                                        params = list()) {
  FILE <- .recordProcessContractFile(processID, paths)
  if (!is.list(params)) stop("params must be a list.", call. = FALSE)
  LIST <- if (file.exists(FILE)) {
    jsonlite::read_json(FILE, simplifyVector = FALSE)
  } else {
    list()
  }
  AUX <- .recordNull(LIST$params, list())
  if (!is.list(AUX)) AUX <- list()
  if (length(params)) AUX <- utils::modifyList(AUX, params)
  OUT <- list(
    processID = processID,
    output = list(record = unique(as.character(record)),
                  table = unique(as.character(table))),
    params = AUX)
  .recordEnsureDir(dirname(FILE))
  jsonlite::write_json(OUT, FILE, auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  invisible(FILE)
}

updateProcessContract <- function(paths,
                                  processID,
                                  record = character(),
                                  table = character(),
                                  params = list()) {
  paths <- .recordPaths(paths, need = "process")
  ProcessID <- as.character(processID)
  if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
    stop("processID must be one non-empty string.", call. = FALSE)
  }
  Contract <- .recordReadProcessContract(processID = ProcessID, paths = paths,
                                         required = FALSE)
  Record <- unique(c(if (is.null(Contract)) character() else Contract$record,
                     as.character(record)))
  Table <- unique(c(if (is.null(Contract)) character() else Contract$process,
                    as.character(table)))
  Record <- Record[nzchar(Record) & !is.na(Record)]
  Table <- Table[nzchar(Table) & !is.na(Table)]
  .recordWriteProcessContract(processID = ProcessID, paths = paths, record = Record,
                              table = Table, params = params)
}

.recordPrepareProcessIntensity <- function(intensity, processID) {
  OUT <- data.table::copy(data.table::as.data.table(intensity))
  .recordRequireColumns(OUT, .record.KEY, "process intensity")
  OUT[, ProcessID := processID]
  if (!"DIR" %chin% names(OUT) && "OCID" %chin% names(OUT)) {
    OUT[, DIR := as.character(OCID)]
  }
  COLS <- c(.record.KEY.ALL, intersect(c("DIR", "OCID"), names(OUT)))
  data.table::setcolorder(OUT, c(COLS, setdiff(names(OUT), COLS)))
  OUT[]
}

.recordPrepareProcessIndex <- function(index, intensity, processID) {
  OUT <- .recordPrepareProcessIntensity(intensity, processID = processID)
  AUX <- data.table::copy(data.table::as.data.table(index))
  .recordRequireColumns(AUX, .record.KEY, "process index source")
  AUX[, (.record.KEY) := lapply(.SD, as.character), .SDcols = .record.KEY]
  OUT[, (.record.KEY) := lapply(.SD, as.character), .SDcols = .record.KEY]
  AUX <- unique(AUX, by = .record.KEY)
  MISS <- OUT[!AUX, on = .record.KEY, unique(RecordID)]
  if (length(MISS)) {
    stop(sprintf("Missing index metadata for RecordID: %s",
                 paste(MISS, collapse = ", ")),
         call. = FALSE)
  }
  COLS <- setdiff(names(AUX), c(names(OUT), "ProcessID"))
  if (length(COLS)) {
    OUT[AUX, on = .record.KEY, (COLS) := mget(paste0("i.", COLS))]
  }
  COLS <- c(.record.KEY.ALL, "DIR", "OCID",
            setdiff(names(OUT), c(.record.KEY.ALL, "DIR", "OCID")))
  data.table::setcolorder(OUT, intersect(COLS, names(OUT)))
  OUT[]
}

writeProcessTables <- function(index,
                               intensity,
                               paths,
                               processID,
                               record = c("TSW", "IMW"),
                               table = c("MasterIndex", "IntensityTable"),
                               extra = list(),
                               params = list(),
                               override = TRUE) {
  paths <- .recordPaths(paths, need = c("index", "process"))
  ProcessID <- as.character(processID)
  if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
    stop("processID must be one non-empty string.", call. = FALSE)
  }
  IntensityTable <- .recordPrepareProcessIntensity(intensity,
                                                   processID = ProcessID)
  MasterIndex <- .recordPrepareProcessIndex(index = index,
                                            intensity = IntensityTable,
                                            processID = ProcessID)
  Contract <- .recordReadProcessContract(processID = ProcessID, paths = paths,
                                         required = FALSE)
  Record <- unique(c(if (is.null(Contract)) character() else Contract$record,
                     record))
  Table <- unique(c(if (is.null(Contract)) character() else Contract$process,
                    table, names(extra)))
  DIR <- .recordTableDir(processID = ProcessID, paths)
  .recordWriteAtomic(data = MasterIndex, file.path(DIR, "MasterIndex.csv"),
                     override = override)
  .recordWriteAtomic(data = IntensityTable, file.path(DIR, "IntensityTable.csv"),
                     override = override)
  if (!is.list(extra) || (length(extra) && is.null(names(extra)))) {
    stop("extra must be a named list.", call. = FALSE)
  }
  for (COL in names(extra)) {
    if (!nzchar(COL)) stop("extra table names must be non-empty.", call. = FALSE)
    DT <- data.table::as.data.table(extra[[COL]])
    .recordRequireColumns(DT, "RecordID", COL)
    .recordWriteAtomic(data = DT, file.path(DIR, sprintf("%s.csv", COL)),
                       override = override)
  }
  .recordWriteProcessContract(processID = ProcessID, paths = paths,
                              record = Record,
                              table = Table,
                              params = params)
  buildProcessIndex(paths = paths)
  invisible(list(MasterIndex = MasterIndex, IntensityTable = IntensityTable))
}

buildProcessIndex <- function(paths, tables = NULL) {
  paths <- .recordPaths(paths, need = c("index", "process"))
  DIRS <- list.files(paths$process, full.names = TRUE, recursive = FALSE)
  DIRS <- DIRS[file.info(DIRS)$isdir %in% TRUE]
  if (!length(DIRS)) stop("No process directories found.", call. = FALSE)
  if (is.null(tables)) {
    tables <- sort(unique(unlist(lapply(DIRS, function(DIR) {
      sub("[.]csv$", "", basename(list.files(DIR, pattern = "[.]csv$",
                                             full.names = FALSE)))
    }), use.names = FALSE)))
  }
  tables <- unique(as.character(tables))
  tables <- tables[nzchar(tables) & !is.na(tables)]
  if (!length(tables)) stop("No process tables found.", call. = FALSE)
  OUT <- vector("list", length(tables))
  names(OUT) <- tables
  for (COL in tables) {
    FILES <- file.path(DIRS, sprintf("%s.csv", COL))
    FILES <- FILES[file.exists(FILES)]
    if (!length(FILES)) next
    DT <- .recordReadMany(FILES, label = sprintf("process %s", COL))
    .recordWriteAtomic(data = DT, file.path(paths$index, sprintf("%s.csv", COL)),
                       override = TRUE)
    OUT[[COL]] <- DT
  }
  invisible(OUT[!vapply(OUT, is.null, logical(1L))])
}
