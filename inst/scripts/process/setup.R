# scripts/process/setup.R -- composed raw-to-ProcessID producer.

.processConfig <- function(config, stage) {
  OUT <- config[[stage]]
  if (!is.null(OUT)) return(OUT)
  Names <- names(config)
  if (!length(Names)) return(list())
  Hit <- Names[tolower(Names) == tolower(stage)]
  if (length(Hit)) config[[Hit[1L]]] else list()
}

.processValue <- function(config, key, default = NULL) {
  .jsonGet(config, c(key), default = default)
}

.processStages <- function(stages) {
  OUT <- unique(tolower(gsub("[._-]", "", as.character(stages))))
  OUT <- OUT[nzchar(OUT) & !is.na(OUT)]
  if (!length(OUT)) stop("stages must contain at least one stage.",
                         call. = FALSE)
  Bad <- setdiff(OUT, c("gmsp", "trim", "imf", "imfts", "psa"))
  if (length(Bad)) {
    stop(sprintf("Unsupported process stage: %s", paste(Bad, collapse = ", ")),
         call. = FALSE)
  }
  OUT
}

.processStageLabel <- function(stages) {
  OUT <- stages
  OUT[OUT == "imfts"] <- "imfTS"
  OUT
}

.processStageRoot <- function(out) {
  DIR <- out$stage
  if (is.null(DIR)) DIR <- .resolvePath(file.path("gmsp", "stage"))
  path.expand(as.character(DIR)[1L])
}

.processStageDir <- function(out, processID, stage) {
  file.path(.processStageRoot(out), sprintf("%s.%s", processID, stage))
}

.processRow <- function(row, processID) {
  OUT <- copy(as.data.table(row))
  .requireColumns(OUT, KEY.record, "record row")
  if (nrow(OUT) != 1L) stop("processRecord() row must have one record.",
                            call. = FALSE)
  OUT[, (KEY.record) := lapply(.SD, as.character), .SDcols = KEY.record]
  OUT[, ProcessID := processID]
  setcolorder(OUT, c("ProcessID", KEY.record,
                     setdiff(names(OUT), c("ProcessID", KEY.record))))
  OUT[]
}

.processOutputFile <- function(row, paths, output) {
  file.path(paths$records, row$OwnerID, row$EventID, row$StationID,
            row$ProcessID, sprintf("%s.%s.csv", output, row$RecordID))
}

.processReadOutput <- function(row, paths, output) {
  FILE <- .processOutputFile(row = row, paths = paths, output = output)
  if (!file.exists(FILE)) {
    stop(sprintf("Missing %s product for %s: %s", output, row$RecordID, FILE),
         call. = FALSE)
  }
  .recordFread(FILE)
}

.processRecordDone <- function(row, paths, output) {
  if (!length(output)) return(FALSE)
  FILES <- vapply(output, function(ID) {
    .processOutputFile(row = row, paths = paths, output = ID)
  }, character(1L))
  all(file.exists(FILES))
}

.processGMSPParams <- function(config, paths, processID) {
  list(
    PATH.records = paths$records,
    PATHS = paths,
    ProcessID = processID,
    Fmax = as.numeric(.processValue(config, "fmax", 25)),
    kNyq = as.numeric(.processValue(config, "kNyq", 4)),
    flatZeros = isTRUE(.processValue(config, "flatZeros", FALSE)),
    trimZeros = isTRUE(.processValue(config, "trimZeros", FALSE)),
    Astop0 = as.numeric(.processValue(config, "Astop0", 1e-3)),
    Units.source = as.character(.processValue(config, "unitsSource", "mm")),
    Units.target = as.character(.processValue(config, "unitsTarget", "mm")),
    MaxF25Rows = as.numeric(.processValue(config, "maxF25Rows", Inf)))
}

.processTrimParams <- function(config) {
  list(
    PATH.source = NULL,
    PATHS = NULL,
    ProcessID = NULL,
    ID = as.character(.processValue(config, "ID", "AI")),
    DIR = .readCharacterVector(.processValue(config, "DIR",
                                             c("H1", "H2", "UP")),
                               "trim DIR"),
    range = .validTrimRange(.processValue(config, "range", c(0.005, 0.99)),
                            "trim range"),
    taper = as.numeric(.processValue(config, "taper", 2)),
    astop = as.numeric(.processValue(config, "astop", 0.01)),
    apass = as.numeric(.processValue(config, "apass", 0.95)),
    Fmax = as.numeric(.processValue(config, "fmax", 25)),
    kNyq = as.numeric(.processValue(config, "kNyq", 4)),
    units = as.character(.processValue(config, "unitsTarget", "mm")),
    files = c("TSW.csv", "IMW.csv", "WINDOW.csv"))
}

.processIMFParams <- function(config) {
  list(
    PATH.source = NULL,
    PATHS = NULL,
    ProcessID = NULL,
    Method = as.character(.processValue(config, "method", "vmd")),
    kIMF = as.integer(.processValue(config, "k", 10L)),
    MaxF25Rows = as.numeric(.processValue(config, "maxF25Rows", Inf)))
}

.processIMFTSParams <- function(config) {
  list(
    remove = as.character(unlist(.processValue(config, "remove",
                                               character()))),
    includeResidue = isTRUE(.processValue(config, "includeResidue", TRUE)),
    Fmax = as.numeric(.processValue(config, "fmax", 25)),
    kNyq = as.numeric(.processValue(config, "kNyq", 4)),
    units = as.character(.processValue(config, "unitsTarget", "mm")))
}

.processPSAParams <- function(config) {
  list(
    XI = as.numeric(unlist(.processValue(config, "xi", 0.05))),
    Tn = .tnGrid(config = config, sourceJson = list()),
    D50 = isTRUE(.processValue(config, "D50", TRUE)),
    D100 = isTRUE(.processValue(config, "D100", FALSE)),
    nTheta = as.integer(.processValue(config, "nTheta", 12L)))
}

.processStageParams <- function(config, stages) {
  OUT <- list()
  Label <- .processStageLabel(stages)
  for (i in seq_along(stages)) OUT[[Label[i]]] <- .processConfig(config, Label[i])
  list(stages = OUT)
}

.processRecordOutputs <- function(stages) {
  unique(c(
    if (any(c("gmsp", "trim", "imfts") %chin% stages)) c("TSW", "IMW"),
    if ("imf" %chin% stages) c("IMF", "IMFF"),
    if ("psa" %chin% stages) "PSW"
  ))
}

.processRunGMSP <- function(row, paths, processID, config, out, override) {
  DIR <- .processStageDir(out = out, processID = processID, stage = "gmsp")
  .ensureDir(.workRoot(DIR))
  OUT <- .processGMSPRecord(
    row = row[, KEY.record, with = FALSE],
    path = DIR,
    params = .processGMSPParams(config = config, paths = paths,
                                processID = processID),
    override = override)
  if (identical(OUT$status, "failed")) stop(OUT$error, call. = FALSE)
  ComponentMapTable <- .recordFread(file.path(.workPath(DIR, row$RecordID),
                                             "OCIDMAP.csv"))
  ComponentMapTable[, ProcessID := processID]
  setcolorder(ComponentMapTable,
              c("ProcessID", KEY.record, "OCID", "DIR", "theta",
                setdiff(names(ComponentMapTable),
                        c("ProcessID", KEY.record, "OCID", "DIR",
                          "theta"))))
  list(status = OUT, ComponentMapTable = ComponentMapTable)
}

.processRunTrim <- function(row, paths, processID, config, out, override) {
  Params <- .processTrimParams(config)
  Row <- copy(row)
  Row[, `:=`(range.start = Params$range[1L],
             range.end = Params$range[2L])]
  TSW <- .processReadOutput(row = row, paths = paths, output = "TSW")
  OUT <- .buildTrimmedTS(TSW = TSW, row = Row, params = Params)
  saveRecordTables(row = row, paths = paths,
                   tables = list(TSW = OUT$TSW, IMW = OUT$IMW),
                   override = override)
  list(status = list(RecordID = row$RecordID, status = "done",
                     n.TSW = nrow(OUT$TSW), n.IMW = nrow(OUT$IMW)),
       IMW = OUT$IMW,
       TrimWindowTable = OUT$WINDOW)
}

.processRunIMF <- function(row, paths, processID, config, out, override) {
  invisible(processID)
  invisible(out)
  TSW <- .processReadOutput(row = row, paths = paths, output = "TSW")
  TSL <- gmsp::TSW2TSL(TSW)
  OUT <- .buildIMF(TSL = TSL, params = .processIMFParams(config))
  saveRecordTables(row = row, paths = paths,
                   tables = list(IMF = OUT$IMF, IMFF = OUT$IMFF),
                   override = override)
  list(RecordID = row$RecordID, status = "done",
       n.IMF = nrow(OUT$IMF), n.IMFF = nrow(OUT$IMFF))
}

.processBuildIMFTS <- function(IMF, row, params) {
  .requireColumns(IMF, c(KEY.record, "OCID", "t"), "IMF table")
  Modes <- grep("^IMF[0-9]+$", names(IMF), value = TRUE)
  Modes <- Modes[order(as.integer(sub("^IMF", "", Modes)))]
  if (!length(Modes)) stop("IMF table has no IMF mode columns.",
                           call. = FALSE)
  Remove <- params$remove[nzchar(params$remove)]
  Missing <- setdiff(Remove, Modes)
  if (length(Missing)) {
    stop(sprintf("Requested IMF modes not found: %s",
                 paste(Missing, collapse = ", ")), call. = FALSE)
  }
  Keep <- setdiff(Modes, Remove)
  if (isTRUE(params$includeResidue)) {
    .requireColumns(IMF, "residue", "IMF table")
  }

  DT <- copy(IMF)
  DT[, s := if (length(Keep)) rowSums(.SD) else 0, .SDcols = Keep]
  if (isTRUE(params$includeResidue)) DT[, s := s + residue]

  AT <- dcast(DT[, .(OCID, t, s)], t ~ OCID, value.var = "s")
  setorder(AT, t)
  TSL <- gmsp::AT2TS(
    .x = AT,
    units.source = params$units,
    units.target = params$units,
    Fmax = params$Fmax,
    kNyq = params$kNyq,
    output = "TSL",
    isRaw = FALSE,
    audit = FALSE)
  for (Key in KEY.record) TSL[, (Key) := row[[Key]][1L]]
  setcolorder(TSL, c(KEY.record, "OCID", "ID", "t", "s"))

  Cumulative <- .cumulativeTSL(TSL, units = params$units)
  TSW <- gmsp::TSL2TSW(
    rbindlist(list(TSL, Cumulative), use.names = TRUE),
    ids = c("AT", "VT", "DT", "AI", "CAV", "CAV5"))
  IMW <- gmsp::TSL2IM(.x = TSL, units.source = params$units,
                      units.target = params$units, output = "IMW")
  list(TSW = TSW[], IMW = IMW[])
}

.processRunIMFTS <- function(row, paths, config, override) {
  IMF <- .processReadOutput(row = row, paths = paths, output = "IMF")
  OUT <- .processBuildIMFTS(IMF = IMF, row = row,
                            params = .processIMFTSParams(config))
  saveRecordTables(row = row, paths = paths,
                   tables = list(TSW = OUT$TSW, IMW = OUT$IMW),
                   override = override)
  list(status = list(RecordID = row$RecordID, status = "done",
                     n.TSW = nrow(OUT$TSW), n.IMW = nrow(OUT$IMW)),
       IMW = OUT$IMW)
}

.processRunPSA <- function(row, paths, processID, config, out, override) {
  invisible(processID)
  invisible(out)
  Params <- .processPSAParams(config)
  TSW <- .processReadOutput(row = row, paths = paths, output = "TSW")
  PSW <- .buildRecordPSW(TSW = TSW, XI = Params$XI, Tn = Params$Tn,
                         D50 = Params$D50, D100 = Params$D100,
                         nTheta = Params$nTheta)
  saveRecordTables(row = row, paths = paths, tables = list(PSW = PSW),
                   override = override)
  list(RecordID = row$RecordID, status = "done", n.PSW = nrow(PSW))
}

processRecord <- function(row,
                          paths,
                          processID,
                          stages,
                          config = list(),
                          out = list(),
                          override = FALSE) {
  paths <- .recordPaths(paths, need = c("records", "index", "process"))
  Stages <- .processStages(stages)
  Row <- .processRow(row = row, processID = processID)
  Outputs <- .processRecordOutputs(Stages)
  if (!isTRUE(override) && .processRecordDone(row = Row, paths = paths,
                                              output = Outputs)) {
    return(list(row = Row, status = "skipped", stages = list(),
                IMW = if ("IMW" %chin% Outputs) {
                  .processReadOutput(row = Row, paths = paths, output = "IMW")
                } else {
                  NULL
                },
                tables = list()))
  }

  OUT <- list()
  Tables <- list()
  IMW <- NULL
  for (Stage in Stages) {
    if (identical(Stage, "gmsp")) {
      OUT$gmsp <- .processRunGMSP(row = Row, paths = paths,
                                  processID = processID,
                                  config = .processConfig(config, "gmsp"),
                                  out = out, override = override)
      IMW <- .processReadOutput(row = Row, paths = paths, output = "IMW")
      Tables$ComponentMapTable <- OUT$gmsp$ComponentMapTable
    }
    if (identical(Stage, "trim")) {
      OUT$trim <- .processRunTrim(row = Row, paths = paths,
                                  processID = processID,
                                  config = .processConfig(config, "trim"),
                                  out = out, override = override)
      IMW <- OUT$trim$IMW
      Tables$TrimWindowTable <- OUT$trim$TrimWindowTable
    }
    if (identical(Stage, "imf")) {
      OUT$imf <- .processRunIMF(row = Row, paths = paths,
                                processID = processID,
                                config = .processConfig(config, "imf"),
                                out = out, override = override)
    }
    if (identical(Stage, "imfts")) {
      OUT$imfTS <- .processRunIMFTS(
        row = Row, paths = paths,
        config = .processConfig(config, "imfTS"),
        override = override)
      IMW <- OUT$imfTS$IMW
    }
    if (identical(Stage, "psa")) {
      OUT$psa <- .processRunPSA(row = Row, paths = paths,
                                processID = processID,
                                config = .processConfig(config, "psa"),
                                out = out, override = override)
    }
  }
  list(row = Row, status = "done", stages = OUT, IMW = IMW, tables = Tables)
}

.processRecordWorker <- function(row, path, params, override) {
  invisible(path)
  tryCatch(
    processRecord(row = row,
                  paths = params$PATHS,
                  processID = params$ProcessID,
                  stages = params$Stages,
                  config = params$Config,
                  out = params$Out,
                  override = override),
    error = function(e) {
      list(row = row, RecordID = as.character(row$RecordID[1L]),
           status = "failed", error = conditionMessage(e),
           stages = list(), IMW = NULL, tables = list())
    })
}

.processWriteSelection <- function(selection, out) {
  DIR <- file.path(.processStageRoot(out),
                   sprintf("%s.selection", selection$ProcessID[1L]))
  .ensureDir(DIR)
  fwrite(selection, file.path(DIR, "selection.csv"))
  DIR
}

processRecords <- function(selection,
                           paths,
                           processID,
                           stages,
                           config = list(),
                           out = list(),
                           parallel = FALSE,
                           workers = 1L,
                           logEvery = 30,
                           override = FALSE) {
  paths <- .recordPaths(paths, need = c("records", "index", "process"))
  Selection <- copy(as.data.table(selection))
  .requireColumns(Selection, KEY.record, "processRecords selection")
  Selection <- unique(Selection, by = KEY.record)
  Selection[, (KEY.record) := lapply(.SD, as.character), .SDcols = KEY.record]
  Selection[, ProcessID := processID]
  setcolorder(Selection, c("ProcessID", KEY.record,
                           setdiff(names(Selection),
                                   c("ProcessID", KEY.record))))

  Stages <- .processStages(stages)
  Params <- list(PATHS = paths, ProcessID = processID, Stages = Stages,
                 Config = config, Out = out)

  LIST <- if (isTRUE(parallel) && workers > 1L) {
    .runRecordsParallel(Selection, path = .processStageRoot(out),
                        params = Params, override = override,
                        files = character(),
                        processRecord = .processRecordWorker,
                        workers = workers, log.every = logEvery,
                        label = "process")
  } else {
    .runRecordsSequential(Selection, path = .processStageRoot(out),
                          params = Params, override = override,
                          files = character(),
                          processRecord = .processRecordWorker,
                          label = "process")
  }

  Status <- vapply(LIST, `[[`, character(1L), "status")
  if (any(Status == "failed")) {
    BAD <- vapply(LIST[Status == "failed"], function(x) {
      sprintf("%s: %s", x[["RecordID"]], x[["error"]])
    }, character(1L))
    stop(sprintf("processRecord failed for %s records:\n%s",
                 length(BAD), paste(BAD, collapse = "\n")),
         call. = FALSE)
  }

  Outputs <- .processRecordOutputs(Stages)
  IMWList <- lapply(LIST, `[[`, "IMW")
  HasIMW <- vapply(IMWList, Negate(is.null), logical(1L))
  Extra <- list()
  ComponentMap <- lapply(LIST, function(x) x$tables$ComponentMapTable)
  ComponentMap <- ComponentMap[!vapply(ComponentMap, is.null, logical(1L))]
  if (length(ComponentMap)) {
    Extra$ComponentMapTable <- rbindlist(ComponentMap, use.names = TRUE)
  }
  Trim <- lapply(LIST, function(x) x$tables$TrimWindowTable)
  Trim <- Trim[!vapply(Trim, is.null, logical(1L))]
  if (length(Trim)) {
    Extra$TrimWindowTable <- rbindlist(Trim, use.names = TRUE)
  }
  if (length(Outputs) && any(HasIMW)) {
    writeProcessTables(
      index = Selection,
      intensity = rbindlist(IMWList[HasIMW], use.names = TRUE),
      paths = paths,
      processID = processID,
      record = Outputs,
      extra = Extra,
      params = .processStageParams(config = config, stages = Stages),
      override = TRUE)
  } else if (length(Outputs)) {
    updateProcessContract(
      paths = paths,
      processID = processID,
      record = Outputs,
      table = names(Extra),
      params = .processStageParams(config = config, stages = Stages))
  }
  SelectionDir <- .processWriteSelection(selection = Selection, out = out)
  OUT <- list(records = LIST, selection = SelectionDir,
              process = .recordProcessContractFile(processID = processID,
                                                   paths = paths))
  OUT
}
