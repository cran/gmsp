# scripts/psa/setup.R -- PSA stage helpers.

.sourceJson <- function(path) {
  FILES <- list.files(path, pattern = "[.]json$", full.names = TRUE)
  FILES <- FILES[!basename(FILES) %chin% c("runGMSP.json", "runExport.json",
                                           "runBuildPSA.json", "runPSA.json",
                                           "runPlot.json")]
  if (length(FILES) == 1L) return(FILES)
  NA_character_
}

.tnGrid <- function(config, sourceJson) {
  Tn <- unlist(.jsonGet(config, c("Tn", "values"), default = NULL))
  if (length(Tn)) return(as.numeric(Tn))

  Fmax <- .jsonGet(sourceJson, c("params", "fmax"), default = NULL)
  if (is.null(Fmax)) Fmax <- .jsonGet(sourceJson, c("fmax"), default = NULL)
  if (is.null(Fmax)) Fmax <- .jsonGet(sourceJson, c("Fmax"), default = NULL)
  if (is.null(Fmax)) Fmax <- .jsonGet(config, c("fmax"), default = 25)
  Fmax <- as.numeric(Fmax)
  TnMin <- unlist(.jsonGet(config, c("Tn", "min"), default = NULL))
  if (!length(TnMin)) TnMin <- 1 / Fmax
  TnMax <- .readScalarNumber(.jsonGet(config, c("Tn", "max"), default = 10),
                             "Tn/max")
  nTn <- as.integer(.jsonGet(config, c("Tn", "n"), default = 101))
  if (nTn < 2L) stop("Tn/n must be >= 2.", call. = FALSE)
  10 ^ seq(log10(as.numeric(TnMin)), log10(TnMax), length.out = nTn)
}

.buildRecordPSW <- function(TSW, XI, Tn, D50, D100, nTheta) {
  TSL <- gmsp::TSW2TSL(TSW)
  TSL <- TSL[ID %chin% c("AT", "VT", "DT")]
  Tn <- Tn[Tn > 0]
  OUT <- rbindlist(lapply(XI, function(xi) {
    PSW <- gmsp::TSL2PS(TSL, xi = xi, Tn = Tn, output = "PSW",
                        D50 = D50, D100 = D100, nTheta = nTheta)
    PSW[, xi := xi]
    PSW
  }), use.names = TRUE)
  Order <- c(KEY.record, "IMF", "xi", "Tn")
  setcolorder(OUT, c(intersect(Order, names(OUT)),
                     setdiff(names(OUT), Order)))
  OUT[]
}

.processPSARecord <- function(row, path, params, override) {
  RecordID <- as.character(row$RecordID[1L])
  PATH.record <- .workPath(path, RecordID)
  FILE <- file.path(PATH.record, "PSW.csv")
  ROW <- copy(row[, .(RecordID, OwnerID, EventID, StationID)])
  if (!is.null(params$ProcessID)) {
    ROW[, ProcessID := params$ProcessID]
    setcolorder(ROW, c("ProcessID", "RecordID", "OwnerID", "EventID",
                       "StationID"))
  }
  FILE.db <- if (is.null(params$PATHS)) {
    character()
  } else {
    file.path(params$PATHS$records, ROW$OwnerID, ROW$EventID, ROW$StationID,
              ROW$ProcessID, sprintf("PSW.%s.csv", RecordID))
  }
  if (!override && file.exists(FILE) &&
      (!length(FILE.db) || file.exists(FILE.db))) {
    return(list(RecordID = RecordID, status = "done", skipped = TRUE))
  }

  .ensureDir(PATH.record)
  T0 <- proc.time()[["elapsed"]]
  tryCatch({
    if (!is.null(params$PATHS)) {
      TSW <- getRecord(keys = ROW, paths = params$PATHS, output = "TSW")$TSW
    } else {
      PATH.source <- if (is.null(params$PATH.source)) {
        path
      } else {
        params$PATH.source
      }
      FILE.source <- file.path(.workPath(PATH.source, RecordID), "TSW.csv")
      TSW <- if (file.exists(FILE.source)) {
        .recordFread(FILE.source)
      } else if (is.null(params$TSW)) {
        stop(sprintf("Missing source checkpoint file: %s", FILE.source),
             call. = FALSE)
      } else {
        params$TSW[params$TSW[["RecordID"]] == RecordID]
      }
    }
    PSW <- .buildRecordPSW(
      TSW = TSW,
      XI = params$XI,
      Tn = params$Tn,
      D50 = params$D50,
      D100 = params$D100,
      nTheta = params$nTheta)
    fwrite(PSW, FILE)
    if (!is.null(params$PATHS)) {
      saveRecordTables(row = ROW, paths = params$PATHS,
                       tables = list(PSW = PSW), override = override)
    }
    list(
      RecordID = RecordID,
      status = "done",
      seconds = proc.time()[["elapsed"]] - T0)
  }, error = function(e) {
    list(
      RecordID = RecordID,
      status = "failed",
      error = conditionMessage(e))
  })
}

buildPSARecord <- function(row, path, params, override) {
  .processPSARecord(row = row, path = path, params = params,
                    override = override)
}

buildPSARecords <- function(selection, path, params, override,
                            parallel = TRUE, workers = 1L,
                            logEvery = 30) {
  if (parallel && workers > 1L) {
    return(.runRecordsParallel(
      selection,
      path = path,
      params = params,
      override = override,
      files = "PSW.csv",
      processRecord = buildPSARecord,
      workers = workers,
      log.every = logEvery,
      label = "PSA"))
  }
  .runRecordsSequential(
    selection,
    path = path,
    params = params,
    override = override,
    files = "PSW.csv",
    processRecord = buildPSARecord,
    label = "PSA")
}

.writePSWAggregate <- function(path, selection) {
  Files <- file.path(.workPath(path, selection$RecordID), "PSW.csv")
  Missing <- Files[!file.exists(Files)]
  if (length(Missing)) {
    stop(sprintf("Missing PSW checkpoint in %s",
                 paste(dirname(Missing), collapse = ", ")),
         call. = FALSE)
  }
  PSW <- rbindlist(lapply(Files, .recordFread), use.names = TRUE)
  fwrite(PSW, file.path(path, "PSW.csv"))
  invisible(NULL)
}

.nPSWRecords <- function(path, selection) {
  sum(file.exists(file.path(.workPath(path, selection$RecordID), "PSW.csv")))
}

.clearPSW <- function(path) {
  invisible(path)
}
