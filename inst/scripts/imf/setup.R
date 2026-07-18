# scripts/imf/setup.R -- IMF/VMD decomposition and filtering helpers.

.buildIMF <- function(TSL, params) {
  LIST <- split(TSL[ID == "AT", c(KEY.record, "OCID", "t", "s"),
                    with = FALSE],
                by = c(KEY.record, "OCID"), keep.by = TRUE)
  if (!length(LIST)) stop("No AT rows available for IMF decomposition.",
                          call. = FALSE)
  OUT <- vector("list", length(LIST))
  for (i in seq_along(LIST)) {
    DT <- LIST[[i]]
    AUX <- gmsp::TS2IMF(.x = DT[, .(t, s)], method = params$Method,
                        K = params$kIMF)
    for (Key in KEY.record) {
      AUX$TSW[, (Key) := DT[[Key]][1L]]
      AUX$IMF[, (Key) := DT[[Key]][1L]]
    }
    AUX$TSW[, OCID := DT$OCID[1L]]
    AUX$IMF[, OCID := DT$OCID[1L]]
    OUT[[i]] <- AUX
  }

  IMF <- rbindlist(lapply(OUT, `[[`, "TSW"), use.names = TRUE)
  IMFF <- rbindlist(lapply(OUT, `[[`, "IMF"), use.names = TRUE)
  list(IMF = IMF[], IMFF = IMFF[])
}

.imfModeColumns <- function(IMF, modes = character()) {
  COLS <- c("signal", grep("^IMF[0-9]+$", names(IMF), value = TRUE),
            "residue")
  COLS <- COLS[COLS %chin% names(IMF)]
  if (!length(COLS)) stop("IMF table has no mode columns.", call. = FALSE)
  if (!length(modes)) return(COLS)

  Missing <- setdiff(modes, COLS)
  if (length(Missing)) {
    stop(sprintf("Requested IMF modes not found: %s",
                 paste(Missing, collapse = ", ")), call. = FALSE)
  }
  modes
}

.buildIMFTSW <- function(IMF, params) {
  .requireColumns(IMF, c(KEY.record, "OCID", "t"), "IMF table")
  Modes <- .imfModeColumns(IMF, modes = params$Modes)
  Meta <- unique(IMF[, KEY.record, with = FALSE], by = KEY.record)
  if (nrow(Meta) != 1L) {
    stop("IMF table must contain exactly one record.", call. = FALSE)
  }

  LIST <- vector("list", length(Modes))
  for (i in seq_along(Modes)) {
    Mode <- Modes[i]
    DT <- IMF[, .(OCID, t, s = get(Mode))]
    .requireColumns(DT, c("OCID", "t", "s"), "IMF mode table")
    DT <- dcast(DT, t ~ OCID, value.var = "s")
    setorder(DT, t)

    TSL <- gmsp::AT2TS(
      .x = DT,
      units.source = params$Units.target,
      units.target = params$Units.target,
      Fmax = params$Fmax,
      kNyq = params$kNyq,
      output = "TSL",
      isRaw = FALSE,
      audit = FALSE)
    for (Key in KEY.record) {
      TSL[, (Key) := Meta[[Key]][1L]]
    }
    TSL[, IMF := Mode]
    LIST[[i]] <- TSL
  }

  TSL <- rbindlist(LIST, use.names = TRUE)
  TSW <- gmsp::TSL2TSW(TSL, by = c(KEY.record, "IMF"))
  setcolorder(TSW, c(KEY.record, "IMF", "t",
                    setdiff(names(TSW), c(KEY.record, "IMF", "t"))))
  setorderv(TSW, c(KEY.record, "IMF", "t"), na.last = TRUE)
  TSW[]
}

.clearIMFTSW <- function(path) {
  invisible(path)
}

.writeIMFTSWAggregate <- function(path, selection) {
  Files <- file.path(.workPath(path, selection$RecordID), "TSW.csv")
  Missing <- Files[!file.exists(Files)]
  if (length(Missing)) {
    stop(sprintf("Missing IMF TSW checkpoint in %s",
                 paste(dirname(Missing), collapse = ", ")),
         call. = FALSE)
  }
  TSW <- rbindlist(lapply(Files, fread), use.names = TRUE)
  fwrite(TSW, file.path(path, "TSW.csv"))
  invisible(NULL)
}

.readIMFSelection <- function(path) {
  FILE <- file.path(path, "manifest.csv")
  if (file.exists(FILE)) {
    DT <- fread(FILE)
    .requireColumns(DT, c(KEY.record, "status"), "source manifest")
    return(unique(DT[status == "done", KEY.record, with = FALSE],
                  by = KEY.record))
  }
  FILE <- file.path(path, "TSW.csv")
  if (!file.exists(FILE)) {
    stop(sprintf("Missing source manifest.csv or TSW.csv under %s", path),
         call. = FALSE)
  }
  DT <- fread(FILE, select = KEY.record)
  unique(DT, by = KEY.record)
}

.sourceRecordFile <- function(path, recordID, file) {
  FILE <- file.path(.workPath(path, recordID), file)
  if (!file.exists(FILE)) {
    stop(sprintf("Missing source checkpoint file: %s", FILE), call. = FALSE)
  }
  FILE
}

.copySourceFile <- function(path.source, path.record, recordID, file) {
  FILE <- .sourceRecordFile(path.source, recordID, file)
  file.copy(FILE, file.path(path.record, file), overwrite = TRUE)
}

.processIMFDecompRecord <- function(row, path, params, override) {
  RecordID <- as.character(row$RecordID[1L])
  PATH.record <- .workPath(path, RecordID)
  ROW <- copy(row[, .(RecordID, OwnerID, EventID, StationID)])
  if (!is.null(params$ProcessID)) {
    ROW[, ProcessID := params$ProcessID]
    setcolorder(ROW, c("ProcessID", "RecordID", "OwnerID", "EventID",
                       "StationID"))
  }
  FILES.db <- if (is.null(params$PATHS)) {
    character()
  } else {
    file.path(params$PATHS$records, ROW$OwnerID, ROW$EventID, ROW$StationID,
              ROW$ProcessID, sprintf("%s.%s.csv", c("IMF", "IMFF"),
                                     RecordID))
  }
  if (!override && .recordDone(PATH.record, files = c("IMF.csv", "IMFF.csv")) &&
      (!length(FILES.db) || all(file.exists(FILES.db)))) {
    return(list(RecordID = RecordID, status = "done", skipped = TRUE))
  }

  if (dir.exists(PATH.record)) unlink(PATH.record, recursive = TRUE)
  .ensureDir(PATH.record)

  Step <- "readSource"
  T0 <- proc.time()[["elapsed"]]
  .writeRecordStatus(PATH.record, row, "running",
                     list(step = Step, pid = Sys.getpid()))

  tryCatch({
    TSW <- if (is.null(params$PATHS)) {
      fread(.sourceRecordFile(params$PATH.source, RecordID, "TSW.csv"))
    } else {
      getRecord(keys = ROW, paths = params$PATHS, output = "TSW")$TSW
    }
    .requireColumns(TSW, c(KEY.record, "t"), "source TSW")
    TSL <- gmsp::TSW2TSL(TSW)
    F25Rows <- TSL[ID == "AT", .N, by = OCID][1L, N]
    if (is.finite(params$MaxF25Rows) && F25Rows > params$MaxF25Rows) {
      stop(sprintf("F25 record has %s rows; MaxF25Rows is %s.",
                   F25Rows, params$MaxF25Rows), call. = FALSE)
    }

    Step <- "buildIMF"
    .writeRecordStatus(PATH.record, row, "running",
                       list(step = Step, pid = Sys.getpid()))
    OUT <- .buildIMF(TSL, params = params)

    Step <- "write"
    .writeRecordStatus(PATH.record, row, "running",
                       list(step = Step, pid = Sys.getpid()))
    fwrite(OUT$IMF, file.path(PATH.record, "IMF.csv"))
    fwrite(OUT$IMFF, file.path(PATH.record, "IMFF.csv"))
    if (!is.null(params$PATHS)) {
      saveRecordTables(row = ROW, paths = params$PATHS,
                       tables = list(IMF = OUT$IMF, IMFF = OUT$IMFF),
                       override = override)
    }

    Seconds <- proc.time()[["elapsed"]] - T0
    .writeRecordStatus(PATH.record, row, "done", list(
      n.IMF = nrow(OUT$IMF),
      n.IMFF = nrow(OUT$IMFF),
      n.F25Rows = F25Rows,
      seconds = round(Seconds, 3)))
    list(RecordID = RecordID, status = "done", seconds = Seconds)
  }, error = function(e) {
    .writeRecordStatus(PATH.record, row, "failed",
                       list(step = Step, error = conditionMessage(e),
                            pid = Sys.getpid()))
    list(RecordID = RecordID, status = "failed",
         error = conditionMessage(e))
  })
}
