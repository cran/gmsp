# scripts/gmsp/setup.R -- F25 signal builders.

.mapRecord <- function(DT) {
  Mapped <- gmsp::mapComponents(DT, rotate = TRUE, output = "wide")
  if (is.null(Mapped)) {
    stop("gmsp::mapComponents() could not map a selected record.",
         call. = FALSE)
  }
  Map <- attr(Mapped, "componentMap", exact = TRUE)
  if (is.null(Map)) {
    stop("gmsp::mapComponents() did not return componentMap.",
         call. = FALSE)
  }
  Map <- as.data.table(Map)
  .requireColumns(Map, c("OCID", "DIR"), "component map")
  Theta <- attr(Mapped, "theta", exact = TRUE)
  Map[, theta := if (is.null(Theta)) NA_real_ else as.numeric(Theta)]
  setcolorder(Map, c("OCID", "DIR", "theta",
                     setdiff(names(Map), c("OCID", "DIR", "theta"))))
  list(AT = Mapped, OCIDMAP = Map[])
}

.buildRawTSL <- function(AT, params) {
  SignalCols <- setdiff(names(AT), c(KEY.record, "t"))
  OUT <- AT[, {
    Mapped <- .mapRecord(.SD)
    TSL <- gmsp::AT2TS(Mapped$AT, units.source = params$Units.source,
                       units.target = params$Units.target,
                       Fmax = params$Fmax, kNyq = params$kNyq,
                       flatZeros = params$flatZeros,
                       trimZeros = params$trimZeros,
                       Astop0 = params$Astop0,
                       output = "TSL", isRaw = TRUE, audit = TRUE)
    .(TSL = list(TSL), OCIDMAP = list(Mapped$OCIDMAP))
  }, by = KEY.record, .SDcols = c("t", SignalCols)]

  TSL <- OUT[, TSL[[1L]], by = KEY.record]
  setcolorder(TSL, c(KEY.record, "OCID", "ID", "t", "s"))

  OCIDMAP <- OUT[, OCIDMAP[[1L]], by = KEY.record]
  setcolorder(OCIDMAP, c(KEY.record, "OCID", "DIR", "theta",
                         setdiff(names(OCIDMAP),
                                 c(KEY.record, "OCID", "DIR", "theta"))))
  list(TSL = TSL[], OCIDMAP = OCIDMAP[])
}

.buildCumulativeTSL <- function(TSL, params) {
  LIST <- list(
    AI = gmsp::TSL2AI(
      .x = TSL,
      units.source = params$Units.target,
      units.target = params$Units.target,
      output = "long"),
    CAV = gmsp::TSL2CAV(
      .x = TSL,
      units.source = params$Units.target,
      units.target = params$Units.target,
      output = "long"),
    CAV5 = gmsp::TSL2CAV5(
      .x = TSL,
      units.source = params$Units.target,
      units.target = params$Units.target,
      output = "long"))

  OUT <- rbindlist(lapply(names(LIST), function(IDValue) {
    DT <- LIST[[IDValue]]
    setnames(DT, "ratio", "s")
    DT[, ID := IDValue]
    DT[]
  }), use.names = TRUE)
  setcolorder(OUT, c(KEY.record, "OCID", "ID", "t", "s"))
  setorderv(OUT, c(KEY.record, "OCID", "ID", "t"), na.last = TRUE)
  OUT[]
}

.processGMSPRecord <- function(row, path, params, override) {
  RecordID <- as.character(row$RecordID[1L])
  PATH.record <- .workPath(path, RecordID)
  ROW <- copy(row[, .(RecordID, OwnerID, EventID, StationID)])
  ROW[, ProcessID := params$ProcessID]
  setcolorder(ROW, c("ProcessID", "RecordID", "OwnerID", "EventID",
                     "StationID"))
  FILES.db <- file.path(params$PATHS$records, ROW$OwnerID, ROW$EventID,
                        ROW$StationID, ROW$ProcessID,
                        sprintf("%s.%s.csv", c("TSW", "IMW"), RecordID))
  if (!override && .recordDone(PATH.record, files = FILES.TS) &&
      all(file.exists(FILES.db))) {
    return(list(RecordID = RecordID, status = "done", skipped = TRUE))
  }

  if (dir.exists(PATH.record)) unlink(PATH.record, recursive = TRUE)
  .ensureDir(PATH.record)

  Step <- "readAT"
  T0 <- proc.time()[["elapsed"]]
  .writeRecordStatus(PATH.record, row, "running",
                     list(step = Step, pid = Sys.getpid()))

  tryCatch({
    AT <- gmsp::readAT(
      .x = row[, .(RecordID, OwnerID, EventID, StationID)],
      path = path.expand(params$PATH.records))
    if (!nrow(AT)) {
      stop("gmsp::readAT returned zero rows for selected record.",
           call. = FALSE)
    }

    Step <- "buildTS"
    .writeRecordStatus(PATH.record, row, "running",
                       list(step = Step, pid = Sys.getpid()))
    Raw <- .buildRawTSL(AT, params = params)
    F25Rows <- Raw$TSL[ID == "AT", .N, by = OCID][1L, N]
    if (is.finite(params$MaxF25Rows) && F25Rows > params$MaxF25Rows) {
      stop(sprintf("F25 record has %s rows; MaxF25Rows is %s.",
                   F25Rows, params$MaxF25Rows), call. = FALSE)
    }

    CumulativeTSL <- .buildCumulativeTSL(Raw$TSL, params = params)
    TSW <- gmsp::TSL2TSW(
      rbindlist(list(Raw$TSL, CumulativeTSL), use.names = TRUE),
      ids = c("AT", "VT", "DT", "AI", "CAV", "CAV5"))
    IMW <- gmsp::TSL2IM(.x = Raw$TSL, units.source = params$Units.target,
                        units.target = params$Units.target, output = "IMW")

    Step <- "write"
    .writeRecordStatus(PATH.record, row, "running",
                       list(step = Step, pid = Sys.getpid()))
    fwrite(TSW, file.path(PATH.record, "TSW.csv"))
    fwrite(IMW, file.path(PATH.record, "IMW.csv"))
    fwrite(Raw$OCIDMAP, file.path(PATH.record, "OCIDMAP.csv"))
    fwrite(row, file.path(PATH.record, "META.csv"))
    saveRecordTables(row = ROW, paths = params$PATHS,
                     tables = list(TSW = TSW, IMW = IMW),
                     override = override)

    Seconds <- proc.time()[["elapsed"]] - T0
    .writeRecordStatus(PATH.record, row, "done", list(
      n.TSW = nrow(TSW),
      n.IMW = nrow(IMW),
      n.OCIDMAP = nrow(Raw$OCIDMAP),
      n.META = 1L,
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
