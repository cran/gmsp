# scripts/trim/setup.R -- cumulative-intensity Butterworth trim prototype.

.sourceJson <- function(path) {
  FILES <- list.files(path, pattern = "[.]json$", full.names = TRUE)
  FILES <- FILES[!basename(FILES) %chin% c("runGMSP.json", "runExport.json",
                                           "runBuildPSA.json", "runPlot.json",
                                           "runTrim.json")]
  if (length(FILES) == 1L) return(FILES)
  NA_character_
}

.trimSelection <- function(path, recordID = character()) {
  FILE <- file.path(path, "manifest.csv")
  if (file.exists(FILE)) {
    DT <- fread(FILE)
    .requireColumns(DT, c(KEY.record, "status"), "source manifest")
    OUT <- unique(DT[status == "done", KEY.record, with = FALSE],
                  by = KEY.record)
  } else {
    FILE <- file.path(path, "TSW.csv")
    if (!file.exists(FILE)) {
      stop(sprintf("Missing source manifest.csv or TSW.csv under %s", path),
           call. = FALSE)
    }
    DT <- fread(FILE, select = KEY.record)
    OUT <- unique(DT, by = KEY.record)
  }
  if (length(recordID)) OUT <- OUT[RecordID %chin% recordID]
  OUT[]
}

.validTrimRange <- function(range, label) {
  range <- .range(unlist(range))
  if (any(!is.finite(range)) || range[1L] < 0 || range[2L] > 1 ||
      range[1L] >= range[2L]) {
    stop(sprintf("%s must be two increasing values in [0, 1].", label),
         call. = FALSE)
  }
  range
}

.trimStages <- function(stages, range) {
  if (is.null(stages) || !length(stages)) {
    return(data.table(RecordID = character(), range.start = numeric(),
                      range.end = numeric()))
  }
  OUT <- rbindlist(lapply(seq_along(stages), function(i) {
    Stage <- stages[[i]]
    Records <- .readCharacterVector(.jsonRequire(Stage, c("recordID")),
                                    sprintf("stages[%s].recordID", i))
    Range <- .validTrimRange(.jsonRequire(Stage, c("range")),
                             sprintf("stages[%s].range", i))
    data.table(RecordID = Records, range.start = Range[1L],
               range.end = Range[2L])
  }), use.names = TRUE)
  DUP <- OUT[, .N, by = RecordID][N > 1L]
  if (nrow(DUP)) {
    stop(sprintf("Duplicate staged trim RecordID: %s",
                 paste(DUP$RecordID, collapse = ", ")), call. = FALSE)
  }
  OUT[!(range.start == range[1L] & range.end == range[2L])]
}

.applyTrimStages <- function(selection, stages, range) {
  selection[, `:=`(range.start = range[1L], range.end = range[2L])]
  if (!nrow(stages)) return(selection[])
  Missing <- setdiff(stages$RecordID, selection$RecordID)
  if (length(Missing)) {
    stop(sprintf("Staged trim RecordID not found in source: %s",
                 paste(Missing, collapse = ", ")), call. = FALSE)
  }
  selection[stages, on = "RecordID",
            `:=`(range.start = i.range.start, range.end = i.range.end)]
  selection[]
}

.sourceRecordFile <- function(path, recordID, file) {
  FILE <- file.path(.workPath(path, recordID), file)
  if (!file.exists(FILE)) {
    FILE <- file.path(path, file)
  }
  if (!file.exists(FILE)) {
    stop(sprintf("Missing source file: %s", FILE), call. = FALSE)
  }
  FILE
}

.copyRecordFile <- function(path.source, path.record, row, file) {
  RecordID <- as.character(row$RecordID[1L])
  FILE <- file.path(.workPath(path.source, RecordID), file)
  if (file.exists(FILE)) {
    file.copy(FILE, file.path(path.record, file), overwrite = TRUE)
    return(invisible(TRUE))
  }

  FILE <- file.path(path.source, file)
  if (!file.exists(FILE)) {
    stop(sprintf("Missing source file: %s", FILE), call. = FALSE)
  }
  DT <- fread(FILE)
  .requireColumns(DT, KEY.record, file)
  for (key in KEY.record) {
    DT <- DT[as.character(get(key)) == as.character(row[[key]][1L])]
  }
  fwrite(DT, file.path(path.record, file))
  invisible(TRUE)
}

.butterOrder <- function(stop, pass, astop, apass) {
  log((astop * sqrt((-1 + apass^2) / (-1 + astop^2))) / apass) /
    log(pass / stop)
}

.butterLP <- function(n, pass, stop, astop, apass) {
  if (pass < 1L || stop <= pass || stop > n) {
    OUT <- rep(1, n)
    if (stop <= n) OUT[seq.int(stop, n)] <- 0
    return(OUT)
  }

  f <- seq_len(n)
  N <- .butterOrder(stop = stop, pass = pass, astop = astop, apass = apass)
  OUT <- 1 / sqrt(1 - (astop^2 - 1) * ((f / stop)^(2 * N)) / astop^2)
  OUT[OUT < astop] <- 0
  OUT[seq_len(pass)] <- 1
  OUT[seq.int(stop, n)] <- 0
  OUT
}

.butterHP <- function(n, stop, pass, astop, apass) {
  if (stop < 1L || pass <= stop || pass > n) {
    OUT <- rep(1, n)
    OUT[seq_len(min(stop, n))] <- 0
    return(OUT)
  }

  f <- seq_len(n)
  N <- .butterOrder(stop = stop, pass = pass, astop = 1 - astop,
                    apass = 1 - apass)
  OUT <- 1 - 1 / sqrt(1 + (-1 + (-1 + astop)^(-2)) *
                        (f / stop)^(2 * N))
  OUT[OUT < astop] <- 0
  OUT[seq_len(stop)] <- 0
  OUT[seq.int(pass, n)] <- 1
  OUT
}

.crossTime <- function(DT, col, p) {
  if (p <= 0) return(min(DT$t))
  if (p >= 1) return(max(DT$t))
  X <- DT[is.finite(get(col)) & get(col) >= p]
  if (!nrow(X)) {
    stop(sprintf("%s does not reach %.4f.", col, p), call. = FALSE)
  }
  X$t[1L]
}

.recordWindow <- function(TSW, id, dir, range, taper, astop, apass) {
  cols <- paste(id, dir, sep = ".")
  Missing <- setdiff(cols, names(TSW))
  if (length(Missing)) {
    stop(sprintf("Source TSW missing trim columns: %s",
                 paste(Missing, collapse = ", ")), call. = FALSE)
  }

  COMP <- rbindlist(lapply(seq_along(cols), function(i) {
    data.table(
      OCID = dir[i],
      t.start = tryCatch(.crossTime(TSW, cols[i], range[1L]),
                         error = function(e) min(TSW$t)),
      t.end = tryCatch(.crossTime(TSW, cols[i], range[2L]),
                       error = function(e) max(TSW$t)))
  }))
  start <- min(COMP$t.start)
  end <- max(COMP$t.end)
  if (!is.finite(start) || !is.finite(end) || start >= end) {
    stop("Invalid trim window.", call. = FALSE)
  }

  t <- TSW$t
  n <- length(t)
  i.start <- which(t >= start)[1L]
  i.end <- tail(which(t <= end), 1L)
  if (is.na(i.start) || is.na(i.end) || i.start >= i.end) {
    stop("Invalid trim indices.", call. = FALSE)
  }

  w <- rep(1, n)
  if (range[1L] > 0) {
    pass <- which(t <= start + taper)
    pass <- if (length(pass)) tail(pass, 1L) else i.start
    w <- w * .butterHP(n, stop = i.start, pass = pass,
                       astop = astop, apass = apass)
  }
  if (range[2L] < 1) {
    pass <- which(t <= end - taper)
    pass <- if (length(pass)) tail(pass, 1L) else i.start
    w <- w * .butterLP(n, pass = pass, stop = i.end,
                       astop = astop, apass = apass)
  }
  w[t < start | t > end] <- 0

  data.table(t = t, Wo = w)[
    , `:=`(t.start = start, t.end = end,
           i.start = i.start, i.end = i.end)]
}

.cumulativeTSL <- function(TSL, units) {
  LIST <- list(
    AI = gmsp::TSL2AI(.x = TSL, units.source = units,
                      units.target = units, output = "long"),
    CAV = gmsp::TSL2CAV(.x = TSL, units.source = units,
                        units.target = units, output = "long"),
    CAV5 = gmsp::TSL2CAV5(.x = TSL, units.source = units,
                          units.target = units, output = "long"))

  OUT <- rbindlist(lapply(names(LIST), function(id) {
    DT <- LIST[[id]]
    setnames(DT, "ratio", "s")
    DT[, ID := id]
    DT[]
  }), use.names = TRUE)
  setcolorder(OUT, c(KEY.record, "OCID", "ID", "t", "s"))
  setorderv(OUT, c(KEY.record, "OCID", "ID", "t"), na.last = TRUE)
  OUT[]
}

.buildTrimmedTS <- function(TSW, row, params) {
  Range <- c(row$range.start[1L], row$range.end[1L])
  W <- .recordWindow(TSW, id = params$ID, dir = params$DIR,
                     range = Range, taper = params$taper,
                     astop = params$astop, apass = params$apass)
  ATcols <- grep("^AT[.]", names(TSW), value = TRUE)
  if (!length(ATcols)) stop("Source TSW has no AT columns.", call. = FALSE)
  OCID <- sub("^AT[.]", "", ATcols)

  AT <- TSW[, c("t", ATcols), with = FALSE]
  setnames(AT, ATcols, OCID)
  AT <- W[AT, on = "t"]
  AT[, (OCID) := lapply(.SD, function(x) x * Wo), .SDcols = OCID]
  AT <- AT[t >= t.start & t <= t.end, c("t", OCID), with = FALSE]
  start <- W$t.start[1L]

  TSL <- gmsp::AT2TS(
    .x = AT,
    units.source = params$units,
    units.target = params$units,
    Fmax = params$Fmax,
    kNyq = params$kNyq,
    output = "TSL",
    isRaw = FALSE,
    audit = FALSE)
  if (start != 0) TSL[, t := t + start]
  for (key in KEY.record) TSL[, (key) := row[[key]][1L]]
  setcolorder(TSL, c(KEY.record, "OCID", "ID", "t", "s"))

  Cumulative <- .cumulativeTSL(TSL, units = params$units)
  TSW.out <- gmsp::TSL2TSW(
    rbindlist(list(TSL, Cumulative), use.names = TRUE),
    ids = c("AT", "VT", "DT", "AI", "CAV", "CAV5"))
  IMW <- gmsp::TSL2IM(.x = TSL, units.source = params$units,
                      units.target = params$units, output = "IMW")
  WINDOW <- unique(W[, .(t.start, t.end, i.start, i.end)])
  WINDOW[, `:=`(
    ID = params$ID,
    range.start = Range[1L],
    range.end = Range[2L])]
  for (key in KEY.record) WINDOW[, (key) := row[[key]][1L]]
  setcolorder(WINDOW, c(KEY.record, "ID", "range.start", "range.end",
                        "t.start", "t.end", "i.start", "i.end"))
  list(TSW = TSW.out[], IMW = IMW[], WINDOW = WINDOW[])
}

.processTrimRecord <- function(row, path, params, override) {
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
              ROW$ProcessID, sprintf("%s.%s.csv", c("TSW", "IMW"), RecordID))
  }
  if (!override && .recordDone(PATH.record, files = params$files) &&
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
    if (uniqueN(TSW$RecordID) > 1L) {
      for (key in KEY.record) {
        TSW <- TSW[as.character(get(key)) == as.character(row[[key]][1L])]
      }
    }

    Step <- "trim"
    .writeRecordStatus(PATH.record, row, "running",
                       list(step = Step, pid = Sys.getpid()))
    OUT <- .buildTrimmedTS(TSW, row = row, params = params)

    Step <- "write"
    .writeRecordStatus(PATH.record, row, "running",
                       list(step = Step, pid = Sys.getpid()))
    fwrite(OUT$TSW, file.path(PATH.record, "TSW.csv"))
    fwrite(OUT$IMW, file.path(PATH.record, "IMW.csv"))
    fwrite(OUT$WINDOW, file.path(PATH.record, "WINDOW.csv"))
    if (is.null(params$PATHS)) {
      .copyRecordFile(params$PATH.source, PATH.record, row, "OCIDMAP.csv")
      .copyRecordFile(params$PATH.source, PATH.record, row, "META.csv")
    } else {
      saveRecordTables(row = ROW, paths = params$PATHS,
                       tables = list(TSW = OUT$TSW, IMW = OUT$IMW),
                       override = override)
    }

    Seconds <- proc.time()[["elapsed"]] - T0
    .writeRecordStatus(PATH.record, row, "done", list(
      n.TSW = nrow(OUT$TSW),
      n.IMW = nrow(OUT$IMW),
      n.WINDOW = nrow(OUT$WINDOW),
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
