# scripts/plot/setup.R -- plot stage helpers.

.requirePlotPackages <- function() {
  Packages <- c("NGR", "htmlwidgets")
  OK <- vapply(Packages, requireNamespace, logical(1L), quietly = TRUE)
  if (!all(OK)) {
    stop(sprintf("Missing required plot packages: %s",
                 paste(Packages[!OK], collapse = ", ")),
         call. = FALSE)
  }
  invisible(NULL)
}

.sourceJson <- function(path) {
  FILES <- c(
    list.files(path, pattern = "[.]json$", full.names = TRUE),
    list.files(file.path(path, "metadata"), pattern = "[.]json$",
               full.names = TRUE)
  )
  FILES <- FILES[!basename(FILES) %chin% c("runGMSP.json", "runExport.json",
                                           "runBuildPSA.json", "runPlot.json")]
  if (length(FILES) == 1L) return(FILES)
  NA_character_
}

.targetUnits <- function(path) {
  FILE <- .sourceJson(path)
  if (is.na(FILE)) return("mm")
  JSON <- .readJson(FILE, simplifyVector = FALSE)
  .jsonGet(JSON, c("unitsTarget"),
           default = .jsonGet(JSON, c("Units.target"),
                              default = .jsonGet(JSON, c("TargetUnits"),
                                                 default = "mm")))
}

.tsUnitLabel <- function(ID, units) {
  if (ID == "AT") return(paste0(units, "/s2"))
  if (ID == "VT") return(paste0(units, "/s"))
  if (ID %chin% c("AI", "CAV", "CAV5")) return("-")
  units
}

.tsDisplayID <- function(ID) {
  if (ID == "AI") return("IA")
  ID
}

.tsAxisLabel <- function(ID, DIR, units) {
  Label <- .tsDisplayID(ID)
  if (ID %chin% c("AI", "CAV", "CAV5")) {
    return(sprintf("%s(%s) / max(%s(%s)) [-]", Label, DIR, Label, DIR))
  }
  sprintf("%s(%s) [%s]", Label, DIR, .tsUnitLabel(ID, units))
}

.psUnitLabel <- function(ID, units) {
  if (ID == "PSA") return(paste0(units, "/s2"))
  if (ID == "PSV") return(paste0(units, "/s"))
  units
}

.convertPSAUnits <- function(x, from, to) {
  From <- tolower(as.character(from))
  To <- tolower(as.character(to))
  if (identical(From, To)) return(x)
  if (From == "g" && To %chin% c("mm/s2", "mm/s^2")) return(x * 9806.65)
  if (From %chin% c("mm/s2", "mm/s^2") && To == "g") return(x / 9806.65)
  stop(sprintf("Unsupported target unit conversion: %s -> %s", from, to),
       call. = FALSE)
}

.readTargetTable <- function(path) {
  FILE <- .resolvePath(path)
  if (!file.exists(FILE)) stop(sprintf("Missing target table: %s", FILE),
                               call. = FALSE)
  if (grepl("[.]rds$", FILE, ignore.case = TRUE)) {
    return(as.data.table(readRDS(FILE)))
  }
  if (grepl("[.]csv$", FILE, ignore.case = TRUE)) return(.recordFread(FILE))
  stop(sprintf("Unsupported target table extension: %s", FILE), call. = FALSE)
}

.readPSTarget <- function(config, units) {
  if (is.null(config)) return(NULL)
  if (!is.list(config)) stop("target must be a JSON object.", call. = FALSE)

  Target <- .readTargetTable(.jsonRequire(config, c("path")))
  Value <- as.character(.jsonGet(config, c("value"), default = "SaF"))
  .requireColumns(Target, c("ID", "TR", "Vs30", "p", "Tn", Value), "target")

  TargetID <- as.character(.jsonRequire(config, c("ID")))
  TargetTR <- as.numeric(.jsonRequire(config, c("TR")))
  TargetVs30 <- as.numeric(.jsonRequire(config, c("Vs30")))
  TargetP <- as.character(.jsonGet(config, c("p"), default = "mean"))
  SiteID <- as.character(.jsonGet(config, c("siteID"), default = ""))

  OUT <- Target[
    ID %chin% TargetID &
      TR %in% TargetTR &
      Vs30 %in% TargetVs30 &
      p %chin% TargetP]
  if (nzchar(SiteID) && "siteID" %chin% names(OUT)) {
    OUT <- OUT[siteID %chin% SiteID]
  }
  if (!nrow(OUT)) stop("target filters produced zero rows.", call. = FALSE)

  Keys <- intersect(c("ID", "TR", "Vs30", "p", "siteID"), names(OUT))
  if (nrow(unique(OUT[, ..Keys])) != 1L) {
    stop("target filters must identify one curve.", call. = FALSE)
  }

  From <- as.character(.jsonGet(config, c("units"), default = "g"))
  To <- .psUnitLabel("PSA", units)
  OUT <- OUT[order(Tn), .(X = Tn, Y = get(Value))]
  OUT[, Y := .convertPSAUnits(Y, from = From, to = To)]
  OUT[is.finite(X) & is.finite(Y)]
}

.hideSeries <- function(plot) {
  plot$x$hc_opts$series <- lapply(plot$x$hc_opts$series, function(Series) {
    Series$visible <- FALSE
    Series
  })
  plot
}

.imfLevels <- function(x) {
  Values <- unique(as.character(x))
  IMFs <- grep("^IMF[0-9]+$", Values, value = TRUE)
  IMFs <- IMFs[order(as.integer(sub("^IMF", "", IMFs)))]
  c(intersect("signal", Values), IMFs, intersect("residue", Values),
    setdiff(Values, c("signal", IMFs, "residue")))
}

.lineDefaults <- function() {
  list(
    TS = list(child = 0.75, master = 0.75),
    PS = list(child = 1.5, master = 2.5),
    TSI = list(child = 0.75, master = 1.5),
    PSI = list(child = 1.5, master = 2.5))
}

.readLine <- function(x) {
  Line <- .lineDefaults()
  if (is.null(x)) return(Line)
  if (!is.list(x)) stop("line must be a JSON object.", call. = FALSE)

  Families <- names(Line)
  Roles <- c("child", "master")
  Extra <- setdiff(names(x), Families)
  if (length(Extra)) {
    stop(sprintf("Unknown line families: %s", paste(Extra, collapse = ", ")),
         call. = FALSE)
  }

  for (Family in intersect(names(x), Families)) {
    if (!is.list(x[[Family]])) {
      stop(sprintf("line.%s must be a JSON object.", Family), call. = FALSE)
    }
    Extra <- setdiff(names(x[[Family]]), Roles)
    if (length(Extra)) {
      stop(sprintf("Unknown line.%s roles: %s",
                   Family, paste(Extra, collapse = ", ")),
           call. = FALSE)
    }
    for (Role in intersect(names(x[[Family]]), Roles)) {
      Value <- .readScalarNumber(x[[Family]][[Role]],
                                 sprintf("line.%s.%s", Family, Role))
      if (!is.finite(Value) || Value <= 0) {
        stop(sprintf("line.%s.%s must be positive.", Family, Role),
             call. = FALSE)
      }
      Line[[Family]][[Role]] <- Value
    }
  }
  Line
}

.saveLinePlot <- function(DATA, path, file, xLegend, yLegend,
                          lineType = "line", xLog = FALSE,
                          hide = TRUE) {
  Plot <- NGR::buildPlot(
    line.type = lineType, plot.height = 1000,
    legend.show = TRUE, legend.layout = "vertical",
    xAxis.log = xLog, xAxis.legend = xLegend,
    yAxis.legend = yLegend,
    plot.theme = NGR::hc_theme_538_gridlines(),
    data.lines = DATA)
  Plot$x$hc_opts$xAxis$gridLineWidth <- 0.3
  Plot$x$hc_opts$xAxis$gridLineColor <- "#e8e8e8"
  Plot$x$hc_opts$xAxis$minorGridLineWidth <- 0.15
  Plot$x$hc_opts$xAxis$minorGridLineColor <- "#f3f3f3"
  Plot$x$hc_opts$yAxis$gridLineWidth <- 0.3
  Plot$x$hc_opts$yAxis$gridLineColor <- "#e8e8e8"
  Plot$x$hc_opts$yAxis$minorGridLineWidth <- 0.15
  Plot$x$hc_opts$yAxis$minorGridLineColor <- "#f3f3f3"
  if (hide) Plot <- .hideSeries(Plot)
  FILE <- file.path(path, file)
  htmlwidgets::saveWidget(Plot, FILE, selfcontained = TRUE)
  DIR <- sub("[.]html$", "_files", FILE)
  if (dir.exists(DIR)) unlink(DIR, recursive = TRUE)
  invisible(NULL)
}

.plotJobs <- function(jobs, worker, parallel = FALSE, workers = 1L) {
  n <- nrow(jobs)
  if (!n) return(0L)
  if (!parallel || workers <= 1L || n == 1L) {
    LIST <- lapply(seq_len(n), worker)
  } else {
    workers <- max(1L, min(as.integer(workers), n))
    LIST <- parallel::mclapply(seq_len(n), worker, mc.cores = workers)
  }
  BAD <- vapply(LIST, inherits, logical(1L), "try-error")
  if (any(BAD)) stop(as.character(LIST[[which(BAD)[1L]]]),
                     call. = FALSE)
  sum(as.integer(unlist(LIST, use.names = FALSE)), na.rm = TRUE)
}

.readTSFiles <- function(path) {
  FILE <- file.path(path, "files.csv")
  if (!file.exists(FILE)) return(NULL)
  INDEX <- .recordFread(FILE)
  .requireColumns(INDEX, c("RecordID", "ID", "DIR", "FILE"), "files.csv")
  INDEX[, FILE := ifelse(grepl("^(/|~)", FILE), path.expand(FILE),
                         file.path(path, basename(FILE)))]
  Missing <- INDEX[!file.exists(FILE), FILE]
  if (length(Missing)) {
    stop(sprintf("Missing plotted source files:\n%s",
                 paste(Missing, collapse = "\n")), call. = FALSE)
  }
  OUT <- rbindlist(lapply(seq_len(nrow(INDEX)), function(i) {
    DT <- .recordFread(INDEX$FILE[i])
    .requireColumns(DT, c("t", "s"), basename(INDEX$FILE[i]))
    data.table(RecordID = INDEX$RecordID[i], ID = INDEX$ID[i],
               OCID = INDEX$DIR[i], t = DT$t, s = DT$s)
  }), use.names = TRUE)
  setorder(OUT, RecordID, ID, OCID, t)
  OUT[]
}

.rawATAsTS <- function(AT) {
  .requireColumns(AT, c("RecordID", "t"), "raw AT")
  COLS <- intersect(c("H1", "H2", "UP"), names(AT))
  if (!length(COLS)) stop("raw AT has no component columns.", call. = FALSE)
  KEYS <- intersect(c(.record.KEY.ALL, "t"), names(AT))
  OUT <- rbindlist(lapply(COLS, function(s) {
    DT <- copy(AT[, ..KEYS])
    DT[, `:=`(ID = "AT", OCID = s, s = AT[[s]])]
    DT
  }), use.names = TRUE)
  setcolorder(OUT, c(intersect(.record.KEY.ALL, names(OUT)),
                     "ID", "OCID", "t", "s"))
  setorder(OUT, RecordID, ID, OCID, t)
  OUT[]
}

plotTS <- function(TSW, records, path, units, ids, dirs, line,
                   parallel = FALSE, workers = 1L) {
  if (is.null(TSW)) return(0L)
  if ("IMF" %chin% names(TSW)) {
    stop("plotTS consumes base TSW without IMF metadata; use TSI/plotTSI.",
         call. = FALSE)
  }
  if (all(c("RecordID", "ID", "OCID", "t", "s") %chin% names(TSW))) {
    Jobs <- unique(TSW[ID %chin% ids & OCID %chin% dirs,
                       .(ID, DIR = OCID)])[order(ID, DIR)]
    Worker <- function(i) {
      .log("TS plot %s/%s %s.%s", i, nrow(Jobs), Jobs$ID[i], Jobs$DIR[i])
      DT <- TSW[RecordID %chin% records & ID == Jobs$ID[i] &
                  OCID == Jobs$DIR[i]]
      Y <- DT$s
      if (!is.numeric(Y)) {
        if (!all(is.na(Y))) {
          stop(sprintf("TSW column is not numeric: %s.%s",
                       Jobs$ID[i], Jobs$DIR[i]), call. = FALSE)
        }
        Y <- as.numeric(Y)
      }
      DATA <- DT[, .(ID = RecordID, X = t, Y = Y,
                     size = line$child)][is.finite(X) & is.finite(Y)][order(X)]
      if (!nrow(DATA)) return(0L)
      .saveLinePlot(
        DATA = DATA,
        path = path,
        file = sprintf("%s-%s.html", Jobs$ID[i], Jobs$DIR[i]),
        xLegend = "t [s]",
        yLegend = .tsAxisLabel(Jobs$ID[i], Jobs$DIR[i], units))
      1L
    }
    return(.plotJobs(Jobs, Worker, parallel = parallel, workers = workers))
  }

  COLS <- CJ(ID = ids, DIR = dirs)[, paste(ID, DIR, sep = ".")]
  COLS <- intersect(COLS, names(TSW))
  if (!length(COLS)) return(0L)

  OUT <- tstrsplit(COLS, ".", fixed = TRUE)
  Jobs <- data.table(ID = OUT[[1L]], DIR = OUT[[2L]], Column = COLS)
  Worker <- function(i) {
    .log("TS plot %s/%s %s", i, nrow(Jobs), Jobs$Column[i])
    DT <- TSW[RecordID %chin% records]
    Y <- DT[[Jobs$Column[i]]]
    if (!is.numeric(Y)) {
      if (!all(is.na(Y))) {
        stop(sprintf("TSW column is not numeric: %s",
                     Jobs$Column[i]), call. = FALSE)
      }
      Y <- as.numeric(Y)
    }
    DATA <- DT[, .(ID = RecordID, X = t, Y = Y,
                   size = line$child)
    ][is.finite(X) & is.finite(Y)][order(X)]
    if (!nrow(DATA)) return(0L)
    .saveLinePlot(
      DATA = DATA,
      path = path,
      file = sprintf("%s-%s.html", Jobs$ID[i], Jobs$DIR[i]),
      xLegend = "t [s]",
      yLegend = .tsAxisLabel(Jobs$ID[i], Jobs$DIR[i], units))
    1L
  }
  .plotJobs(Jobs, Worker, parallel = parallel, workers = workers)
}

plotPS <- function(PSW, records, path, units, ids, dirs, line, target = NULL,
                   parallel = FALSE, workers = 1L) {
  if (is.null(PSW)) return(0L)
  if ("IMF" %chin% names(PSW)) {
    stop("plotPS consumes base PSW without IMF metadata; use PSI/plotPSI.",
         call. = FALSE)
  }
  COLS <- CJ(ID = ids, DIR = dirs)[, paste(ID, DIR, sep = ".")]
  COLS <- intersect(COLS, names(PSW))
  if (!length(COLS)) return(0L)

  OUT <- tstrsplit(COLS, ".", fixed = TRUE)
  Jobs <- data.table(ID = OUT[[1L]], DIR = OUT[[2L]], Column = COLS)
  if ("xi" %chin% names(PSW)) PSW <- PSW[xi == sort(unique(xi))[1L]]
  PSW <- PSW[RecordID %chin% records]
  Worker <- function(i) {
    Column <- Jobs$Column[i]
    .log("PS plot %s/%s %s", i, nrow(Jobs), Column)
    DATA <- rbindlist(list(
      PSW[, .(ID = RecordID, X = Tn, Y = get(Column), size = line$child)],
      PSW[, .(ID = "mean", Y = mean(get(Column), na.rm = TRUE),
              size = line$master),
          by = .(X = Tn)],
      if (!is.null(target) && Jobs$ID[i] == "PSA") {
        target[, .(ID = "target", X, Y, size = line$master)]
      }
    ), use.names = TRUE)[is.finite(X) & is.finite(Y)][order(X)]
    if (!nrow(DATA)) return(0L)
    .saveLinePlot(
      DATA = DATA,
      path = path,
      file = sprintf("%s-%s.html", Jobs$ID[i], Jobs$DIR[i]),
      xLegend = "Tn [s]",
      yLegend = sprintf("%s(%s) [%s]", Jobs$ID[i], Jobs$DIR[i],
                        .psUnitLabel(Jobs$ID[i], units)),
      lineType = "spline",
      xLog = TRUE)
    1L
  }
  .plotJobs(Jobs, Worker, parallel = parallel, workers = workers)
}

plotTSI <- function(TSW, records, path, units, ids, dirs, line,
                    parallel = FALSE, workers = 1L) {
  if (is.null(TSW)) return(0L)
  .requireColumns(TSW, c("RecordID", "IMF", "t"), "TSI TSW")
  COLS <- CJ(ID = ids, DIR = dirs)[, paste(ID, DIR, sep = ".")]
  COLS <- intersect(COLS, names(TSW))
  if (!length(COLS)) return(0L)

  OUT <- tstrsplit(COLS, ".", fixed = TRUE)
  Jobs <- data.table(ID = OUT[[1L]], DIR = OUT[[2L]], Column = COLS)
  Records <- intersect(records, sort(unique(TSW$RecordID)))
  if (!length(Records)) return(0L)
  AUX <- Jobs
  Jobs <- rbindlist(lapply(Records, function(Record) {
    DT <- copy(AUX)
    DT[, RecordID := Record]
    DT
  }), use.names = TRUE)
  setcolorder(Jobs, c("RecordID", "ID", "DIR", "Column"))
  Worker <- function(i) {
    DIR <- file.path(path, "TSI", sprintf("%s-%s", Jobs$ID[i], Jobs$DIR[i]))
    .ensureDir(DIR)
    Record <- Jobs$RecordID[i]
    .log("TSI plot %s.%s %s", Jobs$ID[i], Jobs$DIR[i], Record)
    DATA <- TSW[get("RecordID") == Record,
                .(ID = as.character(IMF), X = t,
                  Y = get(Jobs$Column[i]),
                  size = fifelse(as.character(IMF) == "signal",
                                 line$master, line$child))]
    DATA <- DATA[is.finite(X) & is.finite(Y)]
    if (!nrow(DATA)) return(0L)
    DATA[, ID := factor(ID, levels = .imfLevels(ID))]
    setorder(DATA, ID, X)
    DATA[, ID := as.character(ID)]
    .saveLinePlot(
      DATA = DATA,
      path = DIR,
      file = sprintf("%s.%s.%s.IMF.html", Record, Jobs$ID[i],
                     Jobs$DIR[i]),
      xLegend = "t [s]",
      yLegend = .tsAxisLabel(Jobs$ID[i], Jobs$DIR[i], units),
      hide = FALSE)
    1L
  }
  .plotJobs(Jobs, Worker, parallel = parallel, workers = workers)
}

plotPSI <- function(PSW, records, path, units, ids, dirs, line,
                    parallel = FALSE, workers = 1L) {
  if (is.null(PSW)) return(0L)
  .requireColumns(PSW, c("RecordID", "IMF", "Tn"), "PSI PSW")
  COLS <- CJ(ID = ids, DIR = dirs)[, paste(ID, DIR, sep = ".")]
  COLS <- intersect(COLS, names(PSW))
  if (!length(COLS)) return(0L)

  OUT <- tstrsplit(COLS, ".", fixed = TRUE)
  Jobs <- data.table(ID = OUT[[1L]], DIR = OUT[[2L]], Column = COLS)
  if ("xi" %chin% names(PSW)) PSW <- PSW[xi == sort(unique(xi))[1L]]
  Records <- intersect(records, sort(unique(PSW$RecordID)))
  if (!length(Records)) return(0L)
  AUX <- Jobs
  Jobs <- rbindlist(lapply(Records, function(Record) {
    DT <- copy(AUX)
    DT[, RecordID := Record]
    DT
  }), use.names = TRUE)
  setcolorder(Jobs, c("RecordID", "ID", "DIR", "Column"))
  Worker <- function(i) {
    DIR <- file.path(path, "PSI", sprintf("%s-%s", Jobs$ID[i], Jobs$DIR[i]))
    .ensureDir(DIR)
    Record <- Jobs$RecordID[i]
    .log("PSI plot %s.%s %s", Jobs$ID[i], Jobs$DIR[i], Record)
    DATA <- PSW[get("RecordID") == Record,
                .(ID = as.character(IMF), X = Tn,
                  Y = get(Jobs$Column[i]),
                  size = fifelse(as.character(IMF) == "signal",
                                 line$master, line$child))]
    DATA <- DATA[is.finite(X) & is.finite(Y)]
    if (!nrow(DATA)) return(0L)
    DATA[, ID := factor(ID, levels = .imfLevels(ID))]
    setorder(DATA, ID, X)
    DATA[, ID := as.character(ID)]
    .saveLinePlot(
      DATA = DATA,
      path = DIR,
      file = sprintf("%s.%s.%s.IMF.html", Record, Jobs$ID[i],
                     Jobs$DIR[i]),
      xLegend = "Tn [s]",
      yLegend = sprintf("%s(%s) [%s]", Jobs$ID[i], Jobs$DIR[i],
                        .psUnitLabel(Jobs$ID[i], units)),
      lineType = "spline",
      xLog = TRUE,
      hide = FALSE)
    1L
  }
  .plotJobs(Jobs, Worker, parallel = parallel, workers = workers)
}
