#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
source(.scriptHere("plot", "setup.R"))

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/plot/runPlot.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

PATH.source <- .jsonGet(Config, c("path", "source"), default = NULL)
PATH.source <- if (is.null(PATH.source)) NULL else .resolvePath(PATH.source)
PATH.database <- .jsonGet(Config, c("path", "database"), default = NULL)
PATH.records <- .jsonGet(Config, c("path", "records"), default = NULL)
PATH.index <- .jsonGet(Config, c("path", "index"), default = NULL)
PATH.process <- .jsonGet(Config, c("path", "process"), default = NULL)
if (is.null(PATH.database) && !is.null(PATH.records) &&
    identical(basename(path.expand(PATH.records)), "records")) {
  PATH.database <- dirname(path.expand(PATH.records))
}
DBMode <- !is.null(PATH.database) || !is.null(PATH.records)
PATHS.db <- if (DBMode) {
  .recordPaths(list(database = PATH.database, records = PATH.records,
                    index = PATH.index, process = PATH.process))
} else {
  list(records = NULL, index = PATH.index, process = PATH.process)
}
PATH.records <- if (DBMode) PATHS.db$records else NULL
PATH.index <- PATHS.db$index
PATH.process <- PATHS.db$process
PATH.selection <- .jsonGet(Config, c("path", "selection"), default = NULL)
PATH.selection <- if (is.null(PATH.selection)) NULL else .resolvePath(PATH.selection)
PATH.plot <- .jsonGet(Config, c("path", "plot"), default = NULL)
PATH.plot <- if (is.null(PATH.plot)) {
  if (!is.null(PATH.source)) file.path(PATH.source, "plot") else
    .resolvePath(file.path("gmsp", "plot"))
} else {
  .resolvePath(PATH.plot)
}
ProcessID <- as.character(.jsonGet(Config, c("processID"), default = "raw"))
if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
  stop("processID must be one non-empty string.", call. = FALSE)
}
OVERRIDE <- .readScalarLogical(.jsonGet(Config, c("override"), default = FALSE),
                               "override")
PARALLEL <- .readScalarLogical(.jsonGet(Config, c("parallel"), default = FALSE),
                               "parallel")
WORKERS <- parallel::detectCores(logical = TRUE)
if (is.na(WORKERS)) WORKERS <- 1L
WORKERS <- if (PARALLEL) max(1L, min(WORKERS, 4L)) else 1L
TS <- .readScalarLogical(.jsonGet(Config, c("TS"), default = TRUE), "TS")
PS <- .readScalarLogical(.jsonGet(Config, c("PS"), default = TRUE), "PS")
TSI <- .readScalarLogical(.jsonGet(Config, c("TSI"), default = FALSE), "TSI")
PSI <- .readScalarLogical(.jsonGet(Config, c("PSI"), default = FALSE), "PSI")
ID <- list(
  TS = .readCharacterVector(.jsonGet(Config, c("ID", "TS"),
                                     default = c("AT", "VT", "DT")),
                            "ID/TS"),
  PS = .readCharacterVector(.jsonGet(Config, c("ID", "PS"),
                                     default = c("PSA", "PSV", "SD")),
                            "ID/PS"),
  TSI = .readCharacterVector(.jsonGet(Config, c("ID", "TSI"),
                                      default = c("AT")),
                             "ID/TSI"),
  PSI = .readCharacterVector(.jsonGet(Config, c("ID", "PSI"),
                                      default = c("PSA")),
                             "ID/PSI"))
DIR <- list(
  TS = .readCharacterVector(.jsonGet(Config, c("DIR", "TS"),
                                     default = c("H1", "H2", "UP")),
                            "DIR/TS"),
  PS = .readCharacterVector(.jsonGet(Config, c("DIR", "PS"),
                                     default = c("H1", "H2", "UP", "D50",
                                                 "D100")),
                            "DIR/PS"),
  TSI = .readCharacterVector(.jsonGet(Config, c("DIR", "TSI"),
                                      default = c("H1", "H2", "UP")),
                             "DIR/TSI"),
  PSI = .readCharacterVector(.jsonGet(Config, c("DIR", "PSI"),
                                      default = c("H1", "H2", "UP")),
                             "DIR/PSI"))
Keep <- .readCharacterVector(.jsonGet(Config, c("recordID"), default = NULL),
                             "recordID", allowNull = TRUE)
Line <- .readLine(.jsonGet(Config, c("line"), default = NULL))

if (is.null(PATH.records) && is.null(PATH.source)) {
  stop("Either path.records or path.source is required.", call. = FALSE)
}
if (!is.null(PATH.source) && !dir.exists(PATH.source)) {
  stop(sprintf("path.source does not exist: %s", PATH.source), call. = FALSE)
}
if (!OVERRIDE && dir.exists(PATH.plot) && length(list.files(PATH.plot))) {
  stop(sprintf("%s already exists. Set override=true to rebuild.", PATH.plot),
       call. = FALSE)
}
.ensureDir(PATH.plot)
invisible(file.copy(PATH.config, file.path(PATH.plot, "runPlot.json"),
                    overwrite = TRUE))

TSW <- NULL
PSW <- NULL
RawDB <- !is.null(PATH.records) && identical(ProcessID, "raw")

if (!is.null(PATH.records)) {
  PATHS <- PATHS.db
  if (RawDB && (TSI || PS || PSI)) {
    stop("Raw database plots support TS only.", call. = FALSE)
  }
  Outputs <- if (RawDB) {
    if (TS) "AT" else character()
  } else {
    character()
  }
  if (!RawDB && (TS || TSI)) Outputs <- c(Outputs, "TSW")
  if (!RawDB && (PS || PSI)) Outputs <- c(Outputs, "PSW")
  Outputs <- unique(Outputs)
  Missing <- setdiff(Outputs, .recordDeclaredOutputs(ProcessID, PATHS.db))
  if (length(Missing)) {
    stop(sprintf("ProcessID %s does not declare requested plot outputs: %s",
                 ProcessID, paste(Missing, collapse = ", ")),
         call. = FALSE)
  }
  Package <- getRecords(
    keys = .recordReadSelectionKeys(path = PATH.selection,
                                    processID = ProcessID,
                                    recordID = Keep,
                                    label = "selection",
                                    allowAll = FALSE),
    paths = PATHS,
    output = Outputs
  )
  TSW <- if (RawDB) .rawATAsTS(Package$AT) else Package$TSW
  PSW <- if (RawDB) NULL else Package$PSW
} else {
  FILE <- .recordSourceDataFile(PATH.source, "TSW.csv")
  if ((TS || TSI) && file.exists(FILE)) {
    TSW <- .recordFread(FILE)
  } else if (TS || TSI) {
    TSW <- .readTSFiles(PATH.source)
  }
  FILE <- .recordSourceDataFile(PATH.source, "PSW.csv")
  if ((PS || PSI) && file.exists(FILE)) {
    PSW <- .recordFread(FILE)
  }
}
if (is.null(TSW) && is.null(PSW)) {
  stop(sprintf("No plottable TSW or PSW for %s",
               if (is.null(PATH.source)) ProcessID else PATH.source),
       call. = FALSE)
}

Records <- sort(unique(c(if (!is.null(TSW)) TSW$RecordID,
                         if (!is.null(PSW)) PSW$RecordID)))
if (length(Keep)) Records <- intersect(Records, Keep)
if (!length(Records)) stop("No records selected for plots.", call. = FALSE)

.requirePlotPackages()
Units <- if (is.null(PATH.source)) {
  if (is.null(PATH.process)) {
    "mm"
  } else {
    JSON <- .recordReadProcessJSON(processID = ProcessID, paths = PATHS.db,
                                   required = FALSE)
    if (!is.null(JSON)) {
      as.character(.jsonGet(JSON, c("params", "unitsTarget"), default = "mm"))
    } else {
      "mm"
    }
  }
} else .targetUnits(PATH.source)
Target <- .readPSTarget(.jsonGet(Config, c("target"), default = NULL), Units)
.log("plot start: source=%s out=%s records=%s parallel=%s workers=%s",
     PATH.source, PATH.plot, length(Records), PARALLEL, WORKERS)
N.ts <- if (TS) {
  plotTS(TSW, Records, PATH.plot, Units, ID$TS, DIR$TS, Line$TS,
         parallel = PARALLEL, workers = WORKERS)
} else 0L
N.ps <- if (PS) {
  plotPS(PSW, Records, PATH.plot, Units, ID$PS, DIR$PS, Line$PS,
         target = Target, parallel = PARALLEL, workers = WORKERS)
} else 0L
N.tsi <- if (TSI) {
  plotTSI(TSW, Records, PATH.plot, Units, ID$TSI, DIR$TSI, Line$TSI,
          parallel = PARALLEL, workers = WORKERS)
} else 0L
N.psi <- if (PSI) {
  plotPSI(PSW, Records, PATH.plot, Units, ID$PSI, DIR$PSI, Line$PSI,
          parallel = PARALLEL, workers = WORKERS)
} else 0L
message(sprintf("Wrote %s TS, %s PS, %s TSI, and %s PSI plots to %s",
                N.ts, N.ps, N.tsi, N.psi, PATH.plot))
