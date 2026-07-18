#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))

source(.scriptHere("gmsp", "global.R"))
source(.scriptHere("gmsp", "setup.R"))

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/gmsp/runGMSP.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

ID.selection <- unique(as.character(unlist(
  .jsonRequire(Config, c("selection"), PATH.config))))
ID.process <- as.character(.jsonRequire(Config, c("process"), PATH.config))
PATH.database <- .jsonGet(Config, c("path", "database"), default = NULL)
PATH.records <- .jsonGet(Config, c("path", "records"), default = NULL)
PATH.process <- .jsonGet(Config, c("path", "process"), default = NULL)
if (is.null(PATH.database) && !is.null(PATH.records) &&
    identical(basename(path.expand(PATH.records)), "records")) {
  PATH.database <- dirname(path.expand(PATH.records))
}
PATHS <- .recordPaths(list(database = PATH.database,
                           records = PATH.records,
                           process = PATH.process),
                      need = c("records", "index", "process"))
PATH.selection.root <- .resolvePath(.jsonGet(Config, c("path", "selection"),
                                             default = "gmsp/selection"))
PATH.stage.root <- .resolvePath(.jsonGet(Config, c("path", "stage"),
                                         default = "gmsp/records"))

MaxRecords <- .jsonGet(Config, c("maxRecords"), default = NULL)
MaxRecords <- if (is.null(MaxRecords)) Inf else as.numeric(MaxRecords)
MaxF25Rows <- .jsonGet(Config, c("maxF25Rows"), default = NULL)
MaxF25Rows <- if (is.null(MaxF25Rows)) Inf else as.numeric(MaxF25Rows)

Fmax <- as.numeric(.jsonGet(Config, c("fmax"), default = 25))
kNyq <- as.numeric(.jsonGet(Config, c("kNyq"), default = 4))
flatZeros <- as.logical(.jsonGet(Config, c("flatZeros"), default = FALSE))
trimZeros <- as.logical(.jsonGet(Config, c("trimZeros"), default = FALSE))
Astop0 <- as.numeric(.jsonGet(Config, c("Astop0"), default = 1e-3))
Units.source <- as.character(.jsonGet(Config, c("unitsSource"),
                                      default = "mm"))
Units.target <- as.character(.jsonGet(Config, c("unitsTarget"),
                                      default = "mm"))

PARALLEL <- as.logical(.jsonGet(Config, c("parallel"), default = TRUE))
OVERRIDE <- as.logical(.jsonGet(Config, c("override"), default = FALSE))
WRITE.stage <- .readScalarLogical(.jsonGet(Config, c("writeStageTables"),
                                           default = FALSE),
                                  "writeStageTables")
WORKERS <- parallel::detectCores(logical = TRUE)
if (is.na(WORKERS)) WORKERS <- 1L
WORKERS <- if (PARALLEL) max(1L, WORKERS) else 1L
LOG.every <- as.numeric(.jsonGet(Config, c("logEvery"), default = 30))

Params <- list(
  PATH.records = PATHS$records,
  PATHS = PATHS,
  ProcessID = ID.process,
  Fmax = Fmax,
  kNyq = kNyq,
  flatZeros = flatZeros,
  trimZeros = trimZeros,
  Astop0 = Astop0,
  Units.source = Units.source,
  Units.target = Units.target,
  MaxF25Rows = MaxF25Rows)

.log("runGMSP start: selections=%s process=%s parallel=%s workers=%s MaxRecords=%s MaxF25Rows=%s flatZeros=%s trimZeros=%s Astop0=%s",
     paste(ID.selection, collapse = ","), ID.process, PARALLEL, WORKERS,
     if (is.finite(MaxRecords)) as.character(MaxRecords) else "all",
     if (is.finite(MaxF25Rows)) as.character(MaxF25Rows) else "all",
     flatZeros, trimZeros, Astop0)

for (id in ID.selection) {
  PATH.selection <- file.path(PATH.selection.root, id, "selection.csv")
  PATH.gmsp <- file.path(PATH.stage.root, paste(id, ID.process, sep = "."))

  if (!OVERRIDE && dir.exists(PATH.gmsp) &&
      (any(file.exists(file.path(PATH.gmsp, c("IMF.csv", "IMFF.csv")))) ||
       length(list.files(.workRoot(PATH.gmsp),
                         pattern = "^(IMF|IMFF)[.]csv$",
                         recursive = TRUE)))) {
    stop(sprintf(paste("%s contains IMF artifacts from the old mixed stage.",
                       "Rebuild F25 with OVERRIDE <- TRUE, or write to a new",
                       "process id."), PATH.gmsp), call. = FALSE)
  }
  if (!OVERRIDE && dir.exists(PATH.gmsp) &&
      !length(list.files(.workRoot(PATH.gmsp),
                         pattern = "^status[.]json$", recursive = TRUE)) &&
      any(file.exists(file.path(PATH.gmsp, c("TSW.csv", "IMW.csv"))))) {
    stop(sprintf(paste("%s has aggregate CSV files but no per-record",
                       "checkpoints. Set OVERRIDE <- TRUE to rebuild with",
                       "the checkpointed layout."), PATH.gmsp),
         call. = FALSE)
  }
  if (OVERRIDE && dir.exists(PATH.gmsp)) {
    .unlinkGeneratedDir(PATH.gmsp, PATH.stage.root)
  }
  .ensureDir(.workRoot(PATH.gmsp))
  .log("Processing %s into %s", id, PATH.gmsp)

  FILE <- .resolvePath(PATH.selection)
  DT <- .recordFread(FILE)
  .requireColumns(DT, KEY.record, "selection")
  DT <- unique(DT, by = KEY.record)
  DT[, ProcessID := ID.process]
  if (is.finite(MaxRecords) && nrow(DT) > MaxRecords) {
    DT <- DT[seq_len(MaxRecords)]
  }
  .log("Selection %s: records=%s", id, nrow(DT))
  if (!nrow(DT)) stop(sprintf("Selection %s has no records.", id),
                      call. = FALSE)

  if (PARALLEL && WORKERS > 1L) {
    .runRecordsParallel(DT, path = PATH.gmsp, params = Params,
                        override = OVERRIDE, files = FILES.TS,
                        processRecord = .processGMSPRecord,
                        workers = WORKERS, log.every = LOG.every,
                        label = "F25")
  } else {
    .runRecordsSequential(DT, path = PATH.gmsp, params = Params,
                          override = OVERRIDE, files = FILES.TS,
                          processRecord = .processGMSPRecord,
                          label = "F25")
  }

  Manifest <- .buildManifest(PATH.gmsp, DT)
  if (WRITE.stage) {
    .writeAggregateTables(PATH.gmsp, Manifest, files = FILES.TS)
  }

  Done <- Manifest[status == "done"]
  if (!nrow(Done)) stop("No completed GMSP records to index.", call. = FALSE)
  IMW <- .recordReadMany(file.path(Done$path, "IMW.csv"),
                         label = "GMSP IMW checkpoint")
  ComponentMapTable <- .recordReadMany(file.path(Done$path, "OCIDMAP.csv"),
                                       label = "GMSP component map checkpoint")
  ComponentMapTable[, ProcessID := ID.process]
  setcolorder(ComponentMapTable,
              c("ProcessID", KEY.record, "OCID", "DIR", "theta",
                setdiff(names(ComponentMapTable),
                        c("ProcessID", KEY.record, "OCID", "DIR",
                          "theta"))))
  writeProcessTables(
    index = DT,
    intensity = IMW,
    paths = PATHS,
    processID = ID.process,
    record = c("TSW", "IMW"),
    extra = list(ComponentMapTable = ComponentMapTable),
    params = list(fmax = Fmax, kNyq = kNyq, flatZeros = flatZeros,
                  trimZeros = trimZeros, Astop0 = Astop0,
                  unitsSource = Units.source, unitsTarget = Units.target),
    override = TRUE)

  jsonlite::write_json(Config,
    file.path(PATH.gmsp, sprintf("%s.json", ID.process)),
    auto_unbox = TRUE, pretty = TRUE)
  message(sprintf("Published %s GMSP records for %s to %s",
                  Manifest[, uniqueN(RecordID)], ID.process, PATHS$records))
}
