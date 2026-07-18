#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))
source(.scriptHere("gmsp", "global.R"))
source(.scriptHere("trim", "setup.R"))

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/trim/runTrim.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

ProcessID <- .jsonGet(Config, c("processID"), default = NULL)
ProcessID <- if (is.null(ProcessID)) NULL else as.character(ProcessID)
DB.mode <- !is.null(ProcessID) || .jsonHas(Config, c("path", "database"))
if (DB.mode) {
  if (is.null(ProcessID) || length(ProcessID) != 1L || is.na(ProcessID) ||
      !nzchar(ProcessID)) {
    stop("processID must be one non-empty string in database mode.",
         call. = FALSE)
  }
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
  PATH.stage.root <- .resolvePath(.jsonGet(Config, c("path", "stage"),
                                           default = "gmsp/stage"))
  PATH.target <- file.path(PATH.stage.root, sprintf("%s.trim", ProcessID))
  SourceJson <- .recordReadProcessJSON(processID = ProcessID, paths = PATHS,
                                       required = FALSE)
  if (is.null(SourceJson)) SourceJson <- list()
} else {
  PATH.source <- .resolvePath(.jsonRequire(Config, c("path", "source"),
                                           PATH.config))
  PATH.target <- .resolvePath(.jsonRequire(Config, c("path", "target"),
                                           PATH.config))
  FILE.source <- .sourceJson(PATH.source)
  SourceJson <- if (is.na(FILE.source)) {
    list()
  } else {
    .readJson(FILE.source, simplifyVector = FALSE)
  }
}
ID <- as.character(.jsonRequire(Config, c("ID"), PATH.config))
DIR <- .readCharacterVector(.jsonRequire(Config, c("DIR"), PATH.config),
                            "DIR")
Range <- .range(unlist(.jsonRequire(Config, c("range"), PATH.config)))
if (any(!is.finite(Range)) || Range[1L] < 0 || Range[2L] > 1 ||
    Range[1L] >= Range[2L]) {
  stop("range must be two increasing values in [0, 1].", call. = FALSE)
}
Stages <- .trimStages(.jsonGet(Config, c("stages"), default = NULL),
                      range = Range)

OVERRIDE <- .readScalarLogical(.jsonGet(Config, c("override"), default = FALSE),
                               "override")
Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                         default = NULL),
                                "recordID", allowNull = TRUE)
if (!length(Records) && nrow(Stages)) Records <- Stages$RecordID
PARALLEL <- .readScalarLogical(.jsonGet(Config, c("parallel"), default = TRUE),
                               "parallel")
WORKERS <- parallel::detectCores(logical = TRUE)
if (is.na(WORKERS)) WORKERS <- 1L
WORKERS <- if (PARALLEL) max(1L, WORKERS) else 1L
LOG.every <- as.numeric(.jsonGet(Config, c("logEvery"), default = 30))

Fmax <- as.numeric(.jsonGet(Config, c("fmax"),
                            default = .jsonGet(SourceJson, c("params", "fmax"),
                                               default = .jsonGet(SourceJson,
                                                                  c("fmax"),
                                                                  default = 25))))
kNyq <- as.numeric(.jsonGet(Config, c("kNyq"),
                            default = .jsonGet(SourceJson, c("params", "kNyq"),
                                               default = .jsonGet(SourceJson,
                                                                  c("kNyq"),
                                                                  default = 4))))
Units <- as.character(.jsonGet(Config, c("unitsTarget"),
                               default = .jsonGet(SourceJson,
                                                  c("params", "unitsTarget"),
                                                  default = .jsonGet(SourceJson,
                                                                     c("unitsTarget"),
                                                                     default = "mm"))))
Taper <- 2
Astop <- 0.01
Apass <- 0.95
if (Taper < 0 || Astop <= 0 || Apass <= Astop || Apass >= 1) {
  stop("Invalid window parameters.", call. = FALSE)
}

if (!DB.mode && !dir.exists(PATH.source)) {
  stop(sprintf("path.source does not exist: %s", PATH.source),
       call. = FALSE)
}
if (OVERRIDE && !length(Records) && !nrow(Stages) && dir.exists(PATH.target)) {
  .unlinkGeneratedDir(PATH.target, if (DB.mode) PATH.stage.root else
    .projectHere("gmsp", "records"))
}
.ensureDir(.workRoot(PATH.target))

SourceSelection <- if (DB.mode) {
  selectRecords(keys = list(ProcessID = ProcessID), paths = PATHS)
} else {
  .trimSelection(PATH.source)
}
Selection <- if (length(Records)) {
  SourceSelection[RecordID %chin% Records]
} else {
  copy(SourceSelection)
}
Selection <- .applyTrimStages(Selection, stages = Stages, range = Range)
if (!nrow(Selection)) stop("No records selected for trim.", call. = FALSE)

FILES.trim <- if (DB.mode) {
  c("TSW.csv", "IMW.csv", "WINDOW.csv")
} else {
  c("TSW.csv", "IMW.csv", "OCIDMAP.csv", "META.csv", "WINDOW.csv")
}
Params <- list(
  PATH.source = if (DB.mode) NULL else PATH.source,
  PATHS = if (DB.mode) PATHS else NULL,
  ProcessID = if (DB.mode) ProcessID else NULL,
  ID = ID,
  DIR = DIR,
  range = Range,
  taper = Taper,
  astop = Astop,
  apass = Apass,
  Fmax = Fmax,
  kNyq = kNyq,
  units = Units,
  files = FILES.trim)

.log("trim start: source=%s target=%s records=%s ID=%s range=%.3f..%.3f stages=%s window=butterworth parallel=%s workers=%s",
     if (DB.mode) ProcessID else PATH.source, PATH.target, nrow(Selection),
     ID, Range[1L], Range[2L],
     nrow(Stages), PARALLEL, WORKERS)

if (PARALLEL && WORKERS > 1L) {
  Results <- .runRecordsParallel(
    Selection,
    path = PATH.target,
    params = Params,
    override = OVERRIDE,
    files = FILES.trim,
    processRecord = .processTrimRecord,
    workers = WORKERS,
    log.every = LOG.every,
    label = "trim")
} else {
  Results <- .runRecordsSequential(
    Selection,
    path = PATH.target,
    params = Params,
    override = OVERRIDE,
    files = FILES.trim,
    processRecord = .processTrimRecord,
    label = "trim")
}

FAIL <- vapply(Results, function(x) {
  is.list(x) && identical(x[["status"]], "failed")
}, logical(1L))
if (any(FAIL)) {
  BAD <- vapply(Results[FAIL], function(x) {
    sprintf("%s: %s", x[["RecordID"]], x[["error"]])
  }, character(1L))
  stop(sprintf("Trim failed for %s records:\n%s",
               length(BAD), paste(BAD, collapse = "\n")),
       call. = FALSE)
}

ManifestSelection <- .applyTrimStages(copy(SourceSelection), stages = Stages,
                                      range = Range)
Manifest <- .buildManifest(PATH.target, ManifestSelection)
if (DB.mode) {
  Done <- Manifest[status == "done"]
  if (!nrow(Done)) stop("No completed trim records to index.", call. = FALSE)
  IMW <- .recordReadMany(file.path(Done$path, "IMW.csv"),
                         label = "trim IMW checkpoint")
  WINDOW <- .recordReadMany(file.path(Done$path, "WINDOW.csv"),
                            label = "trim window checkpoint")
  writeProcessTables(
    index = ManifestSelection,
    intensity = IMW,
    paths = PATHS,
    processID = ProcessID,
    record = c("TSW", "IMW"),
    extra = list(TrimWindowTable = WINDOW),
    params = list(trimID = ID, DIR = DIR, range = Range,
                  fmax = Fmax, kNyq = kNyq, unitsTarget = Units),
    override = TRUE)
} else {
  .writeAggregateTables(PATH.target, Manifest, files = FILES.trim)
}
invisible(file.copy(PATH.config, file.path(PATH.target, "runTrim.json"),
                    overwrite = TRUE))

message(sprintf("Trimmed %s records at %s",
                Manifest[, uniqueN(RecordID)],
                if (DB.mode) PATHS$records else PATH.target))
