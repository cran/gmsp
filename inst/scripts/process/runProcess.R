#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))

source(.scriptHere("gmsp", "global.R"))
source(.scriptHere("gmsp", "setup.R"))
source(.scriptHere("trim", "setup.R"))
source(.scriptHere("imf", "setup.R"))
source(.scriptHere("psa", "setup.R"))
source(.scriptHere("process", "setup.R"))

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/process/runProcess.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

ProcessID <- as.character(.jsonRequire(Config, c("processID"), PATH.config))
if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
  stop("processID must be one non-empty string.", call. = FALSE)
}
Stages <- .processStages(.jsonRequire(Config, c("stages"), PATH.config))

PATH.database <- .jsonGet(Config, c("path", "database"), default = NULL)
PATH.records <- .jsonGet(Config, c("path", "records"), default = NULL)
PATH.index <- .jsonGet(Config, c("path", "index"), default = NULL)
PATH.process <- .jsonGet(Config, c("path", "process"), default = NULL)
if (is.null(PATH.database) && !is.null(PATH.records) &&
    identical(basename(path.expand(PATH.records)), "records")) {
  PATH.database <- dirname(path.expand(PATH.records))
}
PATHS <- .recordPaths(list(database = PATH.database, records = PATH.records,
                           index = PATH.index, process = PATH.process),
                      need = c("records", "index", "process"))

PATH.selection <- .resolvePath(.jsonRequire(Config, c("path", "selection"),
                                            PATH.config))
PATH.stage <- .resolvePath(.jsonGet(Config, c("path", "stage"),
                                    default = "gmsp/stage"))
Out <- list(stage = PATH.stage)

MaxRecords <- .jsonGet(Config, c("maxRecords"), default = NULL)
MaxRecords <- if (is.null(MaxRecords) || !length(MaxRecords)) Inf else
  as.numeric(MaxRecords)
Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                         default = NULL),
                                "recordID", allowNull = TRUE)
PARALLEL <- .readScalarLogical(.jsonGet(Config, c("parallel"), default = TRUE),
                               "parallel")
WORKERS <- parallel::detectCores(logical = TRUE)
if (is.na(WORKERS)) WORKERS <- 1L
WORKERS <- if (PARALLEL) max(1L, min(WORKERS, 4L)) else 1L
LOG.every <- as.numeric(.jsonGet(Config, c("logEvery"), default = 30))
OVERRIDE <- .readScalarLogical(.jsonGet(Config, c("override"),
                                        default = FALSE),
                               "override")

FILE <- if (dir.exists(PATH.selection)) {
  file.path(PATH.selection, "selection.csv")
} else {
  PATH.selection
}
Selection <- .recordFread(FILE)
.requireColumns(Selection, KEY.record, "process selection")
Selection <- unique(Selection, by = KEY.record)
if (length(Records)) Selection <- Selection[RecordID %chin% Records]
if (is.finite(MaxRecords) && nrow(Selection) > MaxRecords) {
  Selection <- Selection[seq_len(MaxRecords)]
}
if (!nrow(Selection)) stop("No records selected for runProcess.",
                           call. = FALSE)

.log("runProcess start: process=%s stages=%s records=%s parallel=%s workers=%s",
     ProcessID, paste(.processStageLabel(Stages), collapse = ","),
     nrow(Selection), PARALLEL, WORKERS)
OUT <- processRecords(selection = Selection,
                      paths = PATHS,
                      processID = ProcessID,
                      stages = Stages,
                      config = Config,
                      out = Out,
                      parallel = PARALLEL,
                      workers = WORKERS,
                      logEvery = LOG.every,
                      override = OVERRIDE)

DIR <- .processStageDir(out = Out, processID = ProcessID, stage = "process")
.ensureDir(DIR)
invisible(file.copy(PATH.config, file.path(DIR, "runProcess.json"),
                    overwrite = TRUE))
message(sprintf("Processed %s records for %s at %s",
                nrow(Selection), ProcessID, PATHS$records))
invisible(OUT)
