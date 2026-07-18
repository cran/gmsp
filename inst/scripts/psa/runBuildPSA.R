#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))
source(.scriptHere("psa", "setup.R"))

KEY.record <- c("RecordID", "OwnerID", "EventID", "StationID")

Args <- commandArgs(trailingOnly = TRUE)
DefaultConfig <- Sys.getenv("GMSP_PSA_DEFAULT_CONFIG",
                            unset = "scripts/psa/runBuildPSA.json")
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else DefaultConfig)
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
  PATH.selection <- .jsonGet(Config, c("path", "selection"), default = NULL)
  PATH.selection <- if (is.null(PATH.selection)) NULL else
    .resolvePath(PATH.selection)
  PATH.stage.root <- .resolvePath(.jsonGet(Config, c("path", "stage"),
                                           default = "gmsp/stage"))
  PATH.psa <- file.path(PATH.stage.root, sprintf("%s.psa", ProcessID))
  SourceJson <- .recordReadProcessJSON(processID = ProcessID, paths = PATHS,
                                       required = FALSE)
  if (is.null(SourceJson)) SourceJson <- list()
} else {
  PATH.source <- .resolvePath(.jsonRequire(Config, c("path", "source"),
                                           PATH.config))
  PATH.psa <- .resolvePath(.jsonGet(Config, c("path", "psa"),
                                    default = .jsonRequire(Config,
                                                           c("path", "source"),
                                                           PATH.config)))
  FILE.source <- .sourceJson(PATH.source)
  SourceJson <- if (is.na(FILE.source)) {
    list()
  } else {
    .readJson(FILE.source, simplifyVector = FALSE)
  }
}
OVERRIDE <- .readScalarLogical(.jsonGet(Config, c("override"), default = FALSE),
                               "override")

XI <- as.numeric(unlist(.jsonGet(Config, c("xi"), default = 0.05)))
D50 <- .readScalarLogical(.jsonGet(Config, c("D50"), default = TRUE),
                          "D50")
D100 <- .readScalarLogical(.jsonGet(Config, c("D100"), default = TRUE),
                           "D100")
nTheta <- as.integer(.jsonGet(Config, c("nTheta"), default = 180))
Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                         default = NULL),
                                "recordID", allowNull = TRUE)
PARALLEL <- .readScalarLogical(.jsonGet(Config, c("parallel"), default = TRUE),
                               "parallel")
WORKERS <- parallel::detectCores(logical = TRUE)
if (is.na(WORKERS)) WORKERS <- 1L
WORKERS <- if (PARALLEL) max(1L, min(WORKERS, 4L)) else 1L
LOG.every <- as.numeric(.jsonGet(Config, c("logEvery"), default = 30))

if (!DB.mode) {
  FILE <- file.path(PATH.source, "TSW.csv")
  if (!file.exists(FILE)) stop(sprintf("Missing source TSW.csv: %s", FILE),
                               call. = FALSE)
}
Tn <- .tnGrid(Config, SourceJson)

.ensureDir(.workRoot(PATH.psa))

FILE.manifest <- if (DB.mode) NA_character_ else
  file.path(PATH.source, "manifest.csv")
if (!DB.mode && file.exists(FILE.manifest)) {
  Manifest <- .recordFread(FILE.manifest)
  .requireColumns(Manifest, c(KEY.record, "status"), "manifest")
  Selection <- unique(Manifest[status == "done", KEY.record, with = FALSE],
                      by = KEY.record)
  TSW <- NULL
} else if (DB.mode) {
  Keys <- .recordReadSelectionKeys(path = PATH.selection,
                                   processID = ProcessID,
                                   recordID = Records,
                                   label = "selection",
                                   allowAll = TRUE)
  Selection <- selectRecords(keys = Keys, paths = PATHS)
  TSW <- NULL
} else {
  TSW <- .recordFread(FILE)
  .requireColumns(TSW, c(KEY.record, "t"), "TSW")
  Selection <- unique(TSW[, KEY.record, with = FALSE], by = KEY.record)
}
if (length(Records)) Selection <- Selection[RecordID %chin% Records]
if (!nrow(Selection)) stop("No records selected for PSA.", call. = FALSE)

.log("buildPSA start: source=%s records=%s parallel=%s workers=%s",
     if (DB.mode) ProcessID else PATH.source, nrow(Selection), PARALLEL,
     WORKERS)

Params <- list(
  PATH.source = if (DB.mode) NULL else PATH.source,
  PATHS = if (DB.mode) PATHS else NULL,
  ProcessID = if (DB.mode) ProcessID else NULL,
  TSW = TSW,
  XI = XI,
  Tn = Tn,
  D50 = D50,
  D100 = D100,
  nTheta = nTheta)

Results <- buildPSARecords(selection = Selection, path = PATH.psa,
                           params = Params, override = OVERRIDE,
                           parallel = PARALLEL, workers = WORKERS,
                           logEvery = LOG.every)

FAIL <- vapply(Results, function(x) {
  is.list(x) && identical(x[["status"]], "failed")
}, logical(1L))
if (any(FAIL)) {
  BAD <- vapply(Results[FAIL], function(x) {
    sprintf("%s: %s", x[["RecordID"]], x[["error"]])
  }, character(1L))
  stop(sprintf("PSA failed for %s records:\n%s",
               length(BAD), paste(BAD, collapse = "\n")),
       call. = FALSE)
}

if (DB.mode) {
  updateProcessContract(paths = PATHS, processID = ProcessID, record = "PSW",
                        params = list(PSA = list(xi = XI, Tn = list(
                          min = min(Tn), max = max(Tn), n = length(Tn)))))
} else {
  .writePSWAggregate(PATH.psa, Selection)
}
invisible(file.copy(PATH.config, file.path(PATH.psa, basename(PATH.config)),
                    overwrite = TRUE))

message(sprintf("Built PSA for %s records at %s",
                .nPSWRecords(PATH.psa, Selection),
                if (DB.mode) PATHS$records else PATH.psa))
