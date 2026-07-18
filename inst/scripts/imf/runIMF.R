#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))

source(.scriptHere("gmsp", "global.R"))
source(.scriptHere("imf", "setup.R"))

FILES <- c("IMF.csv", "IMFF.csv")

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/imf/runIMF.json")
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
  PATH.imf <- file.path(PATH.stage.root, sprintf("%s.imf", ProcessID))
} else {
  PATH.source <- .resolvePath(.jsonRequire(Config, c("path", "source"),
                                           PATH.config))
  PATH.imf <- file.path(PATH.source, "imf")
}

MaxRecords <- .jsonGet(Config, c("maxRecords"), default = NULL)
MaxRecords <- if (is.null(MaxRecords) || !length(MaxRecords)) Inf else
  as.numeric(MaxRecords)
MaxF25Rows <- .jsonGet(Config, c("maxF25Rows"), default = NULL)
MaxF25Rows <- if (is.null(MaxF25Rows) || !length(MaxF25Rows)) Inf else
  as.numeric(MaxF25Rows)
Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                         default = NULL),
                                "recordID", allowNull = TRUE)

IMFConfig <- .jsonGet(Config, c("IMF"), default = list())
Method <- as.character(.jsonGet(IMFConfig, c("method"), default = "vmd"))
kIMF <- as.integer(.jsonGet(IMFConfig, c("k"), default = 10L))

PARALLEL <- as.logical(.jsonGet(Config, c("parallel"), default = TRUE))
OVERRIDE <- as.logical(.jsonGet(Config, c("override"), default = FALSE))
WORKERS <- parallel::detectCores(logical = TRUE)
if (is.na(WORKERS)) WORKERS <- 1L
WORKERS <- if (PARALLEL) max(1L, WORKERS) else 1L
LOG.every <- as.numeric(.jsonGet(Config, c("logEvery"), default = 30))

if (!DB.mode && !dir.exists(PATH.source)) {
  stop(sprintf("Missing source records folder: %s", PATH.source),
       call. = FALSE)
}
if (OVERRIDE && dir.exists(PATH.imf)) {
  .unlinkGeneratedDir(PATH.imf, if (DB.mode) PATH.stage.root else PATH.source)
}
.ensureDir(.workRoot(PATH.imf))

DT <- if (DB.mode) {
  selectRecords(keys = list(ProcessID = ProcessID), paths = PATHS)
} else {
  .readIMFSelection(PATH.source)
}
if (length(Records)) {
  DT <- DT[RecordID %chin% Records]
}
FILE <- if (DB.mode) NA_character_ else file.path(PATH.source, "TSW.csv")
if (!DB.mode && file.exists(FILE)) {
  TSW <- fread(FILE, select = c("RecordID", "t"))
  ORD <- TSW[, .(duration = max(t)), by = RecordID]
  DT[ORD, duration := i.duration, on = "RecordID"]
  setorder(DT, duration, RecordID, na.last = TRUE)
  DT[, duration := NULL]
}
if (is.finite(MaxRecords) && nrow(DT) > MaxRecords) DT <- DT[seq_len(MaxRecords)]
if (!nrow(DT)) stop(sprintf("No records selected under %s.",
                            if (DB.mode) ProcessID else PATH.source),
                    call. = FALSE)

Params <- list(
  PATH.source = if (DB.mode) NULL else PATH.source,
  PATHS = if (DB.mode) PATHS else NULL,
  ProcessID = if (DB.mode) ProcessID else NULL,
  Method = Method,
  kIMF = kIMF,
  MaxF25Rows = MaxF25Rows)

.log("runIMF start: source=%s out=%s records=%s parallel=%s workers=%s MaxRecords=%s MaxF25Rows=%s",
     if (DB.mode) ProcessID else PATH.source, PATH.imf, nrow(DT), PARALLEL,
     WORKERS,
     if (is.finite(MaxRecords)) as.character(MaxRecords) else "all",
     if (is.finite(MaxF25Rows)) as.character(MaxF25Rows) else "all")

if (PARALLEL && WORKERS > 1L) {
  .runRecordsParallel(DT, path = PATH.imf, params = Params,
                      override = OVERRIDE, files = FILES,
                      processRecord = .processIMFDecompRecord,
                      workers = WORKERS, log.every = LOG.every,
                      label = "IMF")
} else {
  .runRecordsSequential(DT, path = PATH.imf, params = Params,
                        override = OVERRIDE, files = FILES,
                        processRecord = .processIMFDecompRecord,
                        label = "IMF")
}

invisible(file.copy(PATH.config, file.path(PATH.imf, "runIMF.json"),
                    overwrite = TRUE))
if (DB.mode) {
  updateProcessContract(paths = PATHS, processID = ProcessID,
                        record = c("IMF", "IMFF"),
                        params = list(IMF = list(method = Method, k = kIMF)))
}
message(sprintf("Wrote IMF checkpoints to %s", PATH.imf))
