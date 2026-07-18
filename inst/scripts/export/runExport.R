#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))

.copyHtmlTree <- function(path, out, label) {
  if (!dir.exists(path)) return(0L)
  .ensureDir(out)
  FILES <- list.files(path, pattern = "[.]html$", recursive = TRUE,
                      full.names = TRUE)
  if (!length(FILES)) return(0L)
  REL <- substring(FILES, nchar(path) + 2L)
  OUT <- file.path(out, REL)
  DIRS <- unique(dirname(OUT))
  for (DIR in DIRS) .ensureDir(DIR)
  OK <- file.copy(FILES, OUT, overwrite = TRUE)
  if (!all(OK)) {
    stop(sprintf("Could not copy %s files: %s", label,
                 paste(REL[!OK], collapse = ", ")),
         call. = FALSE)
  }
  length(FILES)
}

.htmlRelFiles <- function(path) {
  if (!dir.exists(path)) return(character())
  FILES <- list.files(path, pattern = "[.]html$", recursive = TRUE,
                      full.names = TRUE)
  if (!length(FILES)) return(character())
  substring(FILES, nchar(path) + 2L)
}

.checkHtmlInputs <- function(paths, label) {
  if (length(paths) <= 1L) return(invisible(NULL))
  REL <- unlist(lapply(paths, .htmlRelFiles), use.names = FALSE)
  AUX <- REL[duplicated(REL)]
  if (length(AUX)) {
    stop(sprintf("%s has duplicate relative HTML files: %s", label,
                 paste(unique(AUX), collapse = ", ")),
         call. = FALSE)
  }
  invisible(NULL)
}

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/export/runExport.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

PATH.source <- .jsonGet(Config, c("path", "source"), default = NULL)
PATH.source <- if (is.null(PATH.source)) NULL else .resolvePath(PATH.source)
PATH.out <- .resolvePath(.jsonRequire(Config, c("path", "out"),
                                      PATH.config))
PATH.database <- .jsonGet(Config, c("path", "database"), default = NULL)
PATH.records <- .jsonGet(Config, c("path", "records"), default = NULL)
PATH.index <- .jsonGet(Config, c("path", "index"), default = NULL)
IndexUser <- !is.null(PATH.index)
PATH.index <- if (is.null(PATH.index)) NULL else .resolvePath(PATH.index)
if (IndexUser && dir.exists(PATH.index)) {
  stop("path.index must point to a MasterIndex CSV file, not an index directory.",
       call. = FALSE)
}
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
PATH.selection.file <- NULL
if (!is.null(PATH.selection)) {
  PATH.selection <- .resolvePath(PATH.selection)
  PATH.selection.file <- if (dir.exists(PATH.selection)) {
    file.path(PATH.selection, "selection.csv")
  } else {
    PATH.selection
  }
}
ProcessID <- as.character(.jsonGet(Config, c("processID"), default = "raw"))
if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
  stop("processID must be one non-empty string.", call. = FALSE)
}
OVERRIDE <- .readScalarLogical(.jsonGet(Config, c("override"),
                                        default = FALSE),
                               "override")
if (!OVERRIDE && dir.exists(PATH.out) && length(list.files(PATH.out))) {
  stop(sprintf("%s already exists. Set override=true to rebuild.", PATH.out),
       call. = FALSE)
}
.ensureDir(PATH.out)
PATH.metadata <- file.path(PATH.out, "metadata")
.ensureDir(PATH.metadata)

Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                         default = NULL),
                                "recordID", allowNull = TRUE)

RawDB <- !is.null(PATH.records) && identical(ProcessID, "raw")

if (!is.null(PATH.records)) {
  PATHS <- PATHS.db
  if (RawDB) {
    Outputs <- "AT"
  } else {
    Outputs <- intersect(c("TSW", "IMW", "PSW"),
                         .recordDeclaredOutputs(ProcessID, PATHS.db))
    Missing <- setdiff(c("TSW", "IMW"), Outputs)
    if (length(Missing)) {
      stop(sprintf("ProcessID %s does not declare required export outputs: %s",
                   ProcessID, paste(Missing, collapse = ", ")),
           call. = FALSE)
    }
  }
  Keys <- .recordReadSelectionKeys(path = PATH.selection,
                                   processID = ProcessID,
                                   recordID = Records,
                                   label = "selection",
                                   allowAll = FALSE)
  Package <- getRecords(keys = Keys, paths = PATHS, output = Outputs)
  TSW <- if (RawDB) Package$AT else Package$TSW
  IMW <- if (RawDB) NULL else Package$IMW
  PSW <- if (RawDB) NULL else Package$PSW
  INDEX <- attr(Package, "index")
  Selection <- attr(Package, "keys")
} else {
  if (is.null(PATH.source)) {
    stop("path.source is required when path.records is not provided.",
         call. = FALSE)
  }
  FILES <- vapply(c("TSW.csv", "IMW.csv"), function(FILE) {
    .recordSourceDataFile(PATH.source, FILE)
  }, character(1L))
  Missing <- FILES[!file.exists(FILES)]
  if (length(Missing)) {
    stop(sprintf("Missing gmsp source files:\n%s",
                 paste(Missing, collapse = "\n")), call. = FALSE)
  }
  TSW <- .recordFread(FILES[["TSW.csv"]])
  IMW <- .recordFread(FILES[["IMW.csv"]])
  FILE <- .recordSourceDataFile(PATH.source, "PSW.csv")
  PSW <- if (file.exists(FILE)) .recordFread(FILE) else NULL
  INDEX <- NULL
  Selection <- NULL
}
PATH.plot <- .jsonGet(Config, c("path", "plot"),
                      default = if (is.null(PATH.source)) NULL else
                        file.path(PATH.source, "plot"))
PATH.plot <- if (is.null(PATH.plot)) character() else
  vapply(as.list(PATH.plot), .resolvePath, character(1L))
PATH.audit <- .jsonGet(Config, c("path", "audit"),
                       default = if (is.null(PATH.source)) NULL else
                         file.path(PATH.source, "audit"))
PATH.audit <- if (is.null(PATH.audit)) character() else
  vapply(as.list(PATH.audit), .resolvePath, character(1L))
.checkHtmlInputs(PATH.plot, "path.plot")
.checkHtmlInputs(PATH.audit, "path.audit")

if (!length(Records)) Records <- sort(unique(TSW$RecordID))

ID <- .readCharacterVector(.jsonGet(Config, c("ID"), default = "AT"), "ID")
DIR <- .readCharacterVector(.jsonGet(Config, c("DIR"), default = "H1"), "DIR")

.log("export start: source=%s out=%s records=%s", PATH.source, PATH.out,
     length(Records))

DT <- TSW[RecordID %chin% Records]
if (!nrow(DT)) stop("No TSW rows selected for export.", call. = FALSE)

if (RawDB && any(ID != "AT")) {
  stop("Raw database export only supports ID = AT.", call. = FALSE)
}

COLS <- if (RawDB) DIR else CJ(ID = ID, DIR = DIR)[, paste(ID, DIR, sep = ".")]
.requireColumns(DT, c("RecordID", "t", COLS), if (RawDB) "AT" else "TSW")

OUT <- CJ(RecordID = sort(unique(DT$RecordID)), ID = ID, DIR = DIR)
if (RawDB) {
  OUT[, FILE := sprintf("AT.%s.%s.csv", RecordID, DIR)]
} else {
  OUT[, FILE := sprintf("TSW.%s.%s.%s.csv", RecordID, ID, DIR)]
}

for (i in seq_len(nrow(OUT))) {
  .log("Export %s/%s %s", i, nrow(OUT), OUT$FILE[i])
  COL <- if (RawDB) OUT$DIR[i] else paste(OUT$ID[i], OUT$DIR[i], sep = ".")
  FILE <- file.path(PATH.out, OUT$FILE[i])
  fwrite(DT[RecordID == OUT$RecordID[i], .(t, s = get(COL))], FILE)
}

fwrite(OUT, file.path(PATH.out, "files.csv"))

AUX <- OUT[, .(
  nFiles = .N,
  ID = paste(sort(unique(ID)), collapse = ","),
  DIR = paste(sort(unique(DIR)), collapse = ",")
), by = RecordID][order(RecordID)]
fwrite(AUX, file.path(PATH.out, "unique.csv"))

if (!is.null(IMW)) {
  IMW <- IMW[RecordID %chin% Records]
  if ("OCID" %chin% names(IMW)) IMW <- IMW[OCID %chin% unique(OUT$DIR)]
  setcolorder(IMW, c(intersect(c("RecordID", "OwnerID", "EventID",
                                 "StationID", "OCID"), names(IMW)),
                     setdiff(names(IMW), c("RecordID", "OwnerID", "EventID",
                                           "StationID", "OCID"))))
  fwrite(IMW, file.path(PATH.out, "IMW.csv"))
}

if (!is.null(PSW)) {
  PSW <- PSW[RecordID %chin% Records]
  setcolorder(PSW, c(intersect(c("RecordID", "OwnerID", "EventID",
                                 "StationID", "xi", "Tn"), names(PSW)),
                     setdiff(names(PSW), c("RecordID", "OwnerID", "EventID",
                                           "StationID", "xi", "Tn"))))
  fwrite(PSW, file.path(PATH.out, "PSW.csv"))
}

if (!is.null(PATH.source)) {
  FILES.meta <- c("OCIDMAP.csv", "source.json", "runMatch.json")
  if (!IndexUser) FILES.meta <- c(FILES.meta, "MasterIndex.csv")
  FILES.metadata <- c(
    vapply(FILES.meta, function(FILE) {
      .recordSourceMetadataFile(PATH.source, FILE)
    }, character(1L)),
    .recordSourceDataFile(PATH.source, "manifest.csv")
  )
  FILES.metadata <- FILES.metadata[file.exists(FILES.metadata)]
  if (length(FILES.metadata)) {
    OK <- file.copy(FILES.metadata, PATH.metadata, overwrite = TRUE)
    if (!all(OK)) {
      stop(sprintf("Could not copy metadata files: %s",
                   paste(basename(FILES.metadata[!OK]), collapse = ", ")),
           call. = FALSE)
    }
  }
}

FILE.index <- if (IndexUser) {
  PATH.index
} else if (is.null(PATH.source)) {
  NA_character_
} else {
  .recordSourceMetadataFile(PATH.source, "MasterIndex.csv")
}
if (!is.na(FILE.index) && file.exists(FILE.index)) {
  INDEX <- .recordFread(FILE.index)
  .requireColumns(INDEX, "RecordID", "MasterIndex")
  INDEX <- INDEX[RecordID %chin% Records]
  if (!nrow(INDEX)) {
    stop("MasterIndex has zero rows for selected records.", call. = FALSE)
  }
  setorder(INDEX, RecordID)
  fwrite(INDEX, file.path(PATH.metadata, "MasterIndex.csv"))
} else if (IndexUser) {
  stop(sprintf("Missing MasterIndex table: %s", PATH.index), call. = FALSE)
} else if (!is.null(INDEX)) {
  setorder(INDEX, RecordID)
  fwrite(INDEX, file.path(PATH.metadata, "MasterIndex.csv"))
}

if (!is.null(PATH.selection.file)) {
  if (!file.exists(PATH.selection.file)) {
    stop(sprintf("Missing selection table: %s", PATH.selection.file),
         call. = FALSE)
  }
  OUT.selection <- .recordFread(PATH.selection.file)
  .requireColumns(OUT.selection, "RecordID", "selection")
  OUT.selection <- OUT.selection[RecordID %chin% Records]
  if (!nrow(OUT.selection)) {
    stop("selection has zero rows for selected records.", call. = FALSE)
  }
  setorder(OUT.selection, RecordID)
  fwrite(OUT.selection, file.path(PATH.metadata, "selection.csv"))
} else if (!is.null(Selection)) {
  setorder(Selection, RecordID)
  fwrite(Selection, file.path(PATH.metadata, "selection.csv"))
}

for (Plot in PATH.plot) {
  if (dir.exists(Plot)) {
    .copyHtmlTree(path = Plot, out = file.path(PATH.out, "plot"),
                  label = "plot")
  }
}

for (Audit in PATH.audit) {
  if (dir.exists(Audit)) {
    .copyHtmlTree(path = Audit, out = file.path(PATH.out, "audit"),
                  label = "audit")
  }
}

OK <- file.copy(PATH.config, file.path(PATH.out, "runExport.json"),
                overwrite = TRUE)
if (!OK) stop("Could not copy runExport.json.", call. = FALSE)

message(sprintf("Exported %s files to %s", format(nrow(OUT), big.mark = ","),
                PATH.out))
