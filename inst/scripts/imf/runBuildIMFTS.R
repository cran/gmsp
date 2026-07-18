#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))

source(.scriptHere("gmsp", "global.R"))
source(.scriptHere("imf", "setup.R"))

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/imf/runBuildIMFTS.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

PATH.source <- .resolvePath(.jsonRequire(Config, c("path", "source"),
                                         PATH.config))
PATH.imf <- file.path(PATH.source, "imf")
OVERRIDE <- .readScalarLogical(.jsonGet(Config, c("override"), default = FALSE),
                               "override")

Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                         default = NULL),
                                "recordID", allowNull = TRUE)
Modes <- .readCharacterVector(.jsonGet(Config, c("IMF"), default = NULL),
                              "IMF", allowNull = TRUE)

Fmax <- .readScalarNumber(.jsonGet(Config, c("fmax"), default = 25),
                          "fmax")
kNyq <- .readScalarNumber(.jsonGet(Config, c("kNyq"), default = 4),
                          "kNyq")
Units.target <- as.character(.jsonGet(Config, c("unitsTarget"),
                                      default = "mm"))

if (!dir.exists(PATH.source)) {
  stop(sprintf("Missing source records folder: %s", PATH.source),
       call. = FALSE)
}
if (!dir.exists(PATH.imf)) {
  stop(sprintf("Missing IMF folder: %s. Run runIMF first.", PATH.imf),
       call. = FALSE)
}
Selection <- .readIMFSelection(PATH.source)
if (length(Records)) Selection <- Selection[RecordID %chin% Records]
if (!nrow(Selection)) stop("No records selected for IMF TSW.", call. = FALSE)

OK <- vapply(Selection$RecordID, function(record) {
  .recordDone(.workPath(PATH.imf, record), files = c("IMF.csv", "IMFF.csv"))
}, logical(1L))
if (!all(OK)) {
  BAD <- Selection[!OK, RecordID]
  stop(sprintf("IMF decomposition is not done for %s records: %s",
               length(BAD), paste(head(BAD, 10L), collapse = ", ")),
       call. = FALSE)
}

Params <- list(
  Fmax = Fmax,
  kNyq = kNyq,
  Units.target = Units.target,
  Modes = Modes)

.log("buildIMFTS start: source=%s records=%s", PATH.source, nrow(Selection))
for (i in seq_len(nrow(Selection))) {
  Row <- Selection[i]
  RecordID <- as.character(Row$RecordID)
  PATH.record <- .workPath(PATH.imf, RecordID)
  FILE <- file.path(PATH.record, "TSW.csv")
  if (!OVERRIDE && file.exists(FILE)) {
    .log("IMF TSW %s/%s %s: already done; skipping",
         i, nrow(Selection), RecordID)
    next
  }

  .log("IMF TSW %s/%s %s", i, nrow(Selection), RecordID)
  IMF <- fread(file.path(PATH.record, "IMF.csv"))
  TSW <- .buildIMFTSW(IMF, params = Params)
  fwrite(TSW, FILE)
}

.writeIMFTSWAggregate(PATH.imf, Selection)
invisible(file.copy(PATH.config, file.path(PATH.imf, "runBuildIMFTS.json"),
                    overwrite = TRUE))

message(sprintf("Built IMF TSW for %s records at %s",
                nrow(Selection), PATH.imf))
