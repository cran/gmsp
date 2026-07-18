#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
source(.scriptHere("gmsp", "global.R"))
source(.scriptHere("imf", "setup.R"))

FILES <- c("IMF.csv", "IMFF.csv")

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/imf/runCompileIMF.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

PATH.source <- .resolvePath(.jsonRequire(Config, c("path", "source"),
                                         PATH.config))
PATH.imf <- file.path(PATH.source, "imf")
Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                         default = NULL),
                                "recordID", allowNull = TRUE)

if (!dir.exists(PATH.imf)) {
  stop(sprintf("Missing IMF folder: %s", PATH.imf), call. = FALSE)
}

DT <- .readIMFSelection(PATH.source)
if (length(Records)) DT <- DT[RecordID %chin% Records]
if (!nrow(DT)) stop("No records selected for IMF compilation.", call. = FALSE)

Manifest <- .buildManifest(PATH.imf, DT)
.writeAggregateTables(PATH.imf, Manifest, files = FILES)

invisible(file.copy(PATH.config, file.path(PATH.imf, "runCompileIMF.json"),
                    overwrite = TRUE))
message(sprintf("Compiled IMF checkpoints for %s done records to %s",
                Manifest[status == "done", uniqueN(RecordID)], PATH.imf))
