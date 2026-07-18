#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
source(.scriptHere("selection", "buildMaster.R"))

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- .resolvePath(if (length(Args)) Args[1L] else
  "scripts/selection/runBuildMaster.json")
Config <- .readJson(PATH.config, simplifyVector = FALSE)

PATH.index <- .resolvePath(.jsonRequire(Config, c("path", "index"),
                                        PATH.config))
Owners <- unique(as.character(.jsonRequire(Config, c("owners"), PATH.config)))
ProcessID <- as.character(.jsonGet(Config, c("processID"), default = "raw"))
Master <- .readScalarLogical(.jsonGet(Config, c("master"), default = TRUE),
                             "master")

.log("buildMaster start: owners=%s processID=%s master=%s",
     paste(Owners, collapse = ","), ProcessID, Master)
writeMaster(path = PATH.index, owners = Owners, processID = ProcessID,
            master = Master)

message(sprintf("Wrote MasterIndex for %s owners to %s",
                length(Owners), PATH.index))
