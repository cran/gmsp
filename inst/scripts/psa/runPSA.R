#!/usr/bin/env Rscript

Sys.setenv(GMSP_PSA_DEFAULT_CONFIG = "scripts/psa/runPSA.json")
source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "psa", "runBuildPSA.R"))
