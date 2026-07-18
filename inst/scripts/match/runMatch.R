#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))
source(.scriptHere("plot", "setup.R"))
source(.scriptHere("match", "setup.R"))

ARGS <- commandArgs(trailingOnly = TRUE)
PATH.config <- .matchConfigPath(if (length(ARGS)) ARGS[1L] else
  "scripts/match/runMatch.json")

.matchRun(path = PATH.config)
