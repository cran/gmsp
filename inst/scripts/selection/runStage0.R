#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))
suppressPackageStartupMessages(library(gmsp))
source(.scriptHere("match", "setup.R"))

BATCH <- 200L

.interpSpectrum <- function(x, y, xout) {
  OK <- is.finite(x) & is.finite(y)
  x <- as.numeric(x[OK])
  y <- as.numeric(y[OK])
  if (length(unique(x)) < 2L) return(rep(NA_real_, length(xout)))
  ORD <- order(x)
  stats::approx(x = x[ORD], y = y[ORD], xout = xout,
                rule = 2, ties = "ordered")$y
}

# Script-side copies of two gmsp-internal band metrics (graduated to the
# package with fitModalFactor/matchCompatibility); runStage0 scoring is
# their only script consumer.
.matchInside <- function(x, low, high) {
  TOL <- 1e-6 * pmax(1, abs(high))
  x >= low - TOL & x <= high + TOL
}

.matchRMSE <- function(x, y) {
  OK <- is.finite(x) & is.finite(y) & y != 0
  sqrt(mean(((x[OK] - y[OK]) / y[OK]) ^ 2))
}

.stage0PSW <- function(package, target, params) {
  if ("PSW" %chin% names(package)) return(package$PSW)
  .requireColumns(package$TSW, c(KEY.record, "t"), "TSW")
  COL <- paste("AT", params$ocid, sep = ".")
  .requireColumns(package$TSW, COL, "TSW")
  TSL <- gmsp::TSW2TSL(package$TSW)
  TSL <- TSL[ID %chin% IDS.signal & OCID == params$ocid]
  if (!nrow(TSL)) {
    stop(sprintf("No signal/%s rows available to build candidate PSW.",
                 params$ocid), call. = FALSE)
  }
  gmsp::TSL2PS(
    .x = TSL,
    xi = params$xi,
    Tn = target[Tn > 0, Tn],
    output = "PSW",
    D50 = FALSE,
    D100 = FALSE,
    nTheta = 180)
}

.stage0Batch <- function(selection, target, params, paths, output) {
  Package <- getRecords(keys = selection, paths = paths, output = output)
  PSW <- .stage0PSW(package = Package, target = target, params = params)
  COL <- paste("PSA", params$ocid, sep = ".")
  .requireColumns(PSW, c("RecordID", "Tn", COL), "PSW")
  Tn <- target[Tn > 0, Tn]
  rbindlist(lapply(selection$RecordID, function(Record) {
    PS <- PSW[RecordID == Record]
    data.table(RecordID = Record, Tn = Tn,
               Sa = .interpSpectrum(x = PS$Tn, y = PS[[COL]], xout = Tn))
  }), use.names = TRUE)
}

.stage0Scores <- function(psa, target, insideWeight = 0.2) {
  DT <- target[Tn > 0][psa, on = "Tn", nomatch = 0L]
  DT <- DT[is.finite(Sa) & is.finite(Sa.mean) &
             is.finite(Sa.low) & is.finite(Sa.high) &
             Sa.mean > 0 & Sa.low > 0 & Sa.high > 0]
  if (!nrow(DT)) stop("Stage 0 scoring found zero usable rows.",
                      call. = FALSE)
  DT[, inside := .matchInside(Sa, Sa.low, Sa.high)]
  DT[, violation := pmax((Sa.low - Sa) / Sa.low,
                         (Sa - Sa.high) / Sa.high,
                         0)]
  DT[, dMean := (Sa - Sa.mean) / Sa.mean]
  DT[, .(
    insideFraction = mean(inside),
    bandRMSE = sqrt(mean(violation ^ 2)),
    q90Violation = as.numeric(quantile(violation, 0.90,
                                       na.rm = TRUE, names = FALSE)),
    q95Violation = as.numeric(quantile(violation, 0.95,
                                       na.rm = TRUE, names = FALSE)),
    maxViolation = max(violation, na.rm = TRUE),
    relaxedRMSE = sqrt(mean(fifelse(inside, insideWeight, 1) * dMean ^ 2)),
    RMSE = .matchRMSE(Sa, Sa.mean),
    medianRatio = median(Sa / Sa.mean, na.rm = TRUE),
    nTn = .N), by = RecordID]
}

.stage0Meta <- function(config, paths, columns) {
  ProcessID <- as.character(.jsonRequire(config, c("processID")))
  FILE <- .recordTableFile(processID = ProcessID, paths = paths,
                           output = "MasterIndex")
  if (!file.exists(FILE)) {
    stop(sprintf("Missing MasterIndex: %s", FILE), call. = FALSE)
  }
  OUT <- fread(FILE, select = unique(c("RecordID", columns)),
               showProgress = FALSE)
  unique(OUT, by = "RecordID")[]
}

.stage0Pool <- function(selection, config) {
  FILE <- .jsonGet(config, c("candidate", "pool"), default = NULL)
  if (is.null(FILE)) return(selection)
  FILE <- path.expand(as.character(FILE))
  if (dir.exists(FILE)) FILE <- file.path(FILE, "selection.csv")
  POOL <- .recordFread(FILE)
  .requireColumns(POOL, "RecordID", "candidate pool")
  OUT <- selection[RecordID %chin% unique(POOL$RecordID)]
  message(sprintf("stage0 pool: %d -> %d candidates (%s)",
                  nrow(selection), nrow(OUT), FILE))
  if (!nrow(OUT)) stop("candidate.pool removed every candidate.",
                       call. = FALSE)
  OUT
}

.stage0Exclude <- function(selection, config) {
  Records <- .readCharacterVector(.jsonGet(config, c("candidate",
                                                     "exclude"),
                                           default = NULL),
                                  "candidate.exclude", allowNull = TRUE)
  if (!length(Records)) return(selection)
  OUT <- selection[!RecordID %chin% Records]
  message(sprintf("stage0 exclude: %d -> %d candidates",
                  nrow(selection), nrow(OUT)))
  if (!nrow(OUT)) stop("candidate.exclude removed every candidate.",
                       call. = FALSE)
  OUT
}

.stage0Filter <- function(selection, config, paths) {
  Filter <- .jsonGet(config, c("candidate", "filter"), default = NULL)
  if (is.null(Filter) || !length(Filter)) return(selection)
  if (!is.list(Filter)) {
    stop("candidate.filter must be a JSON object of column ranges.",
         call. = FALSE)
  }
  META <- .stage0Meta(config = config, paths = paths,
                      columns = names(Filter))
  OK <- rep(TRUE, nrow(META))
  for (Name in names(Filter)) {
    if (!Name %chin% names(META)) {
      stop(sprintf("candidate.filter column not in MasterIndex: %s",
                   Name), call. = FALSE)
    }
    x <- META[[Name]]
    Min <- .jsonGet(Filter[[Name]], c("min"), default = -Inf)
    Max <- .jsonGet(Filter[[Name]], c("max"), default = Inf)
    OK <- OK & is.finite(x) & x >= as.numeric(Min) & x <= as.numeric(Max)
  }
  OUT <- selection[RecordID %chin% META[OK, RecordID]]
  message(sprintf("stage0 filter: %d -> %d candidates (%s)",
                  nrow(selection), nrow(OUT),
                  paste(names(Filter), collapse = ", ")))
  if (!nrow(OUT)) {
    stop("candidate.filter removed every candidate.", call. = FALSE)
  }
  OUT
}

.stage0IM <- function(config, paths, params) {
  ProcessID <- as.character(.jsonRequire(config, c("processID")))
  FILE <- .recordTableFile(processID = ProcessID, paths = paths,
                           output = "IntensityTable")
  if (!file.exists(FILE)) {
    stop(sprintf("Missing IntensityTable: %s", FILE), call. = FALSE)
  }
  OUT <- fread(FILE, select = c("RecordID", "OCID", "PGA", "AI"),
               showProgress = FALSE)
  OUT <- OUT[OCID == params$ocid & is.finite(PGA) & PGA > 0,
             .(PGA = max(PGA, na.rm = TRUE),
               AI = max(AI, na.rm = TRUE)),
             by = RecordID]
  if (!nrow(OUT)) stop("Stage 0 found zero finite PGA rows.", call. = FALSE)
  OUT[]
}

.stage0Run <- function(path) {
  PATH.config <- .matchConfigPath(path)
  Config <- .readJson(PATH.config, simplifyVector = FALSE)
  N <- suppressWarnings(as.integer(.jsonRequire(Config, c("stage0", "N"))))
  if (is.na(N) || N < 2L) {
    stop("stage0.N must be an integer >= 2.", call. = FALSE)
  }
  FILE.selection <- .matchPath(.jsonRequire(Config, c("path", "selection"),
                                            PATH.config))
  if (dir.exists(FILE.selection)) {
    FILE.selection <- file.path(FILE.selection, "selection.csv")
  }
  DIR.out <- dirname(FILE.selection)
  .ensureDir(DIR.out)
  # path.selection is this pipeline's OUTPUT; the candidate pool always
  # comes from the processed database.
  Config$path$selection <- NULL
  Params <- .matchParams(Config)
  Target <- .matchReadTargetBand(config = Config, params = Params)
  if (!nrow(Target[Tn > 0])) {
    stop("Stage 0 target has no positive periods.", call. = FALSE)
  }
  PATHS <- .matchDBPaths(Config)
  Selection <- .matchSelectionDB(config = Config, paths = PATHS)
  Selection <- .stage0Pool(selection = Selection, config = Config)
  Selection <- .stage0Exclude(selection = Selection, config = Config)
  Selection <- .stage0Filter(selection = Selection, config = Config,
                             paths = PATHS)
  IM <- .stage0IM(config = Config, paths = PATHS, params = Params)
  Declared <- .recordDeclaredOutputs(unique(Selection$ProcessID), PATHS)
  Output <- if ("PSW" %chin% Declared) "PSW" else "TSW"

  FILE.psa <- file.path(DIR.out, "stage0.psa.csv")
  Done <- character()
  if (file.exists(FILE.psa)) {
    Done <- unique(fread(FILE.psa, select = "RecordID",
                         showProgress = FALSE)$RecordID)
    .log("stage0 checkpoint: %s records already materialized",
         length(Done))
  }
  Todo <- Selection[!RecordID %chin% Done]
  if (nrow(Todo)) {
    LIST <- split(Todo, ceiling(seq_len(nrow(Todo)) / BATCH))
    for (i in seq_along(LIST)) {
      t0 <- proc.time()[["elapsed"]]
      DT <- .stage0Batch(selection = LIST[[i]], target = Target,
                         params = Params, paths = PATHS, output = Output)
      fwrite(DT, FILE.psa, append = file.exists(FILE.psa))
      .log("stage0 batch %s/%s: %s records in %.1f s",
           i, length(LIST), nrow(LIST[[i]]),
           proc.time()[["elapsed"]] - t0)
    }
  }

  PSA <- unique(fread(FILE.psa, showProgress = FALSE),
                by = c("RecordID", "Tn"))
  PSA <- PSA[RecordID %chin% Selection$RecordID]
  OUT <- .stage0Scores(psa = PSA, target = Target)
  OUT[IM, on = "RecordID", `:=`(PGA = i.PGA, AI = i.AI)]
  META <- .stage0Meta(config = Config, paths = PATHS,
                      columns = c("EventName", "EventMagnitude", "Repi"))
  OUT[META, on = "RecordID",
      `:=`(EventName = i.EventName, Mw = i.EventMagnitude,
           Repi = i.Repi)]
  OUT[, rank.band := frankv(.SD,
                            cols = c("bandRMSE", "q90Violation",
                                     "relaxedRMSE"),
                            ties.method = "min", na.last = "keep")]
  setorder(OUT, bandRMSE, q90Violation, relaxedRMSE, RecordID,
           na.last = TRUE)
  setcolorder(OUT, c("RecordID", "rank.band"))
  fwrite(OUT, file.path(DIR.out, "stage0.spectral.csv"))

  DT <- Selection[OUT[is.finite(rank.band), .(RecordID, rank.band)],
                  on = "RecordID", nomatch = 0L][order(rank.band)]
  if (.readScalarLogical(.jsonGet(Config, c("candidate",
                                            "oneRecordPerEvent"),
                                  default = FALSE),
                         "candidate.oneRecordPerEvent")) {
    AUX <- nrow(DT)
    DT <- DT[, .SD[1L], by = EventID]
    setorder(DT, rank.band)
    .log("stage0 oneRecordPerEvent: %s -> %s candidates", AUX, nrow(DT))
  }
  DT <- DT[seq_len(min(N, .N))]
  if (nrow(DT) < N) {
    stop(sprintf("Stage 0 ranked %d records, selection needs %d.",
                 nrow(DT), N), call. = FALSE)
  }
  fwrite(DT[, c("ProcessID", KEY.record), with = FALSE], FILE.selection)
  .log("stage0 wrote %s records to %s", nrow(DT), FILE.selection)
  invisible(FILE.selection)
}

ARGS <- commandArgs(trailingOnly = TRUE)
PATH.config <- .matchConfigPath(if (length(ARGS)) ARGS[1L] else
  file.path("gmsp", "match.json"))

.stage0Run(path = PATH.config)
