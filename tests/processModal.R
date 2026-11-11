library(gmsp)
library(data.table)

.checkProcessModal <- function() {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  Test <- new.env(parent = Namespace)
  for (Name in ls(Namespace, all.names = TRUE)) {
    Value <- get(Name, envir = Namespace)
    if (is.function(Value) && identical(environment(Value), Namespace)) environment(Value) <- Test
    assign(Name, Value, envir = Test)
  }
  Root <- tempfile("gmsp modal process ")
  dir.create(Root)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  HadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  SeedBefore <- if (HadSeed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (HadSeed) assign(".Random.seed", SeedBefore, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  Config <- list(imf = list(method = "decomposeEEMD", seed = 628,
    control = list(realizationCount = 2, amplitudeRatio = 0.2, divisor = "realizations",
      sifting = list(stopRule = "doubleThreshold", thresholds = c(0.05, 0.5),
                     fraction = 0.05, maxSift = 2000, boundary = "reflection", maxModes = Inf))),
    parallel = FALSE, workers = 1)
  Params <- Test$.processModalExecution("imf", Config, parallel = FALSE, workers = 1)
  stopifnot(Params$stochastic, isTRUE(all.equal(Params$seed, 628)))
  for (Bad in list(within(Config, rm(workers)), within(Config, parallel <- TRUE),
                   within(Config, workers <- 2), within(Config, imf$seed <- NULL))) {
    Error <- tryCatch(Test$.processModalExecution("imf", Bad, parallel = FALSE, workers = 1), error = identity)
    stopifnot(inherits(Error, "moduleUsage"))
  }
  stopifnot(identical(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE), HadSeed))
  if (HadSeed) stopifnot(identical(get(".Random.seed", envir = .GlobalEnv), SeedBefore))
  VMD <- list(method = "vmd", k = 2, alpha = 2000, tau = 0, DC = TRUE,
              init = 0, tolerance = 1e-6, maxIterations = 98)
  stopifnot(!Test$.processIMFParams(VMD)$stochastic)
  Error <- tryCatch(Test$.processIMFParams(within(VMD, rm(tolerance))), error = identity)
  stopifnot(inherits(Error, "moduleUsage"))
  Error <- tryCatch(Test$.processIMFTSParams(list(remove = "IMF1")), error = identity)
  stopifnot(inherits(Error, "moduleUsage"))

  # Writer boundary: a declared empty metrics table is valid; filtering another
  # record down to zero rows is still a key failure.
  Key <- data.table(ProcessID = "TEST", RecordID = "R1", OwnerID = "A", EventID = "E1", StationID = "S1")
  Empty <- Key[0L, Test$RecordKeys, with = FALSE]
  Empty[, `:=`(OCID = character(), IMF = character(), `Fo[Hz]` = numeric(),
                `BW[Hz]` = numeric(), `E[k]/Eo [%]` = numeric())]
  Output <- Test$.recordSubsetOutput(Empty, Key, "IMFF")
  stopifnot(nrow(Output) == 0L, all(Test$ProcessKeys %in% names(Output)),
            is.double(Output[["Fo[Hz]"]]))
  Other <- copy(Key)
  Other[, RecordID := "OTHER"]
  Error <- tryCatch(Test$.recordSubsetOutput(Other, Key, "IMFF"), error = identity)
  stopifnot(inherits(Error, "error"))
  Paths <- list(records = file.path(Root, "records"), index = file.path(Root, "index"), process = file.path(Root, "process"))
  Test$.saveRecordTables(Key, Paths, list(IMFF = Empty))
  Published <- Test$.processOutputFile(Key, Paths, "IMFF")
  stopifnot(file.exists(Published), nrow(fread(Published)) == 0L,
            identical(names(fread(Published)), names(Output)))

  # Exercise ordering, append skips and one RNG initialization across the batch.
  Keys <- data.table(RecordID = c("R3", "R1", "R2"), OwnerID = "A",
                    EventID = c("E3", "E1", "E2"), StationID = c("S3", "S1", "S2"))
  Calls <- 0L
  Seen <- character()
  Draws <- numeric()
  Test$set.seed <- function(seed) { Calls <<- Calls + 1L; base::set.seed(seed) }
  Test$.recordAcquireLock <- function(...) NULL
  Test$.recordReleaseLock <- function(...) NULL
  Test$.recordRequireNoTransaction <- function(...) invisible(NULL)
  Test$.recordTransactionDirs <- function(...) character()
  Test$.processAppendPlan <- function(selection, ...) list(run = selection[RecordID != "R3"],
    skip = selection[RecordID == "R3"], index = selection,
    outputs = c("TSW", "IMW", "IMF", "IMFF"), signature = "test", recipe = list(), provenance = list())
  Test$.processRecord <- function(row, ...) {
    Seen <<- c(Seen, row$RecordID)
    Draws <<- c(Draws, stats::runif(1))
    list(row = row, status = "done", IMW = row, tables = list(),
         stages = list(imf = list(effective = list(control = Config$imf$control))))
  }
  PublishedParams <- NULL
  Test$.writeProcessTables <- function(..., params) PublishedParams <<- params
  Test$.processWriteSelection <- function(...) "selection"
  Test$.processPublishEvidence <- function(...) invisible(NULL)
  Evidence <- file.path(Root, "runProcess.json")
  Result <- Test$.processRecords(Keys, Paths, "TEST", stages = c("imf", "imfts"),
    config = Config, out = list(stage = Root), parallel = FALSE, workers = 1, evidence = Evidence)
  stopifnot(identical(Seen, c("R1", "R2")), Calls == 1L,
            identical(Result$modal$recordOrder$RecordID, c("R1", "R2")),
            identical(Result$modal$skipped$RecordID, "R3"),
            identical(Result$modal$stateAfter, get(".Random.seed", envir = .GlobalEnv)),
            !identical(Result$modal$stateBefore, Result$modal$stateAfter),
            identical(PublishedParams$modalExecution, Result$modal))
  Saved <- jsonlite::read_json(paste0(Evidence, ".modal.json"), simplifyVector = TRUE)
  stopifnot(identical(Saved$stateAfter, Result$modal$stateAfter), Saved$status == "complete")
  base::set.seed(628)
  stopifnot(isTRUE(all.equal(Draws, stats::runif(2))))
  BeforeSkip <- get(".Random.seed", envir = .GlobalEnv)
  Test$.processAppendPlan <- function(selection, ...) list(run = selection[0L], skip = selection,
    index = selection, outputs = c("TSW", "IMW", "IMF", "IMFF"), signature = "test",
    recipe = list(), provenance = list())
  Result <- Test$.processRecords(Keys, Paths, "TEST", stages = "imf", config = Config,
    out = list(stage = Root), parallel = FALSE, workers = 1, evidence = Evidence)
  stopifnot(Calls == 1L, !Result$modal$seeded, identical(get(".Random.seed", envir = .GlobalEnv), BeforeSkip))

  # Effective VMD controls use the persisted grid and retain full JSON precision.
  Signal <- Key[rep(1L, 3)]
  Signal[, `:=`(OCID = "H1", ID = "AT", t = c(0, 0.001, 0.002), s = c(0, 1, -1))]
  Controls <- Test$.processEffectiveControls(list(signal = Signal), Test$.processIMFParams(VMD))
  Test$.recordWriteJSONAtomic(Controls, file.path(Root, "controls.json"), digits = NA)
  Saved <- jsonlite::read_json(file.path(Root, "controls.json"), simplifyVector = FALSE)
  stopifnot(isTRUE(all.equal(Saved[[1L]]$control$alpha, 2000 * 0.001^2 / (8 * pi^2))))
}
.checkProcessModal()
