library(gmsp)
library(data.table)

.checkContractBoundaries <- function() {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  Test <- new.env(parent = Namespace)
  for (Name in ls(Namespace, all.names = TRUE)) {
    Value <- get(Name, envir = Namespace)
    if (is.function(Value) && identical(environment(Value), Namespace)) environment(Value) <- Test
    assign(Name, Value, envir = Test)
  }
  Root <- tempfile("gmsp contract boundaries ")
  dir.create(Root)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  expectUsage <- function(expression, field = NULL) {
    Error <- tryCatch(force(expression), error = identity)
    stopifnot(inherits(Error, "gmspUsage"), inherits(Error, "moduleUsage"))
    if (!is.null(field)) stopifnot(grepl(field, conditionMessage(Error), fixed = TRUE))
    invisible(Error)
  }

  # Astop0 audit probe: edge and low-pass controls deliberately differ.
  Config <- list(fmax = 25, kNyq = 4, flatZeros = TRUE, trimZeros = TRUE,
    Astop0 = 1e-4, unitsSource = "mm", unitsTarget = "mm", windowLength = 128,
    overlap = 75, astopLP = 1e-3, apass0 = 1e-3,
    apassLP = 0.98)
  Params <- Test$.processGMSPParams(Config, paths = list(records = Root), processID = "TEST")
  stopifnot(isTRUE(all.equal(Params$Astop0, 1e-4)),
    isTRUE(all.equal(Params$signalControl$astop0, 1e-4)),
    isTRUE(all.equal(Params$signalControl$astopLP, 1e-3)))
  expectUsage(Test$.processGMSPParams(c(Config, list(astop0 = 1e-4)),
    paths = list(records = Root), processID = "TEST"), "astop0")
  expectUsage(Test$.processGMSPParams(Config[names(Config) != "astopLP"],
    paths = list(records = Root), processID = "TEST"), "astopLP")
  Control <- Config[c("windowLength", "overlap", "astopLP",
                      "apass0", "apassLP")]
  Control$astop0 <- Config$Astop0
  stopifnot(identical(Test$.processSignalControl(Control), Params$signalControl))
  message("Astop0: edge mapping, independent low-pass and explicit admission PASS")

  # Three M3 probes from the published audit; no modal engine.
  t <- seq(0, 5, by = 0.01)
  TSL <- data.table(OCID = "H1", ID = "VT", t = t,
    s = 120 * sin(pi * t / 5)^2 * sin(2 * pi * 1.5 * t))
  Arguments <- list(TSL = TSL, by = character(), unitsSource = "mm", unitsTarget = "mm",
    fmax = 5, overlap = 75, astop0 = 1e-4, apass0 = 1e-3,
    includeWarnings = FALSE, windowLength = 128,
    astopLP = 1e-3, apassLP = 0.98)
  for (Change in list(list(windowLength = 129), list(apassLP = 1.5),
                      list(astopLP = 0.99))) {
    AUX <- Arguments
    AUX[names(Change)] <- Change
    expectUsage(do.call(Test$buildTSL, AUX), names(Change))
  }
  Cause <- structure(list(message = "induced calculation failure", call = NULL, unit = "R1"),
    class = c("boundaryFailure", "error", "condition"))
  Test$.assessSTFT <- function(...) stop(Cause)
  Error <- tryCatch(do.call(Test$buildTSL, Arguments), error = identity)
  stopifnot(inherits(Error, "moduleFailure"), !inherits(Error, "moduleUsage"),
    inherits(Error, "boundaryFailure"), identical(Error$cause, Cause),
    identical(Error$operation, "buildTSL"), grepl("record without metadata", Error$message, fixed = TRUE))
  message("buildTSL: invalid controls and preserved calculation cause PASS")

  # Input guards are usage; filesystem/engine failures keep execution semantics.
  for (Call in list(quote(Test$.recordPaths(list(), need = "process")),
    quote(Test$.recordNormalizeKeys(NULL)), quote(Test$.recordProcessID(data.table(RecordID = "001"))),
    quote(Test$.processStages(character())), quote(Test$.processStages("unsupported")),
    quote(Test$.range(1)), quote(Test$.validTrimRange(c(0, 2), "trim.range")),
    quote(Test$.resolveUnitFactor("mm/s", "mm", "AT")),
    quote(Test$.resolveUnitFactor("mm", "mm/s", "AT")),
    quote(Test$.resolveUnitFactor("mm", "mm", "unsupported")))) expectUsage(eval(Call))
  AUX <- Config
  AUX$astopLP <- 0.99
  expectUsage(Test$.processGMSPParams(AUX, paths = list(records = Root), processID = "TEST"),
              "astopLP")

  # processCommon keys and product fixture; select runs from index-only input.
  Database <- file.path(Root, "database")
  dir.create(file.path(Database, "index"), recursive = TRUE)
  dir.create(file.path(Database, "process", "TEST"), recursive = TRUE)
  Keys <- data.table(ProcessID = "TEST", RecordID = "001", OwnerID = "01",
    EventID = "002", StationID = "003", DIR = "H1", PGA = 1)
  fwrite(Keys, file.path(Database, "process", "TEST", "MasterIndex.csv"))
  Config <- list(processID = "TEST", owners = "01",
    path = list(index = "database/index", selection = "selection"))
  File <- file.path(Root, "select.json")
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  stopifnot(identical(Test$selectProject(file = File, root = Root), 0L),
    identical(fread(file.path(Root, "selection", "selection.csv"), colClasses = "character")$RecordID, "001"))
  # Written products end every line with LF on every system, Windows included.
  for (FILE in file.path(Root, "selection", c("selection.csv", "summary.json"))) {
    AUX <- readBin(FILE, "raw", file.size(FILE))
    stopifnot(!as.raw(13L) %in% AUX, identical(AUX[length(AUX)], as.raw(10L)))
  }
  # .matchDBPaths infers the database with dirname(), which returns "/" on
  # Windows, where Root from tempfile() separates with "\\".
  samePath <- function(x, y) {
    identical(normalizePath(x, winslash = "/", mustWork = FALSE),
              normalizePath(y, winslash = "/", mustWork = FALSE))
  }
  Paths <- Test$.matchDBPaths(list(path = list(records = "database/records")), root = Root)
  stopifnot(samePath(Paths$database, Database), samePath(Paths$index, file.path(Database, "index")),
            samePath(Paths$process, file.path(Database, "process")))
  Paths <- Test$.matchDBPaths(list(path = list(database = "explicit", records = "database/records",
    index = "explicit/index", process = "explicit/process")), root = Root)
  stopifnot(identical(Paths$database, file.path(Root, "explicit")),
    identical(Paths$records, file.path(Database, "records")),
    identical(Paths$index, file.path(Root, "explicit/index")),
    identical(Paths$process, file.path(Root, "explicit/process")))
  expectUsage(Test$.matchDBPaths(list(path = list(records = "other")), root = Root), "index")
  Config$path$index <- "other"
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  expectUsage(Test$selectProject(file = File, root = Root), "process")
  message("project paths: historical inference, explicit priority and terminal-name rule PASS")

  File <- file.path(Root, "selection-file")
  writeLines("occupied", File)
  Error <- tryCatch(Test$writeSelection(Keys, name = "selection", path = File), error = identity)
  stopifnot(inherits(Error, "gmspFailure"), inherits(Error, "moduleFailure"),
    identical(Error$operation, "writeSelection"),
    identical(Error$path, file.path(File, "selection.csv")), inherits(Error$cause, "error"),
    identical(conditionMessage(Error), conditionMessage(Error$cause)),
    identical(readLines(File), "occupied"))

  Directory <- file.path(Root, "foreign.lock")
  dir.create(Directory)
  jsonlite::write_json(list(pid = Sys.getpid() + 1L, host = unname(Sys.info()[["nodename"]])),
    file.path(Directory, "owner.json"), auto_unbox = TRUE)
  Warning <- NULL
  withCallingHandlers(Test$.recordReleaseLock(Directory), warning = function(e) {
    Warning <<- e
    invokeRestart("muffleWarning")
  })
  stopifnot(inherits(Warning, "gmspWarning"), inherits(Warning, "moduleWarning"),
    identical(Warning$operation, ".recordReleaseLock"), identical(Warning$path, Directory),
    identical(conditionMessage(Warning), paste0("Refusing to release lock not owned by this process: ", Directory)),
    file.exists(file.path(Directory, "owner.json")))
  message("publication and lock conditions: CSV failure cause/path and foreign lock warning PASS")

  # Public boundaries must preserve usage, calculation and partial-publication classes.
  Test$.readProjectConfig <- function(...) stop(Cause)
  for (Operation in c("selectProject", "rankProject", "matchProject", "processProject", "exportProject")) {
    Error <- tryCatch(get(Operation, Test)(file = File, root = Root), error = identity)
    stopifnot(inherits(Error, "moduleFailure"), inherits(Error, "boundaryFailure"),
      !inherits(Error, "moduleUsage"), identical(Error$cause, Cause), identical(Error$unit, "R1"))
  }
  Partial <- tryCatch(Test$.stopPartialPublication("induced partial publication", operation = "selectProject",
    path = Root, failures = list(Cause), result = "selection.csv"), error = identity)
  Test$.readProjectConfig <- function(...) stop(Partial)
  for (Operation in c("selectProject", "rankProject", "matchProject")) {
    Error <- tryCatch(get(Operation, Test)(file = File, root = Root), error = identity)
    stopifnot(identical(Error, Partial))
  }
  message("project conditions: usage, original classes/fields/cause and partial publication PASS")
}
.checkContractBoundaries()
