library(gmsp)
library(data.table)

.checkProjectDestinations <- function(requireDestinations = TRUE) {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  Test <- new.env(parent = Namespace)
  for (Name in ls(Namespace, all.names = TRUE)) {
    Value <- get(Name, envir = Namespace)
    if (is.function(Value) && identical(environment(Value), Namespace)) environment(Value) <- Test
    assign(Name, Value, envir = Test)
  }
  Root <- tempfile("gmsp destinations ")
  dir.create(Root)
  Root <- normalizePath(Root, winslash = "/", mustWork = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  File <- file.path(Root, "process.json")
  Keys <- data.table(ProcessID = "raw", RecordID = "R1", OwnerID = "A", EventID = "E1", StationID = "S1")
  fwrite(Keys, file.path(Root, "selection.csv"))
  Config <- list(processID = "TEST", stages = "gmsp", parallel = FALSE,
    path = list(database = "database", selection = "selection.csv"))
  Seen <- NULL
  Test$.processRecipe <- function(...) list()
  Test$.processRecords <- function(..., out) {
    Seen <<- out$stage
    list(appended = 1L, skipped = 0L)
  }
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Before <- list.files(Root, all.files = TRUE, recursive = TRUE)
  Result <- tryCatch(Test$processProject(file = File, root = Root), error = identity)
  if (requireDestinations) {
    stopifnot(inherits(Result, "moduleUsage"), grepl("path.stage", conditionMessage(Result), fixed = TRUE),
      is.null(Seen), identical(list.files(Root, all.files = TRUE, recursive = TRUE), Before))
  } else {
    stopifnot(identical(Result, 0L), identical(Seen, file.path(Root, "gmsp/stage")),
      dir.exists(file.path(Seen, "TEST.process")))
  }
  Config$path$stage <- "stage"
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  stopifnot(identical(Test$processProject(file = File, root = Root), 0L),
    identical(Seen, file.path(Root, "stage")), dir.exists(file.path(Seen, "TEST.process")))
  message("Explicit project destinations: admission and executor paths PASS")
}
.checkProjectDestinations()
