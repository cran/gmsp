library(gmsp)
library(data.table)

.checkProcessAdmission <- function() {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  Test <- new.env(parent = Namespace)
  for (Name in ls(Namespace, all.names = TRUE)) {
    Value <- get(Name, envir = Namespace)
    if (is.function(Value) && identical(environment(Value), Namespace)) environment(Value) <- Test
    assign(Name, Value, envir = Test)
  }
  Root <- tempfile("gmsp process admission ")
  dir.create(Root)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  File <- file.path(Root, "process.json")
  Config <- list(processID = "F25", stages = "gmsp")
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Before <- list.files(Root, all.files = TRUE, recursive = TRUE)
  for (Call in list(
    quote(Test$.readCharacterVector(1, "recordID")),
    quote(Test$.readScalarLogical("TRUE", "parallel")),
    quote(Test$.readScalarNumber("1", "fmax")),
    quote(Test$.requireColumns(data.table(t = 0), "RecordID", "selection")))) {
    Error <- tryCatch(eval(Call), error = identity)
    stopifnot(inherits(Error, "moduleUsage"))
  }
  Error <- tryCatch(Test$processProject(file = File, root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("read-only", conditionMessage(Error)),
            identical(list.files(Root, all.files = TRUE, recursive = TRUE), Before))
  for (Function in c(".processRecords", ".writeProcessTables", ".updateProcessContract",
                     ".recordWriteProcessContract")) {
    Error <- tryCatch(do.call(get(Function, Test), list(processID = "f25")), error = identity)
    stopifnot(inherits(Error, "moduleUsage"), identical(list.files(Root, all.files = TRUE, recursive = TRUE), Before))
  }
  Error <- tryCatch(Test$.saveRecordTables(row = data.table(ProcessID = "F25")), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), identical(list.files(Root, all.files = TRUE, recursive = TRUE), Before))
  # Stop at the admitted executor boundary; the stage and producer are doubles.
  Test$.processRecipe <- function(stages, config) list()
  Test$.processRecords <- function(selection, ...) {
    stop(structure(list(message = "admitted", call = NULL, selection = selection),
                   class = c("admittedProcess", "error", "condition")))
  }
  Config <- list(processID = "TEST", stages = "gmsp",
    path = list(database = "database", selection = "selection.csv", stage = "stage"),
    parallel = FALSE)
  Keys <- data.table(ProcessID = "raw", RecordID = "R1", OwnerID = "A", EventID = "E1", StationID = "S1")
  fwrite(Keys, file.path(Root, "selection.csv"))
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Error <- tryCatch(Test$processProject(file = File, root = Root), error = identity)
  stopifnot(inherits(Error$cause, "admittedProcess"))
  Keys[, ProcessID := "OTHER"]
  fwrite(Keys, file.path(Root, "selection.csv"))
  Before <- list.files(Root, all.files = TRUE, recursive = TRUE)
  Error <- tryCatch(Test$processProject(file = File, root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("contradicts", conditionMessage(Error)),
            identical(list.files(Root, all.files = TRUE, recursive = TRUE), Before))
  Keys[, ProcessID := "raw"]
  Config$stages <- "trim"
  fwrite(Keys, file.path(Root, "selection.csv"))
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Error <- tryCatch(Test$processProject(file = File, root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("contradicts", conditionMessage(Error)))
}
.checkProcessAdmission()
