library(gmsp)
library(data.table)

.checkProcessCommon <- function() {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  Get <- function(name) get(name, envir = Namespace)
  Root <- tempfile("gmsp common ")
  dir.create(Root)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Directory <- getwd()
  Options <- options("gmsp.verbose")
  Has <- Get(".jsonHas")
  Read <- Get(".jsonGet")
  stopifnot(!Has(list(processIDLong = "P"), "processID"),
            is.null(Read(list(a = list(b = NULL)), c("a", "b"), 9)),
            isTRUE(all.equal(Read(list(a = 1), "absent", 9), 9)))
  Error <- tryCatch(Get("processProject")(root = file.path(Root, "missing")), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("root", conditionMessage(Error)),
            identical(getwd(), Directory), identical(options("gmsp.verbose"), Options))
  Error <- tryCatch(Get("processProject")(root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("contract", conditionMessage(Error)))
  File <- file.path(Root, "config.json")
  writeLines('{"processID":"TEST","processID":"OTHER"}', File)
  Error <- tryCatch(Get(".readProjectConfig")(File), error = identity)
  stopifnot(inherits(Error, "moduleUsage"))
  Config <- list(processID = "TEST", stages = "gmsp",
    path = list(database = "database", selection = "selection.csv", stage = "stage"))
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Error <- tryCatch(Get("processProject")(file = File, root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("fmax", conditionMessage(Error)),
            !dir.exists(file.path(Root, "stage")), !dir.exists(file.path(Root, "database")))
  Config$stages <- "imf"
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Error <- tryCatch(Get("processProject")(file = File, root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("method", conditionMessage(Error)))
  Paths <- Get(".recordPaths")(list(database = Root, records = file.path(Root, "records explicit")),
                               need = c("records", "index", "process"))
  stopifnot(identical(Paths$records, file.path(Root, "records explicit")),
            identical(Paths$index, file.path(Root, "index")))
  Keys <- data.table(ProcessID = "TEST", RecordID = "001", OwnerID = "01",
                     EventID = "002", StationID = "003")
  Before <- serialize(Keys, NULL)
  DIR <- file.path(Paths$records, "01", "002", "003", "TEST")
  dir.create(DIR, recursive = TRUE)
  dir.create(file.path(Paths$process, "TEST"), recursive = TRUE)
  jsonlite::write_json(list(ProcessID = "TEST", output = list(record = "TSW", table = character())),
    file.path(Paths$process, "TEST", "process.json"), auto_unbox = TRUE)
  fwrite(data.table(t = c(0, 0.1, 0.2), AT.H1 = c(0, 1, -1)), file.path(DIR, "TSW.001.csv"))
  Products <- Get(".readRecordProducts")(Keys,
    paths = list(records = Paths$records, process = Paths$process), output = "TSW")
  stopifnot(identical(serialize(Keys, NULL), Before), identical(Products$TSW$RecordID, rep("001", 3L)),
            identical(Products$TSW$ProcessID, rep("TEST", 3L)),
            identical(Products$TSW$AT.H1, c(0L, 1L, -1L)),
            identical(attr(Products, "ProcessID"), "TEST"))
  fwrite(data.table(RecordID = "other", t = 0, AT.H1 = 1), file.path(DIR, "TSW.001.csv"))
  Error <- tryCatch(Get(".readRecordProducts")(Keys,
    paths = list(records = Paths$records, process = Paths$process), output = "TSW"), error = identity)
  stopifnot(inherits(Error, "error"), grepl("RecordID", conditionMessage(Error)))
  stopifnot(identical(Get(".readWorkers")(8, parallel = TRUE, cap = 4L), 8L),
            identical(Get(".readWorkers")(8, parallel = FALSE, cap = 4L), 1L))
  Error <- tryCatch(Get(".readWorkers")(1.5, parallel = TRUE), error = identity)
  stopifnot(inherits(Error, "moduleUsage"))
  Error <- tryCatch(Get(".tnGrid")(list(Tn = list(min = 0.1, max = 1)), list()), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("Tn.n", conditionMessage(Error)))
  stopifnot(isTRUE(all.equal(Get(".tnGrid")(list(Tn = list(values = c(0.1, 0.5, 1))), list()), c(0.1, 0.5, 1))),
            identical(getwd(), Directory), identical(options("gmsp.verbose"), Options))
}
.checkProcessCommon()
