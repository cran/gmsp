library(gmsp)
library(data.table)

.checkWorkers <- function() {
  Root <- tempfile("gmsp workers ")
  dir.create(Root)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Verbose <- options(gmsp.verbose = TRUE)
  on.exit(options(Verbose), add = TRUE)
  Profile <- Sys.getenv("R_PROFILE_USER", unset = NA_character_)
  on.exit({
    if (is.na(Profile)) Sys.unsetenv("R_PROFILE_USER")
    if (!is.na(Profile)) Sys.setenv(R_PROFILE_USER = Profile)
  }, add = TRUE)
  File <- file.path(Root, ".Rprofile")
  Marker <- file.path(Root, "profile-ran")
  writeLines(paste0("writeLines('ran', ", deparse(Marker), ")"), File)
  Sys.setenv(R_PROFILE_USER = File)
  DT <- data.table(RecordID = c("R3", "R1", "R2"), Value = c(3L, 1L, 2L))
  Original <- copy(DT)
  Worker <- function(row) {
    writeLines(as.character(Sys.getpid()), file.path(Root, row$RecordID))
    list(id = row$RecordID, value = row$Value * 2L, pid = Sys.getpid(),
         directory = getwd(), verbose = getOption("gmsp.verbose"),
         library = getNamespaceInfo(asNamespace("gmsp"), "path"),
         status = if (row$Value == 1L) "failed" else "done", error = "record failure")
  }
  Run <- get(".runRecordsParallel", envir = getOption("gmsp.testNamespace", asNamespace("gmsp")))
  Result <- withVisible(Run(DT, processRecord = Worker, workers = 2L,
                             logEvery = 0, label = "worker test"))
  stopifnot(!Result$visible, identical(DT, Original),
            !file.exists(Marker),
            identical(vapply(Result$value, "[[", "", "id"), DT$RecordID),
            identical(vapply(Result$value, "[[", 0L, "value"), DT$Value * 2L),
            identical(vapply(Result$value, "[[", "", "status"),
                      c("done", "failed", "done")),
            all(vapply(Result$value, "[[", "", "directory") == getwd()),
            all(vapply(Result$value, "[[", FALSE, "verbose")),
            all(vapply(Result$value, "[[", "", "library") ==
                  getNamespaceInfo(asNamespace("gmsp"), "path")),
            all(vapply(Result$value, "[[", 0L, "pid") != Sys.getpid()))
  Single <- Run(DT[1L], processRecord = Worker, workers = 2L,
                logEvery = 0, label = "single test")
  stopifnot(Single[[1L]]$pid == Sys.getpid())
  stopifnot(nrow(Run(DT[0L], processRecord = Worker, workers = 2L,
                      logEvery = 0, label = "empty test")) == 0L)
  if (.Platform$OS.type == "windows") {
    Pids <- vapply(Result$value, "[[", 0L, "pid")
    stopifnot(length(unique(Pids)) == 2L)
    Error <- tryCatch(Run(DT, processRecord = function(row) {
      stop("uncaught worker failure")
    }, workers = 2L, logEvery = 0, label = "error test"), error = identity)
    stopifnot(inherits(Error, "error"),
              grepl("uncaught worker failure", conditionMessage(Error), fixed = TRUE))
  }
  # Socket workers killed after a failure leave nothing in TMPDIR, where
  # R CMD check looks for detritus. Fail is built in base: a closure of this
  # frame refers to the gmsp namespace, which the workers would have to load
  # to deserialize it.
  Tmpdir <- Sys.getenv("TMPDIR", unset = NA_character_)
  Probe <- tempfile("gmspprobe")
  dir.create(Probe)
  on.exit({
    if (is.na(Tmpdir)) Sys.unsetenv("TMPDIR")
    if (!is.na(Tmpdir)) Sys.setenv(TMPDIR = Tmpdir)
    unlink(Probe, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(TMPDIR = Probe)
  RunSocket <- get(".runSocketJobs", envir = getOption("gmsp.testNamespace", asNamespace("gmsp")))
  Fail <- eval(quote(function(job) stop("uncaught worker failure")), envir = baseenv())
  Error <- tryCatch(RunSocket(as.list(1:3), worker = Fail, workers = 2L, logEvery = 0,
                              label = "socket test"), error = identity)
  stopifnot(inherits(Error, "error"),
            grepl("uncaught worker failure", conditionMessage(Error), fixed = TRUE),
            identical(Sys.getenv("TMPDIR"), Probe),
            !length(list.files(Probe, all.files = TRUE, no.. = TRUE)))
}
.checkWorkers()
