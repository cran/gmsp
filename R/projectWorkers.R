# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readWorkers <- function(x, parallel, cap = NULL) {
  # cap bounds the discovered default only, never a declared value. A caller
  # whose per-record work is already parallel inside (VMD/BLAS in process)
  # oversubscribes the host at one worker per core, so its default stays below
  # the core count while an explicit workers key is still honoured.
  Declared <- !is.null(x) && length(x) > 0L
  if (Declared && (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x != floor(x) || x > .Machine$integer.max)) {
    .stopUsage("workers must be one positive integer.", operation = ".readWorkers")
  }
  if (!Declared) x <- suppressWarnings(parallel::detectCores(logical = TRUE))
  x <- suppressWarnings(as.integer(x))[1L]
  if (length(x) != 1L || is.na(x) || x < 1L) {
    x <- suppressWarnings(as.integer(parallel::detectCores(logical = FALSE)))[1L]
    Declared <- FALSE
  }
  if (length(x) != 1L || is.na(x) || x < 1L) x <- 1L
  if (!Declared && !is.null(cap)) x <- min(x, as.integer(cap)[1L])
  if (isTRUE(parallel)) max(1L, x) else 1L
}

.runRecordsSequential <- function(DT, processRecord, label) {
  OUT <- vector("list", nrow(DT))
  for (i in seq_len(nrow(DT))) {
    Row <- DT[i]
    RecordID <- as.character(Row$RecordID[1L])
    .projectMessage(label, " ", i, "/", nrow(DT), " ", RecordID,
      operation = label, phase = "processing", count = i, total = nrow(DT), unit = RecordID)
    OUT[[i]] <- processRecord(row = Row)
    if (is.list(OUT[[i]]) && identical(OUT[[i]][["status"]], "failed")) {
      .projectMessage(label, " ", i, "/", nrow(DT), " ", RecordID,
        " failed: ", OUT[[i]][["error"]], operation = label, phase = "processing",
        count = i, total = nrow(DT), unit = RecordID)
    }
  }
  invisible(OUT)
}

.runRecordsParallel <- function(DT, processRecord, workers, logEvery, label) {
  n <- nrow(DT)
  if (!n) return(invisible(data.table()))
  workers <- max(1L, min(as.integer(workers), n))
  if (workers <= 1L) {
    return(.runRecordsSequential(DT = DT, processRecord = processRecord,
                                 label = label))
  }

  if (.Platform$OS.type == "windows") {
    return(invisible(.runSocketJobs(
      lapply(seq_len(n), function(i) DT[i]), worker = processRecord,
      workers = workers, logEvery = logEvery, label = label)))
  }

  Queue <- seq_len(n)
  Active <- list()
  on.exit({
    if (length(Active)) {
      for (Job in Active) {
        suppressWarnings(try(tools::pskill(Job$pid, signal = 15L),
                             silent = TRUE))
      }
      suppressWarnings(try(parallel::mccollect(Active, wait = TRUE),
                           silent = TRUE))
      Active <- list()
    }
  }, add = TRUE)
  OUT <- vector("list", n)
  Done <- 0L
  LastLog <- proc.time()[["elapsed"]]

  .launch <- function(i) {
    Row <- DT[i]
    parallel::mcparallel(
      processRecord(row = Row),
      name = as.character(i),
      silent = FALSE)
  }

  while (Done < n) {
    while (length(Queue) && length(Active) < workers) {
      i <- Queue[1L]
      Queue <- Queue[-1L]
      Active[[as.character(i)]] <- .launch(i)
    }

    Collected <- parallel::mccollect(Active, wait = FALSE)
    if (length(Collected)) {
      for (Name in names(Collected)) {
        OUT[[as.integer(Name)]] <- Collected[[Name]]
        Active[[Name]] <- NULL
        Done <- Done + 1L
      }
    }

    Now <- proc.time()[["elapsed"]]
    if ((Now - LastLog) >= logEvery) {
      .projectMessage(label, " progress: done=", Done, "/", n,
        " active=", length(Active), " queued=", length(Queue),
        operation = label, phase = "processing", count = Done, total = n)
      LastLog <- Now
    }
    if (Done < n) Sys.sleep(0.25)
  }

  invisible(OUT)
}

.runSocketJobs <- function(jobs, worker, workers, logEvery, label) {
  # Killed workers cannot remove the temporary files they keep in TMPDIR.
  # They start with a TMPDIR of their own, removed on exit; the caller's
  # TMPDIR is back as soon as they have started.
  WorkerTemp <- tempfile("gmspworkers")
  dir.create(WorkerTemp)
  on.exit(unlink(WorkerTemp, recursive = TRUE), add = TRUE)
  Tmpdir <- Sys.getenv("TMPDIR", unset = NA_character_)
  Cluster <- tryCatch({
    Sys.setenv(TMPDIR = WorkerTemp)
    parallel::makePSOCKcluster(workers, outfile = "", rscript_args = "--vanilla")
  }, finally = {
    if (is.na(Tmpdir)) Sys.unsetenv("TMPDIR")
    if (!is.na(Tmpdir)) Sys.setenv(TMPDIR = Tmpdir)
  })
  Pids <- integer()
  Complete <- FALSE
  # Runs before the removal of WorkerTemp.
  on.exit({
    # An interrupted writer must stop before the caller removes its products.
    if (!Complete) {
      for (Pid in Pids) {
        suppressWarnings(try(tools::pskill(Pid, signal = 9L), silent = TRUE))
      }
    }
    suppressWarnings(try(parallel::stopCluster(Cluster), silent = TRUE))
  }, add = TRUE, after = FALSE)
  # Bootstrap before deserializing a closure that refers to the gmsp namespace.
  Initialize <- eval(quote(function(paths, directory, verbose) {
    .libPaths(paths)
    setwd(directory)
    options(verbose)
    Sys.getpid()
  }), envir = baseenv())
  Pids <- unlist(parallel::clusterCall(
    Cluster, Initialize,
    paths = unique(c(dirname(getNamespaceInfo(asNamespace("gmsp"), "path")),
                     .libPaths())),
    directory = getwd(), verbose = options("gmsp.verbose")), use.names = FALSE)
  n <- length(jobs)
  OUT <- vector("list", n)
  LastLog <- proc.time()[["elapsed"]]
  for (i in seq.int(1L, n, by = workers)) {
    IDX <- seq.int(i, min(i + workers - 1L, n))
    OUT[IDX] <- parallel::clusterApply(Cluster, jobs[IDX], worker)
    Now <- proc.time()[["elapsed"]]
    if (Now - LastLog >= logEvery) {
      .projectMessage(label, " progress: done=", max(IDX), "/", n,
        " active=0 queued=", n - max(IDX), operation = label,
        phase = "processing", count = max(IDX), total = n)
      LastLog <- Now
    }
  }
  Complete <- TRUE
  OUT
}
