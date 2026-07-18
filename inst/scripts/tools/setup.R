# scripts/tools/setup.R

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

SCRIPT.root <- Sys.getenv("GMSP_SCRIPT_ROOT", unset = "")
if (!nzchar(SCRIPT.root)) {
  stop("GMSP_SCRIPT_ROOT must point to gmsp installed scripts.", call. = FALSE)
}
SCRIPT.root <- normalizePath(SCRIPT.root, winslash = "/", mustWork = TRUE)
PROJECT.root <- Sys.getenv("GMSP_PROJECT_ROOT", unset = getwd())
PROJECT.root <- normalizePath(PROJECT.root, winslash = "/", mustWork = FALSE)

.scriptHere <- function(...) {
  file.path(SCRIPT.root, ...)
}

.projectHere <- function(...) {
  file.path(PROJECT.root, ...)
}

source(.scriptHere("tools", "json.R"))
source(.scriptHere("tools", "recordAccess.R"))

.log <- function(fmt, ...) {
  message(sprintf(fmt, ...))
}

.resolvePath <- function(path) {
  path <- as.character(path)
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Path must be one non-empty string.", call. = FALSE)
  }
  if (grepl("^(/|~)", path)) return(path.expand(path))
  .projectHere(path)
}

.requireColumns <- function(DT, cols, label) {
  Missing <- setdiff(cols, names(DT))
  if (length(Missing)) {
    stop(sprintf("%s missing columns: %s",
                 label, paste(Missing, collapse = ", ")),
         call. = FALSE)
  }
  invisible(NULL)
}

.readCharacterVector <- function(x, label, allowNull = FALSE) {
  if (is.null(x) && allowNull) return(character())
  if (allowNull && !length(x)) return(character())
  Value <- as.character(x)
  if (!length(Value) || any(is.na(Value) | !nzchar(Value))) {
    stop(sprintf("%s must be a non-empty character vector.", label),
         call. = FALSE)
  }
  unique(Value)
}

.readScalarLogical <- function(x, label, default = FALSE) {
  if (is.null(x)) return(default)
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("%s must be TRUE or FALSE.", label), call. = FALSE)
  }
  x
}

.readScalarNumber <- function(x, label) {
  Value <- as.numeric(x)
  if (length(Value) != 1L || is.na(Value)) {
    stop(sprintf("%s must be one number.", label), call. = FALSE)
  }
  Value
}

.mkdir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.ensureDir <- function(path) {
  .mkdir(path)
}

.workRoot <- function(path) {
  file.path(path, ".work")
}

.workPath <- function(path, recordID) {
  file.path(.workRoot(path), recordID)
}

.clearGeneratedDir <- function(path, root) {
  Path <- sub("/+$", "", normalizePath(path, winslash = "/", mustWork = FALSE))
  Root <- sub("/+$", "", normalizePath(root, winslash = "/", mustWork = FALSE))
  if (Path == Root || !startsWith(Path, paste0(Root, "/"))) {
    stop(sprintf("Refusing to unlink outside generated root: %s", path),
         call. = FALSE)
  }
  unlink(Path, recursive = TRUE)
}

.unlinkGeneratedDir <- function(path, root) {
  .clearGeneratedDir(path = path, root = root)
}

.range <- function(x) {
  if (is.list(x)) {
    x <- vapply(x, function(v) {
      if (is.null(v) || !length(v)) return(NA_real_)
      as.numeric(unlist(v)[1L])
    }, numeric(1L))
  } else {
    x <- as.numeric(x)
  }
  if (length(x) != 2L) stop("Range filters must have length 2.", call. = FALSE)
  if (is.na(x[1L])) x[1L] <- -Inf
  if (is.na(x[2L])) x[2L] <-  Inf
  if (x[1L] > x[2L]) x <- rev(x)
  x
}

.haversineKm <- function(lat1, lon1, lat2, lon2) {
  RadiusKm <- 6371.0088
  Phi1 <- lat1 * pi / 180
  Phi2 <- lat2 * pi / 180
  DPhi <- (lat2 - lat1) * pi / 180
  DLam <- (lon2 - lon1) * pi / 180
  Aux <- sin(DPhi / 2)^2 + cos(Phi1) * cos(Phi2) * sin(DLam / 2)^2
  2 * RadiusKm * asin(sqrt(Aux))
}

.coalesceColumns <- function(DT, cols, default = NA_real_) {
  Cols <- intersect(cols, names(DT))
  if (!length(Cols)) return(rep(default, nrow(DT)))
  Values <- lapply(Cols, function(Column) {
    if (is.character(default)) return(as.character(DT[[Column]]))
    as.numeric(DT[[Column]])
  })
  do.call(data.table::fcoalesce, Values)
}

.writeRecordStatus <- function(path, row, status, extra = list()) {
  OUT <- list(
    status = status,
    RecordID = as.character(row$RecordID[1L]),
    OwnerID = as.character(row$OwnerID[1L]),
    EventID = as.character(row$EventID[1L]),
    StationID = as.character(row$StationID[1L])
  )
  OUT[names(extra)] <- extra
  jsonlite::write_json(OUT, file.path(path, "status.json"),
                       auto_unbox = TRUE, pretty = TRUE)
}

.recordDone <- function(path, files) {
  FILE <- file.path(path, "status.json")
  if (!file.exists(FILE)) return(FALSE)
  if (!all(file.exists(file.path(path, files)))) return(FALSE)
  Status <- tryCatch(.readJson(FILE, simplifyVector = TRUE),
                     error = function(e) NULL)
  is.list(Status) && identical(Status[["status"]], "done")
}

.recordManifest <- function(path, selection, keys) {
  Files <- list.files(.workRoot(path), pattern = "^status[.]json$",
                      recursive = TRUE, full.names = TRUE)
  if (!length(Files)) stop("No record checkpoints found.", call. = FALSE)
  Manifest <- rbindlist(lapply(Files, function(FILE) {
    Status <- .readJson(FILE, simplifyVector = TRUE)
    data.table(
      RecordID = as.character(Status[["RecordID"]]),
      OwnerID = as.character(Status[["OwnerID"]]),
      EventID = as.character(Status[["EventID"]]),
      StationID = as.character(Status[["StationID"]]),
      status = as.character(Status[["status"]]),
      step = as.character(if (is.null(Status[["step"]])) NA_character_ else Status[["step"]]),
      error = as.character(if (is.null(Status[["error"]])) NA_character_ else Status[["error"]]),
      seconds = as.numeric(if (is.null(Status[["seconds"]])) NA_real_ else Status[["seconds"]]),
      path = dirname(FILE))
  }), use.names = TRUE)
  AUX <- copy(selection[, keys, with = FALSE])
  AUX[, (keys) := lapply(.SD, as.character), .SDcols = keys]
  AUX[, RecordOrder := .I]
  Manifest[AUX, on = keys, RecordOrder := i.RecordOrder]
  Manifest <- Manifest[!is.na(RecordOrder)]
  setorder(Manifest, RecordOrder, RecordID)
  Manifest[]
}

.buildManifest <- function(path, selection, keys = NULL) {
  if (is.null(keys)) {
    keys <- get("KEY.record", envir = parent.frame(), inherits = TRUE)
  }
  .recordManifest(path = path, selection = selection, keys = keys)
}

.writeCheckpointTables <- function(path, manifest, files) {
  Done <- manifest[status == "done"]
  if (!nrow(Done)) stop("No completed checkpoints found.", call. = FALSE)
  fwrite(manifest, file.path(path, "manifest.csv"))
  for (File in files) {
    Files <- file.path(Done$path, File)
    Missing <- Files[!file.exists(Files)]
    if (length(Missing)) {
      stop(sprintf("Missing checkpoint file %s in %s", File,
                   paste(dirname(Missing), collapse = ", ")),
           call. = FALSE)
    }
    fwrite(rbindlist(lapply(Files, fread), use.names = TRUE),
           file.path(path, File))
  }
  invisible(NULL)
}

.writeAggregateTables <- function(path, manifest, files) {
  .writeCheckpointTables(path = path, manifest = manifest, files = files)
}

.runRecordsSequential <- function(DT, path, params, override, files,
                                  processRecord, label = "record") {
  OUT <- vector("list", nrow(DT))
  for (i in seq_len(nrow(DT))) {
    Row <- DT[i]
    RecordID <- as.character(Row$RecordID[1L])
    .log("%s %s/%s %s", label, i, nrow(DT), RecordID)
    OUT[[i]] <- processRecord(row = Row, path = path, params = params,
                              override = override)
    if (is.list(OUT[[i]]) && identical(OUT[[i]][["status"]], "failed")) {
      .log("%s %s/%s %s failed: %s", label, i, nrow(DT), RecordID,
           OUT[[i]][["error"]])
    }
  }
  invisible(OUT)
}

.runRecordsParallel <- function(DT, path, params, override, files,
                                processRecord, workers, log.every = 30,
                                label = "record") {
  n <- nrow(DT)
  if (!n) return(invisible(data.table()))
  workers <- max(1L, min(as.integer(workers), n))
  if (workers <= 1L) {
    return(.runRecordsSequential(DT = DT, path = path, params = params,
                                 override = override, files = files,
                                 processRecord = processRecord, label = label))
  }

  Queue <- seq_len(n)
  Active <- list()
  OUT <- vector("list", n)
  Done <- 0L
  LastLog <- proc.time()[["elapsed"]]

  launch <- function(i) {
    Row <- DT[i]
    parallel::mcparallel(
      processRecord(row = Row, path = path, params = params,
                    override = override),
      name = as.character(i),
      silent = FALSE)
  }

  while (Done < n) {
    while (length(Queue) && length(Active) < workers) {
      i <- Queue[1L]
      Queue <- Queue[-1L]
      Active[[as.character(i)]] <- launch(i)
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
    if ((Now - LastLog) >= log.every) {
      .log("%s progress: done=%s/%s active=%s queued=%s",
           label, Done, n, length(Active), length(Queue))
      LastLog <- Now
    }
    if (Done < n) Sys.sleep(0.25)
  }

  invisible(OUT)
}
