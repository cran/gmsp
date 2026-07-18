KEY.record <- c("RecordID", "OwnerID", "EventID", "StationID")

.requireCols <- function(data, cols, label = "table") {
  COLS <- setdiff(cols, names(data))
  if (length(COLS)) {
    stop(sprintf("%s missing required columns: %s",
                 label, paste(COLS, collapse = ", ")),
         call. = FALSE)
  }
  invisible(NULL)
}

.readIndexTable <- function(path, name, owner, required = character()) {
  FILE <- file.path(path, sprintf("%s.%s.csv", name, owner))
  if (!file.exists(FILE)) {
    stop(sprintf("Missing index file: %s", FILE), call. = FALSE)
  }
  DT <- fread(FILE, showProgress = FALSE)
  .requireCols(DT, required, sprintf("%s.%s.csv", name, owner))
  DT[]
}

.masterPath <- function(path, owner = NULL) {
  if (is.null(owner)) return(file.path(path, "RawMasterIndex.csv"))
  file.path(path, sprintf("MasterIndex.%s.csv", owner))
}

.tableHeaders <- function(path, name) {
  FILES <- sort(list.files(path, pattern = sprintf("^%s[.].+[.]csv$", name),
                           full.names = TRUE))
  if (!length(FILES)) {
    stop(sprintf("Missing %s owner tables in %s.", name, path), call. = FALSE)
  }
  lapply(FILES, function(FILE) names(fread(FILE, nrows = 0L,
                                           showProgress = FALSE)))
}

.commonCols <- function(path, name, owners, required = character()) {
  FILES <- file.path(path, sprintf("%s.%s.csv", name, owners))
  MISS <- FILES[!file.exists(FILES)]
  if (length(MISS)) {
    stop(sprintf("Missing %s files: %s",
                 name, paste(MISS, collapse = ", ")),
         call. = FALSE)
  }
  LIST <- lapply(FILES, function(FILE) {
    names(fread(FILE, nrows = 0L, showProgress = FALSE))
  })
  MISS <- rbindlist(lapply(seq_along(FILES), function(i) {
    COLS <- setdiff(required, LIST[[i]])
    if (!length(COLS)) return(NULL)
    data.table(FILE = FILES[i], COL = COLS)
  }), use.names = TRUE)
  if (nrow(MISS)) {
    MSG <- MISS[, sprintf("%s: %s", basename(FILE), paste(COL, collapse = ", ")),
                by = FILE]$V1
    stop(sprintf("%s missing required columns: %s",
                 name, paste(MSG, collapse = "; ")),
         call. = FALSE)
  }
  Reduce(intersect, LIST)
}

.eventOptionalCols <- function() {
  Sources <- c("USGS", "ISC")
  as.vector(outer(
    c(
      "EventID",
      "EventTimeUTC",
      "EventLatitude",
      "EventLongitude",
      "EventDepth",
      "EventMagnitude",
      "EventMagnitudeType"
    ),
    Sources,
    paste,
    sep = "."
  ))
}

.eventMaster <- function(data, cols) {
  DT <- copy(data)
  COLS <- intersect(cols, .eventOptionalCols())
  for (Col in setdiff(COLS, names(DT))) DT[, (Col) := NA_character_]
  Sources <- c("USGS", "ISC", "owner")
  DT[, `:=`(
    EventMagnitude = .coalesceColumns(.SD, paste0("EventMagnitude.", Sources)),
    EventMagnitudeType = .coalesceColumns(
      .SD,
      paste0("EventMagnitudeType.", Sources),
      NA_character_
    ),
    EventDepth = .coalesceColumns(.SD, paste0("EventDepth.", Sources)),
    EventLatitude = .coalesceColumns(.SD, paste0("EventLatitude.", Sources)),
    EventLongitude = .coalesceColumns(.SD, paste0("EventLongitude.", Sources))
  )]

  cols <- unique(c(cols, "EventMagnitude", "EventMagnitudeType", "EventDepth",
                   "EventLatitude", "EventLongitude"))
  .requireCols(DT, cols = cols, label = "EventTable")
  unique(DT[, ..cols],
         by = c("OwnerID", "EventID"))
}

.stationMaster <- function(data, cols) {
  .requireCols(data, cols = cols, label = "StationTable")
  unique(data[, ..cols],
         by = c("OwnerID", "StationID"))
}

.recordMaster <- function(.x) {
  DT <- copy(.x)
  setnames(DT, c("NP", "Fs"), c("NP.record", "Fs.record"),
           skip_absent = TRUE)
  DT[]
}

.orderMaster <- function(.x) {
  DT <- copy(.x)
  Order <- c(
    "OwnerID", "EventID", "EventID.USGS", "EventID.ISC", "EventName", "StationID",
    "NetworkID", "RecordID", "ProcessID", "DIR", "OCID",
    "StationCount", "EventCount",
    "EventTimeUTC.owner", "EventTimeUTC.USGS", "EventTimeUTC.ISC",
    "EventMagnitude", "EventMagnitudeType", "EventDepth",
    "EventLatitude", "EventLongitude",
    "EventLatitude.owner", "EventLongitude.owner", "EventDepth.owner",
    "EventMagnitude.owner", "EventMagnitudeType.owner",
    "EventLatitude.USGS", "EventLongitude.USGS", "EventDepth.USGS",
    "EventMagnitude.USGS", "EventMagnitudeType.USGS",
    "EventLatitude.ISC", "EventLongitude.ISC", "EventDepth.ISC",
    "EventMagnitude.ISC", "EventMagnitudeType.ISC",
    "EventMechanism.owner", "EventMechanism.STREC",
    "EventTectonicRegion.owner", "EventTectonicRegion.STREC",
    "EventTectonicRegion.inferred",
    "StationLatitude", "StationLongitude", "Vs30.owner", "Vs30.USGS",
    "Vs30.neighbors", "StationVs30",
    "Repi", "Rhyp", "NP.record", "Fs.record", "pad"
  )
  setcolorder(DT, c(intersect(Order, names(DT)), setdiff(names(DT), Order)))
  setorderv(DT, intersect(c("OwnerID", "EventID", "StationID", "RecordID",
                            "ProcessID", "DIR", "OCID"), names(DT)),
            na.last = TRUE)
  DT[]
}

.addCols <- function(.x, aux, by) {
  COLS <- setdiff(names(aux), by)
  .x[aux, on = by, (COLS) := mget(paste0("i.", COLS))]
  .x[]
}

buildMaster <- function(path, owners, processID = "raw") {
  owners <- unique(as.character(owners))
  if (!length(owners) || any(!nzchar(owners))) {
    stop("owners must be a non-empty character vector.", call. = FALSE)
  }
  path <- path.expand(path)
  processID <- as.character(processID)
  if (length(processID) != 1L || is.na(processID) || !nzchar(processID)) {
    stop("processID must be one non-empty string.", call. = FALSE)
  }
  EventCols <- .commonCols(path, "EventTable", owners,
                           required = c("OwnerID", "EventID"))
  EventCols <- unique(c(EventCols, .eventOptionalCols()))
  StationCols <- .commonCols(path, "StationTable", owners,
                             required = c("OwnerID", "StationID"))

  LIST <- lapply(owners, function(Owner) {
    DT <- copy(.readIndexTable(path, "RawIntensityTable", Owner,
                               required = "RecordID"))
    DT[, ProcessID := processID]

    AUX <- .recordMaster(.readIndexTable(path, "RawRecordTable", Owner,
                                         required = KEY.record))
    .addCols(DT, AUX, by = "RecordID")

    AUX <- .eventMaster(.readIndexTable(path, "EventTable", Owner,
                                        required = c("OwnerID", "EventID")),
                        cols = EventCols)
    .addCols(DT, AUX, by = c("OwnerID", "EventID"))

    AUX <- .stationMaster(.readIndexTable(path, "StationTable", Owner,
                                          required = c("OwnerID", "StationID")),
                          cols = StationCols)
    .addCols(DT, AUX, by = c("OwnerID", "StationID"))

    DT[, Repi := .haversineKm(EventLatitude, EventLongitude,
                              StationLatitude, StationLongitude)]
    DT[, Rhyp := sqrt(Repi^2 + EventDepth^2)]
    .orderMaster(DT)
  })

  Names <- lapply(LIST, names)
  OK <- vapply(Names, identical, logical(1L), Names[[1L]])
  if (!all(OK)) {
    stop("Owner master tables produced inconsistent columns.", call. = FALSE)
  }
  rbindlist(LIST, use.names = TRUE)
}

writeMaster <- function(path, owners, processID = "raw", master = TRUE) {
  path <- path.expand(path)
  owners <- unique(as.character(owners))
  DT <- buildMaster(path = path, owners = owners, processID = processID)
  LIST <- lapply(owners, function(Owner) {
    OUT <- DT[OwnerID == Owner]
    fwrite(OUT, .masterPath(path, Owner))
    OUT
  })
  if (isTRUE(master)) fwrite(.orderMaster(DT), .masterPath(path))
  invisible(LIST)
}

readMaster <- function(path, owners, processID = "raw") {
  path <- path.expand(path)
  FILES <- .masterPath(path, owners)
  if (!all(file.exists(FILES))) {
    stop(sprintf("Missing MasterIndex files: %s",
                 paste(FILES[!file.exists(FILES)], collapse = ", ")),
         call. = FALSE)
  }
  DT <- rbindlist(lapply(FILES, fread, showProgress = FALSE),
                  use.names = TRUE)
  if ("ProcessID" %chin% names(DT)) {
    DT <- DT[ProcessID == processID]
  }
  .orderMaster(DT)
}
