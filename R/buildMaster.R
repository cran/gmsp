#' Build the master record table.
#'
#' Two-stage hydration, per owner:
#'
#' **Stage 1D** (one row per record): inner-joins
#'   `RawRecordTable.<O>.csv` with `EventTable.<O>.csv` on `EventID`
#'   and `StationTable.<O>.csv` on `StationID`. Computes record-level
#'   scalars: `Repi` (haversine, km) and `Rhyp`
#'   (`sqrt(Repi^2 + EventDepth^2)`, km). Future record-level
#'   enrichments (geotechnical depth proxies, etc.) belong in this
#'   stage.
#'
#' **Stage 3D** (three rows per record, one per `DIR`): inner-joins
#'   the Stage-1D output with `RawIntensityTable.<O>.csv` on
#'   `RecordID`. Brings per-direction intensities (PGA, AI, ARMS,
#'   ...), plus per-direction `NP`, `Fs`, `dt`.
#'
#' Event-level scalars (mag, depth, lat/lon, time, mechanism, region)
#' are consolidated by source precedence: `*.owner` > `*.USGS` >
#' `*.ISC` (and `*.STREC` for mechanism/region). `StationVs30` is
#' already consolidated upstream in `StationTable`.
#'
#' Required input files per owner under `path`:
#' `RawRecordTable.<OwnerID>.csv` (drives the owner list),
#' `EventTable.<OwnerID>.csv`, `StationTable.<OwnerID>.csv`,
#' `RawIntensityTable.<OwnerID>.csv`. The function errors if any of
#' the joined CSVs is missing; the canonical way to obtain them is to
#' run `buildRawRecordTable()`, `buildRawIntensityTable()`, and the
#' provider-specific event / station table builders.
#'
#' @param path Absolute path to the index root holding the per-owner CSVs.
#'   Required -- no default.
#' @param owners   character vector of OwnerIDs. `NULL` = every owner with a
#'                 `RawRecordTable.<O>.csv` present in `path`.
#' @return `data.table` keyed at the (`RecordID`, `DIR`) level.
#' @examples
#' index <- file.path(tempdir(), "gmsp-master-example")
#' unlink(index, recursive = TRUE)
#' dir.create(index)
#' data.table::fwrite(
#'   data.table::data.table(RecordID = "R1", EventID = "E1",
#'                          StationID = "S1", OwnerID = "AAA",
#'                          NP = 101, Fs = 100, pad = 0),
#'   file.path(index, "RawRecordTable.AAA.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(
#'     EventID = "E1",
#'     EventMagnitude.owner = 5.5, EventMagnitude.USGS = 5.4,
#'     EventMagnitude.ISC = 5.3,
#'     EventMagnitudeType.owner = "Mw", EventMagnitudeType.USGS = "Mw",
#'     EventMagnitudeType.ISC = "Mw",
#'     EventDepth.owner = 10, EventDepth.USGS = 11, EventDepth.ISC = 12,
#'     EventLatitude.owner = 0, EventLatitude.USGS = 0,
#'     EventLatitude.ISC = 0,
#'     EventLongitude.owner = 0, EventLongitude.USGS = 0,
#'     EventLongitude.ISC = 0,
#'     EventTimeUTC.owner = "2026-01-01T00:00:00Z",
#'     EventTimeUTC.USGS = "2026-01-01T00:00:00Z",
#'     EventTimeUTC.ISC = "2026-01-01T00:00:00Z",
#'     EventMechanism.owner = "SS", EventMechanism.STREC = "SS",
#'     EventTectonicRegion.owner = "active",
#'     EventTectonicRegion.STREC = "active"
#'   ),
#'   file.path(index, "EventTable.AAA.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(StationID = "S1", StationLatitude = 0.1,
#'                          StationLongitude = 0.1, StationVs30 = 760),
#'   file.path(index, "StationTable.AAA.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(RecordID = "R1", DIR = "H1",
#'                          OCID = "H1", PGA = 1),
#'   file.path(index, "RawIntensityTable.AAA.csv")
#' )
#' buildMaster(index, owners = "AAA")
#'
#' @importFrom data.table fread rbindlist fcoalesce fcase
#' @export
buildMaster <- function(path, owners = NULL) {
  path <- path.expand(path)
  if (is.null(owners)) {
    FILES  <- list.files(path, pattern = "^RawRecordTable\\..+\\.csv$")
    owners <- sub("^RawRecordTable\\.([^.]+)\\.csv$", "\\1", FILES)
  }
  LIST <- lapply(owners, joinOwner, path = path)
  LIST <- LIST[!vapply(LIST, is.null, logical(1L))]
  if (!length(LIST)) return(data.table::data.table())
  .assertMasterSchema(LIST)
  rbindlist(LIST, use.names = TRUE)
}

#' @noRd
.haversine <- function(lat1, lon1, lat2, lon2) {
  RadiusKm <- 6371.0088
  Phi1 <- lat1 * pi / 180
  Phi2 <- lat2 * pi / 180
  DPhi <- (lat2 - lat1) * pi / 180
  DLam <- (lon2 - lon1) * pi / 180
  Aux  <- sin(DPhi / 2)^2 + cos(Phi1) * cos(Phi2) * sin(DLam / 2)^2
  2 * RadiusKm * asin(sqrt(Aux))
}

#' @noRd
joinOwner <- function(OwnerID, path) {
  Records  <- fread(file.path(path, sprintf("RawRecordTable.%s.csv", OwnerID)))
  if (nrow(Records) == 0L) return(NULL)
  Records[, c("NP", "Fs", "pad") := NULL]

  Events   <- fread(file.path(path, sprintf("EventTable.%s.csv",   OwnerID)))
  Stations <- fread(file.path(path, sprintf("StationTable.%s.csv", OwnerID)))

  COLS.numeric <- c("EventMagnitude.owner", "EventMagnitude.USGS", "EventMagnitude.ISC",
                    "EventDepth.owner",     "EventDepth.USGS",     "EventDepth.ISC",
                    "EventLatitude.owner",  "EventLatitude.USGS",  "EventLatitude.ISC",
                    "EventLongitude.owner", "EventLongitude.USGS", "EventLongitude.ISC")
  COLS.string  <- c("EventMagnitudeType.owner", "EventMagnitudeType.USGS", "EventMagnitudeType.ISC",
                    "EventTimeUTC.owner",       "EventTimeUTC.USGS",       "EventTimeUTC.ISC",
                    "EventMechanism.owner",     "EventMechanism.STREC",
                    "EventTectonicRegion.owner","EventTectonicRegion.STREC")
  Events[, (COLS.numeric) := lapply(.SD, as.numeric),   .SDcols = COLS.numeric]
  Events[, (COLS.string)  := lapply(.SD, as.character), .SDcols = COLS.string]

  Events[, EventMagnitude          := fcoalesce(EventMagnitude.owner, EventMagnitude.USGS, EventMagnitude.ISC)]
  Events[, `EventMagnitude.source` := fcase(!is.na(EventMagnitude.owner), "owner",
                                            !is.na(EventMagnitude.USGS),  "USGS",
                                            !is.na(EventMagnitude.ISC),   "ISC",
                                            default = "none")]
  Events[, MagnitudeType       := fcoalesce(EventMagnitudeType.owner, EventMagnitudeType.USGS, EventMagnitudeType.ISC)]
  Events[, EventDepth          := fcoalesce(EventDepth.owner,     EventDepth.USGS,     EventDepth.ISC)]
  Events[, EventLatitude       := fcoalesce(EventLatitude.owner,  EventLatitude.USGS,  EventLatitude.ISC)]
  Events[, EventLongitude      := fcoalesce(EventLongitude.owner, EventLongitude.USGS, EventLongitude.ISC)]
  Events[, EventTimeUTC        := fcoalesce(EventTimeUTC.owner,   EventTimeUTC.USGS,   EventTimeUTC.ISC)]
  Events[, EventMechanism      := fcoalesce(EventMechanism.owner,      EventMechanism.STREC)]
  Events[, EventTectonicRegion := fcoalesce(EventTectonicRegion.owner, EventTectonicRegion.STREC)]

  COLS.event   <- c("EventID", "EventMagnitude", "EventMagnitude.source",
                    "MagnitudeType", "EventDepth", "EventLatitude",
                    "EventLongitude", "EventTimeUTC", "EventMechanism",
                    "EventTectonicRegion")
  COLS.station <- c("StationID", "StationLatitude", "StationLongitude", "StationVs30")

  # Stage 1D: one row per record. Apply all record-level scalars here so they
  # are not recomputed three times in stage 3D.
  DT <- Records[Events[, ..COLS.event],     on = "EventID",   nomatch = 0L]
  DT <- DT[Stations[, ..COLS.station],      on = "StationID", nomatch = 0L]
  DT[, Repi := .haversine(EventLatitude, EventLongitude,
                          StationLatitude, StationLongitude)]
  DT[, Rhyp := sqrt(Repi^2 + EventDepth^2)]

  # Stage 3D: fan out to three rows per record (DIR = H1/H2/UP) via RecordID.
  Intensities <- fread(file.path(path, sprintf("RawIntensityTable.%s.csv", OwnerID)))
  DT[Intensities, on = "RecordID", nomatch = 0L]
}

#' @noRd
.assertMasterSchema <- function(LIST) {
  if (is.null(names(LIST))) {
    names(LIST) <- sprintf("owner%d", seq_along(LIST))
  }
  SCHEMA <- lapply(LIST, function(DT) {
    if (!data.table::is.data.table(DT)) {
      stop("buildMaster() contains a non-data.table before binding",
           call. = FALSE)
    }
    paste(names(DT),
          vapply(DT, function(col) paste(class(col), collapse = "/"),
                 character(1L)),
          sep = ":")
  })
  SAME <- vapply(SCHEMA, identical, logical(1L), SCHEMA[[1L]])
  if (!all(SAME)) {
    stop(sprintf("buildMaster() owner schema mismatch before binding: %s",
                 paste(names(SAME)[!SAME], collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}
