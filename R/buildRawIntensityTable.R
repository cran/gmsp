# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Build raw acceleration intensity indexes
#'
#' Measures raw AT records and writes `RawIntensityTable.<OwnerID>.csv`,
#' with one row per RecordID and DIR and the provider OCID retained.
#'
#' @param pathRecords Records root containing owner, event and station folders.
#' @param pathIndex Existing destination folder for the index files.
#' @param owners Owner identifiers; NULL scans immediate directories of
#'   pathRecords.
#' @param incremental Reuses records already present in the index when TRUE.
#' @param force Ignores the incremental cache when TRUE.
#' @return An invisible named list of row counts for the owners written.
#' @details The index contains RecordID, DIR, OCID, AI, AId, AIu, ARMS, ATn,
#'   ATo, AZC, CAV, CAV5, D0575, D0595, D2080, Dmax, EPI, Fs, NP, PDI,
#'   PGA, TmA and dt, ordered by RecordID and DIR. Values use a millimetre
#'   length basis. Duration and period fields use one decimal place and dt
#'   uses four; the remaining measure fields are rounded to integers.
#'
#' VT and DT stations are omitted with a warning naming the station and
#' quantity. A station failure retains its cause and path. Valid rows are
#' published; an error of class modulePartialPublication then carries
#' failures, the partial result and destination path. Existing files are
#' replaced when the owner has new intensity rows.
#' @examples
#' Root <- tempfile("buildRawIntensityTable")
#' Raw <- file.path(Root, "records", "NGAW", "E1", "S1", "raw")
#' Index <- file.path(Root, "index")
#' dir.create(Raw, recursive = TRUE)
#' dir.create(Index)
#' Time <- seq(0, 5, by = 0.01)
#' Envelope <- sin(pi * Time / max(Time))^2
#' data.table::fwrite(data.table::data.table(
#'   HNE = 1200 * Envelope * sin(2 * pi * 1.5 * Time),
#'   HNN = 800 * Envelope * sin(2 * pi * 2.5 * Time),
#'   HNZ = 400 * Envelope * sin(2 * pi * 4 * Time)), file.path(Raw, "AT.R1.csv"))
#' jsonlite::write_json(list(RecordID = "R1", DIR = c("H1", "H2", "UP"),
#'   OCID = c("HNE", "HNN", "HNZ"), dt = 0.01), file.path(Raw, "AT.R1.json"),
#'   auto_unbox = TRUE)
#' Counts <- buildRawIntensityTable(file.path(Root, "records"), pathIndex = Index)
#' str(Counts)
#' Intensity <- data.table::fread(file.path(Index, "RawIntensityTable.NGAW.csv"))
#' print(Intensity[, c("RecordID", "DIR", "OCID", "PGA", "AI", "D0595")])
#' unlink(Root, recursive = TRUE)
#' @export
buildRawIntensityTable <- function(pathRecords, pathIndex, owners = NULL,
                                   incremental = TRUE, force = FALSE) {
  for (Value in list(incremental, force)) {
    if (!is.logical(Value) || length(Value) != 1L || is.na(Value)) {
      .stopUsage("incremental and force must be scalar logical values.",
                 operation = "buildRawIntensityTable")
    }
  }
  pathRecords <- path.expand(pathRecords)
  pathIndex <- path.expand(pathIndex)
  if (is.null(owners)) owners <- basename(list.dirs(pathRecords, recursive = FALSE))
  OUT <- list()
  Failures <- list()
  for (Owner in owners) {
    DIR <- list.dirs(file.path(pathRecords, Owner), recursive = FALSE)
    DIR <- unlist(lapply(DIR, list.dirs, recursive = FALSE), use.names = FALSE)
    if (!length(DIR)) next
    FILE <- file.path(pathIndex, paste0("RawIntensityTable.", Owner, ".csv"))
    Existing <- NULL
    if (incremental && !force && file.exists(FILE)) {
      Existing <- tryCatch(.canonicalRawIntensityTable(fread(FILE), context = FILE),
        error = function(e) {
          Failures[[FILE]] <<- list(operation = "buildRawIntensityTable", unit = Owner,
                                    path = FILE, cause = e)
          NULL
        })
      if (is.null(Existing)) next
    }
    if (!is.null(Existing)) {
      Done <- unique(Existing$RecordID)
      AUX <- vapply(DIR, function(path) {
        FILES <- list.files(file.path(path, "raw"), pattern = "^(AT|VT|DT)\\..+\\.json$")
        if (length(FILES) == 1L) return(sub("^(AT|VT|DT)\\.(.+)\\.json$", "\\2", FILES))
        NA_character_
      }, character(1L))
      DIR <- DIR[!AUX %in% Done]
    }
    if (!length(DIR)) next
    LIST <- lapply(DIR, function(path) {
      tryCatch({
        DT <- .measureRawIntensities(path)
        if (is.null(DT)) return(NULL)
        .canonicalRawIntensityTable(DT, context = path)
      }, error = function(e) {
        Failures[[path]] <<- list(operation = "buildRawIntensityTable", unit = basename(path),
                                  path = path, cause = e)
        NULL
      })
    })
    LIST <- Filter(Negate(is.null), LIST)
    if (!length(LIST)) next
    DT <- unique(rbindlist(LIST, use.names = TRUE))
    if (!nrow(DT)) next
    if (!is.null(Existing)) {
      DT <- rbind(Existing[!RecordID %in% DT$RecordID], DT, use.names = TRUE)
    }
    setorder(DT, RecordID, DIR)
    tryCatch({
      fwrite(DT, FILE, eol = "\n")
      OUT[[Owner]] <- nrow(DT)
    }, error = function(e) {
      Failures[[FILE]] <<- list(operation = "buildRawIntensityTable", unit = Owner,
                                path = FILE, cause = e)
    })
  }
  if (length(Failures)) .stopPartialPublication(
    "Raw intensity index has failures at: ", paste(names(Failures), collapse = ", "),
    operation = "buildRawIntensityTable", failures = Failures, result = OUT, path = pathIndex)
  invisible(OUT)
}

.canonicalRawIntensityTable <- function(data, context) {
  COLS <- c("RecordID", "DIR", "OCID", "AI", "AId", "AIu", "ARMS", "ATn", "ATo", "AZC",
            "CAV", "CAV5", "D0575", "D0595", "D2080", "Dmax", "EPI", "Fs", "NP", "PDI",
            "PGA", "TmA", "dt")
  if (!setequal(names(data), COLS) || anyDuplicated(names(data))) {
    stop("Raw intensity schema mismatch at ", context, ": missing=",
         paste(setdiff(COLS, names(data)), collapse = ", "), "; extra=",
         paste(setdiff(names(data), COLS), collapse = ", "), call. = FALSE)
  }
  setcolorder(data, COLS)
  COLS <- c("RecordID", "DIR", "OCID")
  data[, (COLS) := lapply(.SD, as.character), .SDcols = COLS]
  COLS <- c("AI", "AId", "AIu", "ARMS", "ATn", "ATo", "AZC", "CAV", "CAV5",
            "EPI", "Fs", "NP", "PDI", "PGA")
  data[, (COLS) := lapply(.SD, as.integer), .SDcols = COLS]
  COLS <- c("D0575", "D0595", "D2080", "Dmax", "TmA", "dt")
  data[, (COLS) := lapply(.SD, as.numeric), .SDcols = COLS]
  data[]
}

.measureRawIntensities <- function(path) {
  DIR <- file.path(path.expand(path), "raw")
  CSV <- list.files(DIR, pattern = "^(AT|VT|DT)\\..+\\.csv$", full.names = TRUE)
  JSON <- list.files(DIR, pattern = "^(AT|VT|DT)\\..+\\.json$", full.names = TRUE)
  if (!length(CSV) && !length(JSON)) return(NULL)
  if (length(CSV) != 1L || length(JSON) != 1L ||
      sub("\\.csv$", ".json", CSV) != JSON) {
    stop("Expected one matching raw CSV/JSON pair at ", path, call. = FALSE)
  }
  Quantity <- sub("^([ADV]T)\\..+$", "\\1", basename(CSV))
  if (Quantity != "AT") {
    .warnSignal("Skipping ", path, " with quantity ", Quantity,
                 "; the raw intensity index contains AT records.", operation = "buildRawIntensityTable")
    return(NULL)
  }
  RecordID <- sub("^AT\\.(.+)\\.csv$", "\\1", basename(CSV))
  AUX <- .readRawPair(CSV, key = list(RecordID = RecordID))
  Metadata <- AUX$metadata
  DT <- AUX$data
  if (is.null(Metadata$DIR) || is.null(Metadata$OCID) ||
      length(Metadata$DIR) != length(Metadata$OCID)) {
    stop("Raw sidecar must contain matching DIR and OCID arrays.", call. = FALSE)
  }
  Map <- data.table(DIR = as.character(Metadata$DIR), OCID = as.character(Metadata$OCID))
  if (anyNA(Map) || anyDuplicated(Map$DIR) || anyDuplicated(Map$OCID) ||
      !setequal(Map$DIR, c("H1", "H2", "UP")) || !setequal(names(DT), Map$OCID)) {
    stop("Raw CSV columns and sidecar DIR/OCID mapping disagree.", call. = FALSE)
  }
  DT[, t := (seq_len(.N) - 1L) * Metadata$dt]
  DT <- melt(DT, id.vars = "t", variable.name = "OCID", value.name = "s",
              variable.factor = FALSE)
  DT[, `:=`(RecordID = RecordID, ID = "AT")]
  DT <- measureIntensitiesTSL(DT, by = "RecordID", unitsSource = "mm", unitsTarget = "mm")
  if (anyDuplicated(DT, by = c("RecordID", "OCID", "IM"))) {
    stop("Duplicate (RecordID, OCID, IM) rows in raw intensities.", call. = FALSE)
  }
  DT <- dcast(DT, RecordID + OCID ~ IM, value.var = "value")
  COLS <- c("PGA", "ARMS", "AI", "AIu", "AId", "ATo", "ATn", "CAV", "CAV5",
            "EPI", "PDI", "NP", "Fs", "AZC")
  for (j in COLS) set(DT, j = j, value = as.integer(round(DT[[j]])))
  COLS <- c("TmA", "D0595", "D2080", "D0575", "Dmax")
  for (j in COLS) set(DT, j = j, value = round(DT[[j]], 1L))
  DT[, dt := round(dt, 4L)]
  DT <- Map[DT, on = "OCID"]
  DT[, DIR := factor(DIR, levels = c("H1", "H2", "UP"))]
  setorder(DT, RecordID, DIR)
  DT[, DIR := as.character(DIR)]
  setcolorder(DT, c("RecordID", "DIR", "OCID"))
  DT[]
}
