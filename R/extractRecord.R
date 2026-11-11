# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Extract a provider record to raw files
#'
#' Reads and classifies one provider record without rotation, converts its
#' amplitudes to a millimetre length basis and writes a CSV and JSON pair.
#'
#' @param record Table for one owner, event and station, with FileID and Units;
#'   NetworkID is retained in the sidecar when supplied.
#' @param path Records root containing OwnerID/EventID/StationID/raw.owner.
#' @param align Alignment by sample count: "max" pads with trailing zeros;
#'   "min" truncates. Neither choice interpolates time.
#' @param kind Quantity override AT, VT or DT; NULL derives the quantity from
#'   Units. An override still requires recognisable Units.
#' @return Absolute path to the written CSV, or NULL when classification or
#'   unit interpretation cannot produce a record.
#' @details The CSV retains provider OCID columns and omits time, and its lines
#'   end with LF on every system. RecordID is the first 16 characters of the
#'   MD5 digest of the CSV bytes, so it does not depend on the system. The JSON
#'   stores DIR, OCID and pre-alignment NP arrays in H1, H2, UP order; peaks
#'   are measured after alignment. Its dt and Fs use nine and six decimal
#'   places, respectively, and Units is "mm".
#'
#' Existing raw contents are removed after classification and unit resolution,
#' before alignment and writing. A later failure can leave partial products;
#' extraction is not atomic. The supplied table is not modified.
#' @examples
#' Root <- tempfile("extractRecord")
#' Station <- file.path(Root, "NGAW", "E1", "S1", "raw.owner")
#' dir.create(Station, recursive = TRUE)
#' Samples <- c(HNE = "0.01 0.03 -0.02 0.01", HNN = "0.02 -0.01 0.01 0",
#'              HNZ = "0 0.01 -0.01 0")
#' for (Channel in names(Samples)) {
#'   writeLines(c("header", paste0("Event, date, station, ", Channel), "units",
#'                "NPTS= 4, DT= 0.01 SEC", Samples[[Channel]]),
#'              file.path(Station, paste0(Channel, ".AT2")))
#' }
#' Record <- data.table::data.table(OwnerID = "NGAW", EventID = "E1",
#'   StationID = "S1", FileID = paste0(names(Samples), ".AT2"), Units = "g")
#' CSV <- extractRecord(Record, path = Root)
#' basename(CSV)
#' data.table::fread(CSV)
#' Sidecar <- jsonlite::read_json(sub("[.]csv$", ".json", CSV), simplifyVector = TRUE)
#' str(Sidecar[c("DIR", "OCID", "NP", "PGA", "dt", "Units")])
#' unlink(Root, recursive = TRUE)
#' @importFrom data.table fcase fread fwrite setorder set
#' @export
extractRecord <- function(record, path, align = "max", kind = NULL) {
  if (!is.null(kind) && (length(kind) != 1L || is.na(kind) ||
                         !kind %in% c("AT", "VT", "DT"))) {
    .stopUsage("kind must be AT, VT or DT.", operation = "extractRecord")
  }
  path <- normalizePath(path.expand(path), mustWork = TRUE)
  Key <- as.data.table(record)[1L]
  DT <- parseRecord(record = record, path = path)
  DT <- mapComponents(DT, rotate = FALSE)
  if (is.null(DT)) return(NULL)
  Unit <- .parseUnits(Key$Units)
  if (is.na(Unit)) return(NULL)
  UnitFactor <- switch(Unit, mm = 1, cm = 10, m = 1000, g = 9806.650)
  if (is.null(kind)) kind <- .parseKind(Key$Units)
  if (is.na(kind)) return(NULL)
  DT[, s := s * UnitFactor]

  RawDir <- file.path(path, Key$OwnerID, Key$EventID, Key$StationID, "raw")
  dir.create(RawDir, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(RawDir, full.names = TRUE))
  Map <- DT[, .(NP = .N, OCID = OCID[1L]), by = DIR]
  Map <- Map[match(c("H1", "H2", "UP"), DIR)]
  if (anyNA(Map$DIR) || anyNA(Map$OCID)) return(NULL)
  DT <- alignComponents(DT, align = align)$DT
  if (anyDuplicated(DT, by = c("t", "OCID"))) {
    .stopUsage("Duplicate (t, OCID) rows in ", RawDir, ".",
               operation = "extractRecord")
  }
  SamplingInterval <- round(
    DT[OCID == Map$OCID[1L]][2L, t] - DT[OCID == Map$OCID[1L]][1L, t], 9L)
  Peak <- vapply(Map$OCID, function(x) max(abs(DT[OCID == x, s])), numeric(1L))
  DT <- dcast(DT, t ~ OCID, value.var = "s")[, -"t"]
  FILE <- tempfile(tmpdir = RawDir, fileext = ".csv")
  fwrite(DT, FILE, eol = "\n")
  RecordID <- substr(unname(tools::md5sum(FILE)), 1L, 16L)
  FileID <- paste0(kind, ".", RecordID, ".csv")
  CSV <- file.path(RawDir, FileID)
  if (!file.rename(FILE, CSV)) stop("Cannot publish raw CSV: ", CSV, call. = FALSE)
  Metadata <- list(RecordID = RecordID, OwnerID = Key$OwnerID,
    EventID = Key$EventID, StationID = Key$StationID,
    FileID = FileID, DIR = Map$DIR, OCID = Map$OCID, NP = Map$NP)
  if ("NetworkID" %chin% names(Key)) Metadata$NetworkID <- Key$NetworkID
  Metadata[[paste0("PG", substr(kind, 1L, 1L))]] <- Peak
  Metadata$dt <- SamplingInterval
  Metadata$Fs <- round(1 / SamplingInterval, 6L)
  Metadata$Units <- "mm"
  .writeJSON(Metadata, file.path(RawDir, paste0(kind, ".", RecordID, ".json")),
             auto_unbox = TRUE, pretty = TRUE)
  CSV
}

.parseUnits <- function(text) {
  if (!length(text) || is.na(text) || !nzchar(text)) return(NA_character_)
  AUX <- tolower(text)
  fcase(grepl("^mm", AUX), "mm", grepl("^cm|^gal", AUX), "cm",
        AUX == "m" | grepl("^m/", AUX), "m", grepl("^g('s)?$|^g/", AUX), "g",
        default = NA_character_)
}

.parseKind <- function(text) {
  if (!length(text) || is.na(text) || !nzchar(text)) return(NA_character_)
  AUX <- tolower(text)
  fcase(grepl("/s2$|/sec2$|/s/s$|/sec/sec$", AUX), "AT",
        grepl("^g('s)?$|^gal", AUX), "AT", grepl("/s$|/sec$", AUX), "VT",
        grepl("^(mm|cm|m)$", AUX), "DT", default = NA_character_)
}
