# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Parse provider files for one record
#'
#' Reads unique FileID entries under
#' \code{path/OwnerID/EventID/StationID/raw.owner/} using the owner's format.
#'
#' @param record A data.table with OwnerID, EventID, StationID and FileID.
#'   OwnerID, EventID and StationID identify one station record.
#' @param path Path to the records root.
#' @return A long data.table with \code{t, OCID, s} in native units.
#' @details Missing keys, NA keys, multiple records and owners without a
#'   format raise errors before file reading. Duplicate FileID entries are
#'   read once. This operation does not add ID, convert amplitudes or write files.
#' @examples
#' Root <- tempfile("parseRecord")
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
#'   StationID = "S1", FileID = paste0(names(Samples), ".AT2"))
#' parseRecord(Record, path = Root)
#' unlink(Root, recursive = TRUE)
#' @export
parseRecord <- function(record, path) {
  COLS <- c("OwnerID", "EventID", "StationID", "FileID")
  if (!is.data.table(record) || !all(COLS %in% names(record)) || !nrow(record)) {
    .stopUsage("record requires OwnerID, EventID, StationID and FileID rows.",
               operation = "parseRecord")
  }
  for (j in COLS) {
    if (!is.character(record[[j]]) || anyNA(record[[j]]) || any(!nzchar(record[[j]]))) {
      .stopUsage(j, " must contain nonempty character keys.", operation = "parseRecord")
    }
  }
  if (any(vapply(record[, .(OwnerID, EventID, StationID)], uniqueN, integer(1L)) != 1L)) {
    .stopUsage("record must identify one owner, event and station.", operation = "parseRecord")
  }
  Format <- .ownerFormat[record$OwnerID[1L]]
  if (is.na(Format)) .stopUsage("Owner has no provider format: ", record$OwnerID[1L], operation = "parseRecord")
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .stopUsage("path must be one nonempty records root.", operation = "parseRecord")
  }
  DIR <- file.path(path.expand(path), record$OwnerID[1L], record$EventID[1L],
                   record$StationID[1L], "raw.owner")
  rbindlist(lapply(unique(record$FileID), function(x) {
    readProviderFile(file = file.path(DIR, x), format = unname(Format))
  }))
}

.ownerFormat <- c(
  CAL = "TWO_COL", CENA = "TWO_COL", CLSMD = "TWO_COL", ESM = "TWO_COL",
  NCEDC = "TWO_COL", NN = "TWO_COL", PNSN = "TWO_COL", SCEDC = "TWO_COL",
  CESMD = "V2", GSC = "TRA", IGP = "ACA", NGAW = "AT2", NWZ = "V2A",
  UCR = "LIS", ISEE = "ISEE"
)
