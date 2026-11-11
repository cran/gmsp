# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Read raw records as long time histories
#'
#' Reads the requested raw CSV and JSON pairs and reconstructs time from
#' zero using the sidecar sampling interval, without converting amplitudes.
#'
#' @param records Table with RecordID, OwnerID, EventID and StationID keys.
#' @param path Records root containing the per-station raw folders.
#' @param ID Required quantity AT, VT or DT, selecting the file prefix.
#' @return A new data.table with columns RecordID, OwnerID, EventID,
#'   StationID, OCID, ID, t and s, ordered by those keys and t. OCID retains
#'   the provider channel names. Time t is in seconds.
#' @details A missing requested pair, a nonpositive or nonfinite sampling
#'   interval, or a sidecar key contradicting the selection causes an error
#'   with the record and path. The supplied table is not modified.
#' @examples
#' Root <- tempfile("readRecordTSL")
#' Raw <- file.path(Root, "AAA", "E1", "S1", "raw")
#' dir.create(Raw, recursive = TRUE)
#' data.table::fwrite(data.table::data.table(H1 = c(1, 2), H2 = c(0, 1)),
#'                    file.path(Raw, "AT.R1.csv"))
#' jsonlite::write_json(list(dt = 0.01), file.path(Raw, "AT.R1.json"), auto_unbox = TRUE)
#' Keys <- data.table::data.table(RecordID = "R1", OwnerID = "AAA",
#'                                EventID = "E1", StationID = "S1")
#' readRecordTSL(Keys, path = Root, ID = "AT")
#' unlink(Root, recursive = TRUE)
#' @export
readRecordTSL <- function(records, path, ID) {
  if (length(ID) != 1L || is.na(ID) || !ID %in% c("AT", "VT", "DT")) {
    .stopUsage("ID must be AT, VT or DT.", operation = "readRecordTSL")
  }
  Keys <- c("RecordID", "OwnerID", "EventID", "StationID")
  DT <- as.data.table(records)
  .checkTableKeys(DT, by = character(), columns = Keys, keys = Keys,
                  operation = "readRecordTSL")
  if (anyNA(DT[, Keys, with = FALSE])) {
    .stopUsage("Record keys cannot be missing.", operation = "readRecordTSL")
  }
  if (!nrow(DT)) return(data.table(RecordID = character(), OwnerID = character(),
    EventID = character(), StationID = character(), OCID = character(),
    ID = character(), t = numeric(), s = numeric()))
  path <- path.expand(path)
  Quantity <- ID
  OUT <- DT[, {
    FILE <- file.path(path, OwnerID, EventID, StationID, "raw",
                       paste0(Quantity, ".", RecordID, ".csv"))
    AUX <- .readRawPair(FILE, key = .BY)
    SamplingInterval <- AUX$metadata$dt
    AUX <- AUX$data
    AUX[, t := seq(0, by = SamplingInterval, length.out = .N)]
    AUX <- melt(AUX, id.vars = "t", variable.name = "OCID", value.name = "s",
                variable.factor = FALSE)
    AUX[, ID := Quantity]
    AUX
  }, by = Keys]
  setcolorder(OUT, c(Keys, "OCID", "ID", "t", "s"))
  setorderv(OUT, c(Keys, "OCID", "ID", "t"))
  OUT[]
}

.readRawPair <- function(file, key) {
  JSON <- sub("\\.csv$", ".json", file)
  Context <- paste0("Raw record ", paste(unlist(key), collapse = "/"), " at ", file)
  tryCatch({
    if (!file.exists(file) || !file.exists(JSON)) {
      stop("Requested CSV/JSON pair is absent.", call. = FALSE)
    }
    Metadata <- jsonlite::read_json(JSON, simplifyVector = TRUE)
    if (!is.numeric(Metadata$dt) || length(Metadata$dt) != 1L ||
        !is.finite(Metadata$dt) || Metadata$dt <= 0) {
      stop("Sidecar dt must be one finite positive number.", call. = FALSE)
    }
    for (j in intersect(names(key), names(Metadata))) {
      if (length(Metadata[[j]]) != 1L || is.na(Metadata[[j]]) ||
          as.character(Metadata[[j]]) != as.character(key[[j]])) {
        stop("Sidecar key contradicts selection: ", j, call. = FALSE)
      }
    }
    list(data = fread(file), metadata = Metadata)
  }, error = function(e) {
    e$message <- paste0(Context, ": ", conditionMessage(e))
    e$path <- file
    stop(e)
  })
}
