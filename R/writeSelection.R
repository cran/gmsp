# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Write a station-folder selection
#'
#' Writes unique station-folder keys to CSV and a JSON summary.
#'
#' @param data Table containing OwnerID, EventID and StationID.
#' @param name Nonempty selection name used as the output filename stem.
#' @param path Nonempty destination directory, created recursively if needed.
#' @return Invisibly, a data.table with OwnerID, EventID and StationID,
#'   sorted by these keys and without duplicate rows.
#' @details The files are named from name with .csv and .json suffixes.
#'   Existing files at these paths are overwritten. The JSON fields are
#'   name, dateUTC, nHits and hitsPerOwner; dateUTC uses UTC to the second.
#'   An empty selection has nHits zero. RecordID is not part of these keys.
#'   A JSON failure after CSV publication raises modulePartialPublication
#'   with the original condition, partial result and affected paths;
#'   the CSV remains. Completion signals a moduleNote condition.
#'   Missing required columns and invalid name/path arguments are input errors.
#' @seealso [selectRecords()]
#' @examples
#' Root <- tempfile("writeSelection")
#' Records <- data.table::data.table(OwnerID = "AAA", EventID = c("E1", "E1", "E2"),
#'                                   StationID = c("S1", "S2", "S1"))
#' Selection <- writeSelection(Records, name = "demo", path = Root)
#' print(Selection)
#' list.files(Root)
#' str(jsonlite::read_json(file.path(Root, "demo.json")))
#' unlink(Root, recursive = TRUE)
#' @export
writeSelection <- function(data, name, path) {
  DT <- as.data.table(data)
  COLS <- c("OwnerID", "EventID", "StationID")
  if (anyDuplicated(names(DT)) || length(setdiff(COLS, names(DT)))) {
    .stopUsage("Selection requires OwnerID, EventID and StationID.", operation = "writeSelection")
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name) ||
      !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .stopUsage("name and path must be nonempty strings.", operation = "writeSelection")
  }
  Selection <- unique(DT[, COLS, with = FALSE])
  setorderv(Selection, COLS)
  path <- path.expand(path)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  CSV <- file.path(path, paste0(name, ".csv"))
  JSON <- file.path(path, paste0(name, ".json"))
  tryCatch(data.table::fwrite(Selection, file = CSV, eol = "\n"), error = function(e) {
    .stopFailure(conditionMessage(e), operation = "writeSelection", path = CSV, cause = e)
  })
  DT <- Selection[, .N, by = OwnerID]
  tryCatch(.writeJSON(list(name = name,
    dateUTC = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ"),
    nHits = nrow(Selection), hitsPerOwner = as.list(stats::setNames(DT$N, DT$OwnerID))),
    path = JSON, auto_unbox = TRUE, pretty = TRUE), error = function(e) {
      .stopPartialPublication("Selection CSV was written but JSON publication failed: ",
        conditionMessage(e), operation = "writeSelection", path = c(CSV, JSON),
        failures = list(e), result = Selection)
    })
  message(structure(list(message = sprintf("%s: %d hits across %d owners -> %s",
    name, nrow(Selection), nrow(DT), CSV), call = NULL, operation = "writeSelection",
    path = CSV, count = nrow(Selection)), class = c("gmspNote", "moduleNote", "message", "condition")))
  invisible(Selection)
}
