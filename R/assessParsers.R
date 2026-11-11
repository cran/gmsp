# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Assess provider parsing by station
#'
#' Reads each station of an owner through parseRecord and reports its status.
#'
#' @param records Record metadata with OwnerID, EventID, StationID and the
#'   FileID rows required by parseRecord.
#' @param owner One nonempty OwnerID string.
#' @param path Nonempty path to the records root, passed to parseRecord.
#' @return A data.table with OwnerID, EventID, StationID, status and reason,
#'   in first-appearance station order. An owner without rows returns an
#'   empty table with these columns.
#' @details Status is OK for a returned parser result, with its row and column
#'   counts; FAIL for an error; WARN for a warning. The latter statuses retain
#'   conditionMessage as reason. Missing grouping columns or invalid owner/path
#'   arguments are input errors. The operation writes no extracted records.
#' @seealso [parseRecord()]
#' @examples
#' Root <- tempfile("assessParsers")
#' Station <- file.path(Root, "NGAW", "E1", "S1", "raw.owner")
#' dir.create(Station, recursive = TRUE)
#' writeLines(c("header", "Event, date, station, H1", "units",
#'              "NPTS= 4, DT= 0.01 SEC", "1.0 2.0 3.0 4.0"),
#'            file.path(Station, "H1.AT2"))
#' Records <- data.table::data.table(OwnerID = "NGAW", EventID = "E1",
#'   StationID = c("S1", "S2"), FileID = "H1.AT2")
#' Status <- assessParsers(Records, owner = "NGAW", path = Root)
#' print(Status[, c("StationID", "status")])
#' unlink(Root, recursive = TRUE)
#' @export
assessParsers <- function(records, owner, path) {
  DT <- as.data.table(records)
  if (anyDuplicated(names(DT)) ||
      length(setdiff(c("OwnerID", "EventID", "StationID"), names(DT)))) {
    .stopUsage("Parser assessment requires OwnerID, EventID and StationID.", operation = "assessParsers")
  }
  if (!is.character(owner) || length(owner) != 1L || is.na(owner) || !nzchar(owner) ||
      !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .stopUsage("owner and path must be nonempty strings.", operation = "assessParsers")
  }
  DT <- DT[DT[["OwnerID"]] == owner]
  Keys <- unique(DT[, .(EventID, StationID)])
  if (!nrow(Keys)) {
    return(data.table(OwnerID = character(), EventID = character(),
                      StationID = character(), status = character(), reason = character()))
  }
  rbindlist(lapply(seq_len(nrow(Keys)), function(i) {
    Key <- Keys[i]
    AUX <- DT[EventID == Key$EventID & StationID == Key$StationID]
    OUT <- tryCatch({
      AUX <- parseRecord(record = AUX, path = path.expand(path))
      list(status = "OK", reason = sprintf("rows=%d cols=%d", nrow(AUX), ncol(AUX)))
    }, error = function(e) list(status = "FAIL", reason = conditionMessage(e)),
       warning = function(w) list(status = "WARN", reason = conditionMessage(w)))
    data.table(OwnerID = owner, EventID = Key$EventID, StationID = Key$StationID,
                status = OUT$status, reason = OUT$reason)
  }), use.names = TRUE)
}
