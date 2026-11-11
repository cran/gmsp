# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Select record identities
#'
#' Intersects identity filters and returns unique record keys without
#' modifying the supplied table.
#'
#' @param data Table containing RecordID, OwnerID, EventID and StationID.
#' @param recordID,eventID,stationID,ownerID Identity vectors for the
#'   corresponding columns. NULL leaves a dimension unrestricted;
#'   an empty vector selects zero rows.
#' @return A data.table with RecordID, OwnerID, EventID and StationID,
#'   in that column order, sorted by OwnerID, EventID and StationID.
#' @details Duplicate key rows are collapsed. With every filter NULL,
#'   the result contains the unique keys of the supplied table.
#'   Missing required columns are input errors. The operation performs
#'   no file access.
#' @seealso [readRecordTSL()], [writeSelection()]
#' @examples
#' Records <- data.table::data.table(RecordID = c("R1", "R2", "R3"), OwnerID = "AAA",
#'   EventID = c("E1", "E1", "E2"), StationID = c("S1", "S2", "S1"))
#' selectRecords(Records, eventID = "E1")
#' selectRecords(Records, stationID = "S1", ownerID = "AAA")
#' @export
selectRecords <- function(data, recordID = NULL, eventID = NULL,
                           stationID = NULL, ownerID = NULL) {
  DT <- as.data.table(data)
  COLS <- c("RecordID", "OwnerID", "EventID", "StationID")
  if (anyDuplicated(names(DT)) || length(setdiff(COLS, names(DT)))) {
    .stopUsage("Selection requires RecordID, OwnerID, EventID and StationID.", operation = "selectRecords")
  }
  if (!is.null(recordID)) DT <- DT[RecordID %in% recordID]
  if (!is.null(eventID)) DT <- DT[EventID %in% eventID]
  if (!is.null(stationID)) DT <- DT[StationID %in% stationID]
  if (!is.null(ownerID)) DT <- DT[OwnerID %in% ownerID]
  DT <- unique(DT[, COLS, with = FALSE])
  setorderv(DT, c("OwnerID", "EventID", "StationID"))
  DT[]
}
