#' Select records from a record metadata table.
#'
#' Filters a record metadata table by any combination of `RecordID`,
#' `EventID`, `StationID`, and `OwnerID`, then deduplicates to one row
#' per record. Output is the canonical selection shape consumed by the
#' [readTS()] family ([readAT()] / [readVT()] / [readDT()]) and
#' [writeSelection()].
#'
#' Filter args are character vectors (length 1+). `NULL` means "no
#' restriction on this dimension". With all `NULL`, returns every
#' record in `M` -- protect with explicit filters for non-trivial work.
#'
#' For richer filters (magnitude, distance, intensity), filter `M`
#' first and pass the subset:
#' ```r
#' selectRecords(M[EventMagnitude > 7 & Repi < 100 & PGA > 600 & DIR == "H1"])
#' ```
#'
#' @param M         record metadata `data.table`.
#' @param RecordID  character. Filter by `RecordID`. Default `NULL`.
#' @param EventID   character. Filter by `EventID`. Default `NULL`.
#' @param StationID character. Filter by `StationID`. Default `NULL`.
#' @param OwnerID   character. Filter by `OwnerID`. Default `NULL`.
#' @return `data.table(RecordID, OwnerID, EventID, StationID)`.
#' @examples
#' master <- data.table::data.table(
#'   RecordID = c("R1", "R1", "R2"),
#'   OwnerID = c("AAA", "AAA", "BBB"),
#'   EventID = c("E1", "E1", "E2"),
#'   StationID = c("S1", "S1", "S2"),
#'   DIR = c("H1", "H2", "H1")
#' )
#' selectRecords(master, OwnerID = "AAA")
#'
#' @importFrom data.table setorder
#' @export
selectRecords <- function(M,
                          RecordID  = NULL,
                          EventID   = NULL,
                          StationID = NULL,
                          OwnerID   = NULL) {
  RID <- RecordID; EID <- EventID; SID <- StationID; OID <- OwnerID
  SEL <- M
  if (!is.null(RID)) SEL <- SEL[RecordID  %in% RID]
  if (!is.null(EID)) SEL <- SEL[EventID   %in% EID]
  if (!is.null(SID)) SEL <- SEL[StationID %in% SID]
  if (!is.null(OID)) SEL <- SEL[OwnerID   %in% OID]
  SEL <- unique(SEL[, .(RecordID, OwnerID, EventID, StationID)])
  setorder(SEL, OwnerID, EventID, StationID)
  SEL[]
}
