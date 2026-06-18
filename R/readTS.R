#' Read parsed time-series records into the shape [AT2TS()] / [VT2TS()] / [DT2TS()] expect.
#'
#' Stacks `raw/<KIND>.<RID>.csv` from each record in `.x`, builds the time
#' axis `t` from `dt` (in `raw/<KIND>.<RID>.json`), and returns a single
#' `data.table` keyed at `(RecordID, OwnerID, EventID, StationID, t)`.
#'
#' The sidecar shape produced by [extractRecord()] is identical across KINDs;
#' KIND only selects the file prefix. The CSV columns are provider `OCID`
#' values preserved by [extractRecord()]. Direction labels (`H1`/`H2`/`UP`)
#' live in the JSON sidecar mapping, not in the CSV header. Use the thin
#' wrappers [readAT()] / [readVT()] / [readDT()] at call sites where the KIND
#' is fixed.
#'
#' ```r
#' Selection <- selectRecords(M, EventID = "...")
#' TS  <- readTS(.x = Selection, path = "/path/to/records", kind = "VT")
#' # Sidecar declares units.source as the length base ("mm"); KIND is set by kind=.
#' TS[, VT2TS(.SD, units.source = "mm"), by = .(RecordID, OwnerID, EventID, StationID)]
#' ```
#'
#' @param .x `data.table` with columns `RecordID, OwnerID, EventID, StationID`
#'   (one row per record). Output of [selectRecords()].
#' @param path Absolute path to the records root. The function reads
#'   per-station files under
#'   `<path>/<OwnerID>/<EventID>/<StationID>/raw/`. Required -- no default.
#' @param kind One of `"AT"`, `"VT"`, `"DT"`. Selects the sidecar file
#'   prefix (`<kind>.<RID>.csv` / `<kind>.<RID>.json`).
#' @return `data.table` with columns
#'   `RecordID, OwnerID, EventID, StationID, t, <OCID columns>`.
#'   Records whose sidecars are missing are skipped.
#' @examples
#' root <- file.path(tempdir(), "gmsp-readts-example")
#' unlink(root, recursive = TRUE)
#' raw <- file.path(root, "AAA", "E1", "S1", "raw")
#' dir.create(raw, recursive = TRUE)
#' data.table::fwrite(
#'   data.table::data.table(H1 = c(1, 2), H2 = c(0, 1)),
#'   file.path(raw, "AT.R1.csv")
#' )
#' jsonlite::write_json(list(dt = 0.01), file.path(raw, "AT.R1.json"),
#'                      auto_unbox = TRUE)
#' selection <- data.table::data.table(
#'   RecordID = "R1", OwnerID = "AAA", EventID = "E1", StationID = "S1"
#' )
#' readTS(selection, path = root, kind = "AT")
#'
#' @importFrom data.table fread
#' @importFrom jsonlite read_json
#' @export
readTS <- function(.x, path, kind = c("AT", "VT", "DT")) {
  kind <- match.arg(kind)
  path <- path.expand(path)
  .x[, {
    CSV  <- file.path(path, OwnerID, EventID, StationID, "raw",
                      sprintf("%s.%s.csv",  kind, RecordID))
    JSON <- file.path(path, OwnerID, EventID, StationID, "raw",
                      sprintf("%s.%s.json", kind, RecordID))
    if (!file.exists(CSV) || !file.exists(JSON)) {
      NULL
    } else {
      X  <- fread(CSV)
      dt <- jsonlite::read_json(JSON, simplifyVector = TRUE)$dt
      X[, t := seq(0, by = dt, length.out = .N)]
      setcolorder(X, c("t", setdiff(names(X), "t")))
      X[]
    }
  }, by = .(RecordID, OwnerID, EventID, StationID)]
}
