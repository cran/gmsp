#' Write a selection (subset of master) to `selection/<name>.csv` / `.json`.
#'
#' Deduplicates `(OwnerID, EventID, StationID)` so the CSV
#' carries one row per station folder (= one record). The JSON sidecar
#' captures audit metadata: name, timestamp, total hits, hits per owner.
#'
#' The CSV is the canonical input contract for any downstream
#' orchestrator that iterates over a selection: each row identifies one
#' `(OwnerID, EventID, StationID)` station folder under whichever
#' records root the orchestrator was given.
#'
#' @param DT           subset of the master `data.table`
#'                     (output of `buildMaster()` after the user's filters).
#' @param name         identifier for the selection, used as filename stem.
#' @param path Absolute path to the directory where `<name>.csv` and
#'   `<name>.json` will be written. Required -- no default.
#' @return Invisibly, the deduplicated selection `data.table`.
#' @examples
#' x <- data.table::data.table(
#'   OwnerID = c("AAA", "AAA"),
#'   EventID = c("E1", "E1"),
#'   StationID = c("S1", "S1"),
#'   DIR = c("H1", "H2")
#' )
#' path <- file.path(tempdir(), "gmsp-selection-example")
#' unlink(path, recursive = TRUE)
#' suppressMessages(writeSelection(x, name = "demo", path = path))
#' list.files(path)
#'
#' @importFrom data.table fwrite setorder
#' @importFrom jsonlite write_json
#' @export
writeSelection <- function(DT, name, path) {
  path <- path.expand(path)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  SEL <- unique(DT[, .(OwnerID, EventID, StationID)])
  setorder(SEL, OwnerID, EventID, StationID)

  CSV  <- file.path(path, sprintf("%s.csv",  name))
  JSON <- file.path(path, sprintf("%s.json", name))
  fwrite(SEL, CSV)

  AUX <- SEL[, .N, by = OwnerID]
  jsonlite::write_json(
    list(name         = name,
         dateUTC      = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ"),
         nHits        = nrow(SEL),
         hitsPerOwner = as.list(setNames(AUX$N, AUX$OwnerID))),
    JSON, auto_unbox = TRUE, pretty = TRUE
  )

  message(sprintf("[writeSelection] %s: %d hits across %d owners -> %s",
                  name, nrow(SEL), uniqueN(SEL$OwnerID), CSV))
  invisible(SEL)
}
