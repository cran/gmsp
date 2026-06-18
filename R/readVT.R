#' Read velocity records via [readTS()] with `kind = "VT"`.
#'
#' Thin wrapper around [readTS()]; see there for full semantics.
#'
#' @inheritParams readTS
#' @return See [readTS()].
#' @examples
#' root <- file.path(tempdir(), "gmsp-readvt-example")
#' unlink(root, recursive = TRUE)
#' raw <- file.path(root, "AAA", "E1", "S1", "raw")
#' dir.create(raw, recursive = TRUE)
#' data.table::fwrite(data.table::data.table(H1 = c(1, 2)),
#'                    file.path(raw, "VT.R1.csv"))
#' jsonlite::write_json(list(dt = 0.01), file.path(raw, "VT.R1.json"),
#'                      auto_unbox = TRUE)
#' selection <- data.table::data.table(
#'   RecordID = "R1", OwnerID = "AAA", EventID = "E1", StationID = "S1"
#' )
#' readVT(selection, path = root)
#'
#' @export
readVT <- function(.x, path) readTS(.x = .x, path = path, kind = "VT")
