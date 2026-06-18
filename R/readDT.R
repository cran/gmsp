#' Read displacement records via [readTS()] with `kind = "DT"`.
#'
#' Thin wrapper around [readTS()]; see there for full semantics.
#'
#' @inheritParams readTS
#' @return See [readTS()].
#' @examples
#' root <- file.path(tempdir(), "gmsp-readdt-example")
#' unlink(root, recursive = TRUE)
#' raw <- file.path(root, "AAA", "E1", "S1", "raw")
#' dir.create(raw, recursive = TRUE)
#' data.table::fwrite(data.table::data.table(H1 = c(1, 2)),
#'                    file.path(raw, "DT.R1.csv"))
#' jsonlite::write_json(list(dt = 0.01), file.path(raw, "DT.R1.json"),
#'                      auto_unbox = TRUE)
#' selection <- data.table::data.table(
#'   RecordID = "R1", OwnerID = "AAA", EventID = "E1", StationID = "S1"
#' )
#' readDT(selection, path = root)
#'
#' @export
readDT <- function(.x, path) readTS(.x = .x, path = path, kind = "DT")
