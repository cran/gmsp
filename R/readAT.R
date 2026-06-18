#' Read acceleration records via [readTS()] with `kind = "AT"`.
#'
#' Thin wrapper around [readTS()]; see there for full semantics.
#'
#' @inheritParams readTS
#' @return See [readTS()].
#' @examples
#' root <- file.path(tempdir(), "gmsp-readat-example")
#' unlink(root, recursive = TRUE)
#' raw <- file.path(root, "AAA", "E1", "S1", "raw")
#' dir.create(raw, recursive = TRUE)
#' data.table::fwrite(data.table::data.table(H1 = c(1, 2)),
#'                    file.path(raw, "AT.R1.csv"))
#' jsonlite::write_json(list(dt = 0.01), file.path(raw, "AT.R1.json"),
#'                      auto_unbox = TRUE)
#' selection <- data.table::data.table(
#'   RecordID = "R1", OwnerID = "AAA", EventID = "E1", StationID = "S1"
#' )
#' readAT(selection, path = root)
#'
#' @export
readAT <- function(.x, path) readTS(.x = .x, path = path, kind = "AT")
