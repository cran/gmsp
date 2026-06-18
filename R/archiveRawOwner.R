#' Compress `raw.owner/` to `raw.owner.tar.gz` and delete the directory.
#'
#' Saves disk space after the parser has succeeded. Returns silently if
#' nothing to do (no `raw.owner/` directory) or if the archive already
#' exists. Only deletes `raw.owner/` after the tar archive is verified
#' (re-readable). On any failure, leaves both intact.
#'
#' @param path Absolute path to the station folder containing `raw.owner/`.
#' @return Logical, `TRUE` if archive newly written, `FALSE` if no-op,
#'                  `NA` on failure.
#' @examples
#' station <- file.path(tempdir(), "gmsp-archive-example")
#' unlink(station, recursive = TRUE)
#' dir.create(file.path(station, "raw.owner"), recursive = TRUE)
#' writeLines("provider bytes", file.path(station, "raw.owner", "record.txt"))
#' archiveRawOwner(station)
#' file.exists(file.path(station, "raw.owner.tar.gz"))
#'
#' @export
archiveRawOwner <- function(path) {
  path <- path.expand(path)
  RAW <- file.path(path, "raw.owner")
  TAR <- file.path(path, "raw.owner.tar.gz")
  if (!dir.exists(RAW)) return(invisible(FALSE))
  if (file.exists(TAR)) return(invisible(FALSE))

  OWD <- getwd(); on.exit(setwd(OWD), add = TRUE)
  setwd(path)

  OK <- tryCatch(
    utils::tar(basename(TAR), "raw.owner", compression = "gzip") == 0L,
    error = function(e) FALSE
  )
  if (!OK || !file.exists(TAR)) {
    if (file.exists(TAR)) file.remove(TAR)
    return(invisible(NA))
  }
  if (length(utils::untar(basename(TAR), list = TRUE)) == 0L) {
    file.remove(TAR)
    return(invisible(NA))
  }
  unlink("raw.owner", recursive = TRUE)
  invisible(TRUE)
}
