# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Archive a station's provider files
#'
#' Creates raw.owner.tar.gz from raw.owner and removes the source directory
#' after confirming that the archive exists and has a nonempty listing.
#'
#' @param path Station folder containing raw.owner.
#' @return An invisible logical value: TRUE after archiving, FALSE when the
#'   source directory is absent or the destination exists, and NA for a
#'   treated archive creation or listing failure.
#' @details The working directory is restored on exit. The archive check is
#'   a listing check and does not compare extracted file bytes. A failed
#'   creation or listing leaves the source directory and removes the archive.
#' @examples
#' Station <- tempfile("archiveRawOwner")
#' dir.create(file.path(Station, "raw.owner"), recursive = TRUE)
#' writeLines(c("header", "Event, date, station, H1", "units",
#'              "NPTS= 4, DT= 0.01 SEC", "1.0 2.0 3.0 4.0"),
#'            file.path(Station, "raw.owner", "H1.AT2"))
#' print(archiveRawOwner(Station))
#' list.files(Station)
#' utils::untar(file.path(Station, "raw.owner.tar.gz"), list = TRUE)
#' unlink(Station, recursive = TRUE)
#' @export
archiveRawOwner <- function(path) {
  path <- normalizePath(path.expand(path), mustWork = FALSE)
  RAW <- file.path(path, "raw.owner")
  TAR <- file.path(path, "raw.owner.tar.gz")
  if (!dir.exists(RAW) || file.exists(TAR)) return(invisible(FALSE))
  WorkingDir <- getwd()
  on.exit(setwd(WorkingDir), add = TRUE)
  setwd(path)
  OK <- tryCatch(
    utils::tar(basename(TAR), "raw.owner", compression = "gzip") == 0L &&
      file.exists(TAR) && length(utils::untar(basename(TAR), list = TRUE)) > 0L,
    error = function(e) {
      .warnSignal("Cannot archive ", path, ": ", conditionMessage(e),
                   operation = "archiveRawOwner")
      FALSE
    })
  if (!OK) {
    if (file.exists(TAR)) file.remove(TAR)
    return(invisible(NA))
  }
  unlink("raw.owner", recursive = TRUE)
  invisible(TRUE)
}
