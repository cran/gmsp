# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Assess station velocity metadata
#'
#' Returns flagged rows with the first applicable station-velocity reason,
#' preserving the input.
#'
#' @param data Table with numeric StationVs30 in metres per second.
#'   Additional columns are retained.
#' @param vs30Low,vs30High Finite scalar cuts, ordered from low to high,
#'   in metres per second.
#' @return A data.table containing flagged rows in input order, with Reason.
#' @details Missing StationVs30 gives vs30NA; values below vs30Low give
#'   vs30Low, and values above vs30High give vs30High, in that precedence.
#'   Values equal to a cut are not flagged.
#'   A missing or nonnumeric StationVs30 column and invalid cuts are input
#'   errors. The operation performs no file access.
#' @seealso [assessDistances()]
#' @examples
#' Stations <- data.frame(StationID = c("S1", "S2", "S3", "S4"),
#'                        StationVs30 = c(NA, 150, 400, 2000))
#' assessSite(Stations, vs30Low = 180, vs30High = 1500)
#' @export
assessSite <- function(data, vs30Low, vs30High) {
  DT <- copy(as.data.table(data))
  if (anyDuplicated(names(DT)) || !"StationVs30" %in% names(DT) ||
      !is.numeric(DT[["StationVs30"]])) {
    .stopUsage("Site metadata requires numeric StationVs30.", operation = "assessSite")
  }
  if (!is.numeric(vs30Low) || length(vs30Low) != 1L || !is.finite(vs30Low) ||
      !is.numeric(vs30High) || length(vs30High) != 1L || !is.finite(vs30High) ||
      vs30Low > vs30High) {
    .stopUsage("vs30Low and vs30High must be finite ordered scalar cuts.", operation = "assessSite")
  }
  DT[, Reason := data.table::fcase(
    is.na(StationVs30), "vs30NA",
    StationVs30 < vs30Low, "vs30Low",
    StationVs30 > vs30High, "vs30High",
    default = NA_character_)]
  DT[!is.na(Reason)]
}
