# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readTwoCol <- function(file) {
  DT <- fread(file = file, header = FALSE)
  DT <- DT[, .SD, .SDcols = !vapply(DT, function(x) {
    is.logical(x) && all(is.na(x))
  }, logical(1L))]
  if (ncol(DT) != 2L) {
    .stopUsage("TWO_COL requires two columns.", operation = "readProviderFile")
  }
  setnames(DT, c("t", "s"))
  FILE <- basename(file)
  OCID <- NULL
  if (grepl("^[^.]+\\.[^.]+\\.[^.]*\\.[^.]+?__", FILE, perl = TRUE)) {
    OCID <- sub("^[^.]+\\.[^.]+\\.[^.]*\\.([^.]+?)__.*", "\\1", FILE, perl = TRUE)
  }
  if (is.null(OCID) && grepl("\\.[A-Z0-9]+_AccTH\\.txt$", FILE, perl = TRUE)) {
    OCID <- sub(".*\\.([A-Z0-9]+)_AccTH\\.txt$", "\\1", FILE, perl = TRUE)
  }
  if (is.null(OCID) && grepl("^[NEZ]_acc\\.", FILE, perl = TRUE)) {
    OCID <- sub("^([NEZ])_acc\\..*", "\\1", FILE, perl = TRUE)
  }
  if (is.null(OCID) || !nzchar(OCID)) {
    .stopUsage("Unknown TWO_COL filename pattern: ", FILE, operation = "readProviderFile")
  }
  DT[, OCID := OCID][, .(t, OCID, s)]
}
