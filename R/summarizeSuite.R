# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.summarizeSuite <- function(data, by, probabilities = NULL) {
  if (!is.null(probabilities) && (!is.numeric(probabilities) || !length(probabilities) ||
      any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1) ||
      anyDuplicated(probabilities))) {
    .stopUsage("summary.probabilities requires distinct finite numbers in [0,1].",
                operation = ".summarizeSuite")
  }
  DT <- as.data.table(data)
  .checkTableKeys(DT, by = by, columns = c("RecordID", "value"),
                  keys = c(by, "RecordID"), operation = ".summarizeSuite")
  if (!nrow(DT) || !is.numeric(DT$value) || any(!is.finite(DT$value)) ||
      anyNA(DT[, c(by, "RecordID"), with = FALSE])) {
    .stopUsage("Summary requires finite values and complete population keys.",
                operation = ".summarizeSuite")
  }
  if (is.null(probabilities)) return(DT[, .(mean = mean(value), recordCount = .N), by = by])
  DT[, .(mean = mean(value), recordCount = .N, p = probabilities,
          value = as.numeric(stats::quantile(value, probs = probabilities,
            names = FALSE, type = 7L, na.rm = FALSE))), by = by]
}
