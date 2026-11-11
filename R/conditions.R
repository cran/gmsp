# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.stopUsage <- function(..., operation) {
  stop(structure(
    list(message = paste0(...), call = NULL, operation = operation),
    class = c("gmspUsage", "moduleUsage", "moduleFailure", "error", "condition")))
}

.warnSignal <- function(..., operation, path = NULL, phase = NULL) {
  OUT <- list(message = paste0(...), call = NULL, operation = operation)
  if (!is.null(path)) OUT$path <- path
  if (!is.null(phase)) OUT$phase <- phase
  warning(structure(OUT,
    class = c("gmspWarning", "moduleWarning", "warning", "condition")))
}

.stopFailure <- function(..., operation, path = NULL, cause = NULL) {
  OUT <- list(message = paste0(...), call = NULL, operation = operation)
  if (!is.null(path)) OUT$path <- path
  if (!is.null(cause)) OUT$cause <- cause
  stop(structure(OUT, class = c("gmspFailure", "moduleFailure", "error", "condition")))
}

.stopPartialPublication <- function(..., operation, path, failures, result) {
  stop(structure(
    list(message = paste0(...), call = NULL, operation = operation,
         phase = "publication", failures = failures, result = result, path = path),
    class = c("gmspPartialPublication", "modulePartialPublication",
              "moduleFailure", "error", "condition")))
}
