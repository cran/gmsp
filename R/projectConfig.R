# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
# Exact JSON access helpers. Avoid `$` on JSON-derived lists because R partial
# matching can resolve missing keys.

.jsonHas <- function(data, key) {
  Value <- data
  for (Part in key) {
    if (!is.list(Value) || !(Part %in% names(Value))) return(FALSE)
    Value <- Value[[Part, exact = TRUE]]
  }
  TRUE
}

.jsonGet <- function(data, key, default = NULL) {
  Value <- data
  for (Part in key) {
    if (!is.list(Value) || !(Part %in% names(Value))) return(default)
    Value <- Value[[Part, exact = TRUE]]
  }
  Value
}

.jsonRequire <- function(data, key, path = NULL) {
  if (.jsonHas(data = data, key = key)) return(.jsonGet(data = data, key = key))
  Label <- paste(key, collapse = ".")
  if (!is.null(path)) Label <- paste(Label, "in", path)
  .stopUsage("Missing required JSON key: ", Label, operation = ".readProjectConfig")
}

.isAbsolutePath <- function(path) {
  grepl("^(/|~)", path) |
    (.Platform$OS.type == "windows" &
       grepl("^([A-Za-z]:[/\\\\]|\\\\\\\\)", path))
}

.resolvePath <- function(path, root) {
  path <- as.character(path)
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    .stopUsage("Path must be one non-empty string.", operation = ".resolvePath")
  }
  if (.isAbsolutePath(path)) return(path.expand(path))
  file.path(root, path)
}

# Entry checks shared by the project runners: a missing root or contract is
# reported by name instead of surfacing as a raw path or connection error.
.projectRoot <- function(root) {
  root <- as.character(root)
  if (length(root) != 1L || is.na(root) || !nzchar(root)) {
    .stopUsage("root must be one non-empty string.", operation = ".projectRoot")
  }
  if (!dir.exists(root)) .stopUsage("Project root does not exist: ", root, operation = ".projectRoot")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

.readProjectConfig <- function(path) {
  if (!file.exists(path)) .stopUsage("Project contract does not exist: ", path, operation = ".readProjectConfig")
  OUT <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) stop(structure(list(message = conditionMessage(e),
      call = NULL, cause = e, operation = ".readProjectConfig", path = path),
      class = c("gmspUsage", "moduleUsage", "moduleFailure", "error", "condition"))))
  .checkProjectObject(OUT, path = path)
  OUT
}

.checkProjectObject <- function(data, path) {
  if (!is.list(data) || is.null(names(data)) || any(!nzchar(names(data))) ||
      anyDuplicated(names(data))) {
    .stopUsage("Expected a JSON object with distinct keys at ", path,
               operation = ".readProjectConfig")
  }
  for (j in names(data)) {
    if (is.list(data[[j]]) && !is.null(names(data[[j]]))) {
      .checkProjectObject(data[[j]], path = paste(path, j, sep = "."))
    }
  }
  invisible(NULL)
}

.readCharacterVector <- function(x, label, allowNull = FALSE) {
  if (is.null(x) && allowNull) return(character())
  if (allowNull && !length(x)) return(character())
  Value <- unlist(x, use.names = FALSE)
  if (!is.character(Value) || !length(Value) || any(is.na(Value) | !nzchar(Value))) {
    .stopUsage(label, " must be a non-empty character vector.", operation = ".readCharacterVector")
  }
  unique(Value)
}

.readScalarLogical <- function(x, label, default = FALSE) {
  if (is.null(x)) return(default)
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .stopUsage(label, " must be TRUE or FALSE.", operation = ".readScalarLogical")
  }
  x
}

.mkdir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Could not create directory: ", path, call. = FALSE)
  invisible(path)
}

# JSON files keep LF line endings on every system: writing the text of
# jsonlite::write_json() through a path would use a text-mode connection,
# which writes CRLF on Windows.
.writeJSON <- function(x, path, ...) {
  writeBin(charToRaw(paste0(jsonlite::toJSON(x, ...), "\n")), path)
}

.requireColumns <- function(DT, cols, label) {
  COLS <- setdiff(cols, names(DT))
  if (length(COLS)) .stopUsage(label, " missing columns: ", paste(COLS, collapse = ", "), operation = ".requireColumns")
  invisible(NULL)
}

.requireWritableProcess <- function(processID) {
  if (any(toupper(as.character(processID)) == "F25", na.rm = TRUE)) {
    .stopUsage("F25 is a read-only historical process and cannot be recalculated or resealed.",
               operation = "processProject")
  }
  invisible(NULL)
}

.projectFailure <- function(cause, operation, path = NULL, phase = NULL, unit = NULL) {
  if (inherits(cause, "moduleFailure")) stop(cause)
  OUT <- cause
  class(OUT) <- unique(c("gmspFailure", "moduleFailure", class(OUT)))
  if (is.null(OUT$cause)) OUT$cause <- cause
  Context <- list(operation = operation, path = path, phase = phase, unit = unit)
  for (j in names(Context)) {
    if (is.null(OUT[[j]]) && !is.null(Context[[j]])) OUT[[j]] <- Context[[j]]
  }
  stop(OUT)
}

.projectMessage <- function(..., operation, phase, count = NULL, total = NULL,
                            unit = NULL, path = NULL) {
  message(structure(list(message = paste0(..., "\n"), call = NULL, operation = operation,
    phase = phase, count = count, total = total, unit = unit, path = path),
    class = c("gmspProgress", "moduleProgress", "message", "condition")))
}

.range <- function(x) {
  if (is.list(x)) {
    x <- vapply(x, function(v) {
      if (is.null(v) || !length(v)) return(NA_real_)
      as.numeric(unlist(v)[1L])
    }, numeric(1L))
  } else {
    x <- as.numeric(x)
  }
  if (length(x) != 2L) .stopUsage("Range filters must have length 2.", operation = ".range")
  if (is.na(x[1L])) x[1L] <- -Inf
  if (is.na(x[2L])) x[2L] <-  Inf
  if (x[1L] > x[2L]) x <- rev(x)
  x
}
