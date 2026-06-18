#' Private parser helpers (Stage 1 of plan 02h).
#'
#' Six low-level utilities factored out of the 6 format parsers to remove
#' duplicated code, unify regex dialect (PCRE / `perl = TRUE` everywhere),
#' and standardise guards. NOT exported; consumed by `read*` helpers.
#'
#' @name utilsParse
#' @keywords internal
NULL

#' @noRd
.readClean <- function(file) {
  DATA <- suppressWarnings(readLines(file))
  DATA[DATA != ""]
}

#' @noRd
.unstickNeg <- function(text) {
  gsub("(?<=[0-9])-(?=[0-9])", " -", text, perl = TRUE)
}

#' @noRd
.scanN <- function(text, n) {
  s <- tryCatch(
    suppressWarnings(scan(text = .unstickNeg(text), what = double(), quiet = TRUE)),
    error = function(e) NULL
  )
  if (is.null(s) || length(s) < n || anyNA(s[1L:n])) return(NULL)
  s[1L:n]
}

#' @noRd
.toLong <- function(dt, OCID, s) {
  data.table::data.table(
    t    = seq.int(0, by = dt, length.out = length(s)),
    OCID = OCID,
    s    = s
  )
}

#' @noRd
.guard <- function(dt, n, OCID, s = NULL) {
  stopifnot(length(dt)   == 1L, !is.na(dt), dt > 0,
            length(n)    == 1L, !is.na(n),  n  > 0L,
            length(OCID) == 1L, nzchar(OCID))
  if (!is.null(s)) stopifnot(length(s) == n, !anyNA(s))
  invisible(TRUE)
}

#' @noRd
.extract1 <- function(lines, pattern, perl = TRUE) {
  m <- regmatches(lines, regexpr(pattern, lines, perl = perl))
  m <- m[nzchar(m)]
  stopifnot(length(m) >= 1L)
  m[1L]
}
