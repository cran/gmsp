#' Regularize time series to a uniform time grid.
#'
#' Internal helper. Accepts an input time selector and always returns
#' canonical `t` plus the interpolated signal columns.
#' @noRd
#'
.regularize <- function(.x, time = "t") {
  # Check if time column exists
  stopifnot(is.data.table(.x))
  stopifnot(nrow(.x) > 2L)

  # Copy to avoid modifying original data.table by reference
  X <- copy(.x)
  X <- .canonicalizeTimeColumn(X, time = time)

  # Use setorderv for dynamic column ordering
  setorderv(X, "t")

  # Assign time column to standard 't' variable
  t <- X[["t"]]
  # at this point we know that either dt is not uniform or not rational
  dt <- min(t - shift(t), na.rm = TRUE)

  dt <- .rationalize(dt)

  t0 <- min(t)
  t1 <- max(t)
  n <- floor((t1 - t0) / dt + 1e-12)
  ts <- (0:n) * dt

  OCID <- setdiff(names(X), "t")
  LIST <- X[, lapply(.SD, function(s) {
    f <- stats::splinefun(t - t0, s, method = "monoH.FC")
    f(ts)
  }), .SDcols = OCID]

  setnames(LIST, OCID)
  data.table(t = ts, LIST)
}

.canonicalizeTimeColumn <- function(X, time) {
  if (!is.character(time) || length(time) != 1L || is.na(time) || !nzchar(time)) {
    stop("time must be a non-empty character scalar.", call. = FALSE)
  }
  if (!time %chin% names(X)) {
    stop(sprintf("time column not found: %s.", time), call. = FALSE)
  }
  if (!identical(time, "t")) {
    if ("t" %chin% names(X)) {
      stop("time cannot be canonicalized because input already has a column named t.",
           call. = FALSE)
    }
    setnames(X, old = time, new = "t")
  }
  X
}
