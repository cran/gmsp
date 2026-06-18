

.taperA <- function(x, Astop = 1e-4, Apass = 1e-3) {
  stopifnot(is.vector(x))
  n <- length(x)

  # Track max abs
  Xo <- max(abs(x))
  
  # If signal is too weak (near numeric zero), return unit taper (no tapering)
  if (Xo <= .Machine$double.eps) {
    return(rep(1, n))
  }
  
  # Scale x to unit amplitude (thresholds are dimensionless)
  x <- x / Xo
  AUX <- abs(x)
  TMP <- max(AUX)  # Should be 1.0

  # Adjust thresholds if they're too high relative to signal
  if (Apass >= TMP) {
    Apass <- TMP * 0.8  # Use 80% of max as pass threshold
  }
  if (Astop >= Apass) {
    Astop <- Apass * 0.5   # Stop threshold is half of pass threshold
  }

  # Find indices with better handling of edge cases
  idx <- which(AUX > Astop) |> first()
  idx <- ifelse(is.na(idx), 1, idx)

  TMP <- which(AUX > Apass) |> first()
  TMP <- ifelse(is.na(TMP), min(n, idx + 2), TMP)
  TMP <- max(TMP, idx + 1L)

  # Ensure valid range
  if (TMP >= n) {
    TMP <- n - 1
  }
  if (TMP <= idx) {
    TMP <- min(n - 1, idx + 1)
  }

  AUX <- which(AUX > Astop) |> last()
  AUX <- ifelse(is.na(AUX), n, AUX)

  idx <- which(abs(x) > Apass) |> last()
  idx <- ifelse(is.na(idx), max(1, AUX - 2), idx)
  idx <- min(idx, AUX - 1L)

  # Ensure valid range
  if (idx <= 1) {
    idx <- 2
  }
  if (idx >= AUX) {
    idx <- max(2, AUX - 1)
  }

  # Final validation
  if (TMP <= which(abs(x) > Astop) |> first() ||
    idx >= AUX ||
    TMP >= idx) {
    # If we can't create valid ranges, return unit taper
    return(rep(1, n))
  }

  tryCatch({
    HP <- .buildHighPassButtterworth(f = seq(1, n), Fpass = TMP, Fstop = which(abs(x) > Astop) |> first(), Astop = 0.01, Apass = 0.95)
    LP <- .buildLowPassButtterworth(f = seq(1, n), Fstop = AUX, Fpass = idx, Astop = 0.01, Apass = 0.95)
    return(LP * HP)
  }, error = function(e) {
    # If filter construction fails, return unit taper
    return(rep(1, n))
  })
}

