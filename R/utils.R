# ============================================================================
# GMSP UTILS - Consolidated Utility Functions
# ============================================================================
# This file contains all utility functions for the GMSP package:
# - Internal core functions (prefixed with .)
# - Public TS helpers (exported functions)
# - Processing, metrics, and utility functions
# ============================================================================

#' @importFrom stats na.omit
#' @importFrom stats fft
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom seewave stdft
#' @importFrom seewave istft
#' @importFrom utils tail
#' @importFrom digest digest
#' @importFrom spectral spec.fft
#' @importFrom EMD emd
#' @importFrom hht CEEMD
#' @importFrom signal resample
#' @importFrom pracma detrend
#' @importFrom stringr str_split
#' @importFrom purrr map
#' 
#' @noRd
#'
# Declare global variables to satisfy R CMD check (data.table NSE)
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "i", "i.H1", "i.H2", "i.SF", "s.rm", "s.sig"))

# Verbose helpers (use options; no reliance on global variables)
.verbose.set <- function(v) {
  options(gmsp.verbose = isTRUE(v))
  invisible(isTRUE(v))
}
.verbose <- function() {
  isTRUE(getOption("gmsp.verbose", FALSE))
}


.getSF <- function(SourceUnits, TargetUnits, g_mms2 = 9806.650) {
  switch(SourceUnits,
    "m" = switch(TargetUnits, "mm" = 1000, "cm" = 100, "m" = 1, NULL),
    "cm" = switch(TargetUnits, "mm" = 10, "cm" = 1, "m" = 1 / 100, NULL),
    "mm" = switch(TargetUnits, "mm" = 1, "cm" = 1 / 10, "m" = 1 / 1000, NULL),
    "g" = switch(TargetUnits, "m" = g_mms2 / 1000, "cm" = g_mms2 / 10, "mm" = g_mms2 / 1, NULL),
    "gal" = switch(TargetUnits, "mm" = 10, "cm" = 1, "m" = 1 / 100, NULL),
    NULL
  )
}

.getG <- function(TargetUnits, g_mms2 = 9806.650) {
  switch(TargetUnits,
    "m" = g_mms2 / 1000,
    "cm" = g_mms2 / 10,
    "mm" = g_mms2 / 1,
    NULL
  )
}


.getAI <- function(x, t, g = NULL, TargetUnits = "mm") {
  if (is.null(g) & !is.null(TargetUnits)) { g <- .getG(TargetUnits) }
  dt <- mean(diff(t))
  as.numeric(x %*% x) * dt * pi / (2 * g)
}

.getRMS <- function(x) {
  sqrt(1 / length(x) * as.numeric(x %*% x)) }

.getPeak <- function(x) {
  max(abs(x))
}


.getZC <- function(x) {
  NP <- length(x)
  ZC <- 0
  if (NP > 2) { ZC <- sum(sign(x[2:NP]) == -sign(x[1:(NP - 1)])) }
  return(ZC)
}

.getHBD <- function(x, t, a = 0.05, b = 0.95, g = 9806.650) {
  dt <- mean(diff(t))
  IA <- dt * (x %*% x) * pi / (2 * g)
  SumIA <- pi * dt * cumsum(x^2)
  A <- a * 2 * g * IA
  B <- b * 2 * g * IA
  k <- 1
  while (SumIA[k] < A) {
    k <- k + 1
  }
  ta <- t[k]
  while (SumIA[k] < B) {
    k <- k + 1
  }
  tb <- t[k]
  D <- tb - ta
  return(D)
}

.getTm <- function(x, t, fmin = 0, fmax = Inf) {
  if (max(abs(x)) == 0) { return(0) }
  NP <- length(x)
  dt <- mean(diff(t))
  Fs <- 1 / dt
  # NFFT <- nextn(NP,factors = 2)
  AW <- 1 / NP * fft(x, inverse = FALSE)
  df <- Fs / NP
  NUP = ceiling(NP / 2) + 1
  fs <- seq(from = 1, to = NUP) * df
  fmin <- max(fmin, min(fs))
  fmax <- min(fmax, max(fs))
  # idx <- inrange(fs[1:NUP],lower=max(fmin,min(fs)),upper= min(fmax,max(fs)))
  idx <- fs >= fmin & fs <= fmax
  Co <- sqrt(Re(AW[1:NUP] * Conj(AW[1:NUP])))
  # f1 <- max(fmin,min(fs))
  # f2 <- min(fmax,max(fs))
  # ix <- fs[1:NUP] %inrange% c(f1,(f2+fs[2]))
  Tm <- sum(Co[idx]^2 / fs[idx]) / sum(Co[idx]^2)
  return(Tm)
}


.rationalize <- function(x) {
  stopifnot(is.numeric(x), length(x) == 1L, is.finite(x), x > 0)
  f <- 1 / x
  eps <- 16 * .Machine$double.eps * max(1, f)
  f <- f - eps
  L2 <- log(f, 2)
  L5 <- log(5, 2)
  b <- 0L:max(0L, ceiling(log(f, 5)) + 6L)
  a <- pmax(0L, ceiling(L2 - b * L5))
  C <- (2^a) * (5^b)
  C <- C[C >= f]
  if (!length(C)) stop("Error in .rationalize(): no candidate found")
  Fs <- C[which.min(C)]                 # el menor >= f
  if (Fs <= .Machine$integer.max) Fs <- as.integer(round(Fs))
  return(1 / Fs)                                   # dt
}

