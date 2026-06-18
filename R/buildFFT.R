buildFFT <- function(.x = NULL, s = NULL, t = NULL, zp = 256, kf = 1) {
  if (!is.null(.x)) {
    stopifnot(all(c("s", "t") %in% names(.x)))
    s <- .x$s - mean(.x$s)
    t <- .x$t
  } else {
    stopifnot(!is.null(s) & !is.null(t))
  }
  dt <- mean(diff(t))
  Fs <- 1 / dt # Sampling frequency

  # Zero-padding
  X <- c(rep(0, zp), s, rep(0, zp))
  N <- length(X)
  NP <- 2^ceiling(log2(N)) # Next power of 2 greater than N
  # Perform Fourier Transform on the zero-padded signal
  FFT <- 1 / NP * fft(X)
  # Calculate the double-sided amplitude spectra
  Ao <- Mod(FFT)
  # Calculate the single-sided amplitude spectra
  if (NP %% 2 == 0) { # NP is even
    A <- Ao[1:(NP / 2 + 1)]
    A[2:(NP / 2)] <- 2 * A[2:(NP / 2)]
    fs <- (0:(NP / 2)) * (Fs / NP)
  } else { # NP is odd
    A <- Ao[1:((NP + 1) / 2)]
    A[2:((NP - 1) / 2)] <- 2 * A[2:((NP - 1) / 2)]
    fs <- (0:((NP - 1) / 2)) * (Fs / NP)
  }
  DT <- data.table(fs = fs, A = A)

  # Cut freq
  Fnyq <- Fs / 2
  Fmax <- round(kf * Fnyq)
  DT <- DT[fs <= Fmax]
  return(DT)
}
