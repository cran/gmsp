#' Decompose one time series into intrinsic mode functions.
#'
#' @description
#' Orchestrates VMD/EMD/EEMD decomposition for one canonical `t`/`s` signal and
#' returns either IMFs, a recomposed time series, or long/wide tables depending
#' on `output`.
#'
#' `TS2IMF()` is a worker. It does not detect or own record/component grouping.
#' Use `data.table` grouping at the call site for `TSL` tables.
#'
#' @param .x `data.table` with canonical `t` and `s` columns.
#' @param method character. One of `"vmd"`, `"emd"`, `"eemd"` (default `"vmd"`).
#' @param K integer. Number of IMFs (default `12`).
#' @param alpha,tau,DC,init,tol numeric. Parameters for VMD.
#' @param output character or `NULL`. One of `"TSL"`, `"TSW"`, `"IMF"`
#'   (default `NULL`).
#' @param verbose logical. Engine verbosity (default `FALSE`).
#' @param boundary,stop.rule character. EMD/EEMD parameters.
#' @param noise.type,noise.amp,trials parameters for EEMD.
#' @param imf.remove character or integer. IMF selection (optional). Character
#'   values remove explicit mode names such as `"IMF1"`. Numeric values select
#'   IMF indices to remove: positive indices count from the first IMF, negative
#'   indices count from the last IMF, and both sets are unioned. For example,
#'   `c(1L, -1L)` removes `IMF1` and the last IMF. Zero, non-finite,
#'   and out-of-range numeric values are ignored.
#' @param remove.Fo,keep.Fo numeric length-2 (Hz) frequency band rules
#'   (optional).
#' @param keep.Residue logical. If TRUE (default), include `residue` in the
#'   reconstruction of `signal.R`. If FALSE, `signal.R` is built only from the
#'   kept IMFs.
#'
#' @return Depending on `output`, returns `TSL`, `TSW`, `IMF` or a list with the
#'   decomposition.
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' x <- data.table::data.table(
#'   t = t,
#'   s = sin(2 * pi * t) + 0.1 * sin(10 * pi * t)
#' )
#' imf <- TS2IMF(x, method = "vmd", K = 2, output = "IMF")
#' imf
#'
#' @export
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table melt
#' @importFrom data.table setnames
#' @importFrom VMDecomp vmd
#' @importFrom hht EEMD
#' @importFrom hht EEMDCompile
#' @importFrom EMD emd
TS2IMF <- function(
  .x,
  method = "vmd", K = 12,
  alpha = 2000, tau = 0, DC = TRUE, init = 0, tol = 1e-7,
  output = NULL, verbose = FALSE,
  boundary = "wave", stop.rule = "type5",
  noise.type = "gaussian", noise.amp = 0.5e-7, trials = 10,
  imf.remove = NULL, remove.Fo = NULL, keep.Fo = NULL,
  keep.Residue = TRUE
) {
  X <- copy(as.data.table(.x))
  stopifnot(is.data.table(X))
  stopifnot(c("t", "s") %chin% names(X))
  stopifnot(nrow(X) > 2L)
  X <- X[, c("t", "s"), with = FALSE]

  method <- tolower(method[1L])
  stopifnot(method %in% c("vmd", "emd", "eemd"))
  stopifnot(
    is.null(output) ||
      (is.character(output) && length(output) == 1L &&
         output %chin% c("TSL", "TSW", "IMF"))
  )

  .imfPick <- function(X, imf) {
    v <- unique(X$IMF)
    v <- v[!(v %chin% c("residue", "signal"))]
    k <- length(v)
    if (k == 0L || !length(imf)) return(character(0))

    Imf <- imf[is.finite(imf)]
    Pos <- suppressWarnings(as.integer(Imf[Imf > 0L]))
    Pos <- unique(Pos[!is.na(Pos) & Pos >= 1L & Pos <= k])
    Neg <- suppressWarnings(as.integer(Imf[Imf < 0L]))
    Neg <- k + Neg[!is.na(Neg)] + 1L
    Neg <- unique(Neg[Neg >= 1L & Neg <= k])

    Idx <- sort(unique(c(Pos, Neg)))
    if (!length(Idx)) return(character(0))
    v[Idx]
  }

  .decompVMD <- function(s, K, alpha, tau, DC, init, tol, verbose) {
    OUT <- VMDecomp::vmd(
      data = s, alpha = alpha, tau = tau, K = K,
      DC = DC, init = init, tol = tol, verbose = verbose
    )
    U <- as.matrix(OUT$u)
    if (nrow(U) != length(s)) {
      if (ncol(U) == length(s)) {
        U <- t(U)
      } else {
        stop("Unexpected VMD output dimensions.")
      }
    }
    residue <- s - rowSums(U)
    list(U = U, residue = residue)
  }

  .decompEMD <- function(s, ts, K, boundary, stop.rule) {
    AUX <- EMD::emd(
      xt = s, tt = ts, boundary = boundary,
      max.imf = K, stoprule = stop.rule
    )
    U <- as.matrix(AUX$imf)
    if (nrow(U) != length(s)) {
      if (ncol(U) == length(s)) {
        U <- t(U)
      } else {
        stop("Unexpected EMD output dimensions.")
      }
    }
    residue <- as.numeric(AUX$residue)
    list(U = U, residue = residue)
  }

  .decompEEMD <- function(s, ts, K, boundary, noise.amp, noise.type,
                          trials, stop.rule, verbose) {
    nimf <- tryCatch(
      as.integer(EMD::emd(
        xt = s, tt = ts, boundary = boundary,
        max.imf = K, stoprule = stop.rule
      )$nimf),
      error = function(e) K
    )
    dir <- tempdir(check = TRUE)
    on.exit(unlink(dir, force = TRUE, recursive = TRUE), add = TRUE)
    hht::EEMD(
      sig = s, tt = ts, nimf = nimf, max.imf = K,
      boundary = boundary, noise.amp = noise.amp,
      noise.type = noise.type, trials = trials,
      stop.rule = stop.rule, trials.dir = dir, verbose = verbose
    )
    nimf.use <- as.integer(nimf)
    files <- try(list.files(dir, recursive = TRUE, full.names = FALSE),
                 silent = TRUE)
    if (!inherits(files, "try-error")) {
      imf.idx <- suppressWarnings(
        as.integer(gsub("^.*IMF\\s*([0-9]+).*$", "\\1", files))
      )
      if (any(is.finite(imf.idx))) {
        nimf.trials.max <- max(imf.idx[is.finite(imf.idx)], na.rm = TRUE)
        if (is.finite(nimf.trials.max) && nimf.trials.max > 0) {
          if (nimf.use > nimf.trials.max) nimf.use <- nimf.trials.max
        }
      }
    }
    nimf.use <- max(1L, min(nimf.use, K))
    AUX <- suppressWarnings(hht::EEMDCompile(
      trials.dir = dir, trials = trials, nimf = nimf.use
    ))
    U <- as.matrix(AUX$averaged.imfs)
    if (nrow(U) != length(s)) {
      if (ncol(U) == length(s)) {
        U <- t(U)
      } else {
        stop("Unexpected EEMD output dimensions.")
      }
    }
    residue <- as.numeric(AUX$averaged.residue)
    list(U = U, residue = residue)
  }

  .buildTSW <- function(ts, s, U, residue) {
    IMF <- as.data.table(U)
    setnames(IMF, paste0("IMF", seq_len(ncol(IMF))))
    data.table(t = ts, signal = s, IMF, residue = residue)
  }

  .applyIMFFilter <- function(TSW, dt, imf.remove, remove.Fo, keep.Fo,
                              keep.Residue) {
    imf.cols <- grep("^IMF[0-9]+$", names(TSW), value = TRUE)
    n <- nrow(TSW)
    w <- 0.5 - 0.5 * cos(2 * pi * (0:(n - 1L)) / (n - 1L))
    N <- as.integer(2^ceiling(log2(n)))
    kmax <- floor(N / 2L)
    f <- (0:kmax) * ((1 / dt) / N)
    Fo <- numeric(length(imf.cols))
    BW <- numeric(length(imf.cols))
    for (i in seq_along(imf.cols)) {
      y <- as.numeric(TSW[[imf.cols[i]]])
      yw <- y * w
      if (N > n) yw <- c(yw, rep(0, N - n))
      Xf <- fft(yw)
      P <- (Mod(Xf)^2) / sum(w^2)
      P <- Re(P[1:(kmax + 1L)])
      dB.window <- 10
      thr <- max(P, na.rm = TRUE) * 10^(-dB.window / 10)
      msk <- P >= thr
      if (!any(msk)) {
        idx <- if (kmax >= 2L) 2:(kmax + 1L) else 1:(kmax + 1L)
        ipeak <- idx[which.max(P[idx])]
        Fo[i] <- f[ipeak]
        BW[i] <- 0
      } else {
        fm <- f[msk]
        Pm <- P[msk]
        Fo[i] <- sum(fm * Pm) / sum(Pm)
        bw2 <- sum(Pm * (fm - Fo[i])^2) / sum(Pm)
        BW[i] <- sqrt(max(0, bw2))
      }
    }
    rm.cols <- character(0)
    if (!is.null(remove.Fo) && length(remove.Fo) == 2L) {
      L <- Fo - BW
      R <- Fo + BW
      inrng <- (
        is.finite(Fo) &
          is.finite(BW) &
          (L >= as.numeric(remove.Fo[1L])) &
          (R <= as.numeric(remove.Fo[2L]))
      )
      rm.cols <- unique(c(rm.cols, imf.cols[which(inrng)]))
    }
    if (!is.null(keep.Fo) && length(keep.Fo) == 2L) {
      L <- Fo - BW
      R <- Fo + BW
      inrng <- (
        is.finite(Fo) &
          is.finite(BW) &
          (L >= as.numeric(keep.Fo[1L])) &
          (R <= as.numeric(keep.Fo[2L]))
      )
      rm.cols <- unique(c(rm.cols, setdiff(imf.cols, imf.cols[which(inrng)])))
    }
    if (!is.null(imf.remove) && length(imf.remove)) {
      if (is.numeric(imf.remove)) {
        v <- .imfPick(data.table(IMF = imf.cols), imf.remove)
        rm.cols <- unique(c(rm.cols, v))
      } else {
        rm.cols <- unique(c(rm.cols, intersect(imf.cols,
                                               as.character(imf.remove))))
      }
    }
    keep.cols <- setdiff(imf.cols, rm.cols)
    if (length(keep.cols)) {
      if (isTRUE(keep.Residue)) {
        TSW[, `signal.R` := rowSums(.SD) + residue, .SDcols = keep.cols]
      } else {
        TSW[, `signal.R` := rowSums(.SD), .SDcols = keep.cols]
      }
    } else {
      if (isTRUE(keep.Residue)) {
        TSW[, `signal.R` := residue]
      } else {
        TSW[, `signal.R` := 0]
      }
    }
    TSW
  }

  .imfMetrics <- function(TSW, dt) {
    imf.cols <- grep("^IMF[0-9]+$", names(TSW), value = TRUE)
    n <- nrow(TSW)
    w <- 0.5 - 0.5 * cos(2 * pi * (0:(n - 1L)) / (n - 1L))
    N <- as.integer(2^ceiling(log2(n)))
    kmax <- floor(N / 2L)
    f <- (0:kmax) * ((1 / dt) / N)
    Eo <- sum(TSW$signal^2)
    if (!is.finite(Eo) || Eo <= 0) Eo <- 1
    Fo <- numeric(length(imf.cols))
    BW <- numeric(length(imf.cols))
    Ek <- numeric(length(imf.cols))
    for (i in seq_along(imf.cols)) {
      y <- as.numeric(TSW[[imf.cols[i]]])
      yw <- y * w
      if (N > n) yw <- c(yw, rep(0, N - n))
      Xf <- fft(yw)
      P <- (Mod(Xf)^2) / sum(w^2)
      P <- Re(P[1:(kmax + 1L)])
      Ek[i] <- sum(y^2)
      idx <- if (kmax >= 2L) 2:(kmax + 1L) else 1:(kmax + 1L)
      if (length(idx)) {
        thr <- max(P, na.rm = TRUE) * 10^(-10 / 10)
        msk <- P >= thr
        if (!any(msk)) {
          ipeak <- idx[which.max(P[idx])]
          Fo[i] <- f[ipeak]
          BW[i] <- 0
        } else {
          fm <- f[msk]
          Pm <- P[msk]
          Fo[i] <- sum(fm * Pm) / sum(Pm)
          bw2 <- sum(Pm * (fm - Fo[i])^2) / sum(Pm)
          BW[i] <- sqrt(max(0, bw2))
        }
      } else {
        Fo[i] <- 0
        BW[i] <- 0
      }
    }
    M <- data.table(
      IMF = imf.cols, `Fo[Hz]` = Fo, `BW[Hz]` = BW,
      `E[k]/Eo [%]` = 100 * pmax(0, Ek) / Eo
    )
    if ("signal.R" %chin% names(TSW)) {
      y <- as.numeric(TSW[["signal.R"]])
      yw <- y * w
      if (N > n) yw <- c(yw, rep(0, N - n))
      Xf <- fft(yw)
      P <- (Mod(Xf)^2) / sum(w^2)
      P <- Re(P[1:(kmax + 1L)])
      idx <- if (kmax >= 2L) 2:(kmax + 1L) else 1:(kmax + 1L)
      if (length(idx)) {
        thr <- max(P, na.rm = TRUE) * 10^(-10 / 10)
        msk <- P >= thr
        if (!any(msk)) {
          ipeak <- idx[which.max(P[idx])]
          Fo.R <- f[ipeak]
          BW.R <- 0
        } else {
          fm <- f[msk]
          Pm <- P[msk]
          Fo.R <- sum(fm * Pm) / sum(Pm)
          bw2 <- sum(Pm * (fm - Fo.R)^2) / sum(Pm)
          BW.R <- sqrt(max(0, bw2))
        }
      } else {
        Fo.R <- 0
        BW.R <- 0
      }
      ER <- sum(y^2)
      M <- rbind(
        M,
        data.table(
          IMF = "signal.R", `Fo[Hz]` = Fo.R, `BW[Hz]` = BW.R,
          `E[k]/Eo [%]` = 100 * ER / Eo
        )
      )
    }
    M
  }

  .ts2imf <- function(X, method, K, alpha, tau, DC, init, tol,
                      verbose, boundary, stop.rule, noise.type, noise.amp,
                      trials, imf.remove, remove.Fo, keep.Fo,
                      keep.Residue) {
    X <- .regularize(.x = X, time = "t")
    t <- X[["t"]]
    dt <- t[2L] - t[1L]
    t0 <- min(t)
    t1 <- max(t)
    n <- floor((t1 - t0) / dt + 1e-12)
    ts <- (0:n) * dt
    s <- X[["s"]]

    if (method == "vmd") {
      D <- .decompVMD(s, K, alpha, tau, DC, init, tol, verbose)
    }
    if (method == "emd") {
      D <- .decompEMD(s, ts, K, boundary, stop.rule)
    }
    if (method == "eemd") {
      D <- .decompEEMD(s, ts, K, boundary, noise.amp, noise.type, trials,
                       stop.rule, verbose)
    }

    TSW <- .buildTSW(ts, s, D$U, D$residue)
    if (!is.null(imf.remove) || !is.null(remove.Fo) || !is.null(keep.Fo)) {
      TSW <- .applyIMFFilter(TSW, dt, imf.remove, remove.Fo, keep.Fo,
                             keep.Residue)
    }
    M <- .imfMetrics(TSW, dt)
    TSL <- melt(TSW, id.vars = "t", variable.name = "IMF", value.name = "s")
    list(TSW = TSW, TSL = TSL, IMF = M)
  }

  RES <- .ts2imf(
    X,
    method = method, K = K,
    alpha = alpha, tau = tau, DC = DC, init = init, tol = tol,
    verbose = verbose,
    boundary = boundary, stop.rule = stop.rule,
    noise.type = noise.type, noise.amp = noise.amp, trials = trials,
    imf.remove = imf.remove, remove.Fo = remove.Fo, keep.Fo = keep.Fo,
    keep.Residue = keep.Residue
  )

  if (is.null(output)) return(RES)
  if (identical(output, "TSW")) return(RES$TSW)
  if (identical(output, "TSL")) return(RES$TSL)
  if (identical(output, "IMF")) return(RES$IMF)
  stop("Invalid output")
}
