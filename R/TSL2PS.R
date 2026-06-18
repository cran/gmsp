#' Convert canonical long time series to response spectra.
#'
#' `TSL2PS()` is the spectra helper for canonical `TSL` tables produced by
#' [AT2TS()], [VT2TS()], and [DT2TS()]. It derives grouping keys from the
#' `TSL` schema instead of exposing `BY` or column-name arguments.
#'
#' @param .x Canonical `TSL` `data.table` with columns `t`, `s`, `ID`, and
#'   `OCID`, plus optional metadata columns.
#' @param xi numeric. Damping ratio(s) `0..1`. Scalar input preserves the
#'   historical output schema; vector input adds an `xi` column.
#' @param Tn numeric vector. Natural periods in seconds. Must not include `0`;
#'   the `Tn = 0` peak-value anchor is prepended internally.
#' @param output character. One of `"PSL"` or `"PSW"`. Default `"PSL"`.
#' @param D50 logical scalar. If `TRUE`, add the median rotated horizontal
#'   component as `OCID = "D50"` for every metadata group with `H1` and `H2`.
#' @param D100 logical scalar. If `TRUE`, add the maximum rotated horizontal
#'   component as `OCID = "D100"` for every metadata group with `H1` and `H2`.
#' @param nTheta integer. Number of rotation angles in `[0, 180)` for
#'   `D50 = TRUE` or `D100 = TRUE`. Default `180L` (1-degree step).
#'
#' @return A `data.table`.
#'   - `output = "PSL"` returns a long table with metadata columns, `OCID`,
#'     `Tn`, spectral `ID` (`"PSA"`, `"PSV"`, `"SD"`), and `S`. If `xi` is a
#'     vector, the output also includes `xi`.
#'   - `output = "PSW"` returns a wide table with metadata columns, `Tn`, and
#'     spectral component columns such as `PSA.H1`, `PSV.H1`, and `SD.H1`. If
#'     `xi` is a vector, the output also includes `xi`.
#'     If requested, D50 and D100 appear as ordinary component suffixes such as
#'     `PSA.D50` and `PSA.D100`.
#'
#' @seealso [AT2TS()], [VT2TS()], [DT2TS()]
#' @examples
#' t <- seq(0, 1, by = 0.01)
#' tsl <- data.table::rbindlist(list(
#'   data.table::data.table(t = t, s = sin(2 * pi * t),
#'                          ID = "AT", OCID = "H1"),
#'   data.table::data.table(t = t, s = cos(2 * pi * t),
#'                          ID = "VT", OCID = "H1"),
#'   data.table::data.table(t = t, s = sin(pi * t),
#'                          ID = "DT", OCID = "H1"),
#'   data.table::data.table(t = t, s = sin(2 * pi * t + 0.3),
#'                          ID = "AT", OCID = "H2"),
#'   data.table::data.table(t = t, s = cos(2 * pi * t + 0.4),
#'                          ID = "VT", OCID = "H2"),
#'   data.table::data.table(t = t, s = sin(pi * t + 0.2),
#'                          ID = "DT", OCID = "H2")
#' ))
#' ps <- TSL2PS(tsl, Tn = c(0.1, 0.2))
#' head(ps)
#' rot <- TSL2PS(tsl, Tn = 0.1, D50 = TRUE, D100 = TRUE, nTheta = 6L)
#' rot[OCID %in% c("D50", "D100")]
#'
#' @importFrom data.table := as.data.table copy dcast is.data.table melt rbindlist setcolorder setorderv
#' @export
TSL2PS <- function(.x, xi = 0.05, Tn = NULL, output = "PSL",
                   D50 = FALSE, D100 = FALSE, nTheta = 180L) {
  output <- match.arg(output, c("PSL", "PSW"))
  stopifnot(is.numeric(xi), length(xi) > 0L, all(is.finite(xi)),
            all(xi >= 0), all(xi <= 1))
  stopifnot(is.logical(D50), length(D50) == 1L, !is.na(D50))
  stopifnot(is.logical(D100), length(D100) == 1L, !is.na(D100))
  stopifnot(is.numeric(nTheta), length(nTheta) == 1L, is.finite(nTheta), nTheta >= 2L)
  if (length(xi) > 1L) {
    return(.tsl2psVector(.x = .x, xi = xi, Tn = Tn, output = output,
                         D50 = D50, D100 = D100, nTheta = nTheta))
  }

  DATA <- copy(as.data.table(.x))
  stopifnot(is.data.table(DATA))
  nTheta <- as.integer(nTheta)

  REQUIRED <- c("t", "s", "ID", "OCID")
  MISSING <- setdiff(REQUIRED, names(DATA))
  if (length(MISSING)) {
    stop(sprintf("TSL2PS requires canonical TSL columns; missing: %s.",
                 paste(MISSING, collapse = ", ")), call. = FALSE)
  }

  Tn <- .resolveTnPS(Tn)

  SOURCEIDS <- c("AT", "VT", "DT")
  MissingSourceID <- setdiff(SOURCEIDS, unique(as.character(DATA[["ID"]])))
  if (length(MissingSourceID)) {
    stop(sprintf("TSL2PS requires AT, VT, and DT source rows; missing: %s.",
                 paste(MissingSourceID, collapse = ", ")), call. = FALSE)
  }

  META <- setdiff(names(DATA), REQUIRED)
  GROUP <- c(META, "OCID")

  IDCheck <- DATA[, .(
    HasAT = "AT" %chin% as.character(ID),
    HasVT = "VT" %chin% as.character(ID),
    HasDT = "DT" %chin% as.character(ID)
  ), by = GROUP]
  OK <- IDCheck[["HasAT"]] & IDCheck[["HasVT"]] & IDCheck[["HasDT"]]
  BAD <- IDCheck[!OK]
  if (nrow(BAD)) {
    stop("TSL2PS requires AT, VT, and DT rows for every metadata/OCID group.",
         call. = FALSE)
  }

  PSL <- DATA[, .tslBuildComponentPSL(.SD, xi = xi, Tn = Tn),
              by = GROUP, .SDcols = c("t", "s", "ID")]

  if (D50 || D100) {
    RotDPSL <- .tslBuildRotDPSL(DATA, META = META, xi = xi,
                                Tn = Tn, D50 = D50, D100 = D100,
                                nTheta = nTheta)
    PSL <- rbindlist(list(PSL, RotDPSL), use.names = TRUE)
  }

  setcolorder(PSL, c(META, "OCID", "Tn", "ID", "S"))
  setorderv(PSL, c(META, "OCID", "ID", "Tn"), na.last = TRUE)

  if (identical(output, "PSL")) return(PSL[])

  PSL2PSW(PSL, by = META)
}

.tsl2psVector <- function(.x, xi, Tn, output, D50, D100, nTheta) {
  if ("xi" %chin% names(as.data.table(.x))) {
    stop("TSL2PS vector xi cannot be used when input metadata contains xi.",
         call. = FALSE)
  }
  LIST <- lapply(seq_along(xi), function(i) {
    OUT <- TSL2PS(.x = .x, xi = xi[i], Tn = Tn, output = output,
                  D50 = D50, D100 = D100, nTheta = nTheta)
    set(OUT, j = "xi", value = xi[i])
    set(OUT, j = "XiOrder", value = i)
    OUT
  })
  OUT <- rbindlist(LIST, use.names = TRUE)

  if (identical(output, "PSL")) {
    META <- setdiff(names(OUT), c("xi", "XiOrder", "OCID", "Tn", "ID", "S"))
    setcolorder(OUT, c(META, "xi", "XiOrder", "OCID", "Tn", "ID", "S"))
    setorderv(OUT, c(META, "XiOrder", "OCID", "ID", "Tn"), na.last = TRUE)
    OUT[, "XiOrder" := NULL]
    setcolorder(OUT, c(META, "xi", "OCID", "Tn", "ID", "S"))
    return(OUT[])
  }

  SPECTRA <- grep("^(PSA|PSV|SD)[.]", names(OUT), value = TRUE)
  META <- setdiff(names(OUT), c("xi", "XiOrder", "Tn", SPECTRA))
  setcolorder(OUT, c(META, "xi", "XiOrder", "Tn", SPECTRA,
                     setdiff(names(OUT), c(META, "xi", "XiOrder", "Tn", SPECTRA))))
  setorderv(OUT, c(META, "XiOrder", "Tn"), na.last = TRUE)
  OUT[, "XiOrder" := NULL]
  setcolorder(OUT, c(META, "xi", "Tn", SPECTRA,
                     setdiff(names(OUT), c(META, "xi", "Tn", SPECTRA))))
  OUT[]
}

TNPS <- c(
  0.01, 0.02, 0.022, 0.025, 0.029, 0.03, 0.032, 0.035, 0.036,
  0.04, 0.042, 0.044, 0.045, 0.046, 0.048, 0.05, 0.055, 0.06, 0.065, 0.067,
  0.07, 0.075, 0.08, 0.085, 0.09, 0.095, 0.1, 0.11, 0.12, 0.125, 0.13, 0.133,
  0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.22, 0.24, 0.25, 0.26, 0.28, 0.29,
  0.3, 0.32, 0.34, 0.35, 0.36, 0.38, 0.4, 0.42, 0.44, 0.45, 0.46, 0.48, 0.5,
  0.55, 0.6, 0.65, 0.667, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1, 1.1, 1.2, 1.3,
  1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.2, 2.4, 2.5, 2.6, 2.8, 3, 3.2, 3.4, 3.5,
  3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 7.5, 10
)

.resolveTnPS <- function(Tn) {
  if (is.null(Tn)) return(TNPS)
  stopifnot(is.numeric(Tn), length(Tn) > 0L, all(is.finite(Tn)))
  if (any(Tn == 0)) {
    stop("Tn must not include 0: TSL2PS adds Tn=0 as peak-value anchor automatically.",
         call. = FALSE)
  }
  if (any(Tn < 0)) {
    stop("Tn must contain positive periods.", call. = FALSE)
  }
  as.numeric(Tn)
}

.tslBuildComponentPSL <- function(DAT, xi, Tn) {
  PAIRS <- list(AT = "PSA", VT = "PSV", DT = "SD")
  rbindlist(lapply(names(PAIRS), function(SourceID) {
    SRC <- DAT[ID == SourceID, .(t, s)]
    .tslSeriesSpectrum(SRC, ID = PAIRS[[SourceID]], xi = xi, Tn = Tn)
  }), use.names = TRUE)
}

.tslSeriesSpectrum <- function(DAT, ID, xi, Tn) {
  if (nrow(DAT) <= 2L) {
    stop(sprintf("TSL2PS requires more than two samples for %s.", ID),
         call. = FALSE)
  }
  DAT <- .regularize(.x = DAT, time = "t")
  Tv <- DAT[["t"]]
  dt <- Tv[2L] - Tv[1L]
  Fs <- 1 / dt
  stopifnot(isTRUE(all.equal(Fs, round(Fs))))

  s <- as.numeric(DAT[["s"]])
  if (any(!is.finite(s))) {
    stop(sprintf("TSL2PS requires finite samples for %s.", ID), call. = FALSE)
  }

  S <- numeric(length(Tn))
  for (j in seq_along(Tn)) {
    S[j] <- .sdofMax(s, dt = dt, xi = xi, Tn = Tn[j], ID = ID)
  }

  data.table(Tn = c(0, Tn), ID = ID, S = c(max(abs(s)), S))
}

.sdofMax <- function(s, dt, xi, Tn, ID) {
  omegan <- 2 * pi / Tn
  Damping <- 2 * xi * omegan
  Stiffness <- omegan ^ 2
  StateMatrix <- matrix(c(0, 1, -Stiffness, -Damping), 2L, 2L, byrow = TRUE)
  StepMatrix <- expm::expm(StateMatrix * dt)
  StepInput <- (StepMatrix - diag(2L)) %*% pracma::pinv(StateMatrix) %*%
    matrix(c(0, 1), 2L, 1L)
  State <- matrix(0, 2L, length(s))
  for (k in 2L:length(s)) {
    State[, k] <- StepMatrix %*% State[, k - 1L] + StepInput * s[k]
  }
  SD <- max(abs(State[1L, ]))
  switch(ID, PSA = SD * Stiffness, PSV = SD * omegan, SD = SD)
}

.tslBuildRotDPSL <- function(DATA, META, xi, Tn, D50, D100, nTheta) {
  SDcols <- c("t", "s", "ID", "OCID")
  if (length(META) > 0L) {
    return(DATA[, .tslBuildRotDGroup(.SD, xi = xi, Tn = Tn,
                                     D50 = D50, D100 = D100,
                                     nTheta = nTheta),
                by = META, .SDcols = SDcols])
  }
  .tslBuildRotDGroup(DATA[, SDcols, with = FALSE], xi = xi,
                     Tn = Tn, D50 = D50, D100 = D100, nTheta = nTheta)
}

.tslBuildRotDGroup <- function(DAT, xi, Tn, D50, D100, nTheta) {
  PAIRS <- list(AT = "PSA", VT = "PSV", DT = "SD")
  rbindlist(lapply(names(PAIRS), function(SourceID) {
    WIDE <- .tslRotDPair(DAT, SourceID)
    .tslRotDSpectrum(WIDE, ID = PAIRS[[SourceID]], xi = xi,
                     Tn = Tn, D50 = D50, D100 = D100,
                     nTheta = nTheta)
  }), use.names = TRUE)
}

.tslRotDPair <- function(DAT, SourceID) {
  OUT <- DAT[ID == SourceID & OCID %chin% c("H1", "H2"), .(t, OCID, s)]
  if (!setequal(unique(as.character(OUT[["OCID"]])), c("H1", "H2"))) {
    stop(sprintf("TSL2PS rotated spectra require OCID values H1 and H2 for %s.",
                 SourceID), call. = FALSE)
  }

  COUNTS <- OUT[, .(RowCount = .N), by = c("t", "OCID")]
  if (any(COUNTS[["RowCount"]] > 1L)) {
    stop("TSL2PS rotated spectra cannot cast duplicated (t, OCID) rows.",
         call. = FALSE)
  }

  OUT <- dcast(OUT, t ~ OCID, value.var = "s")
  if (anyNA(OUT[, .(H1, H2)])) {
    stop("TSL2PS rotated spectra require aligned H1/H2 samples.", call. = FALSE)
  }
  .regularize(OUT, time = "t")
}

.tslRotDSpectrum <- function(DAT, ID, xi, Tn, D50, D100, nTheta) {
  H1 <- as.numeric(DAT[["H1"]])
  H2 <- as.numeric(DAT[["H2"]])
  Tv <- DAT[["t"]]
  if (length(Tv) <= 2L) {
    stop("Rotated spectra require more than two samples per horizontal component.",
         call. = FALSE)
  }
  if (any(!is.finite(H1)) || any(!is.finite(H2))) {
    stop("Rotated spectra require finite horizontal samples.", call. = FALSE)
  }

  dt <- Tv[2L] - Tv[1L]
  Fs <- 1 / dt
  stopifnot(isTRUE(all.equal(Fs, round(Fs))))

  THETA <- seq(0, pi, length.out = nTheta + 1L)[seq_len(nTheta)]
  HMAT <- outer(H1, cos(THETA)) + outer(H2, sin(THETA))
  AUX <- rbind(apply(abs(HMAT), 2L, max),
               .tslRotDResponse(HMAT, Tv = Tv, dt = dt, xi = xi, Tn = Tn,
                                ID = ID))

  OUT <- list()
  if (D50) {
    S <- apply(AUX, 1L, stats::quantile, probs = 0.5,
               names = FALSE, na.rm = FALSE)
    OUT[["D50"]] <- data.table(Tn = c(0, Tn), ID = ID,
                               OCID = "D50", S = as.numeric(S))
  }
  if (D100) {
    S <- apply(AUX, 1L, max)
    OUT[["D100"]] <- data.table(Tn = c(0, Tn), ID = ID,
                                OCID = "D100", S = as.numeric(S))
  }

  rbindlist(OUT, use.names = TRUE)
}

.tslRotDResponse <- function(HMAT, Tv, dt, xi, Tn, ID) {
  OUT <- matrix(NA_real_, length(Tn), ncol(HMAT))
  for (j in seq_along(Tn)) {
    omegan <- 2 * pi / Tn[j]
    Damping <- 2 * xi * omegan
    Stiffness <- omegan ^ 2
    StateMatrix <- matrix(c(0, 1, -Stiffness, -Damping), 2L, 2L, byrow = TRUE)
    StepMatrix <- expm::expm(StateMatrix * dt)
    StepInput <- (StepMatrix - diag(2L)) %*% pracma::pinv(StateMatrix) %*%
      matrix(c(0, 1), 2L, 1L)
    State <- matrix(0, 2L, ncol(HMAT))
    MaxAbs <- numeric(ncol(HMAT))
    for (k in 2L:length(Tv)) {
      State <- StepMatrix %*% State + StepInput %*%
        matrix(HMAT[k, ], 1L, ncol(HMAT))
      MaxAbs <- pmax(MaxAbs, abs(State[1L, ]))
    }
    OUT[j, ] <- switch(ID, PSA = MaxAbs * Stiffness,
                       PSV = MaxAbs * omegan, SD = MaxAbs)
  }
  OUT
}

.tslPslToPsw <- function(PSL, META) {
  PSL2PSW(PSL, by = META)
}
