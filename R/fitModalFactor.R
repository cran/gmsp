# Spectral-match modal shaping. Graduated verbatim from
# inst/scripts/match/setup.R (V3 contract) on the match-graduation SoT:
# bodies unchanged except bare package-internal calls (gmsp:: prefix
# dropped) and explicit stats:: imports. Do not edit the internals
# without a Tier-2 identity harness.

utils::globalVariables(c(
  "RecordID", "OCID", "Tn", "Sa", "kind", "ID", "Units", "inside",
  "..KEY.record"))

KEY.record <- c("RecordID", "OwnerID", "EventID", "StationID")
IDS.signal <- c("AT", "VT", "DT")
MAXIT <- 1L
LAMBDA.band <- 1
LAMBDA.move <- 0.01

.requireColumns <- function(DT, cols, label) {
  Missing <- setdiff(cols, names(DT))
  if (length(Missing)) {
    stop(sprintf("%s missing columns: %s",
                 label, paste(Missing, collapse = ", ")),
         call. = FALSE)
  }
  invisible(NULL)
}

.matchIMFRemove <- function(x, modes) {
  if (is.null(x) || !length(x)) return(character())
  X <- unlist(x, use.names = FALSE)
  if (!length(X)) return(character())
  Text <- as.character(X)
  IDX <- suppressWarnings(as.integer(Text))
  Numeric <- !is.na(IDX) & grepl("^-?[0-9]+$", Text)
  OUT <- character()
  if (any(Numeric)) {
    Pos <- IDX[Numeric & IDX > 0L]
    Pos <- Pos[Pos >= 1L & Pos <= length(modes)]
    Neg <- length(modes) + IDX[Numeric & IDX < 0L] + 1L
    Neg <- Neg[Neg >= 1L & Neg <= length(modes)]
    OUT <- c(OUT, modes[sort(unique(c(Pos, Neg)))])
  }
  Named <- Text[!Numeric]
  if (length(Named)) {
    Missing <- setdiff(Named, modes)
    if (length(Missing)) {
      stop(sprintf("Requested IMF modes not found: %s",
                   paste(Missing, collapse = ", ")), call. = FALSE)
    }
    OUT <- c(OUT, Named)
  }
  unique(OUT)
}

.matchModeColumns <- function(DT, params) {
  COLS <- grep("^IMF[0-9]+$", names(DT), value = TRUE)
  COLS <- COLS[order(as.integer(sub("^IMF", "", COLS)))]
  if (!length(COLS)) stop("IMF table has no IMF mode columns.",
                          call. = FALSE)
  Remove <- .matchIMFRemove(params$imfRemove, modes = COLS)
  OUT <- setdiff(COLS, Remove)
  if (!length(OUT)) stop("IMF mode filter removed all IMF modes.",
                         call. = FALSE)
  if (params$includeResidue) OUT <- c(OUT, intersect("residue", names(DT)))
  OUT
}

.matchReadIMF <- function(path, record, params, imf = NULL) {
  if (is.null(imf)) {
    stop("runMatch requires IMF rows from getRecords() or data/IMF.csv.",
         call. = FALSE)
  }
  OUT <- imf[RecordID == record & OCID == params$ocid]
  if (!nrow(OUT)) {
    stop(sprintf("No %s IMF rows for %s.", params$ocid, record),
         call. = FALSE)
  }
  .requireColumns(OUT, "signal", "IMF")
  OUT[]
}

.matchBuildTSW <- function(IMF, signal, params) {
  AT <- data.table(t = IMF$t)
  AT[, (params$ocid) := signal]
  TSW <- AT2TS(
    .x = AT,
    units.source = params$units$source,
    units.target = params$units$target,
    Fmax = params$fmax,
    kNyq = params$kNyq,
    output = "TSW",
    isRaw = FALSE,
    audit = FALSE)
  if ("ts" %chin% names(TSW)) setnames(TSW, "ts", "t")
  if ("Units" %chin% names(TSW)) TSW[, Units := NULL]
  for (COL in KEY.record) TSW[, (COL) := IMF[[COL]][1L]]
  setcolorder(TSW, c(KEY.record, "t",
                    setdiff(names(TSW), c(KEY.record, "t"))))
  TSW[]
}

.matchBuildPSA <- function(TSW, target, params) {
  TSL <- TSW2TSL(TSW)
  TSL <- TSL[ID %chin% IDS.signal]
  Tn <- target[Tn > 0, Tn]
  PSW <- TSL2PS(.x = TSL, xi = params$xi, Tn = Tn, output = "PSW",
                D50 = FALSE, D100 = FALSE, nTheta = 180)
  COL <- paste("PSA", params$ocid, sep = ".")
  OUT <- rbindlist(list(
    data.table(Tn = 0,
               Sa = max(abs(TSW[[paste("AT", params$ocid, sep = ".")]]),
                        na.rm = TRUE)),
    PSW[, .(Tn, Sa = get(COL))]
  ), use.names = TRUE)
  OUT[, .(Sa = max(Sa, na.rm = TRUE)), by = Tn][order(Tn)]
}

.matchModalSpectra <- function(IMF, modes, target, params) {
  OUT <- rbindlist(lapply(modes, function(COL) {
    TSW <- .matchBuildTSW(IMF = IMF, signal = IMF[[COL]], params = params)
    PSA <- .matchBuildPSA(TSW = TSW, target = target, params = params)
    PSA[, IMF := COL]
    PSA
  }), use.names = TRUE)
  OUT[, .(Sa = mean(Sa, na.rm = TRUE)), by = .(Tn, IMF)]
}

.matchInside <- function(x, low, high) {
  TOL <- 1e-6 * pmax(1, abs(high))
  x >= low - TOL & x <= high + TOL
}

.matchRMSE <- function(x, y) {
  OK <- is.finite(x) & is.finite(y) & y != 0
  sqrt(mean(((x[OK] - y[OK]) / y[OK]) ^ 2))
}

.matchBandMetrics <- function(PSA, target) {
  DT <- target[PSA, on = "Tn", nomatch = 0L]
  DT[, inside := .matchInside(Sa, Sa.low, Sa.high)]
  data.table(
    RMSE = .matchRMSE(DT$Sa, DT$Sa.mean),
    inside = mean(DT$inside),
    bandLoss = mean(pmax((DT$Sa.low - DT$Sa) / DT$Sa.low,
                         (DT$Sa - DT$Sa.high) / DT$Sa.high,
                         0) ^ 2),
    below = sum(DT$Sa < DT$Sa.low),
    above = sum(DT$Sa > DT$Sa.high),
    maxViolation = max(pmax((DT$Sa.low - DT$Sa) / DT$Sa.low,
                            (DT$Sa - DT$Sa.high) / DT$Sa.high,
                            0), na.rm = TRUE),
    medianRatio = stats::median(DT$Sa / DT$Sa.mean),
    medianBandPos = stats::median((DT$Sa - DT$Sa.low) /
                                    (DT$Sa.mean - DT$Sa.low)))
}

.matchSignal <- function(IMF, modes, coeff) {
  rowSums(sweep(IMF[, modes, with = FALSE], 2L, coeff, `*`))
}

.matchEnergyFactor <- function(source, rebuilt) {
  if (length(source) != length(rebuilt)) {
    stop("Stage A energy restoration requires aligned signals.",
         call. = FALSE)
  }
  OK <- is.finite(source) & is.finite(rebuilt)
  E0 <- sum(source[OK] ^ 2)
  E1 <- sum(rebuilt[OK] ^ 2)
  if (!is.finite(E0) || E0 <= 0 || !is.finite(E1) || E1 <= 0) {
    stop("Stage A energy restoration requires positive finite energy.",
         call. = FALSE)
  }
  sqrt(E0 / E1)
}

.matchEvaluate <- function(theta, coeff0, IMF, modes, target, params,
                           materialize = FALSE, restoreEnergy = FALSE) {
  Coeff <- coeff0 * exp(theta)
  Signal <- .matchSignal(IMF = IMF, modes = modes, coeff = Coeff)
  EnergyFactor <- .matchEnergyFactor(source = IMF$signal, rebuilt = Signal)
  if (isTRUE(restoreEnergy)) Signal <- Signal * EnergyFactor
  TSW <- .matchBuildTSW(IMF = IMF, signal = Signal, params = params)
  PSA <- .matchBuildPSA(TSW = TSW, target = target, params = params)
  Metrics <- .matchBandMetrics(PSA = PSA, target = target)
  Move <- mean(theta ^ 2)
  Loss <- Metrics$RMSE ^ 2 +
    LAMBDA.band * Metrics$bandLoss +
    LAMBDA.move * Move
  OUT <- list(loss = Loss, energyFactor = EnergyFactor,
              metrics = Metrics, move = Move)
  if (materialize) {
    OUT$TSW <- copy(TSW)
    OUT$PSA <- copy(PSA)
    TSL <- TSW2TSL(OUT$TSW)
    TSL <- TSL[ID %chin% IDS.signal]
    OUT$PSW <- TSL2PS(.x = TSL, xi = params$xi,
                      Tn = target[Tn > 0, Tn], output = "PSW",
                      D50 = FALSE, D100 = FALSE, nTheta = 180)
  }
  OUT
}

.matchRecord <- function(row, target, source, path, params, imf = NULL) {
  Record <- row$RecordID[1L]
  IMF <- .matchReadIMF(path = path, record = Record, params = params,
                       imf = imf)
  Modes <- .matchModeColumns(IMF, params = params)
  Modal <- .matchModalSpectra(IMF = IMF, modes = Modes, target = target,
                              params = params)
  B0 <- rep(1, length(Modes))
  Theta0 <- rep(0, length(B0))
  Eval <- list()
  n <- 0L
  objective <- function(theta) {
    n <<- n + 1L
    OUT <- .matchEvaluate(theta = theta, coeff0 = B0, IMF = IMF,
                          modes = Modes, target = target, params = params)
    Eval[[n]] <<- data.table(eval = n, loss = OUT$loss,
                             energyFactor = OUT$energyFactor,
                             move = OUT$move, OUT$metrics)
    OUT$loss
  }
  Start <- .matchEvaluate(theta = Theta0, coeff0 = B0, IMF = IMF,
                          modes = Modes, target = target, params = params,
                          materialize = TRUE)
  Fit <- stats::optim(Theta0, fn = objective, method = "L-BFGS-B",
                      lower = rep(log(params$bMin), length(Theta0)),
                      upper = rep(log(params$bMax), length(Theta0)),
                      control = list(maxit = MAXIT))
  Final <- .matchEvaluate(theta = Fit$par, coeff0 = B0, IMF = IMF,
                          modes = Modes, target = target, params = params,
                          materialize = TRUE, restoreEnergy = TRUE)
  B <- B0 * exp(Fit$par)
  SourceTSW <- source[RecordID == Record][order(t)]
  MSG <- if (is.null(Fit$message)) NA_character_ else as.character(Fit$message)
  Coeff <- data.table(row[, ..KEY.record],
                      IMF = Modes,
                      b0 = B0,
                      b = B,
                      ratio = exp(Fit$par),
                      energyFactor = Final$energyFactor,
                      stageFactor = B * Final$energyFactor)
  Metrics <- data.table(
    row[, ..KEY.record],
    status = "done",
    convergence = Fit$convergence,
    message = MSG,
    error = NA_character_,
    evals = length(Eval),
    loss.start = Start$loss,
    loss.final = Final$loss,
    energyFactor.start = Start$energyFactor,
    energyFactor.final = Final$energyFactor,
    RMSE.start = Start$metrics$RMSE,
    RMSE.final = Final$metrics$RMSE,
    inside.start = Start$metrics$inside,
    inside.final = Final$metrics$inside,
    bandLoss.start = Start$metrics$bandLoss,
    bandLoss.final = Final$metrics$bandLoss,
    maxViolation.start = Start$metrics$maxViolation,
    maxViolation.final = Final$metrics$maxViolation,
    bMax.start = max(B0),
    bMax.final = max(B),
    bMedian.final = stats::median(B),
    stageFactorMin.final = min(B * Final$energyFactor),
    stageFactorMedian.final = stats::median(B * Final$energyFactor),
    stageFactorMax.final = max(B * Final$energyFactor),
    ratioMin.final = min(exp(Fit$par)),
    ratioMax.final = max(exp(Fit$par)),
    ratioMedian.final = stats::median(exp(Fit$par)))
  PSA <- rbindlist(list(
    .matchBuildPSA(TSW = SourceTSW, target = target,
                   params = params)[, .(RecordID = Record,
                                        kind = "source", Tn, Sa)],
    Start$PSA[, .(RecordID = Record, kind = "start", Tn, Sa)],
    Final$PSA[, .(RecordID = Record, kind = "final", Tn, Sa)]
  ), use.names = TRUE)
  Eval <- rbindlist(Eval, use.names = TRUE)
  Eval[, RecordID := Record]
  list(TSW = Final$TSW, PSW = Final$PSW, PSA = PSA, coeff = Coeff,
       metrics = Metrics, eval = Eval)
}

#' Fit the modal shaping factors of one record
#'
#' Per-record modal shaping of the spectral-match workflow: released
#' modal amplitudes are fitted against a target response-spectrum band
#' (`shape.modalFactor` in the batch JSON contract), with energy
#' restoration applied after the modal objective.
#'
#' @param .x data.table. IMF rows for the suite (or the record):
#'   `RecordID`, `OwnerID`, `EventID`, `StationID`, `OCID`, `t`,
#'   `signal`, `IMF*` mode columns, and optionally `residue`. Rows are
#'   filtered to `key`'s record and `ocid`.
#' @param target data.table. Target band with columns `Tn`, `Sa.low`,
#'   `Sa.mean`, `Sa.high`; must include `Tn == 0` (PGA) and positive
#'   periods, in the PSA units of `.x`.
#' @param source data.table. Source TSW rows for the suite (columns
#'   `RecordID`, `t`, `AT.<ocid>`, ...) used for the source spectra.
#' @param key data.table. One row carrying `RecordID`, `OwnerID`,
#'   `EventID`, `StationID` of the record to fit.
#' @param ocid character. Component to shape (`"H1"`, `"H2"`, `"UP"`).
#' @param xi numeric. Damping ratio.
#' @param fmax numeric. Analysis bandwidth (Hz).
#' @param kNyq numeric. Nyquist multiple.
#' @param factorMin numeric. Lower bound of the modal factors
#'   (`shape.modalFactor.min`), in (0, 1].
#' @param factorMax numeric. Upper bound of the modal factors
#'   (`shape.modalFactor.max`); must contain 1. Default `Inf`.
#' @param remove character or integer. IMF modes excluded from the
#'   reconstruction (`imf.remove`).
#' @param includeResidue logical. Keep the residue in the
#'   reconstruction.
#' @param unitsSource character. Units of `.x` signals.
#' @param unitsTarget character. Units of the shaped products.
#'
#' @return list with `TSW` (shaped time histories of the fitted
#'   component), `PSW` (shaped spectra), `PSA` (long table with
#'   `kind = source/start/final` curves), `coeff` (per-mode factors:
#'   `b`, `energyFactor`, `stageFactor`), `metrics` (one-row fit
#'   diagnostics), and `eval` (objective evaluations).
#'
#' @examples
#' \dontrun{
#' Fit <- fitModalFactor(.x = IMF, target = Band, source = TSW,
#'                       key = Selection[1L], factorMin = 0.001)
#' }
#' @importFrom stats optim median
#' @export
fitModalFactor <- function(.x, target, source, key,
                           ocid = "H1", xi = 0.05, fmax = 25, kNyq = 4,
                           factorMin, factorMax = Inf,
                           remove = NULL, includeResidue = TRUE,
                           unitsSource = "mm", unitsTarget = "mm") {
  factorMin <- as.numeric(factorMin)
  factorMax <- as.numeric(factorMax)
  if (is.null(factorMax) || !length(factorMax)) factorMax <- Inf
  if (!is.finite(factorMin) || factorMin <= 0 || factorMin > 1 ||
      is.na(factorMax) || factorMax < factorMin ||
      (is.finite(factorMax) && factorMax < 1)) {
    stop("factorMin must be finite, positive, and <= 1; optional factorMax must contain 1.",
         call. = FALSE)
  }
  Params <- list(
    ocid = as.character(ocid),
    xi = as.numeric(xi),
    fmax = as.numeric(fmax),
    kNyq = as.numeric(kNyq),
    bMin = factorMin,
    bMax = factorMax,
    imfRemove = remove,
    includeResidue = isTRUE(includeResidue),
    units = list(source = as.character(unitsSource),
                 target = as.character(unitsTarget)))
  .matchRecord(row = key, target = target, source = source, path = NULL,
               params = Params, imf = .x)
}
