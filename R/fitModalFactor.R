# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.matchSpectra <- function(TSW, target, params) {
  TSL <- lengthenTSW(TSW, by = params$keys)
  PSL <- computeSpectraTSL(TSL, by = params$keys, xi = params$xi,
    Tn = target[Tn > 0, Tn], includeD50 = FALSE, includeD100 = FALSE)
  PSW <- widenPSL(PSL, by = params$keys)
  COL <- paste("PSA", params$OCID, sep = ".")
  PSA <- data.table::rbindlist(list(
    data.table::data.table(Tn = 0, Sa = max(abs(TSW[[paste("AT", params$OCID, sep = ".")]]))),
    PSW[, .(Tn, Sa = .SD[[1L]]), .SDcols = COL]), use.names = TRUE)
  list(PSA = PSA[, .(Sa = max(Sa)), by = Tn][order(Tn)], PSW = PSW)
}

.matchEvaluate <- function(theta, coeff0, IMF, modes, target, params,
                           materialize = FALSE, restoreEnergy = FALSE) {
  Coeff <- coeff0 * exp(theta)
  Signal <- rowSums(sweep(IMF[, modes, with = FALSE], 2L, Coeff, `*`))
  if ("residue" %in% names(IMF)) Signal <- Signal + IMF$residue
  EnergyFactor <- .matchEnergyFactor(source = IMF$signal, rebuilt = Signal)
  if (restoreEnergy) Signal <- Signal * EnergyFactor
  TSL <- IMF[, c(params$keys, "OCID", "t"), with = FALSE]
  TSL[, `:=`(ID = "AT", s = Signal * params$unitFactor)]
  TSL <- do.call(buildTSL, c(list(TSL = TSL, by = params$keys,
    unitsTarget = params$unitsTarget, fmax = params$fmax, kNyq = params$kNyq,
    isRaw = FALSE), params$signalControl))
  TSW <- widenTSL(TSL, by = params$keys)
  Spectra <- .matchSpectra(TSW = TSW, target = target, params = params)
  Metrics <- .matchBandMetrics(PSA = Spectra$PSA, target = target)
  Move <- mean(theta ^ 2)
  OUT <- list(loss = Metrics$RMSE ^ 2 + Metrics$bandLoss + 0.01 * Move,
              energyFactor = EnergyFactor, metrics = Metrics, move = Move)
  if (materialize) {
    OUT$TSW <- TSW
    OUT$PSA <- Spectra$PSA
    OUT$PSW <- Spectra$PSW
  }
  OUT
}

#' Fit modal factors against a response-spectrum band
#'
#' Fits the retained modes of one component of one record, then restores
#' signal energy in the final reconstruction.
#'
#' @param IMF Table for one record with OCID, t, signal, optional \code{IMF<k>}
#'   mode columns and optional residue. Time is in seconds and amplitudes
#'   use unitsSource. A mode entirely NA within a component is absent;
#'   partial NA or non-finite samples are errors.
#' @param target Table with unique nonnegative Tn in seconds and finite
#'   positive Sa.low, Sa.mean and Sa.high, in that order at each period.
#'   The band includes Tn = 0 and positive periods; amplitudes use
#'   unitsTarget/s^2.
#' @param source Wide time-series table for the same record, with its key
#'   columns, t and \code{AT.<OCID>}, \code{VT.<OCID>}, \code{DT.<OCID>}, in unitsSource.
#' @param key One-row table with RecordID, OwnerID, EventID and StationID,
#'   plus ProcessID when present in the context. Missing IMF key columns
#'   are filled from this row; conflicting values are errors.
#' @param OCID Component to fit: "H1", "H2" or "UP".
#' @param xi Positive finite damping ratio accepted by computeSpectraTSL.
#' @param fmax Positive finite frequency of interest in Hz.
#' @param kNyq Positive finite sampling-frequency multiplier.
#' @param signalControl Named list with windowLength, overlap,
#'   astop0, apass0, astopLP and
#'   apassLP, as in buildTSL. Optional flatZeros, trimZeros,
#'   removeMean, forceRegularization and includeWarnings retain their
#'   buildTSL defaults; duplicate or other fields are errors.
#' @param factorMin Finite lower modal-factor bound in (0,1].
#' @param factorMax Upper modal-factor bound at least one; Inf is admitted.
#' @param criterion Explicit xemd selection list: rule "band" with fMin
#'   and fMax in Hz, or rule "energyFraction" with minimum, and action
#'   "keep" or "remove". Band limits are finite with 0 <= fMin <= fMax;
#'   minimum is finite and nonnegative. At least one mode must remain; selection by
#'   mode name or index is not admitted.
#' @param unitsSource,unitsTarget Source and destination length bases:
#'   "mm", "cm" or "m", matched case-insensitively. Source histories and
#'   IMF amplitudes are converted to the destination basis.
#' @param includeResidue Logical scalar, TRUE by default, retaining the
#'   residue when present with a fixed coefficient of one, outside the modal
#'   optimization and its factor bounds. Final energy restoration applies to
#'   the complete reconstruction, including the residue.
#'
#' @return A list with TSW and PSW for the fitted component; PSA with
#'   RecordID, kind (source/start/final), Tn and Sa; coeff with the record
#'   keys, IMF, b0, b, ratio, energyFactor and stageFactor; metrics with
#'   optimizer status and fit diagnostics; and eval in objective-call order.
#'   Time histories and spectra are ordered by record and time or period;
#'   coefficients follow the retained modal order, then the residue. Its row has
#'   b0, b and ratio equal to one, and stageFactor equal to energyFactor.
#'
#' @details Modes are measured and selected through xemd. The objective
#'   uses relative spectral RMSE, squared relative band violations and a
#'   0.01 weight on mean squared log-factor movement. L-BFGS-B starts with
#'   factors equal to one and maxit = 1. Energy restoration applies after
#'   optimization.
#'
#'   In metrics, status is done and convergence and message retain the
#'   stats::optim result: convergence 0 is successful completion, 1 means
#'   that the iteration limit maxit was reached, and 51 and 52 are a warning
#'   and an error of L-BFGS-B explained by message. message is the optimizer
#'   text, such as NEW_X, or NA when the optimizer returns none. With
#'   maxit = 1, code 1 is the expected result and does not indicate a
#'   failure. The convergence code does not change status.
#'
#'   Invalid keys, controls, samples or target bands cause an error with
#'   record context. Zero reconstruction energy is an error. Dependency
#'   conditions retain their classes. Inputs are preserved; the operation
#'   reads or writes no files and uses no random numbers.
#'
#' @seealso [fitRecordFactor()], [assessCompatibility()], [buildTSL()],
#'   [xemd::selectModesTSL()]
#' @examples
#' Key <- data.table::data.table(RecordID = "R1", OwnerID = "A", EventID = "E1",
#'                               StationID = "S1")
#' Time <- seq(0, 4, by = 0.01)
#' Mode <- 1000 * sin(2 * pi * Time)
#' Residue <- 100 * sin(pi * Time)
#' IMF <- data.table::data.table(OCID = "H1", t = Time, signal = Mode + Residue,
#'                               IMF1 = Mode, residue = Residue)
#' Source <- data.table::data.table(Key, t = Time, AT.H1 = Mode + Residue,
#'                                  VT.H1 = 0, DT.H1 = 0)
#' Target <- data.table::data.table(Tn = c(0, 1, 2), Sa.low = 1000,
#'                                  Sa.mean = 2000, Sa.high = 3000)
#' Control <- list(windowLength = 128, overlap = 75, astop0 = 0.001, apass0 = 0.001,
#'                 astopLP = 0.001, apassLP = 0.98, includeWarnings = FALSE)
#' Fit <- fitModalFactor(IMF, target = Target, source = Source, key = Key,
#'   OCID = "H1", xi = 0.05, fmax = 25, kNyq = 4, signalControl = Control,
#'   factorMin = 0.001, factorMax = Inf,
#'   criterion = list(rule = "energyFraction", minimum = 0, action = "keep"),
#'   unitsSource = "mm", unitsTarget = "mm")
#' print(Fit$coeff[, c("IMF", "b", "energyFactor", "stageFactor")])
#' print(Fit$PSA[Fit$PSA$kind == "final"])
#' print(Fit$metrics[, c("status", "convergence", "RMSE.start", "RMSE.final")])
#' @export
fitModalFactor <- function(IMF, target, source, key, OCID, xi, fmax, kNyq,
                           signalControl, factorMin, factorMax, criterion,
                           unitsSource, unitsTarget, includeResidue = TRUE) {
  Decomposition <- .lengthenIMF(IMF, key = key)
  ModeColumns <- grep("^IMF[0-9]+$", names(IMF), value = TRUE)
  Keys <- setdiff(names(Decomposition$signal), c("OCID", "ID", "t", "s"))
  Key <- data.table::as.data.table(key)[, Keys, with = FALSE]
  Context <- paste(paste0(Keys, "=", unlist(Key)), collapse = ", ")
  tryCatch({
    Target <- .checkTargetBand(target, operation = "fitModalFactor")
    if (!any(Target$Tn == 0) || !any(Target$Tn > 0)) {
      .stopUsage("target requires PGA at Tn = 0 and positive periods.", operation = "fitModalFactor")
    }
    if (!is.character(OCID) || length(OCID) != 1L || is.na(OCID) ||
        !OCID %in% c("H1", "H2", "UP")) {
      .stopUsage("OCID must be H1, H2 or UP.", operation = "fitModalFactor")
    }
    for (j in c("xi", "fmax", "kNyq")) {
      x <- switch(j, xi = xi, fmax = fmax, kNyq = kNyq)
      if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
        .stopUsage(j, " must be a finite positive number.", operation = "fitModalFactor")
      }
    }
    if (!is.numeric(factorMin) || length(factorMin) != 1L ||
        !is.finite(factorMin) || factorMin <= 0 || factorMin > 1 ||
        !is.numeric(factorMax) || length(factorMax) != 1L ||
        is.na(factorMax) || factorMax < 1 || factorMax < factorMin) {
      .stopUsage("Modal factor bounds must be positive and contain one.", operation = "fitModalFactor")
    }
    if (!is.logical(includeResidue) || length(includeResidue) != 1L || is.na(includeResidue)) {
      .stopUsage("includeResidue must be one logical value.", operation = "fitModalFactor")
    }
    Required <- c("windowLength", "overlap", "astop0", "apass0",
                  "astopLP", "apassLP")
    Allowed <- c(Required, "flatZeros", "trimZeros", "removeMean", "forceRegularization",
                 "includeWarnings")
    if (!is.list(signalControl) || is.null(names(signalControl)) ||
        anyNA(names(signalControl)) || anyDuplicated(names(signalControl)) ||
        !all(Required %in% names(signalControl)) || !all(names(signalControl) %in% Allowed)) {
      .stopUsage("signalControl requires the signal window/filter controls and no duplicate or unknown keys.",
                 operation = "fitModalFactor")
    }
    if (!is.list(criterion) || is.null(names(criterion)) ||
        anyNA(names(criterion)) || anyDuplicated(names(criterion)) ||
        !is.character(criterion$rule) || length(criterion$rule) != 1L ||
        is.na(criterion$rule) || !criterion$rule %in% c("band", "energyFraction")) {
      .stopUsage("criterion must declare rule band or energyFraction.", operation = "fitModalFactor")
    }
    Decomposition <- lapply(Decomposition, function(x) {
      IDX <- x[["OCID"]] == OCID
      x[IDX]
    })
    if (!nrow(Decomposition$signal)) {
      .stopUsage("No IMF samples for OCID ", OCID, ".", operation = "fitModalFactor")
    }
    Metrics <- xemd::measureModesTSL(Decomposition)$metrics
    Selection <- xemd::selectModesTSL(Decomposition, metrics = Metrics, criterion = criterion)
    Modes <- unique(Selection$modes[["mode"]])
    if (!length(Modes)) .stopUsage("The criterion retains no modes.", operation = "fitModalFactor")
    IMF <- Decomposition$signal[, c(Keys, "OCID", "t", "s"), with = FALSE]
    data.table::setnames(IMF, "s", "signal")
    COLS <- ModeColumns[match(Modes, as.integer(sub("^IMF", "", ModeColumns)))]
    for (j in seq_along(Modes)) {
      DT <- Selection$modes[mode == Modes[j]][order(t)]
      data.table::set(IMF, j = COLS[j], value = DT[["s"]])
    }
    if (includeResidue && "residue" %in% names(Decomposition)) {
      data.table::set(IMF, j = "residue", value = Decomposition$residue[order(t), s])
    }
    Source <- data.table::copy(data.table::as.data.table(source))
    SignalColumns <- paste(c("AT", "VT", "DT"), OCID, sep = ".")
    .checkTableKeys(Source, by = Keys, columns = c("t", SignalColumns),
                    keys = c(Keys, "t"), operation = "fitModalFactor")
    if (!nrow(Source)) .stopUsage("source has no samples.", operation = "fitModalFactor")
    for (j in Keys) {
      if (anyNA(Source[[j]]) || any(Source[[j]] != Key[[j]])) {
        .stopUsage("source contradicts key column ", j, ".", operation = "fitModalFactor")
      }
    }
    Source <- Source[, c(Keys, "t", SignalColumns), with = FALSE]
    data.table::setorderv(Source, "t")
    for (j in c("t", SignalColumns)) {
      if (!is.numeric(Source[[j]]) || any(!is.finite(Source[[j]]))) {
        .stopUsage("source ", j, " must contain finite numeric values.", operation = "fitModalFactor")
      }
    }
    if (!is.character(unitsSource) || length(unitsSource) != 1L || is.na(unitsSource) ||
        !tolower(unitsSource) %in% c("mm", "cm", "m") ||
        !is.character(unitsTarget) || length(unitsTarget) != 1L || is.na(unitsTarget) ||
        !tolower(unitsTarget) %in% c("mm", "cm", "m")) {
      .stopUsage("Source and target length units must be mm, cm or m.", operation = "fitModalFactor")
    }
    UnitFactor <- .resolveUnitFactor(unitsSource = unitsSource, unitsTarget = unitsTarget, ID = "AT")
    for (j in seq_along(SignalColumns)) {
      x <- .resolveUnitFactor(unitsSource = unitsSource, unitsTarget = unitsTarget,
                              ID = c("AT", "VT", "DT")[j])
      if (x != 1) data.table::set(Source, j = SignalColumns[j], value = Source[[SignalColumns[j]]] * x)
    }
    Params <- list(keys = Keys, OCID = OCID, xi = xi, fmax = fmax, kNyq = kNyq,
                   signalControl = signalControl, unitFactor = UnitFactor, unitsTarget = unitsTarget)
    B0 <- rep(1, length(COLS))
    Theta0 <- rep(0, length(B0))
    Eval <- list()
    n <- 0L
    objective <- function(theta) {
      n <<- n + 1L
      OUT <- .matchEvaluate(theta, coeff0 = B0, IMF = IMF, modes = COLS,
                            target = Target, params = Params)
      Eval[[n]] <<- data.table::data.table(eval = n, loss = OUT$loss,
        energyFactor = OUT$energyFactor, move = OUT$move, OUT$metrics)
      OUT$loss
    }
    Start <- .matchEvaluate(Theta0, coeff0 = B0, IMF = IMF, modes = COLS,
                            target = Target, params = Params, materialize = TRUE)
    Fit <- stats::optim(Theta0, fn = objective, method = "L-BFGS-B",
      lower = rep(log(factorMin), length(Theta0)), upper = rep(log(factorMax), length(Theta0)),
      control = list(maxit = 1L))
    Final <- .matchEvaluate(Fit$par, coeff0 = B0, IMF = IMF, modes = COLS,
      target = Target, params = Params, materialize = TRUE, restoreEnergy = TRUE)
    B <- B0 * exp(Fit$par)
    MSG <- if (is.null(Fit$message)) NA_character_ else as.character(Fit$message)
    Coeff <- data.table::data.table(Key, IMF = COLS, b0 = B0, b = B,
      ratio = exp(Fit$par), energyFactor = Final$energyFactor, stageFactor = B * Final$energyFactor)
    if ("residue" %in% names(IMF)) {
      Coeff <- data.table::rbindlist(list(Coeff, data.table::data.table(Key,
        IMF = "residue", b0 = 1, b = 1, ratio = 1,
        energyFactor = Final$energyFactor, stageFactor = Final$energyFactor)), use.names = TRUE)
    }
    Metrics <- data.table::data.table(Key, status = "done", convergence = Fit$convergence,
      message = MSG, error = NA_character_, evals = length(Eval),
      loss.start = Start$loss, loss.final = Final$loss,
      energyFactor.start = Start$energyFactor, energyFactor.final = Final$energyFactor,
      RMSE.start = Start$metrics$RMSE, RMSE.final = Final$metrics$RMSE,
      inside.start = Start$metrics$inside, inside.final = Final$metrics$inside,
      bandLoss.start = Start$metrics$bandLoss, bandLoss.final = Final$metrics$bandLoss,
      maxViolation.start = Start$metrics$maxViolation, maxViolation.final = Final$metrics$maxViolation,
      bMax.start = max(B0), bMax.final = max(B), bMedian.final = stats::median(B),
      stageFactorMin.final = min(B * Final$energyFactor),
      stageFactorMedian.final = stats::median(B * Final$energyFactor),
      stageFactorMax.final = max(B * Final$energyFactor),
      ratioMin.final = min(exp(Fit$par)), ratioMax.final = max(exp(Fit$par)),
      ratioMedian.final = stats::median(exp(Fit$par)))
    SourcePSA <- .matchSpectra(Source, target = Target, params = Params)$PSA
    PSA <- data.table::rbindlist(list(source = SourcePSA, start = Start$PSA, final = Final$PSA),
                               use.names = TRUE, idcol = "kind")
    PSA[, RecordID := Key[["RecordID"]]]
    data.table::setcolorder(PSA, c("RecordID", "kind", "Tn", "Sa"))
    Eval <- data.table::rbindlist(Eval, use.names = TRUE)
    Eval[, RecordID := Key[["RecordID"]]]
    list(TSW = Final$TSW, PSW = Final$PSW, PSA = PSA, coeff = Coeff, metrics = Metrics, eval = Eval)
  }, error = function(e) {
    e$message <- paste0(Context, ": ", conditionMessage(e))
    if (is.null(e$unit)) e$unit <- Context
    e$operation <- "fitModalFactor"
    stop(e)
  })
}
