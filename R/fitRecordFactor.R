# Spectral-match suite record scaling. Graduated verbatim from
# inst/scripts/match/setup.R (V3 contract) on the match-graduation SoT.
# The product diagnostics keep the frozen `stage = "B"` label of the
# stage.csv contract. Do not edit without a Tier-2 identity harness.

utils::globalVariables(c("Sa.mean", "Sa.low", "Sa.high"))

.matchStageBProblem <- function(A, y) {
  OK <- is.finite(y) & y > 0
  if (!any(OK)) {
    stop("Stage B target has no positive finite ordinates.", call. = FALSE)
  }
  list(A = A[OK, , drop = FALSE] / y[OK], y = rep(1, sum(OK)))
}

.matchStageBMatrix <- function(PSA, target, records) {
  Records <- records[records %chin% unique(PSA$RecordID)]
  if (length(Records) < 2L) {
    stop("Stage B requires at least two completed records.", call. = FALSE)
  }
  DT <- PSA[kind == "final" & RecordID %chin% Records,
            .(RecordID, Tn, Sa)]
  WIDE <- dcast(DT, Tn ~ RecordID, value.var = "Sa")
  WIDE <- WIDE[target[, .(Tn, Sa.mean, Sa.low, Sa.high)], on = "Tn",
               nomatch = 0L]
  Records <- Records[Records %chin% names(WIDE)]
  if (length(Records) < 2L) {
    stop("Stage B matrix lost records after target alignment.",
         call. = FALSE)
  }
  S <- as.matrix(WIDE[, Records, with = FALSE])
  storage.mode(S) <- "double"
  if (any(!is.finite(S)) || any(!is.finite(WIDE$Sa.mean))) {
    stop("Stage B matrix contains non-finite values.", call. = FALSE)
  }
  if (any(!is.finite(WIDE$Sa.low) | WIDE$Sa.low <= 0 |
            !is.finite(WIDE$Sa.high) | WIDE$Sa.high <= 0)) {
    stop("Stage B target band must be finite and positive.", call. = FALSE)
  }
  list(Tn = WIDE$Tn, target = WIDE$Sa.mean, low = WIDE$Sa.low,
       high = WIDE$Sa.high, records = Records, S = S,
       A = S / length(Records))
}

.matchStageBStats <- function(A) {
  SVD <- svd(A, nu = 0, nv = 0)$d
  TOL <- max(dim(A)) * .Machine$double.eps * max(SVD)
  data.table(
    equations = nrow(A),
    records = ncol(A),
    rankSVD = sum(SVD > TOL),
    cond2 = max(SVD) / min(SVD),
    rcond = tryCatch(base::rcond(A), error = function(e) NA_real_))
}

.matchStageBBandOSQP <- function(matrix, params) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package Matrix is required for Stage B.", call. = FALSE)
  }
  if (!requireNamespace("osqp", quietly = TRUE)) {
    stop("Package osqp is required for Stage B.", call. = FALSE)
  }
  M <- matrix$A / matrix$target
  N <- ncol(M)
  J <- nrow(M)
  K <- J * N
  W <- params$bandWeight / N
  IDX.record <- rep(seq_len(N), each = J)
  IDX.period <- rep(seq_len(J), times = N)
  Sk <- matrix$S[cbind(IDX.period, IDX.record)]
  # Variables: c (N), band slacks u/l (K each), mean-residual split
  # p/n (J each) with (M c)_j - 1 = p_j - n_j. The deficit weight
  # penalizes n (suite mean below target) more than p when > 1.
  P <- Matrix::bdiag(
    Matrix::Matrix(0, N, N, sparse = TRUE),
    Matrix::Diagonal(2L * K, 2 * W),
    Matrix::Diagonal(J, 2),
    Matrix::Diagonal(J, 2 * params$deficitWeight))
  q <- rep(0, N + 2L * K + 2L * J)
  CON <- Matrix::sparseMatrix(
    i = c(seq_len(N),
          rep(N + seq_len(K), 2L),
          rep(N + K + seq_len(K), 2L),
          N + 2L * K + seq_len(2L * K),
          N + 4L * K + rep(seq_len(J), times = N),
          N + 4L * K + seq_len(J),
          N + 4L * K + seq_len(J),
          N + 4L * K + J + seq_len(2L * J)),
    j = c(seq_len(N),
          IDX.record, N + seq_len(K),
          IDX.record, N + K + seq_len(K),
          N + seq_len(2L * K),
          rep(seq_len(N), each = J),
          N + 2L * K + seq_len(J),
          N + 2L * K + J + seq_len(J),
          N + 2L * K + seq_len(2L * J)),
    x = c(rep(1, N),
          -Sk / matrix$high[IDX.period], rep(1, K),
          Sk / matrix$low[IDX.period], rep(1, K),
          rep(1, 2L * K),
          as.numeric(M),
          rep(-1, J),
          rep(1, J),
          rep(1, 2L * J)),
    dims = c(N + 4L * K + 3L * J, N + 2L * K + 2L * J))
  Settings <- osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-8,
                                 eps_rel = 1e-8, max_iter = 200000L,
                                 polishing = TRUE)
  FIT <- osqp::solve_osqp(
    P = P, q = q, A = CON,
    l = c(rep(params$aMin, N), rep(-1, K), rep(1, K), rep(0, 2L * K),
          rep(1, J), rep(0, 2L * J)),
    u = c(rep(params$aMax, N), rep(Inf, 4L * K),
          rep(1, J), rep(Inf, 2L * J)),
    pars = Settings)
  if (!FIT$info$status %chin% c("solved", "solved inaccurate")) {
    stop(sprintf("Stage B OSQP status: %s", FIT$info$status),
         call. = FALSE)
  }
  list(scale = as.numeric(FIT$x[seq_len(N)]), status = FIT$info$status)
}

.matchStageB <- function(PSA, target, records, params) {
  Matrix <- .matchStageBMatrix(PSA = PSA, target = target,
                               records = records)
  Fit <- .matchStageBBandOSQP(matrix = Matrix, params = params)
  Scale <- data.table(RecordID = Matrix$records, scaleFactor = Fit$scale)
  Match <- as.numeric(Matrix$A %*% Fit$scale)
  V <- sweep(Matrix$S, 2L, Fit$scale, `*`)
  V <- pmax((Matrix$low - V) / Matrix$low,
            (V - Matrix$high) / Matrix$high, 0)
  IDX <- match(0, Matrix$Tn)
  Stats <- .matchStageBStats(
    .matchStageBProblem(A = Matrix$A, y = Matrix$target)$A)
  Stage <- data.table(
    stage = "B",
    method = "osqp",
    objective = "targetNorm+band",
    status = Fit$status,
    mode = "global",
    bandWeight = params$bandWeight,
    deficitWeight = params$deficitWeight,
    RMSE = .matchRMSE(Match, Matrix$target),
    pgaRatio = if (is.na(IDX)) NA_real_ else
      Match[IDX] / Matrix$target[IDX],
    insideFraction = mean(V <= 1e-12),
    meanViolation = mean(V),
    maxViolation = max(V),
    scaleMin = min(Fit$scale),
    scaleMedian = stats::median(Fit$scale),
    scaleMax = max(Fit$scale),
    nAtMin = sum(abs(Fit$scale - params$aMin) < 1e-8),
    nAtMax = if (is.finite(params$aMax))
      sum(abs(Fit$scale - params$aMax) < 1e-8) else 0L)
  list(scale = Scale, stage = cbind(Stage, Stats))
}

#' Fit the suite record-scaling factors
#'
#' Suite-level record scaling of the spectral-match workflow: one scale
#' coefficient per record (`shape.recordFactor` in the batch JSON
#' contract), solved as a bounded quadratic program against the target
#' band (global band-conditioned objective).
#'
#' @param .x data.table. Suite PSA long table with columns `RecordID`,
#'   `Tn`, `Sa`, `kind`; the `kind == "final"` curves are scaled.
#' @param target data.table. Target band with columns `Tn`, `Sa.low`,
#'   `Sa.mean`, `Sa.high`, in the same units as `.x`.
#' @param records character. RecordIDs of the completed records to
#'   scale (at least two).
#' @param factorMin numeric. Lower bound of the record factors
#'   (`shape.recordFactor.min`). Default `0`.
#' @param factorMax numeric. Upper bound of the record factors
#'   (`shape.recordFactor.max`). Default `Inf`.
#' @param bandWeight numeric. Weight of the band-violation term.
#'   Default `2`.
#' @param deficitWeight numeric. Extra weight of the suite-mean deficit
#'   below the target. Default `1`.
#'
#' @return list with `scale` (data.table `RecordID`, `scaleFactor`) and
#'   `stage` (one-row solver diagnostics; the `stage` column keeps the
#'   frozen product label `"B"`).
#'
#' @examples
#' \dontrun{
#' Suite <- fitRecordFactor(.x = PSA, target = Band,
#'                          records = Metrics[status == "done", RecordID],
#'                          factorMin = 0.5, factorMax = 1.6)
#' }
#' @export
fitRecordFactor <- function(.x, target, records,
                            factorMin = 0, factorMax = Inf,
                            bandWeight = 2, deficitWeight = 1) {
  factorMin <- as.numeric(factorMin)
  factorMax <- as.numeric(factorMax)
  if (is.na(factorMin) || factorMin < 0 || is.na(factorMax) ||
      factorMax < factorMin) {
    stop("factorMin/factorMax must define a non-negative interval.",
         call. = FALSE)
  }
  bandWeight <- as.numeric(bandWeight)
  deficitWeight <- as.numeric(deficitWeight)
  if (!is.finite(bandWeight) || bandWeight < 0) {
    stop("bandWeight must be finite and non-negative.", call. = FALSE)
  }
  if (!is.finite(deficitWeight) || deficitWeight <= 0) {
    stop("deficitWeight must be finite and positive.", call. = FALSE)
  }
  Params <- list(aMin = factorMin, aMax = factorMax,
                 bandWeight = bandWeight, deficitWeight = deficitWeight)
  .matchStageB(PSA = .x, target = target, records = records,
               params = Params)
}
