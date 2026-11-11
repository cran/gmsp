# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.matchStageBMatrix <- function(PSA, target, records) {
  Records <- records[records %in% unique(PSA$RecordID)]
  if (length(Records) < 2L) {
    .stopUsage("Stage B requires at least two completed records.",
               operation = "fitRecordFactor")
  }
  DT <- PSA[kind == "final" & RecordID %in% Records, .(RecordID, Tn, Sa)]
  DT <- dcast(DT, Tn ~ RecordID, value.var = "Sa")
  DT <- DT[target[, .(Tn, Sa.mean, Sa.low, Sa.high)], on = "Tn", nomatch = 0L]
  Records <- Records[Records %in% names(DT)]
  if (length(Records) < 2L || !nrow(DT)) {
    .stopUsage("Stage B requires two records after target alignment.",
               operation = "fitRecordFactor")
  }
  Spectra <- as.matrix(DT[, Records, with = FALSE])
  storage.mode(Spectra) <- "double"
  if (any(!is.finite(Spectra))) {
    .stopUsage("Stage B matrix contains non-finite values for records: ",
               paste(Records, collapse = ", "), operation = "fitRecordFactor")
  }
  list(Tn = DT$Tn, target = DT$Sa.mean, low = DT$Sa.low,
       high = DT$Sa.high, records = Records, S = Spectra,
       A = Spectra / length(Records))
}

.matchStageBStats <- function(A) {
  SingularValues <- svd(A, nu = 0, nv = 0)$d
  TOL <- max(dim(A)) * .Machine$double.eps * max(SingularValues)
  data.table(equations = nrow(A), records = ncol(A),
             rankSVD = sum(SingularValues > TOL),
             cond2 = max(SingularValues) / min(SingularValues),
             rcond = tryCatch(base::rcond(A), error = function(e) NA_real_))
}

.matchStageBBandOSQP <- function(matrix, params) {
  Mean <- matrix$A / matrix$target
  n <- ncol(Mean)
  j <- nrow(Mean)
  k <- j * n
  Weight <- params$bandWeight / n
  IDX.record <- rep(seq_len(n), each = j)
  IDX.period <- rep(seq_len(j), times = n)
  Values <- matrix$S[cbind(IDX.period, IDX.record)]
  Quadratic <- Matrix::bdiag(
    Matrix::Matrix(0, nrow = n, ncol = n, sparse = TRUE),
    Matrix::Diagonal(2L * k, x = 2 * Weight),
    Matrix::Diagonal(j, x = 2),
    Matrix::Diagonal(j, x = 2 * params$deficitWeight))
  q <- rep(0, n + 2L * k + 2L * j)
  Constraints <- Matrix::sparseMatrix(
    i = c(seq_len(n),
          rep(n + seq_len(k), 2L),
          rep(n + k + seq_len(k), 2L),
          n + 2L * k + seq_len(2L * k),
          n + 4L * k + rep(seq_len(j), times = n),
          n + 4L * k + seq_len(j),
          n + 4L * k + seq_len(j),
          n + 4L * k + j + seq_len(2L * j)),
    j = c(seq_len(n),
          IDX.record, n + seq_len(k),
          IDX.record, n + k + seq_len(k),
          n + seq_len(2L * k),
          rep(seq_len(n), each = j),
          n + 2L * k + seq_len(j),
          n + 2L * k + j + seq_len(j),
          n + 2L * k + seq_len(2L * j)),
    x = c(rep(1, n),
          -Values / matrix$high[IDX.period], rep(1, k),
          Values / matrix$low[IDX.period], rep(1, k),
          rep(1, 2L * k),
          as.numeric(Mean),
          rep(-1, j), rep(1, j), rep(1, 2L * j)),
    dims = c(n + 4L * k + 3L * j, n + 2L * k + 2L * j))
  Settings <- osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-8,
                                 eps_rel = 1e-8, max_iter = 200000L,
                                 polishing = TRUE)
  Fit <- osqp::solve_osqp(
    P = Quadratic, q = q, A = Constraints,
    l = c(rep(params$aMin, n), rep(-1, k), rep(1, k), rep(0, 2L * k),
          rep(1, j), rep(0, 2L * j)),
    u = c(rep(params$aMax, n), rep(Inf, 4L * k),
          rep(1, j), rep(Inf, 2L * j)), pars = Settings)
  if (!Fit$info$status %in% c("solved", "solved inaccurate")) {
    stop(structure(list(message = paste0("Stage B OSQP status: ", Fit$info$status),
                        call = NULL, operation = "fitRecordFactor",
                        unit = matrix$records, status = Fit$info$status),
                   class = c("gmspFailure", "moduleFailure", "error", "condition")))
  }
  list(scale = as.numeric(Fit$x[seq_len(n)]), status = Fit$info$status)
}

#' Fit suite record-scaling factors
#'
#' Fits one bounded scale factor per record with a quadratic program against
#' a target response-spectrum band, without modifying the input tables.
#'
#' @param PSA A data.table with character \code{RecordID} and \code{kind},
#'   numeric \code{Tn} in seconds and numeric \code{Sa} in acceleration units.
#'   The \code{kind == "final"} curves enter the fit.
#' @param target A table with \code{Tn}, \code{Sa.low}, \code{Sa.mean} and
#'   \code{Sa.high}, in the same acceleration units as \code{PSA}.
#'   Periods are finite, non-negative and unique; ordinates are finite,
#'   positive and ordered from low through mean to high.
#' @param records Character vector of distinct completed record identities,
#'   in the requested output order.
#' @param factorMin Finite non-negative lower factor bound.
#' @param factorMax Upper factor bound, at least \code{factorMin};
#'   \code{Inf} permits an unbounded upper factor.
#' @param bandWeight Finite non-negative weight of the record band-violation term.
#' @param deficitWeight Finite positive weight of the suite-mean deficit term.
#'
#' @return A list with \code{scale} and \code{stage} data.tables.
#'   \code{scale} contains \code{RecordID} and \code{scaleFactor} in the
#'   requested record order after alignment.
#'   \code{stage} contains one row labelled \code{B}, the \code{osqp} method
#'   and solver status, objective weights, relative RMSE, PGA ratio, band
#'   violation and scale statistics, and matrix diagnostics
#'   \code{equations}, \code{records}, \code{rankSVD}, \code{cond2} and \code{rcond}.
#'
#' @details Duplicate \code{RecordID/Tn/kind} keys are errors.
#'   Alignment retains target periods present in the final curves and requires
#'   at least two records with a finite spectral matrix.
#'   The solver uses absolute and relative tolerances of \code{1e-8},
#'   a maximum of \code{200000} iterations and polishing; the returned status is
#'   \code{solved} or \code{solved inaccurate}, and other statuses are errors.
#'   The stage RMSE is relative to \code{Sa.mean}; PGA ratio is \code{NA}
#'   when the aligned periods lack zero, and \code{rcond} is \code{NA}
#'   when its matrix calculation fails.
#'   The function calculates in memory and performs no file access.
#' @seealso [fitModalFactor()], [assessCompatibility()]
#' @examples
#' Target <- data.table::data.table(Tn = c(0, 1), Sa.low = 0.50 * 9806.65,
#'   Sa.mean = 0.84 * 9806.65, Sa.high = 0.95 * 9806.65)
#' PSA <- data.table::data.table(RecordID = rep(c("R1", "R2"), each = 2),
#'   kind = "final", Tn = c(0, 1, 0, 1), Sa = c(7000, 8500, 4000, 6000))
#' Fit <- fitRecordFactor(PSA, target = Target, records = c("R1", "R2"),
#'   factorMin = 0.5, factorMax = 1.6, bandWeight = 2, deficitWeight = 1)
#' print(Fit$scale)
#' print(Fit$stage[, c("status", "RMSE", "pgaRatio", "insideFraction", "nAtMax")])
#' @export
fitRecordFactor <- function(PSA, target, records, factorMin, factorMax,
                            bandWeight, deficitWeight) {
  if (!inherits(PSA, "data.table")) {
    .stopUsage("PSA must be a data.table.", operation = "fitRecordFactor")
  }
  .checkTableKeys(PSA, by = character(), columns = c("RecordID", "Tn", "Sa", "kind"),
                  keys = c("RecordID", "Tn", "kind"), operation = "fitRecordFactor")
  for (j in c("RecordID", "kind")) {
    if (!is.character(PSA[[j]]) || anyNA(PSA[[j]]) || any(!nzchar(PSA[[j]]))) {
      .stopUsage(j, " must contain nonempty character values.", operation = "fitRecordFactor")
    }
  }
  if (!is.numeric(PSA$Tn) || any(!is.finite(PSA$Tn)) || any(PSA$Tn < 0) ||
      !is.numeric(PSA$Sa)) {
    .stopUsage("PSA requires finite non-negative Tn and numeric Sa.", operation = "fitRecordFactor")
  }
  if (!is.character(records) || anyNA(records) || any(!nzchar(records)) || anyDuplicated(records)) {
    .stopUsage("records must contain distinct nonempty character identities.", operation = "fitRecordFactor")
  }
  if (!is.numeric(factorMin) || length(factorMin) != 1L || !is.finite(factorMin) || factorMin < 0 ||
      !is.numeric(factorMax) || length(factorMax) != 1L || is.na(factorMax) || factorMax < factorMin) {
    .stopUsage("factorMin/factorMax must define a non-negative interval with finite minimum.",
               operation = "fitRecordFactor")
  }
  if (!is.numeric(bandWeight) || length(bandWeight) != 1L || !is.finite(bandWeight) || bandWeight < 0) {
    .stopUsage("bandWeight must be finite and non-negative.", operation = "fitRecordFactor")
  }
  if (!is.numeric(deficitWeight) || length(deficitWeight) != 1L || !is.finite(deficitWeight) || deficitWeight <= 0) {
    .stopUsage("deficitWeight must be finite and positive.", operation = "fitRecordFactor")
  }
  Target <- .checkTargetBand(target, operation = "fitRecordFactor")
  Params <- list(aMin = factorMin, aMax = factorMax,
                 bandWeight = bandWeight, deficitWeight = deficitWeight)
  Matrix <- .matchStageBMatrix(PSA = copy(PSA), target = Target, records = records)
  Fit <- tryCatch(.matchStageBBandOSQP(matrix = Matrix, params = Params),
                  error = function(e) {
                    e$message <- paste0("fitRecordFactor [", paste(Matrix$records, collapse = ", "),
                                        "]: ", conditionMessage(e))
                    e$operation <- "fitRecordFactor"
                    e$unit <- Matrix$records
                    stop(e)
                  })
  Scale <- data.table(RecordID = Matrix$records, scaleFactor = Fit$scale)
  Match <- as.numeric(Matrix$A %*% Fit$scale)
  Violation <- sweep(Matrix$S, 2L, Fit$scale, `*`)
  Violation <- pmax((Matrix$low - Violation) / Matrix$low,
                    (Violation - Matrix$high) / Matrix$high, 0)
  IDX <- match(0, Matrix$Tn)
  Ratio <- NA_real_
  if (!is.na(IDX)) Ratio <- Match[IDX] / Matrix$target[IDX]
  n <- 0L
  if (is.finite(factorMax)) n <- sum(abs(Fit$scale - factorMax) < 1e-8)
  Stage <- data.table(
    stage = "B", method = "osqp", objective = "targetNorm+band", status = Fit$status,
    mode = "global", bandWeight = bandWeight, deficitWeight = deficitWeight,
    RMSE = .matchRMSE(Match, Matrix$target), pgaRatio = Ratio,
    insideFraction = mean(Violation <= 1e-12), meanViolation = mean(Violation),
    maxViolation = max(Violation), scaleMin = min(Fit$scale),
    scaleMedian = stats::median(Fit$scale), scaleMax = max(Fit$scale),
    nAtMin = sum(abs(Fit$scale - factorMin) < 1e-8), nAtMax = n)
  list(scale = Scale, stage = cbind(Stage, .matchStageBStats(Matrix$A / Matrix$target)))
}
