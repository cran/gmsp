# Spectral-match compatibility diagnostics. Graduated verbatim from
# inst/scripts/match/setup.R (V3 contract) on the match-graduation SoT.
# Do not edit without a Tier-2 identity harness.

utils::globalVariables(c(
  "isPGA", "violation", "inside", "PGA.final", "PGA.target.low",
  "PGA.target.high", "scaleAtMin", "scaleAtMax", "i.scaleFactor",
  "scaleFactor", "status", "PGA.target.mean"))

.matchRecordCompatibility <- function(PSW, target, scale, params) {
  COL <- paste("PSA", params$ocid, sep = ".")
  COLS.key <- intersect(KEY.record, names(PSW))
  .requireColumns(PSW, c(COLS.key, "Tn", COL), "final PSW")
  .requireColumns(target, c("Tn", "Sa.low", "Sa.mean", "Sa.high"),
                  "target band")
  .requireColumns(scale, c("RecordID", "scaleFactor"), "Stage B scale")

  TargetPGA <- target[abs(Tn) <= 1e-12]
  if (nrow(TargetPGA) != 1L) {
    stop("record compatibility requires exactly one target PGA row.",
         call. = FALSE)
  }

  DT <- PSW[, c(COLS.key, "Tn", COL), with = FALSE]
  setnames(DT, COL, "Sa")
  DT <- target[DT, on = "Tn", nomatch = 0L]
  if (!nrow(DT)) {
    stop("record compatibility lost all PSW rows after target join.",
         call. = FALSE)
  }
  DT[, `:=`(
    isPGA = abs(Tn) <= 1e-12,
    inside = .matchInside(Sa, Sa.low, Sa.high),
    violation = pmax((Sa.low - Sa) / Sa.low,
                     (Sa - Sa.high) / Sa.high, 0))]

  OUT <- DT[, .(
    nTn = .N,
    PGA.final = if (any(isPGA)) Sa[which(isPGA)[1L]] else NA_real_,
    PGA.target.low = TargetPGA$Sa.low,
    PGA.target.mean = TargetPGA$Sa.mean,
    PGA.target.high = TargetPGA$Sa.high,
    PGA.ratio.mean = if (any(isPGA)) {
      Sa[which(isPGA)[1L]] / TargetPGA$Sa.mean
    } else NA_real_,
    PGA.ratio.high = if (any(isPGA)) {
      Sa[which(isPGA)[1L]] / TargetPGA$Sa.high
    } else NA_real_,
    PSA.RMSE = .matchRMSE(Sa, Sa.mean),
    PSA.inside = mean(inside, na.rm = TRUE),
    PSA.below = sum(Sa < Sa.low, na.rm = TRUE),
    PSA.above = sum(Sa > Sa.high, na.rm = TRUE),
    PSA.maxViolation = max(violation, na.rm = TRUE)
  ), by = COLS.key]
  OUT[scale, scaleFactor := i.scaleFactor, on = "RecordID"]
  if (any(!is.finite(OUT$scaleFactor))) {
    stop("record compatibility has records without scaleFactor.",
         call. = FALSE)
  }
  OUT[, `:=`(
    scaleAtMin = abs(scaleFactor - params$aMin) < 1e-8,
    scaleAtMax = abs(scaleFactor - params$aMax) < 1e-8,
    status = fifelse(PGA.final < PGA.target.low, "below",
                     fifelse(PGA.final > PGA.target.high, "above",
                             "inside")))]
  setorder(OUT, -PGA.final)
  OUT[]
}

.matchCompatibilitySummary <- function(x) {
  data.table(
    records = nrow(x),
    nPGA.inside = sum(x$status == "inside", na.rm = TRUE),
    nPGA.below = sum(x$status == "below", na.rm = TRUE),
    nPGA.above = sum(x$status == "above", na.rm = TRUE),
    nScaleAtMin = sum(x$scaleAtMin, na.rm = TRUE),
    nScaleAtMax = sum(x$scaleAtMax, na.rm = TRUE),
    medianRecordRMSE = stats::median(x$PSA.RMSE, na.rm = TRUE),
    maxRecordRMSE = max(x$PSA.RMSE, na.rm = TRUE),
    medianInside = stats::median(x$PSA.inside, na.rm = TRUE),
    minInside = min(x$PSA.inside, na.rm = TRUE),
    medianMaxViolation = stats::median(x$PSA.maxViolation, na.rm = TRUE),
    maxViolation = max(x$PSA.maxViolation, na.rm = TRUE))
}

#' Compatibility diagnostics of a matched suite
#'
#' Record-level and suite-level compatibility of the scaled spectra
#' against the target band: PGA position, band containment, and RMSE
#' diagnostics per record, plus their suite aggregate.
#'
#' @param .x data.table. Final PSW rows (after record scaling) with the
#'   record key columns, `Tn`, and `PSA.<ocid>`.
#' @param target data.table. Target band with columns `Tn`, `Sa.low`,
#'   `Sa.mean`, `Sa.high`; must include exactly one `Tn == 0` row.
#' @param scale data.table. Record scaling table (`RecordID`,
#'   `scaleFactor`) from [fitRecordFactor()].
#' @param ocid character. Component evaluated (`"H1"`, `"H2"`, `"UP"`).
#' @param factorMin numeric. Lower record-factor bound used to flag
#'   saturated records. Default `0`.
#' @param factorMax numeric. Upper record-factor bound used to flag
#'   saturated records. Default `Inf`.
#'
#' @return list with `record` (per-record diagnostics, one row per
#'   record) and `summary` (one-row suite aggregate).
#'
#' @examples
#' \dontrun{
#' QA <- matchCompatibility(.x = PSW, target = Band, scale = Suite$scale,
#'                          factorMin = 0.5, factorMax = 1.6)
#' }
#' @export
matchCompatibility <- function(.x, target, scale, ocid = "H1",
                               factorMin = 0, factorMax = Inf) {
  Params <- list(ocid = as.character(ocid),
                 aMin = as.numeric(factorMin),
                 aMax = as.numeric(factorMax))
  Record <- .matchRecordCompatibility(PSW = .x, target = target,
                                      scale = scale, params = Params)
  list(record = Record, summary = .matchCompatibilitySummary(Record))
}
