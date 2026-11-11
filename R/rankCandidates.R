# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.interpSpectrum <- function(x, y, xout) {
  OK <- is.finite(x) & is.finite(y)
  x <- as.numeric(x[OK])
  y <- as.numeric(y[OK])
  if (length(unique(x)) < 2L) return(rep(NA_real_, length(xout)))
  IDX <- order(x)
  stats::approx(x = x[IDX], y = y[IDX], xout = xout,
                method = "linear", rule = 2, ties = "ordered")$y
}

.rankCandidates <- function(PSW, target, keys, intensity, metadata, OCID,
                            recordCount, oneRecordPerEvent) {
  Target <- .checkTargetBand(target, operation = ".rankCandidates")
  if (!is.character(OCID) || length(OCID) != 1L || is.na(OCID) || !nzchar(OCID)) {
    .stopUsage("OCID must be one non-empty component.", operation = ".rankCandidates")
  }
  if (!is.numeric(recordCount) || length(recordCount) != 1L || !is.finite(recordCount) ||
      recordCount < 2L || recordCount != floor(recordCount)) {
    .stopUsage("recordCount must be an integer of at least two.", operation = ".rankCandidates")
  }
  if (!is.logical(oneRecordPerEvent) || length(oneRecordPerEvent) != 1L || is.na(oneRecordPerEvent)) {
    .stopUsage("oneRecordPerEvent must be a logical scalar.", operation = ".rankCandidates")
  }
  PSW <- as.data.table(PSW)
  Keys <- as.data.table(keys)
  Intensity <- as.data.table(intensity)
  Metadata <- as.data.table(metadata)
  COL <- paste("PSA", OCID, sep = ".")
  .checkTableKeys(PSW, by = character(), columns = c("RecordID", "Tn", COL),
                  keys = c("RecordID", "Tn"), operation = ".rankCandidates")
  .checkTableKeys(Keys, by = character(), columns = c("RecordID", "EventID"),
                  keys = "RecordID", operation = ".rankCandidates")
  .checkTableKeys(Intensity, by = character(), columns = c("RecordID", "PGA", "AI"),
                  keys = "RecordID", operation = ".rankCandidates")
  .checkTableKeys(Metadata, by = character(), columns = c("RecordID", "EventName", "EventMagnitude", "Repi"),
                  keys = "RecordID", operation = ".rankCandidates")
  Periods <- Target[Tn > 0, Tn]
  if (!length(Periods) || !nrow(Keys)) {
    .stopUsage("Ranking requires records and positive target periods.", operation = ".rankCandidates")
  }
  DT <- rbindlist(lapply(Keys[["RecordID"]], function(x) {
    AUX <- PSW[RecordID == x]
    if (identical(as.numeric(AUX[["Tn"]]), as.numeric(Periods))) {
      return(data.table(RecordID = x, Tn = Periods, Sa = AUX[[COL]]))
    }
    data.table(RecordID = x, Tn = Periods,
      Sa = .interpSpectrum(x = AUX[["Tn"]], y = AUX[[COL]], xout = Periods))
  }), use.names = TRUE)
  DT <- Target[Tn > 0][DT, on = "Tn", nomatch = 0L]
  DT <- DT[is.finite(Sa) & is.finite(Sa.mean) & is.finite(Sa.low) &
             is.finite(Sa.high) & Sa.mean > 0 & Sa.low > 0 & Sa.high > 0]
  if (!nrow(DT)) .stopUsage("Ranking found zero usable rows.", operation = ".rankCandidates")
  DT[, inside := .matchInside(Sa, low = Sa.low, high = Sa.high)]
  DT[, violation := pmax((Sa.low - Sa) / Sa.low, (Sa - Sa.high) / Sa.high, 0)]
  DT[, dMean := (Sa - Sa.mean) / Sa.mean]
  Scores <- DT[, .(
    insideFraction = mean(inside),
    bandRMSE = sqrt(mean(violation ^ 2)),
    q90Violation = as.numeric(stats::quantile(violation, probs = 0.90, type = 7L, na.rm = TRUE, names = FALSE)),
    q95Violation = as.numeric(stats::quantile(violation, probs = 0.95, type = 7L, na.rm = TRUE, names = FALSE)),
    maxViolation = max(violation, na.rm = TRUE),
    relaxedRMSE = sqrt(mean(data.table::fifelse(inside, 0.2, 1) * dMean ^ 2)),
    RMSE = .matchRMSE(Sa, Sa.mean),
    medianRatio = stats::median(Sa / Sa.mean, na.rm = TRUE),
    nTn = .N), by = RecordID]
  Scores[Intensity, on = "RecordID", `:=`(PGA = i.PGA, AI = i.AI)]
  Scores[Metadata, on = "RecordID", `:=`(EventName = i.EventName, Mw = i.EventMagnitude, Repi = i.Repi)]
  Scores[, rank.band := data.table::frankv(.SD,
    cols = c("bandRMSE", "q90Violation", "relaxedRMSE"), ties.method = "min", na.last = "keep")]
  setorderv(Scores, c("bandRMSE", "q90Violation", "relaxedRMSE", "RecordID"), na.last = TRUE)
  setcolorder(Scores, c("RecordID", "rank.band"))
  Selection <- Keys[Scores[is.finite(rank.band), .(RecordID, rank.band)],
                    on = "RecordID", nomatch = 0L][order(rank.band)]
  if (oneRecordPerEvent) {
    Selection <- Selection[!duplicated(.eventIdentity(Metadata[Selection, on = "RecordID"]))]
  }
  Selection <- Selection[seq_len(min(recordCount, .N))]
  if (nrow(Selection) < recordCount) {
    .stopUsage("Ranking produced ", nrow(Selection), " records; selection needs ", recordCount, ".",
               operation = ".rankCandidates")
  }
  list(scores = Scores[], selection = Selection[], candidateCount = nrow(Scores))
}
