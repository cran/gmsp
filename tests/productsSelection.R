library(gmsp)
library(data.table)

.checkProductSummaries <- function() {
  Summarize <- getFromNamespace(".summarizeSuite", "gmsp")
  DT <- data.table(RecordID = c("001", "003"), OCID = "H1", IM = "PGA",
                   value = c(4903.325, 7354.9875))
  Probabilities <- c(0.50, 0.84, 0.95)
  Before <- copy(DT)
  OUT <- Summarize(DT, by = c("OCID", "IM"), probabilities = Probabilities)
  # Exact input preservation is distinct from the computed summary comparisons.
  stopifnot(isTRUE(all.equal(OUT$value, as.numeric(stats::quantile(DT$value,
    probs = Probabilities, type = 7L, names = FALSE, na.rm = FALSE)))),
    isTRUE(all.equal(OUT$p, Probabilities)), all(OUT$recordCount == 2L),
    isTRUE(all.equal(OUT$mean, rep(mean(DT$value), length(OUT$mean)))), identical(DT, Before))
  Mean <- Summarize(DT, by = c("OCID", "IM"), probabilities = NULL)
  # The mean-only path must also leave the supplied table unchanged.
  stopifnot(identical(names(Mean), c("OCID", "IM", "mean", "recordCount")),
    isTRUE(all.equal(as.list(Mean), as.list(unique(OUT[, .(OCID, IM, mean, recordCount)])))),
    identical(DT, Before))
  Duplicate <- rbindlist(list(DT, DT[1L]))
  Missing <- copy(DT)
  Missing[1L, value := NA_real_]
  for (DATA in list(Duplicate, Missing)) {
    Error <- tryCatch(Summarize(DATA, by = c("OCID", "IM"), probabilities = Probabilities), error = identity)
    stopifnot(inherits(Error, "moduleUsage"))
    Error <- tryCatch(Summarize(DATA, by = c("OCID", "IM"), probabilities = NULL), error = identity)
    stopifnot(inherits(Error, "moduleUsage"))
  }
  Error <- tryCatch(Summarize(DT, by = c("OCID", "IM"), probabilities = c(0.50, 0.50)), error = identity)
  stopifnot(inherits(Error, "moduleUsage"))
  Params <- getFromNamespace(".matchParams", "gmsp")
  Config <- list(shape = list(modalFactor = list(min = 0.001)),
                  units = list(source = "mm", target = "mm", psa = "g"))
  Error <- tryCatch(Params(Config), error = identity)
  stopifnot(inherits(Error, "moduleUsage"),
    grepl("units.psa", conditionMessage(Error), fixed = TRUE))
}

.checkProductSummaries()
