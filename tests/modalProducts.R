library(gmsp)
library(data.table)

local({
  widenModes <- getFromNamespace(".widenModes", "gmsp")
  lengthenIMF <- getFromNamespace(".lengthenIMF", "gmsp")
  checkCriterion <- getFromNamespace(".checkModeCriterion", "gmsp")
  checkControl <- getFromNamespace(".checkReconstructionControl", "gmsp")
  expectUsage <- function(expr) {
    Error <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(Error, "gmspUsage"), inherits(Error, "moduleUsage"))
    invisible(Error)
  }
  Key <- data.table(RecordID = "R1", OwnerID = "A", EventID = "E1", StationID = "S1")
  t <- seq(0, 4, by = 0.01)
  IMF <- data.table(OCID = "H1", t = t, signal = 1000 * sin(2 * pi * t),
    IMF1 = 1000 * sin(2 * pi * t))
  Native <- lengthenIMF(IMF, key = Key)
  Original <- copy(Native)
  OUT <- widenModes(Native)
  stopifnot(isTRUE(all.equal(OUT$signal, IMF$signal)), isTRUE(all.equal(OUT$IMF1, IMF$IMF1)),
    identical(Native, Original), !"residue" %in% names(OUT), !"ID" %in% names(OUT),
    all(OUT$RecordID == "R1"),
    identical(lapply(lengthenIMF(OUT, key = Key), as.list), lapply(Native, as.list)))

  Key[, ProcessID := "F25"]
  Native <- lengthenIMF(IMF, key = Key)
  DT <- copy(Native$modes)
  DT[, mode := 2L]
  Native$modes <- rbindlist(list(DT, Native$modes))
  DT <- copy(Native$signal)
  DT[, OCID := "H2"]
  Native$signal <- rbindlist(list(Native$signal, DT))
  DT <- copy(Native$modes[mode == 1L])
  DT[, OCID := "H2"]
  Native$modes <- rbindlist(list(Native$modes, DT))
  Original <- copy(Native)
  OUT <- widenModes(Native)
  stopifnot(identical(grep("^IMF", names(OUT), value = TRUE), c("IMF2", "IMF1")),
    all(is.na(OUT[OCID == "H2", IMF2])), identical(Native, Original),
    identical(lapply(lengthenIMF(OUT, key = Key), as.list), lapply(Native, as.list)))
  Native$residue <- copy(Native$signal)
  OUT <- widenModes(Native)
  stopifnot(identical(OUT$residue, Native$residue$s))
  names(Native)[names(Native) == "residue"] <- "fitResidual"
  stopifnot(identical(widenModes(Native), OUT))
  Native$modes <- Native$modes[0L]
  OUT <- widenModes(Native)
  stopifnot(!any(grepl("^IMF", names(OUT))), identical(OUT$residue, Native$signal$s))
  Native$fitResidual <- NULL
  stopifnot(!"residue" %in% names(widenModes(Native)))
  Native$residue <- copy(Native$signal)
  Native$fitResidual <- copy(Native$signal)
  expectUsage(widenModes(Native))
  Native$fitResidual <- NULL
  Native$signal[, ID := "VT"]
  expectUsage(widenModes(Native))

  Control <- list(unitsTarget = "mm", fmax = 25, kNyq = 4,
    windowLength = 128, overlap = 75, astop0 = 1e-3,
    apass0 = 1e-3, astopLP = 1e-3,
    apassLP = 0.98, includeWarnings = TRUE)
  stopifnot(is.null(checkControl(Control)))
  for (j in c("unitsTarget", "windowLength", "apassLP")) {
    Invalid <- Control
    Invalid[[j]] <- NULL
    expectUsage(checkControl(Invalid))
  }
  Invalid <- Control
  Invalid$unitsSource <- "mm"
  expectUsage(checkControl(Invalid))
  Invalid <- Control
  Invalid$kNyq <- 0
  expectUsage(checkControl(Invalid))
  expectUsage(checkControl(c(Control, list(fmax = 25))))
  stopifnot(is.null(checkCriterion(list(rule = "energyFraction", minimum = 0, action = "keep"))),
    is.null(checkCriterion(list(rule = "band", fMin = 2, fMax = 15, action = "keep"))))
  expectUsage(checkCriterion(list(rule = "modes", modes = 1L, action = "keep")))
  expectUsage(checkCriterion(list(rule = "band", fMin = 15, fMax = 2, action = "keep")))
  expectUsage(checkCriterion(list(rule = "energyFraction", minimum = 0, action = "keep", remove = "IMF1")))
  message("PASS: modal projections, round trips, absence, ordering, preservation and admission.")
})
