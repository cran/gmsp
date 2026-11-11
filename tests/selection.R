library(gmsp)
library(data.table)

.testSelection <- function() {
  stopifnot(identical(names(formals(gmsp:::.readTargetBand)),
                      c("path.data", "target", "unitsTarget", "range")))
  # Fixtures de 1963841 tests/runSelect.R y tests/runTarget.R; contrato E4.
  DT <- data.table(RecordID = c("001", "002", "003"), OwnerID = "A",
    EventID = c("2020A", "2020A", "2021B"), StationID = c("01", "02", "03"),
    DIR = "H1", OCID = "H1", PGA = c(4903.325, 4903.325, 7354.9875), ProcessID = "raw")
  Before <- copy(DT)
  Keys <- data.table(ProcessID = "raw", OwnerID = "A")
  OUT <- gmsp:::.selectCandidates(DT, keys = Keys,
    criteria = list(list(criteria = list(PGA = c(0.84 / 4, 0.84) * 9806.65), criterion = "all")),
    exclude = character(), oneRecordPerEvent = TRUE)
  # Selection preserves the supplied table; the selected identifiers are strings.
  stopifnot(identical(DT, Before), identical(OUT$selection$RecordID, c("002", "003")),
    identical(OUT$selection$StationID, c("02", "03")), nrow(OUT$candidates) == 3L)
  OUT <- gmsp:::.selectCandidates(DT, keys = Keys,
    criteria = list(list(criteria = list(PGA = c(7000, 8000)), criterion = "a"),
      list(criteria = list(PGA = c(7000, 8000)), criterion = "B")),
    exclude = character(), oneRecordPerEvent = TRUE)
  # Exact equality checks the textual label and the unchanged input.
  stopifnot(identical(OUT$selection$criterion, "B+a"), identical(DT, Before))

  # Owner decision 1 of 2026-11-09: one record per earthquake across owners by
  # EventID.USGS (usgsEventId in legacy processed indexes), EventID when missing.
  DT <- data.table(RecordID = c("101", "102", "103", "104"), OwnerID = c("A", "B", "A", "B"),
    EventID = c("2020A", "2020B", "2021A", "2022A"), EventID.USGS = c("us1", "us1", "", NA),
    StationID = "01", DIR = "H1", OCID = "H1", PGA = c(1, 2, 3, 4), ProcessID = "raw")
  OUT <- gmsp:::.selectCandidates(DT, keys = data.table(ProcessID = "raw", OwnerID = c("A", "B")),
    criteria = list(list(criteria = list(PGA = c(0, 10)), criterion = "all")),
    exclude = character(), oneRecordPerEvent = TRUE)
  stopifnot(identical(OUT$selection$RecordID, c("102", "103", "104")), nrow(OUT$candidates) == 4L)
  Root <- tempfile("gmsp-select-")
  dir.create(file.path(Root, "database", "index"), recursive = TRUE)
  dir.create(file.path(Root, "database", "process", "TEST"), recursive = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  setnames(DT, "EventID.USGS", "usgsEventId")
  DT[, ProcessID := "TEST"]
  fwrite(DT, file.path(Root, "database", "process", "TEST", "MasterIndex.csv"))
  jsonlite::write_json(list(processID = "TEST", owners = c("A", "B"), oneRecordPerEvent = FALSE,
    path = list(index = "database/index", selection = "selection")),
    file.path(Root, "select.json"), auto_unbox = TRUE)
  stopifnot(identical(selectProject(file = "select.json", root = Root), 0L))
  Summary <- jsonlite::read_json(file.path(Root, "selection", "summary.json"))
  stopifnot(identical(Summary$records, 4L), identical(Summary$events, 3L))

  # Owner decisions of 2026-11-09: published Mw first (index, then owner, also over an
  # ML or an Ms), Ms as Mw = 0.67 Ms + 2.13 also above 6.47, ML as Mw by identity;
  # Scordilis (2006, eq. 22) for mb in [3.5, 6.2]; identity outside by the owner's
  # decision of 2026-11-10. Md and empty types are unchanged; criteria use the result.
  DT <- data.table(RecordID = as.character(201:215),
    OwnerID = "A", EventID = c("2020A", "2020B", "2020C", "2020D", "2020E", "2020F", "2020G", "2020H",
      "2020I", "2020J", "2020K", "2020L", "2020M", "2020N", "2020O"),
    StationID = "01", DIR = "H1", OCID = "H1", PGA = 1, ProcessID = "raw",
    EventMagnitude = c(5.0, 4.8, 5.2, 6.0, 5.5, 4.5, 5.8, 4.0, 7.0, 5.0, 5.0, 3.5, 6.2, 3.4, 6.3),
    EventMagnitudeType = c("mwc", "mb", "ML", "Ms", "", "Md", "ML", "MS", "ms", "Ms", "mb", "MB", "mb", "mB", "mb"),
    EventMagnitude.owner = c(4.8, 4.6, 5.2, 6.0, 5.5, 4.5, 5.4, 4.0, 7.0, 5.9, 5.0, 3.5, 6.2, 3.4, 6.3),
    EventMagnitudeType.owner = c("Mw", "Mw", "ML", "MS", "Mw", "Md", "Mw", "MS", "Ms", "Mw", "mb", "mb", "mb", "mb", "mb"))
  Before <- copy(DT)
  Keys <- data.table(ProcessID = "raw", OwnerID = "A")
  OUT <- gmsp:::.selectCandidates(DT, keys = Keys,
    criteria = list(list(criteria = list(EventMagnitude = c(0, 10)), criterion = "all")),
    exclude = character(), oneRecordPerEvent = FALSE)
  # Conversion occurs in a copy; the caller's table must remain unchanged.
  stopifnot(identical(DT, Before), isTRUE(all.equal(OUT$selection$EventMagnitude,
      c(5.0, 4.6, 5.2, 6.15, 5.5, 4.5, 5.4, 4.81, 6.82, 5.9, 5.28, 4.005, 6.3, 3.4, 6.3))),
    identical(OUT$selection$EventMagnitudeType, c("mwc", "Mw", "Mw", "Mw", "Mw", "Md", "Mw", "Mw", "Mw", "Mw", "Mw", "Mw", "Mw", "Mw", "Mw")),
    # Provider magnitude and type are transported verbatim, without conversion.
    identical(OUT$selection$EventMagnitude.owner, Before$EventMagnitude.owner),
    identical(OUT$selection$EventMagnitudeType.owner, Before$EventMagnitudeType.owner))
  OUT <- gmsp:::.selectCandidates(DT, keys = Keys,
    criteria = list(list(criteria = list(EventMagnitude = c(4.7, 5.6)), criterion = "all")),
    exclude = character(), oneRecordPerEvent = FALSE)
  stopifnot(identical(OUT$selection$RecordID, c("201", "203", "205", "207", "208", "211")))

  Directory <- tempfile("gmsp-target-")
  dir.create(Directory)
  on.exit(unlink(Directory, recursive = TRUE), add = TRUE)
  MCE <- CJ(ID = "MCE", siteID = c("ANTPY", "CRCCO"), Vs30 = 800,
    Tn = c(0, 1), p = c("0.50", "0.84", "0.95"))
  MCE[, SaF := as.numeric(p) + fifelse(siteID == "CRCCO", 10, 0)]
  MDE <- CJ(ID = "base", siteID = c("ANTPY", "CRCCO"), Vs30 = 800,
    TR = 10000, Tn = c(0, 1), p = c("0.50", "0.84", "0.95"))
  MDE[, SaF := 1 + as.numeric(p) + fifelse(siteID == "CRCCO", 10, 0)]
  Decoy <- copy(MDE[siteID == "ANTPY"])
  Decoy[, `:=`(TR = 1000, SaF = 99)]
  DecoyOQ <- copy(MDE[siteID == "ANTPY"])
  DecoyOQ[, `:=`(ID = "base.oq", SaF = 199)]
  DecoySite <- copy(MDE[siteID == "ANTPY"])
  DecoySite[, `:=`(ID = "base.site", SaF = 299)]
  saveRDS(MCE, file.path(Directory, "MCETable.Rds"))
  saveRDS(rbindlist(list(MDE, Decoy, DecoyOQ, DecoySite), use.names = TRUE),
    file.path(Directory, "UHSTable.Rds"))
  Target <- list(demand = "MCE", siteID = "ANTPY", Vs30 = 800, p = "0.84",
    value = "SaF", units = "g", envelope = list(lower = "0.50", upper = "0.95"))
  OUT <- gmsp:::.readTargetBand(path.data = Directory, target = Target, unitsTarget = "g")
  stopifnot(identical(names(OUT), c("Tn", "Sa.low", "Sa.mean", "Sa.high")),
    isTRUE(all.equal(OUT$Tn, c(0, 1))), isTRUE(all.equal(OUT$Sa.low, rep(0.50, length(OUT$Sa.low)))),
    isTRUE(all.equal(OUT$Sa.mean, rep(0.84, length(OUT$Sa.mean)))),
    isTRUE(all.equal(OUT$Sa.high, rep(0.95, length(OUT$Sa.high)))))
  Target$demand <- "MDE"
  Target$modelID <- "base"
  OUT <- gmsp:::.readTargetBand(path.data = Directory, target = Target, unitsTarget = "g")
  stopifnot(isTRUE(all.equal(OUT$Sa.low, rep(1 + 0.50, length(OUT$Sa.low)))),
    isTRUE(all.equal(OUT$Sa.mean, rep(1 + 0.84, length(OUT$Sa.mean)))),
    isTRUE(all.equal(OUT$Sa.high, rep(1 + 0.95, length(OUT$Sa.high)))))
  Target$envelope <- NULL
  OUT <- gmsp:::.readTargetBand(path.data = Directory, target = Target, unitsTarget = "mm/s2")
  stopifnot(isTRUE(all.equal(OUT$Tn, c(0, 1))),
    isTRUE(all.equal(OUT$Sa, rep((1 + 0.84) * 9806.65, length(OUT$Sa)))))
  Target$demand <- "MCE"
  Target$modelID <- NULL
  Target$envelope <- list(lower = "0.50", upper = "0.95")
  expectError <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))
  Invalid <- Target
  Invalid$ID <- "MCE"
  expectError(gmsp:::.readTargetBand(path.data = Directory, target = Invalid, unitsTarget = "g"))
  Invalid <- Target
  Invalid$p <- "p84"
  expectError(gmsp:::.readTargetBand(path.data = Directory, target = Invalid, unitsTarget = "g"))
  saveRDS(rbindlist(list(MCE, MCE[1L]), use.names = TRUE), file.path(Directory, "MCETable.Rds"))
  expectError(gmsp:::.readTargetBand(path.data = Directory, target = Target, unitsTarget = "g"))
  saveRDS(MCE[!(siteID == "ANTPY" & p == "0.95" & Tn == 1)], file.path(Directory, "MCETable.Rds"))
  expectError(gmsp:::.readTargetBand(path.data = Directory, target = Target, unitsTarget = "g"))
  saveRDS(MCE[!(siteID == "ANTPY" & Tn == 0)], file.path(Directory, "MCETable.Rds"))
  expectError(gmsp:::.readTargetBand(path.data = Directory, target = Target, unitsTarget = "g"))
  MCE[siteID == "ANTPY" & p == "0.84" & Tn == 1, SaF := NA_real_]
  saveRDS(MCE, file.path(Directory, "MCETable.Rds"))
  expectError(gmsp:::.readTargetBand(path.data = Directory, target = Target, unitsTarget = "g"))
}
.testSelection()
