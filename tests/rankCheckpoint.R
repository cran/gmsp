library(gmsp)
library(data.table)

.checkRankCheckpoint <- function() {
  Rank <- getFromNamespace(".rankCandidates", "gmsp")
  Keys <- data.table(RecordID = c("R1", "R2"), EventID = "E1")
  Target <- data.table(Tn = c(0, 1), Sa.low = 0.50 * 9806.65,
                       Sa.mean = 0.84 * 9806.65, Sa.high = 0.95 * 9806.65)
  PSW <- data.table(RecordID = c("R1", "R2"), Tn = 1, PSA.H1 = 0.84 * 9806.65)
  Intensity <- data.table(RecordID = c("R1", "R2"), PGA = c(100, 200), AI = c(10, 20))
  Metadata <- data.table(RecordID = c("R1", "R2"), EventName = "First",
                         EventMagnitude = 6, Repi = c(10, 20))
  OUT <- Rank(PSW, target = Target, keys = Keys, intensity = Intensity,
              metadata = Metadata, OCID = "H1", recordCount = 2L, oneRecordPerEvent = FALSE)
  stopifnot(identical(OUT$selection$RecordID, c("R1", "R2")),
            all(OUT$scores$nTn == 1L),
            isTRUE(all.equal(OUT$scores$insideFraction, rep(1, length(OUT$scores$insideFraction)))))
  # Owner decision 1 of 2026-11-09: R1 and R2 share their USGS event across owners.
  Keys <- data.table(RecordID = c("R1", "R2", "R3"), EventID = c("E1", "E2", "E3"))
  PSW <- data.table(RecordID = c("R1", "R2", "R3"), Tn = 1, PSA.H1 = 0.84 * 9806.65)
  Intensity <- data.table(RecordID = c("R1", "R2", "R3"), PGA = 100, AI = 10)
  Metadata <- data.table(RecordID = c("R1", "R2", "R3"), EventName = "First",
                         EventMagnitude = 6, Repi = 10, usgsEventId = c("us1", "us1", ""))
  OUT <- Rank(PSW, target = Target, keys = Keys, intensity = Intensity,
              metadata = Metadata, OCID = "H1", recordCount = 2L, oneRecordPerEvent = TRUE)
  stopifnot(identical(OUT$selection$RecordID, c("R1", "R3")))
  # The same rule through rankProject, which reads usgsEventId from the processed MasterIndex.
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  Test <- new.env(parent = Namespace)
  for (Name in ls(Namespace, all.names = TRUE)) {
    Value <- get(Name, envir = Namespace)
    if (is.function(Value) && identical(environment(Value), Namespace)) environment(Value) <- Test
    assign(Name, Value, envir = Test)
  }
  Test$.projectTarget <- function(...) Target
  Test$.readRecordProducts <- function(keys, ...) {
    list(PSW = data.table(RecordID = rep(keys$RecordID, each = 2L),
                          Tn = rep(c(0.5, 2), times = nrow(keys)), PSA.H1 = 0.84 * 9806.65))
  }
  Root <- tempfile("gmsp-rank-")
  DIR <- file.path(Root, "database", "process", "TEST")
  dir.create(DIR, recursive = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Keys <- data.table(ProcessID = "TEST", RecordID = c("R1", "R2", "R3", "R4"), OwnerID = c("A", "B", "A", "A"),
                     EventID = c("E1", "E2", "E3", "E4"), StationID = "S1")
  fwrite(data.table(Keys, EventName = "First", Repi = 10, usgsEventId = c("us1", "us1", "", "us4"),
                    EventMagnitude = c(6, 5, 6.3, 5), EventMagnitudeType = c("Ms", "ML", "mww", "mb"),
                    EventMagnitude.owner = c(6, 5.5, 6.3, 5), EventMagnitudeType.owner = c("Ms", "Mw", "Mw", "mb")),
         file.path(DIR, "MasterIndex.csv"))
  fwrite(data.table(RecordID = Keys$RecordID, OCID = "H1", PGA = 100, AI = 10),
         file.path(DIR, "IntensityTable.csv"))
  fwrite(data.table(Keys, Tn = 1, PSA.H1 = 0.84 * 9806.65), file.path(DIR, "PSW.csv"))
  jsonlite::write_json(list(processID = "TEST", output = list(record = "PSW",
    table = c("MasterIndex", "IntensityTable", "PSW"))), file.path(DIR, "process.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(processID = "TEST", xi = 0.05, units = list(psa = "mm/s2"), stage0 = list(N = 2),
    path = list(database = "database", selection = "rank/selection.csv"),
    candidate = list(ownerID = c("A", "B"), oneRecordPerEvent = TRUE)),
    file.path(Root, "match.json"), auto_unbox = TRUE)
  stopifnot(identical(Test$rankProject(file = "match.json", root = Root), 0L),
            identical(fread(file.path(Root, "rank", "selection.csv"))$RecordID, c("R1", "R3")))
  # Owner decision of 2026-11-09: candidate.filter and the Mw column of stage0.spectral.csv use
  # the moment magnitude of select; R1 (Ms 6) becomes 6.15 and R2 takes the owner Mw 5.5.
  # R4 (mb 5) becomes Mw 5.28 by Scordilis (2006, eq. 22) and enters the filter.
  jsonlite::write_json(list(processID = "TEST", xi = 0.05, units = list(psa = "mm/s2"), stage0 = list(N = 2),
    path = list(database = "database", selection = "filter/selection.csv"),
    candidate = list(ownerID = c("A", "B"), filter = list(EventMagnitude = list(min = 5.2, max = 6.2)))),
    file.path(Root, "filter.json"), auto_unbox = TRUE)
  stopifnot(identical(Test$rankProject(file = "filter.json", root = Root), 0L),
            identical(fread(file.path(Root, "filter", "selection.csv"))$RecordID, c("R1", "R2")))
  Scores <- fread(file.path(Root, "filter", "stage0.spectral.csv"))
  stopifnot(identical(Scores[order(RecordID), RecordID], c("R1", "R2", "R4")),
            isTRUE(all.equal(Scores[order(RecordID), Mw], c(6.15, 5.5, 5.28))))
}

.checkRankCheckpoint()
