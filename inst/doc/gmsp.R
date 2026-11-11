## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----install, eval = FALSE----------------------------------------------------
# install.packages("gmsp")

## ----record, message = FALSE--------------------------------------------------
library(gmsp)
library(data.table)

Time <- seq(0, 20, by = 0.01)
Envelope <- sin(pi * Time / max(Time))^2
TSL <- rbindlist(list(
  data.table(RecordID = "R1", OCID = "H1", ID = "AT", t = Time,
             s = 1200 * Envelope * sin(2 * pi * 1.5 * Time)),
  data.table(RecordID = "R1", OCID = "H2", ID = "AT", t = Time,
             s = 900 * Envelope * sin(2 * pi * 2.5 * Time + 0.4))
))
head(TSL, 3)

## ----histories----------------------------------------------------------------
Histories <- buildTSL(
  TSL, by = "RecordID", unitsSource = "mm", unitsTarget = "mm", fmax = 5,
  windowLength = 128, overlap = 75,
  astop0 = 1e-3, apass0 = 1e-3,
  astopLP = 1e-3, apassLP = 0.98,
  includeWarnings = FALSE
)
Histories[, .(samples = .N, start = min(t), peak = max(abs(s))), by = .(OCID, ID)]

## ----spectra------------------------------------------------------------------
PSL <- computeSpectraTSL(Histories, by = "RecordID", xi = 0.05,
                         Tn = c(0.1, 0.2, 0.5, 1, 2),
                         includeD50 = TRUE, nTheta = 8)
PSL[ID == "PSA" & OCID == "D50"]

## ----wide---------------------------------------------------------------------
widenPSL(PSL, by = "RecordID")[, .(Tn, PSA.H1, PSA.H2, PSA.D50)]

## ----intensities--------------------------------------------------------------
IML <- measureIntensitiesTSL(Histories, by = "RecordID",
                             unitsSource = "mm", unitsTarget = "mm")
IML[IM %in% c("PGA", "AI", "D0595", "CAV", "PGV", "PGD")]

## ----project------------------------------------------------------------------
Root <- file.path(tempdir(), "gmsp-introduction")
dir.create(file.path(Root, "database", "index"), recursive = TRUE)
dir.create(file.path(Root, "database", "process", "demo"), recursive = TRUE)
Index <- data.table(
  RecordID = c("101", "102", "103", "104"), OwnerID = c("A", "B", "A", "B"),
  EventID = c("2019a", "2019b", "2021a", "2022a"),
  EventID.USGS = c("us01", "us01", "us02", "us03"),
  StationID = c("S1", "S2", "S3", "S4"), DIR = "H1", OCID = "H1",
  PGA = c(2000, 3500, 1500, 900), ProcessID = "demo",
  EventMagnitude = c(6.8, 6.9, 7.1, 5.6),
  EventMagnitudeType = c("Mw", "mww", "ML", "Mw")
)
fwrite(Index, file.path(Root, "database", "process", "demo", "MasterIndex.csv"))
jsonlite::write_json(list(
  processID = "demo", owners = c("A", "B"),
  units = list(PGA = "g"),
  criteria = list(list(criterion = "strong",
                       EventMagnitude = c(6, 8), PGA = c(0.1, 0.5))),
  path = list(index = "database/index", selection = "selection")
), file.path(Root, "select.json"), auto_unbox = TRUE, pretty = TRUE)

## ----select-------------------------------------------------------------------
selectProject(file = "select.json", root = Root)
Selection <- fread(file.path(Root, "selection", "selection.csv"))
Candidates <- fread(file.path(Root, "selection", "candidates.csv"))
Selection[, .(RecordID, EventID.USGS, EventMagnitude, EventMagnitudeType, PGA)]
Candidates[, .(RecordID, EventID.USGS, PGA)]

## ----summary------------------------------------------------------------------
Summary <- jsonlite::read_json(file.path(Root, "selection", "summary.json"))
Summary[c("candidates", "records", "events")]
unlink(Root, recursive = TRUE)

