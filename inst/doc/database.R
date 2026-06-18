## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----minimal-example, eval = TRUE---------------------------------------------
# Minimal executable example — selectRecords() works entirely in memory
library(gmsp)
library(data.table)
master <- data.table(
  RecordID  = c("aabbccdd00112233", "aabbccdd00112233", "eeff00112233aabb"),
  OwnerID   = c("NGAW", "NGAW", "CESMD"),
  EventID   = c("20100227T063452Z", "20100227T063452Z", "20110311T054624Z"),
  StationID = c("ANTU", "ANTU", "MYG004"),
  DIR       = c("H1", "H2", "H1"),
  EventMagnitude = c(8.8, 8.8, 9.1),
  Repi      = c(90, 90, 140)
)
sel <- selectRecords(master[EventMagnitude > 8 & DIR == "H1"])
print(sel)

