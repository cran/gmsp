library(gmsp)
library(data.table)

.testProviders <- function() {
  Directory <- tempfile("gmsp-providers-")
  dir.create(Directory)
  on.exit(unlink(Directory, recursive = TRUE), add = TRUE)
  File <- file.path(Directory, "record.AT2")
  # D8: encabezado de cuatro muestras; D10 conserva NULL si falta cuerpo.
  writeLines(c("header", "Event, date, station, H1", "units",
    "NPTS= 4, DT= 0.01 SEC", "1.0 2.0 3.0"), File)
  stopifnot(is.null(readProviderFile(File, format = "AT2")))
  expectError <- function(expr) {
    Error <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(Error, "error"))
    Error
  }
  expectError(readProviderFile(File, format = "unknown"))
  # El ejemplo D8 de lectura tiene dos canales; la API no inventa el tercero.
  Raw <- file.path(Directory, "AAA", "E1", "S1", "raw")
  dir.create(Raw, recursive = TRUE)
  fwrite(data.table(H1 = c(1, 2), H2 = c(0, 1)), file.path(Raw, "AT.R1.csv"))
  jsonlite::write_json(list(dt = 0.01), file.path(Raw, "AT.R1.json"), auto_unbox = TRUE)
  Keys <- data.table(RecordID = "R1", OwnerID = "AAA", EventID = "E1", StationID = "S1")
  Before <- serialize(Keys, NULL)
  OUT <- readRecordTSL(Keys, path = Directory, ID = "AT")
  stopifnot(identical(names(OUT), c(names(Keys), "OCID", "ID", "t", "s")),
    identical(OUT$OCID, c("H1", "H1", "H2", "H2")),
    identical(OUT$ID, rep("AT", 4L)), identical(OUT$s, c(1L, 2L, 0L, 1L)),
    isTRUE(all.equal(OUT$t, c(0, 0.01, 0, 0.01))), identical(Before, serialize(Keys, NULL)))
  Error <- expectError(readRecordTSL(Keys, path = Directory, ID = "VT"))
  stopifnot(grepl("R1", conditionMessage(Error), fixed = TRUE),
    grepl("VT.R1.csv", conditionMessage(Error), fixed = TRUE))
  Empty <- selectRecords(Keys, recordID = character())
  stopifnot(identical(names(Empty), names(Keys)), nrow(Empty) == 0L)
  Written <- suppressMessages(withVisible(writeSelection(Empty, name = "demo", path = file.path(Directory, "selection"))))
  Metadata <- jsonlite::read_json(file.path(Directory, "selection", "demo.json"), simplifyVector = TRUE)
  stopifnot(!Written$visible, nrow(Written$value) == 0L, Metadata$nHits == 0L)
  Channel <- data.table(t = c(0, 0.01, 0.02), OCID = "H1", DIR = "H1", s = c(1, 2, 3))
  expectError(alignComponents(Channel, align = "other"))

  # The maintained extractRecord example, with NetworkID absent and supplied.
  Records <- file.path(Directory, "records")
  Station <- file.path(Records, "NGAW", "E1", "S1", "raw.owner")
  dir.create(Station, recursive = TRUE)
  Samples <- c(HNE = "0.01 0.03 -0.02 0.01", HNN = "0.02 -0.01 0.01 0",
               HNZ = "0 0.01 -0.01 0")
  for (Channel in names(Samples)) {
    writeLines(c("header", paste0("Event, date, station, ", Channel), "units",
                 "NPTS= 4, DT= 0.01 SEC", Samples[[Channel]]),
               file.path(Station, paste0(Channel, ".AT2")))
  }
  Record <- data.table(OwnerID = "NGAW", EventID = "E1", StationID = "S1",
                       FileID = paste0(names(Samples), ".AT2"), Units = "g")
  CSV <- extractRecord(Record, path = Records)
  Metadata <- jsonlite::read_json(sub("[.]csv$", ".json", CSV), simplifyVector = TRUE)
  stopifnot(!"NetworkID" %in% names(Metadata))
  Hash <- tools::md5sum(CSV)
  Record[, NetworkID := OwnerID]
  CSV <- extractRecord(Record, path = Records)
  Sidecar <- jsonlite::read_json(sub("[.]csv$", ".json", CSV), simplifyVector = TRUE)
  stopifnot(identical(Sidecar$NetworkID, Record$NetworkID[1L]),
            identical(Sidecar$RecordID, Metadata$RecordID), identical(tools::md5sum(CSV), Hash))
}
.testProviders()
