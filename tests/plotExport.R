library(gmsp)
library(data.table)

.testPlotExport <- function() {
  # Datos D8: 1963841 tests/runExport.R; fronteras E4 de export.
  Root <- tempfile("gmsp plot export ")
  dir.create(file.path(Root, "source"), recursive = TRUE)
  dir.create(file.path(Root, "gmsp"))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Directory <- getwd()
  TSW <- data.table(RecordID = "001", t = c(0, 0.1, 0.2),
    AT.H1 = c(0L, 1L, -1L), AT.H2 = c(1L, 0L, -1L))
  fwrite(TSW, file.path(Root, "source/TSW.csv"))
  fwrite(data.table(RecordID = "001", OCID = c("H1", "H2"), PGA = c(100, 200)),
    file.path(Root, "source/IMW.csv"))
  Inputs <- list.files(file.path(Root, "source"), full.names = TRUE)
  Hash <- tools::md5sum(Inputs)
  Config <- list(path = list(source = "source", out = "delivery"), DIR = c("H1", "H2"))
  File <- file.path(Root, "gmsp/export.json")
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Result <- withVisible(exportProject(root = Root))
  stopifnot(identical(Result$value, 0L), !Result$visible, identical(getwd(), Directory),
    isTRUE(all.equal(fread(file.path(Root, "delivery/TSW.001.AT.H1.csv")), data.table(t = TSW$t, s = TSW$AT.H1))),
    identical(tools::md5sum(Inputs), Hash))
  Files <- list.files(file.path(Root, "delivery"), recursive = TRUE, full.names = TRUE)
  Before <- tools::md5sum(Files)
  Error <- tryCatch(exportProject(root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), identical(tools::md5sum(Files), Before),
    identical(getwd(), Directory))
  writeLines("preserve", file.path(Root, "delivery/foreign.txt"))
  Config$override <- TRUE
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  exportProject(root = Root)
  stopifnot(identical(readLines(file.path(Root, "delivery/foreign.txt")), "preserve"),
    identical(tools::md5sum(Inputs), Hash), identical(getwd(), Directory))

  # Las colisiones se detectan antes de crear el destino.
  for (x in c("plot", "audit")) {
    dir.create(file.path(Root, x, "TSI"), recursive = TRUE)
    writeLines("preserve", file.path(Root, x, "TSI/AT-H1.html"))
  }
  Config$path$out <- "collision"
  Config$path$plot <- c("plot", "audit")
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Error <- tryCatch(exportProject(root = Root), error = identity)
  stopifnot(inherits(Error, "moduleUsage"), grepl("duplicate relative HTML", conditionMessage(Error)),
    !dir.exists(file.path(Root, "collision")), identical(getwd(), Directory))
  Config$path$out <- "nested"
  Config$path$plot <- "plot"
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  exportProject(root = Root)
  stopifnot(identical(readLines(file.path(Root, "nested/plot/TSI/AT-H1.html")), "preserve"))
}
.testPlotExport()

.testRawExport <- function() {
  Root <- tempfile("gmsp-raw-export-")
  Records <- file.path(Root, "database", "records")
  Index <- file.path(Root, "database", "index")
  dir.create(Index, recursive = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  extract <- function(channels) {
    # The extractRecord example, with channel names varied for the collision case.
    Station <- file.path(Records, "NGAW", "E1", "S1", "raw.owner")
    dir.create(Station, recursive = TRUE, showWarnings = FALSE)
    Samples <- setNames(c("0.01 0.03 -0.02 0.01", "0.02 -0.01 0.01 0",
                          "0 0.01 -0.01 0"), channels)
    for (Channel in names(Samples)) {
      writeLines(c("header", paste0("Event, date, station, ", Channel), "units",
                   "NPTS= 4, DT= 0.01 SEC", Samples[[Channel]]),
                 file.path(Station, paste0(Channel, ".AT2")))
    }
    Record <- data.table(OwnerID = "NGAW", EventID = "E1", StationID = "S1",
                        FileID = paste0(channels, ".AT2"), Units = "g")
    extractRecord(Record, path = Records)
  }
  CSV <- extract(c("HNE", "HNN", "HNZ"))
  JSON <- sub("[.]csv$", ".json", CSV)
  Sidecar <- jsonlite::read_json(JSON, simplifyVector = TRUE)
  AT <- fread(CSV)
  Hash <- tools::md5sum(c(CSV, JSON))
  Keys <- data.table(ProcessID = "raw", RecordID = Sidecar$RecordID,
                    OwnerID = "NGAW", EventID = "E1", StationID = "S1")
  fwrite(Keys, file.path(Index, "MasterIndex.csv"))
  File <- file.path(Root, "export.json")
  Config <- list(path = list(database = "database", out = "delivery",
                            index = "database/index/MasterIndex.csv"),
                 recordID = Sidecar$RecordID, DIR = "HNE")
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  stopifnot(identical(exportProject(file = File, root = Root), 0L))
  Expected <- data.table(t = (seq_len(nrow(AT)) - 1L) * Sidecar$dt, s = AT$HNE)
  OUT <- fread(file.path(Root, "delivery", paste0("AT.", Sidecar$RecordID, ".HNE.csv")))
  stopifnot(isTRUE(all.equal(OUT, Expected)))

  Config$path$index <- "database/index"
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Error <- tryCatch(exportProject(file = File, root = Root), error = identity)
  stopifnot(inherits(Error, "gmspUsage"),
            grepl("path.index", conditionMessage(Error), fixed = TRUE))

  Config$path$index <- NULL
  Config$path$out <- "canonical"
  Config$DIR <- NULL
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  stopifnot(identical(exportProject(file = File, root = Root), 0L))
  OUT <- fread(file.path(Root, "canonical", paste0("AT.", Sidecar$RecordID, ".H1.csv")))
  Files <- fread(file.path(Root, "canonical", "files.csv"))
  stopifnot(identical(Sidecar$OCID[match("H1", Sidecar$DIR)], "HNE"),
            isTRUE(all.equal(OUT, Expected)), identical(Files$DIR, "H1"),
            identical(tools::md5sum(c(CSV, JSON)), Hash))

  CSV <- extract(c("H2", "H1", "HNZ"))
  Sidecar <- jsonlite::read_json(sub("[.]csv$", ".json", CSV), simplifyVector = TRUE)
  AT <- fread(CSV)
  Keys[, RecordID := Sidecar$RecordID]
  fwrite(Keys, file.path(Index, "MasterIndex.csv"))
  Config$recordID <- Sidecar$RecordID
  Config$path$out <- "literal"
  Config$DIR <- "H1"
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  stopifnot(identical(exportProject(file = File, root = Root), 0L))
  OUT <- fread(file.path(Root, "literal", paste0("AT.", Sidecar$RecordID, ".H1.csv")))
  stopifnot(identical(Sidecar$OCID[match("H1", Sidecar$DIR)], "H2"),
    isTRUE(all.equal(OUT, data.table(t = (seq_len(nrow(AT)) - 1L) * Sidecar$dt, s = AT$H1))))
}
.testRawExport()
