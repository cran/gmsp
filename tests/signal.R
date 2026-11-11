library(gmsp)
library(data.table)

checkSignal <- function() {
  expectError <- function(expression) {
    stopifnot(inherits(tryCatch(force(expression), error = identity), "error"))
  }
  # B03: tableContracts.R:49-56 de 1963841; contrato explícito E4.
  TSL <- data.table(RecordID = "R1", OCID = "H1",
    ID = rep(c("AT", "VT"), each = 2L), t = rep(c(0, 0.01), 2L), s = c(1, 2, 0.1, 0.2))
  Saved <- copy(TSL)
  TSW <- widenTSL(TSL, by = "RecordID")
  # Reshaping preserves the supplied samples and the declared column names.
  stopifnot(identical(TSL, Saved), identical(names(TSW), c("RecordID", "t", "AT.H1", "VT.H1")),
    isTRUE(all.equal(TSW$AT.H1, c(1, 2))), isTRUE(all.equal(TSW$VT.H1, c(0.1, 0.2))))
  stopifnot(identical(as.data.frame(lengthenTSW(TSW, by = "RecordID")), as.data.frame(TSL)))
  expectError(widenTSL(rbind(TSL, TSL[1L]), by = "RecordID"))
  expectError(widenTSL(TSL, by = c("RecordID", "RecordID")))
  Sparse <- lengthenTSW(widenTSL(TSL[-1L], by = "RecordID"), by = "RecordID")
  stopifnot(sum(is.na(Sparse$s)) == 1L, is.na(Sparse[ID == "AT" & t == 0, s]))
  Legacy <- copy(TSW)
  setnames(Legacy, "t", "ts")
  stopifnot(identical(as.data.frame(lengthenTSW(Legacy, by = "RecordID")), as.data.frame(TSL)))
  Legacy[, t := ts]
  expectError(lengthenTSW(Legacy, by = "RecordID"))
  expectError(lengthenTSW(rbind(TSW, TSW[1L]), by = "RecordID"))
  # B04: normalizeTS.R:26-35; mismo factor para las tres cantidades.
  t <- seq(0, 1, by = 0.01)
  TSL <- rbindlist(list(data.table(t = t, s = 2 * sin(2 * pi * t), ID = "AT", OCID = "H1"),
    data.table(t = t, s = cos(2 * pi * t), ID = "VT", OCID = "H1"),
    data.table(t = t, s = sin(pi * t), ID = "DT", OCID = "H1")))
  Saved <- copy(TSL)
  Normalized <- normalizeTSL(TSL, by = character(), reference = "PGA")
  stopifnot(identical(TSL, Saved), isTRUE(all.equal(Normalized$s, TSL$s / 2)),
    isTRUE(all.equal(max(abs(Normalized[ID == "AT", s])), 1)))
  expectError(normalizeTSL(TSL[ID != "AT"], by = character(), reference = "PGA"))
  TSL[ID == "AT", s := 0]
  expectError(normalizeTSL(TSL, by = character(), reference = "PGA"))
  TSL[ID == "AT", s := NA_real_]
  expectError(normalizeTSL(TSL, by = character(), reference = "PGA"))
  # B01: signal-processing.Rmd:70-98; controles históricos explícitos D5.
  t <- seq(0, 5, by = 0.01)
  TSL <- data.table(OCID = "H1", ID = "AT", t = t,
    s = 120 * sin(pi * t / max(t))^2 * sin(2 * pi * 1.5 * t))
  Saved <- copy(TSL)
  Arguments <- list(TSL = TSL, by = character(), unitsSource = "mm", unitsTarget = "mm", fmax = 5,
    windowLength = 128L, overlap = 75, astop0 = 1e-3, apass0 = 1e-3,
    astopLP = 1e-3, apassLP = 0.98, includeWarnings = FALSE)
  Built <- do.call(buildTSL, Arguments)
  # Processing must leave the caller's input table unchanged.
  stopifnot(identical(TSL, Saved), identical(names(Built), c("OCID", "ID", "t", "s")),
    setequal(Built$ID, c("AT", "VT", "DT")), all(is.finite(Built$s)))
  Grids <- split(Built$t, Built$ID)
  # The three grids are copies of the same Time vector, not independent calculations.
  stopifnot(identical(Grids$AT, Grids$VT), identical(Grids$AT, Grids$DT),
    isTRUE(all.equal(min(Built$t), 0)))
  Arguments$unitsTarget <- "mm/s"
  expectError(do.call(buildTSL, Arguments))
  Arguments$unitsTarget <- "mm"
  Arguments$fmax <- 0
  expectError(do.call(buildTSL, Arguments))
  Arguments$fmax <- 5
  Arguments$TSL <- rbind(TSL, TSL[1L])
  expectError(do.call(buildTSL, Arguments))
}
checkSignal()
