library(gmsp)
library(data.table)

checkIntensity <- function() {
  expectError <- function(expression) {
    stopifnot(inherits(tryCatch(force(expression), error = identity), "gmspUsage"))
  }
  # C01: getIntensity.R:22-34 de 1963841.
  t <- seq(0, 1, by = 0.01)
  TSL <- rbindlist(list(
    data.table(t = t, s = sin(2 * pi * t), ID = "AT", OCID = "H1"),
    data.table(t = t, s = cos(2 * pi * t), ID = "VT", OCID = "H1"),
    data.table(t = t, s = sin(pi * t), ID = "DT", OCID = "H1")))
  Saved <- copy(TSL)
  IML <- measureIntensitiesTSL(TSL, by = character(), unitsSource = "mm", unitsTarget = "mm")
  stopifnot(identical(TSL, Saved), nrow(IML) == 32L,
    identical(names(IML), c("OCID", "ID", "IM", "value", "units")),
    isTRUE(all.equal(IML[IM == "PGA", value], 1)), isTRUE(all.equal(IML[IM == "PGV", value], 1)),
    isTRUE(all.equal(IML[IM == "PGD", value], 1)))
  expectError(measureIntensitiesTSL(TSL[ID != "VT"], by = character(), unitsSource = "mm", unitsTarget = "mm"))
  expectError(measureIntensitiesTSL(TSL, by = character(), unitsSource = "g", unitsTarget = "mm"))
  expectError(measureIntensitiesTSL(rbind(TSL, TSL[1L]), by = character(), unitsSource = "mm", unitsTarget = "mm"))
  # C02/C03: intensityRatios.R:79-91,196-208; cero y umbral de E4.
  AT <- data.table(RecordID = "R1", OCID = "H1", ID = "AT",
    t = seq(0, 1, by = 0.25), s = c(0, 200, 500, 200, 0))
  Saved <- copy(AT)
  for (ID in c("AI", "CAV")) {
    OUT <- accumulateIntensityTSL(AT, by = "RecordID", ID = ID, unitsSource = "mm", unitsTarget = "mm")
    # Exact equality checks that the supplied samples are unchanged.
    stopifnot(identical(AT, Saved), isTRUE(all.equal(tail(OUT$s, 1L), 1)), all(diff(OUT$s) >= 0), identical(unique(OUT$ID), ID))
  }
  AT[, s := c(0, 400, 500, 400, 0)]
  OUT <- accumulateIntensityTSL(AT, by = "RecordID", ID = "CAV5", unitsSource = "mm", unitsTarget = "mm")
  stopifnot(isTRUE(all.equal(OUT$s, c(0, 0, 1, 1, 1))))
  AT[, s := 0]
  stopifnot(all(is.na(accumulateIntensityTSL(AT, by = "RecordID", ID = "AI", unitsSource = "mm", unitsTarget = "mm")$s)))
  IML <- measureIntensitiesTSL(AT, by = "RecordID", unitsSource = "mm", unitsTarget = "mm")
  AUX <- IML[IM %chin% c("D0595", "D2080", "D0575", "TmA"), value]
  stopifnot(isTRUE(all.equal(AUX, rep(0, length(AUX)))))
  AT[, s := NA_real_]
  expectError(accumulateIntensityTSL(AT, by = "RecordID", ID = "AI", unitsSource = "mm", unitsTarget = "mm"))
  # C04: TSL2PS.R:45-59; controles de rotación y ancla del contrato.
  TSL <- rbindlist(list(TSL,
    data.table(t = t, s = sin(2 * pi * t + 0.3), ID = "AT", OCID = "H2"),
    data.table(t = t, s = cos(2 * pi * t + 0.4), ID = "VT", OCID = "H2"),
    data.table(t = t, s = sin(pi * t + 0.2), ID = "DT", OCID = "H2")))
  Saved <- copy(TSL)
  PSL <- computeSpectraTSL(TSL, by = character(), xi = 0.05, Tn = c(0.1, 0.2))
  # Anclas calculadas con 0.8.0: caso C04 de TSL2PS.R en 1963841.
  stopifnot(identical(TSL, Saved), nrow(PSL) == 18L,
    identical(names(PSL), c("OCID", "Tn", "ID", "S")),
    isTRUE(all.equal(PSL[OCID == "H1" & Tn == 0, S],
      c(0.99992094453550862, 1, 0.99992102628418045))))
  Rotated <- computeSpectraTSL(TSL, by = character(), xi = 0.05, Tn = 0.1,
    includeD50 = TRUE, includeD100 = TRUE, nTheta = 6L)
  stopifnot(identical(TSL, Saved), setequal(Rotated$OCID, c("H1", "H2", "D50", "D100")), nrow(Rotated) == 24L)
  PSW <- widenPSL(PSL, by = character())
  Back <- lengthenPSW(PSW, by = character())
  setorderv(Back, c("OCID", "ID", "Tn"))
  stopifnot(identical(as.data.frame(Back), as.data.frame(PSL)), nrow(lengthenPSW(PSW[0], by = character())) == 0L)
  expectError(computeSpectraTSL(TSL, by = character(), xi = 0.05, Tn = 0))
  expectError(computeSpectraTSL(TSL[-1L], by = character(), xi = 0.05, Tn = 0.1, includeD50 = TRUE, nTheta = 6L))
  expectError(widenPSL(rbind(PSL, PSL[1L]), by = character()))
  # C05: tableContracts.R:222-235 y transporte de metadatos E4.
  IML <- data.table(RecordID = "R1", OCID = "H1", ID = "AT",
    IM = c("PGA", "CAV"), value = c(500, 120), units = c("mm /s2", "mm /s"))
  Saved <- copy(IML)
  IMW <- widenIML(IML, by = "RecordID")
  # Input and metadata are preserved; calculated values use all.equal.
  stopifnot(identical(IML, Saved), identical(names(IMW), c("RecordID", "OCID", "PGA", "CAV")),
    isTRUE(all.equal(unname(unlist(IMW[, .(PGA, CAV)])), c(500, 120))),
    identical(as.data.frame(attr(IMW, "measureMetadata")), as.data.frame(IML[, .(RecordID, OCID, IM, ID, units)])),
    identical(names(widenIML(IML[0], by = "RecordID")), c("RecordID", "OCID")))
  expectError(widenIML(rbind(IML, IML[1L]), by = "RecordID"))
}
checkIntensity()
