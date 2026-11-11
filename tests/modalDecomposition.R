library(gmsp)
library(data.table)

.checkModalDecomposition <- function(decompose = decomposeRecords,
  translate = getFromNamespace(".translateControl", "gmsp"),
  admit = getFromNamespace(".checkDecompositionControl", "gmsp")) {
  .expectUsage <- function(expr, unit = NULL) {
    Error <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(Error, "moduleUsage"))
    if (!is.null(unit)) stopifnot(grepl(unit, Error$unit, fixed = TRUE))
    invisible(Error)
  }
  Controls <- list(k = 3, alpha = 2000, tau = 0.5, DC = FALSE,
                   init = 0, tolerance = 1e-6, maxIterations = 98)
  Before <- serialize(Controls, NULL)
  OUT <- translate("vmd", Controls, samplingInterval = 0.001, signal = NULL)
  stopifnot(identical(OUT$method, "decomposeVMD"),
    isTRUE(all.equal(OUT$control, list(initialCenters = rep(0, 3),
      alpha = 2000 * 0.001^2 / (8 * pi^2), tau = 0.5, fixedZeroCenter = FALSE,
      tolerance = 1e-6, maxIterations = 98))), identical(Before, serialize(Controls, NULL)))
  Native <- OUT$control
  stopifnot(identical(translate("decomposeVMD", Native, NULL, NULL)$control, Native),
    identical(admit("vmd", Controls), "decomposeVMD"))
  AUX <- Controls
  AUX$tolerance <- NULL
  AUX$tol <- 1e-7
  .expectUsage(admit("vmd", AUX))
  .expectUsage(admit("decomposeVMD", c(Native, list(signal = 0))))

  Time <- seq(0, 9, length.out = 1000)
  Signal <- sin(pi * Time) + sin(2 * pi * Time) + sin(6 * pi * Time) + 0.5 * Time
  Ensemble <- list(k = 10, noise.amp = 0.5e-7, trials = 10,
    sifting = list(stopRule = "doubleThreshold", thresholds = c(0.05, 0.5),
      fraction = 0.05, maxSift = 2000, boundary = "reflection"))
  Before <- serialize(Ensemble, NULL)
  OUT <- translate("eemd", Ensemble, samplingInterval = NULL, signal = Signal)
  stopifnot(identical(OUT$method, "decomposeEEMD"),
    isTRUE(all.equal(OUT$control$realizationCount, 10)), OUT$control$divisor == "realizations",
    isTRUE(all.equal(OUT$control$sifting$maxModes, 10)),
    isTRUE(all.equal(OUT$control$amplitudeRatio, Ensemble$noise.amp / stats::sd(Signal))),
    identical(Before, serialize(Ensemble, NULL)))
  AUX <- Ensemble
  AUX$sifting$maxModes <- Inf
  .expectUsage(admit("eemd", AUX))
  .expectUsage(translate("eemd", Ensemble, NULL, rep(0, length(Time))))

  TSL <- data.table(RecordID = "R1", OCID = "H1", ID = "AT", t = Time,
                     s = Signal, Units = "mm")
  Before <- serialize(TSL, NULL)
  HadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  Seed <- if (HadSeed) get(".Random.seed", envir = .GlobalEnv) else NULL
  .expectUsage(decompose(TSL, "vmd", Controls, by = c("RecordID", "RecordID")))
  .expectUsage(decompose(TSL, "vmd", Controls, by = "missing"))
  .expectUsage(decompose(TSL[1:2], "vmd", Controls, by = "RecordID"), "RecordID=R1")
  .expectUsage(decompose(rbind(TSL, TSL[1]), "vmd", Controls, by = "RecordID"), "RecordID=R1")
  AUX <- copy(TSL)
  AUX[1, Units := "m"]
  .expectUsage(decompose(AUX, "vmd", Controls, by = "RecordID"), "RecordID=R1")
  AUX <- copy(TSL)
  AUX[, t := as.list(t)]
  .expectUsage(decompose(AUX, "vmd", Controls, by = "RecordID"), "RecordID=R1")
  AUX <- rbind(TSL, TSL[1:(.N - 1)][, OCID := "H2"])
  .expectUsage(decompose(AUX, "vmd", Controls, by = "RecordID"), "RecordID=R1")
  AUX <- copy(TSL)
  AUX[, mode := 1L]
  .expectUsage(decompose(AUX, "vmd", Controls, by = "RecordID"))
  AUX <- copy(TSL)
  AUX[, s := 0]
  .expectUsage(decompose(AUX, "eemd", Ensemble, by = "RecordID"), "OCID=H1")
  stopifnot(identical(Before, serialize(TSL, NULL)),
    identical(HadSeed, exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)))
  if (HadSeed) stopifnot(identical(Seed, get(".Random.seed", envir = .GlobalEnv)))
  invisible(TRUE)
}

.checkModalDecomposition()
