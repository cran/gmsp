library(gmsp)

.checkProcessModalJSON <- function() {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  File <- tempfile(fileext = ".json")
  on.exit(unlink(File), add = TRUE)
  Config <- list(imf = list(method = "decomposeVMD", control = list(
    initialCenters = (0:2) / (6 * 0.001), alpha = 2000 * 0.001^2 / (8 * pi^2),
    tau = 0.5, fixedZeroCenter = FALSE, tolerance = 1e-6, maxIterations = 98)))
  jsonlite::write_json(Config, File, auto_unbox = TRUE, digits = NA)
  Params <- Namespace$.processIMFParams(Namespace$.readProjectConfig(File)$imf)
  stopifnot(is.numeric(Params$control$initialCenters),
            isTRUE(all.equal(Params$control$initialCenters, Config$imf$control$initialCenters)))
  Config$imf <- list(method = "eemd", k = 2, noise.amp = 0.5e-7, trials = 10,
    seed = 628, sifting = list(stopRule = "doubleThreshold", thresholds = c(0.05, 0.5)))
  jsonlite::write_json(Config, File, auto_unbox = TRUE, digits = NA)
  Params <- Namespace$.processIMFParams(Namespace$.readProjectConfig(File)$imf)
  stopifnot(isTRUE(all.equal(Params$control$sifting$thresholds, c(0.05, 0.5))))
  Mixed <- list(sifting = list(thresholds = list(0.05, "0.5")))
  # A mixed list is returned unchanged, preserving both values and types.
  stopifnot(identical(Namespace$.processControlVectors(Mixed), Mixed))
}
.checkProcessModalJSON()
