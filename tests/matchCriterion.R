library(gmsp)
library(data.table)

.checkMatchCriterion <- function() {
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  Test <- new.env(parent = Namespace)
  for (Name in ls(Namespace, all.names = TRUE)) {
    Value <- get(Name, envir = Namespace)
    if (is.function(Value) && identical(environment(Value), Namespace)) environment(Value) <- Test
    assign(Name, Value, envir = Test)
  }
  Root <- tempfile("gmsp match criterion ")
  dir.create(Root)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  expectUsage <- function(expression, text) {
    Error <- tryCatch(force(expression), error = identity)
    stopifnot(inherits(Error, "gmspUsage"), grepl(text, conditionMessage(Error), fixed = TRUE))
  }
  Control <- list(windowLength = 128, overlap = 75, astop0 = 0.001,
    apass0 = 0.001, astopLP = 0.001, apassLP = 0.98,
    flatZeros = FALSE, trimZeros = FALSE, removeMean = FALSE,
    forceRegularization = FALSE, includeWarnings = FALSE)
  Config <- list(id = "test", path = list(data = Root, out = "out", source = "source"),
    target = list(demand = "MCE", siteID = "ANTPY", Vs30 = 800, p = "0.84",
      value = "SaF", units = "g", envelope = list(lower = "0.50", upper = "0.95")),
    Tn = list(min = 0.04, max = 5),
    shape = list(modalFactor = list(min = 0.001, max = "Inf"),
      recordFactor = list(min = 0.5, max = 1.6), bandWeight = 2, deficitWeight = 1),
    units = list(source = "mm", target = "mm", psa = "mm/s2"),
    imf = list(criterion = list(rule = "target", action = "remove")),
    signalControl = Control, xi = 0.05, fmax = 25, kNyq = 4, nTheta = 180)
  stopifnot(isTRUE(Test$.matchParams(Config)$includeResidue))
  for (Criterion in list(list(rule = "target", action = "keep"),
    list(rule = "target", action = "remove", fMin = 0.2),
    list(rule = "target", action = "remove", action = "remove"))) {
    Invalid <- Config
    Invalid$imf$criterion <- Criterion
    expectUsage(Test$.matchParams(Invalid), "exactly rule and action")
  }
  expectUsage(Test$.processIMFTSParams(c(list(criterion = Config$imf$criterion,
    fmax = 25, kNyq = 4, unitsTarget = "mm"), Control)), "only by match")
  Target <- CJ(ID = "MCE", siteID = "ANTPY", Vs30 = 800,
    Tn = c(0, 0.01, 0.04, 1, 5, 10), p = c("0.50", "0.84", "0.95"))
  Target[, SaF := as.numeric(p)]
  saveRDS(Target, file.path(Root, "MCETable.Rds"))
  Target <- Test$.projectTarget(Config, units = "mm/s2", root = Root, envelope = TRUE)
  stopifnot(isTRUE(all.equal(Target$Tn, c(0, 0.04, 1, 5))))
  for (Field in c("lower", "upper")) {
    for (x in list("p50", "50%", "50", NULL)) {
      AUX <- Config
      AUX$target$envelope[Field] <- list(x)
      Error <- tryCatch(Test$.projectTarget(AUX, units = "mm/s2", root = Root,
        envelope = TRUE), error = identity)
      stopifnot(inherits(Error, "gmspUsage"),
        grepl(paste0("target.envelope.", Field), conditionMessage(Error), fixed = TRUE),
        grepl("0.<digits> strictly between 0 and 1", conditionMessage(Error), fixed = TRUE),
        grepl("or mean", conditionMessage(Error), fixed = TRUE))
    }
  }
  for (Envelope in list("0.50", list(lower = "0.50", lower = "0.84", upper = "0.95"),
    list(ID = "MCE", lower = "0.50", upper = "0.95"),
    list(lower = "0.84", upper = "0.95"))) {
    AUX <- Config
    AUX$target$envelope <- Envelope
    Error <- tryCatch(Test$.projectTarget(AUX, units = "mm/s2", root = Root,
      envelope = TRUE), error = identity)
    stopifnot(inherits(Error, "gmspUsage"),
      grepl("target.envelope", conditionMessage(Error), fixed = TRUE),
      grepl("0.<digits> strictly between 0 and 1", conditionMessage(Error), fixed = TRUE),
      grepl("or mean", conditionMessage(Error), fixed = TRUE))
  }
  stopifnot(identical(Test$.targetQuantile("mean", label = "target.envelope.lower"), "mean"),
    identical(Test$.targetQuantile("0.50", label = "target.envelope.lower"), "0.50"),
    identical(Test$.targetQuantile(0.50, label = "target.envelope.lower"), "0.5"))
  message("PASS: envelope errors name decimal quantiles and mean; literal selectors are preserved.")
  Resolved <- Test$.matchCriterion(Config$imf$criterion, target = Target, fmax = Config$fmax)
  stopifnot(isTRUE(all.equal(Resolved$criterion,
    list(rule = "band", fMin = 0.2, fMax = 25, action = "keep"))),
    isTRUE(all.equal(Resolved$metadata, list(rule = "target", action = "remove",
      fMin = 0.2, fMax = 25, TnMax = 5, origin = "target"))))
  Target[Tn == 0.04, Tn := 0.05]
  Resolved <- Test$.matchCriterion(Config$imf$criterion, target = Target, fmax = Config$fmax)
  stopifnot(isTRUE(all.equal(Resolved$criterion$fMin, 0.2)), isTRUE(all.equal(Resolved$criterion$fMax, 25)),
    isTRUE(all.equal(Resolved$metadata$TnMax, 5)), !"TnMin" %in% names(Resolved$metadata))
  Invalid <- Config
  Invalid$Tn$max <- 2
  AUX <- readRDS(file.path(Root, "MCETable.Rds"))
  AUX[Tn == 1, Tn := 2]
  saveRDS(AUX, file.path(Root, "MCETable.Rds"))
  Resolved <- Test$.matchCriterion(Config$imf$criterion,
    target = Test$.projectTarget(Invalid, units = "mm/s2", root = Root, envelope = TRUE),
    fmax = Config$fmax)
  stopifnot(isTRUE(all.equal(Resolved$criterion$fMin, 0.5)), isTRUE(all.equal(Resolved$criterion$fMax, 25)))
  for (x in c(0.1, 0.2)) {
    expectUsage(Test$.matchCriterion(Config$imf$criterion, target = Target, fmax = x),
      "fmax must exceed 1/max(Tn)")
  }
  for (Criterion in list(list(rule = "band", fMin = 2, fMax = 15, action = "keep"),
    list(rule = "energyFraction", minimum = 0, action = "keep"))) {
    Resolved <- Test$.matchCriterion(Criterion, target = Target, fmax = Config$fmax)
    stopifnot(identical(Resolved$criterion, Criterion),
      identical(Resolved$metadata, list(rule = Criterion$rule, action = Criterion$action,
        fMin = Criterion$fMin, fMax = Criterion$fMax, TnMax = NULL, origin = "explicit")))
  }
  message("PASS: target uses restricted positive periods; strict action/fields; process rejection; explicit criteria preserved.")

  # Exercise matchProject publication with mixed integer/double IMW measurements.
  Key <- data.table(RecordID = c("R1", "R2"), OwnerID = "A", EventID = "E1", StationID = "S1")
  Intensity <- data.table(Key, OCID = "H1", PGA = c(1L, 2L), AI = c(0.5, 1.5))
  Test$.matchSource <- function(...) list(selection = Key,
    source = list(META = Key, OCIDMAP = data.table(Key, OCID = "H1")))
  Test$.matchRecords <- function(selection, target, source, params, ...) {
    stopifnot(isTRUE(all.equal(params$criterion,
      list(rule = "band", fMin = 0.2, fMax = 25, action = "keep"))))
    list(coeff = Key, eval = Key, failures = list())
  }
  Test$.matchProducts <- function(...) list(TSW = Key, PSW = Key, IMW = Intensity,
    PSA = data.table(RecordID = Key$RecordID, kind = "final", Tn = 1, Sa = c(1, 2)),
    scale = Key, stage = Key, shaping = Key, metrics = Key,
    compatibility = list(record = Key, summary = Key))
  Test$.matchModalProducts <- function(...) list(ITS = Key, IPSA = Key)
  File <- file.path(Root, "match.json")
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Warnings <- character()
  withCallingHandlers(Test$matchProject(file = File, root = Root), warning = function(e) {
    Warnings <<- c(Warnings, conditionMessage(e))
    invokeRestart("muffleWarning")
  })
  stopifnot(!length(Warnings))
  OUT <- fread(file.path(Root, "out", "match", "intensitySummary.csv"))
  stopifnot(isTRUE(all.equal(OUT[IM == "PGA", mean], mean(Intensity$PGA))),
    isTRUE(all.equal(OUT[IM == "AI", mean], mean(Intensity$AI))), all(OUT$recordCount == 2L),
    is.integer(Intensity$PGA), is.double(Intensity$AI))
  Metadata <- jsonlite::read_json(file.path(Root, "out", "metadata", "criterion.json"))
  stopifnot(isTRUE(all.equal(Metadata, list(rule = "target", action = "remove",
    fMin = 0.2, fMax = 25, TnMax = 5, origin = "target"))))
  message("PASS: match publication records target criterion and summarizes integer/double IMW without warnings or value changes.")

  # Plot keys are ignored with a warning and match writes no plot or audit folder.
  Config$plot <- list(audit = TRUE, standard = TRUE)
  Config$override <- TRUE
  jsonlite::write_json(Config, File, auto_unbox = TRUE)
  Warnings <- list()
  Result <- withCallingHandlers(Test$matchProject(file = File, root = Root), warning = function(e) {
    Warnings[[length(Warnings) + 1L]] <<- e
    invokeRestart("muffleWarning")
  })
  stopifnot(identical(Result, 0L), length(Warnings) == 1L,
    inherits(Warnings[[1L]], "gmspWarning"), inherits(Warnings[[1L]], "moduleWarning"),
    identical(Warnings[[1L]]$operation, "matchProject"), identical(Warnings[[1L]]$path, File),
    identical(conditionMessage(Warnings[[1L]]), paste("The keys plot.audit and plot.standard are ignored",
      "since gmsp 1.0.0 because figures are produced in the reports scaffold.")),
    !dir.exists(file.path(Root, "out", "audit")), !dir.exists(file.path(Root, "out", "plot")))
  message("PASS: match warns once that plot.audit and plot.standard are ignored and writes no plot or audit folder.")

  # Synthetic sinusoidal fixture from modalProducts, with a nonzero remnant.
  Key <- Key[1L]
  t <- seq(0, 4, by = 0.01)
  IMF <- data.table(Key, OCID = "H1", t = t,
    IMF1 = 1000 * sin(2 * pi * t), residue = 100 * sin(pi * t))
  IMF[, signal := IMF1 + residue]
  Before <- copy(IMF)
  Source <- data.table(Key, t = t, AT.H1 = IMF$signal, VT.H1 = 0, DT.H1 = 0)
  Target <- data.table(Tn = c(0, 1, 2), Sa.low = 1000, Sa.mean = 2000, Sa.high = 3000)
  fit <- get("fitModalFactor", Namespace)
  OUT <- fit(IMF, target = Target, source = Source, key = Key, OCID = "H1", xi = 0.05,
    fmax = 25, kNyq = 4, signalControl = Control, factorMin = 0.001, factorMax = Inf,
    criterion = list(rule = "energyFraction", minimum = 0, action = "keep"),
    unitsSource = "mm", unitsTarget = "mm")
  stopifnot(nrow(OUT$coeff[IMF == "residue"]) == 1L,
    isTRUE(all.equal(OUT$coeff[IMF == "residue", b0], 1)), isTRUE(all.equal(OUT$coeff[IMF == "residue", b], 1)),
    isTRUE(all.equal(OUT$coeff[IMF == "residue", ratio], 1)),
    isTRUE(all.equal(OUT$coeff[IMF == "residue", stageFactor], OUT$coeff[IMF == "residue", energyFactor])),
    # The fit must preserve the supplied modes, including their exact samples.
    identical(IMF, Before))
  Signal <- IMF$IMF1 * OUT$coeff[IMF == "IMF1", b] + IMF$residue
  Energy <- get(".matchEnergyFactor", Namespace)(source = IMF$signal, rebuilt = Signal)
  TSL <- data.table(Key, OCID = "H1", ID = "AT", t = t, s = Signal * Energy)
  TSL <- do.call(get("buildTSL", Namespace), c(list(TSL = TSL, by = names(Key),
    unitsTarget = "mm", fmax = 25, kNyq = 4, isRaw = FALSE), Control))
  TSW <- get("widenTSL", Namespace)(TSL, by = names(Key))
  PSA <- get(".matchSpectra", Namespace)(TSW, target = Target,
    params = list(keys = names(Key), OCID = "H1", xi = 0.05))$PSA
  stopifnot(isTRUE(all.equal(OUT$PSA[kind == "final", Sa], PSA$Sa)),
    isTRUE(all.equal(OUT$TSW$AT.H1, TSW$AT.H1)),
    isTRUE(all.equal(OUT$coeff[IMF == "residue", energyFactor], Energy)))
  Omitted <- fit(IMF, target = Target, source = Source, key = Key, OCID = "H1", xi = 0.05,
    fmax = 25, kNyq = 4, signalControl = Control, factorMin = 0.001, factorMax = Inf,
    criterion = list(rule = "energyFraction", minimum = 0, action = "keep"),
    unitsSource = "mm", unitsTarget = "mm", includeResidue = FALSE)
  stopifnot(!"residue" %in% Omitted$coeff$IMF)
  message("PASS: residue coefficient is one, final PSA/TSW equal explicit unscaled addition with complete energy restoration; FALSE omits it.")
}
.checkMatchCriterion()
