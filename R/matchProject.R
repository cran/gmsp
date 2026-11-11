# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.matchSource <- function(config, paths, root) {
  KeyColumns <- c("RecordID", "OwnerID", "EventID", "StationID")
  if (is.null(paths)) {
    Path <- .resolvePath(.jsonRequire(config, c("path", "source")), root = root)
    Selection <- .recordFread(file.path(Path, "data", "manifest.csv"))
    .requireColumns(Selection, c(KeyColumns, "status"), "manifest.csv")
    Selection <- unique(Selection[status == "done", KeyColumns, with = FALSE])
    Records <- .readCharacterVector(.jsonGet(config, c("candidate", "recordID")),
                                    "candidate.recordID", allowNull = TRUE)
    if (length(Records)) Selection <- Selection[RecordID %in% Records]
    Files <- c(TSW = file.path(Path, "data", "TSW.csv"),
      IMF = file.path(Path, "data", "IMF.csv"),
      META = file.path(Path, "metadata", "MasterIndex.csv"),
      OCIDMAP = file.path(Path, "metadata", "OCIDMAP.csv"))
    Source <- lapply(Files, function(x) .recordFread(x)[RecordID %in% Selection$RecordID])
  }
  if (!is.null(paths)) {
    Selection <- .matchSelectionDB(config, paths = paths, root = root)
    Contract <- .recordReadProcessContract(Selection$ProcessID[1L], paths = paths, required = TRUE)
    Output <- c("TSW", "IMW", "IMF")
    if ("ComponentMapTable" %in% Contract$process) Output <- c(Output, "ComponentMapTable")
    Package <- .readRecordProducts(keys = Selection, paths = paths, output = Output)
    Source <- list(TSW = Package$TSW, IMF = Package$IMF, META = copy(attr(Package, "index")))
    Source$OCIDMAP <- Package$ComponentMapTable
    if (is.null(Source$OCIDMAP)) {
      Source$OCIDMAP <- unique(Package$IMW[, c(KeyColumns, "OCID"), with = FALSE])
      Source$OCIDMAP[, theta := NA_real_]
    }
    if (is.null(Source$META) || !nrow(Source$META)) {
      Source$META <- unique(Package$IMW[, c(KeyColumns, "OCID"), with = FALSE])
    }
    for (x in c("META", "OCIDMAP")) {
      if (!"DIR" %in% names(Source[[x]]) && "OCID" %in% names(Source[[x]])) {
        Source[[x]][, DIR := as.character(OCID)]
      }
    }
  }
  if (!nrow(Selection)) .stopUsage("Candidate selection produced zero records.", operation = "matchProject")
  .checkTableKeys(Selection, by = character(), columns = KeyColumns, keys = "RecordID", operation = "matchProject")
  for (x in names(Source)) {
    .requireColumns(Source[[x]], KeyColumns, paste("source", x))
    Keys <- unique(Source[[x]][, KeyColumns, with = FALSE])
    if (nrow(Keys) != nrow(Selection) ||
        nrow(fsetdiff(Keys, Selection[, KeyColumns, with = FALSE])) ||
        nrow(fsetdiff(Selection[, KeyColumns, with = FALSE], Keys))) {
      .stopUsage("Source ", x, " does not match the complete selected keys.", operation = "matchProject")
    }
  }
  list(selection = Selection, source = Source)
}

.matchCompleteTSW <- function(source, final, params) {
  KeyColumns <- c("RecordID", "OwnerID", "EventID", "StationID")
  Columns <- as.vector(t(outer(c("AT", "VT", "DT"), c("H1", "H2", "UP"), paste, sep = ".")))
  .requireColumns(source, c(KeyColumns, "t", Columns), "source TSW")
  Keep <- paste(c("AT", "VT", "DT"), params$ocid, sep = ".")
  .requireColumns(final, c(KeyColumns, "t", Keep), "match TSW")
  if (nrow(source) < nrow(final)) {
    .stopUsage("Source TS is shorter than matched TS.", operation = "matchProject")
  }
  OUT <- copy(source[seq_len(nrow(final)), c(KeyColumns, "t", Columns), with = FALSE])
  Factor <- .resolveUnitFactor(unitsSource = params$units$source, unitsTarget = params$units$target, ID = "DT")
  if (Factor != 1) for (x in Columns) set(OUT, j = x, value = OUT[[x]] * Factor)
  OUT[, t := final$t]
  OUT[, (Keep) := final[, Keep, with = FALSE]]
  OUT
}

.matchRecords <- function(selection, target, source, params, parallel, workers, logEvery) {
  .worker <- function(row) {
    OUT <- tryCatch({
      TSW <- source$TSW[RecordID == row$RecordID[1L]][order(t)]
      IMF <- source$IMF[RecordID == row$RecordID[1L]]
      OUT <- fitModalFactor(IMF, target = target, source = TSW, key = row,
        OCID = params$ocid, xi = params$xi, fmax = params$fmax, kNyq = params$kNyq,
        signalControl = params$signalControl, factorMin = params$bMin, factorMax = params$bMax,
        criterion = params$criterion, unitsSource = params$units$source,
        unitsTarget = params$units$target, includeResidue = params$includeResidue)
      OUT$TSW <- .matchCompleteTSW(TSW, final = OUT$TSW[order(t)], params = params)
      OUT
    }, error = identity)
    if (inherits(OUT, "error")) {
      return(list(cause = OUT, metrics = data.table(row, status = "error",
        convergence = NA_integer_, message = NA_character_, error = conditionMessage(OUT))))
    }
    OUT
  }
  LIST <- NULL
  if (parallel && workers > 1L) {
    LIST <- .runRecordsParallel(selection, processRecord = .worker,
      workers = workers, logEvery = logEvery, label = "match")
  }
  if (!parallel || workers <= 1L) LIST <- .runRecordsSequential(selection, processRecord = .worker, label = "match")
  OK <- vapply(LIST, function(x) is.list(x) && !is.null(x$TSW), logical(1L))
  OUT <- lapply(c("TSW", "PSA", "coeff", "eval"), function(x) {
    rbindlist(lapply(LIST[OK], `[[`, x), use.names = TRUE)
  })
  names(OUT) <- c("TSW", "PSA", "coeff", "eval")
  OUT$metrics <- rbindlist(lapply(LIST, `[[`, "metrics"), use.names = TRUE, fill = TRUE)
  OUT$failures <- lapply(LIST[!OK], `[[`, "cause")
  OUT
}

.matchProducts <- function(results, target, params) {
  KeyColumns <- c("RecordID", "OwnerID", "EventID", "StationID")
  Records <- results$metrics[status == "done", RecordID]
  Stage <- fitRecordFactor(results$PSA, target = target, records = Records,
    factorMin = params$aMin, factorMax = params$aMax,
    bandWeight = params$bandWeight, deficitWeight = params$deficitWeight)
  PSA <- copy(results$PSA)
  Shaping <- dcast(PSA[kind %in% c("source", "final") & Tn > 0], RecordID + Tn ~ kind, value.var = "Sa")
  Shaping <- Shaping[is.finite(source) & source > 0 & is.finite(final)]
  if (!nrow(Shaping)) .stopUsage("Shaping QA found zero usable spectra rows.", operation = "matchProject")
  Shaping[, ratio := final / source]
  Shaping <- Shaping[, .(minRatio = min(ratio), TnAtMin = Tn[which.min(ratio)],
    medianRatio = stats::median(ratio), maxRatio = max(ratio), nTn = .N), by = RecordID]
  PSA[Stage$scale, on = "RecordID", scaleFactor := i.scaleFactor]
  if (any(PSA[kind == "final", !is.finite(scaleFactor)])) {
    .stopUsage("Missing final spectral scale factors.", operation = "matchProject")
  }
  PSA[kind == "final", Sa := Sa * scaleFactor]
  PSA[, scaleFactor := NULL]
  TSL <- lengthenTSW(results$TSW, by = KeyColumns)
  TSL[Stage$scale, on = "RecordID", scaleFactor := i.scaleFactor]
  if (any(!is.finite(TSL$scaleFactor))) .stopUsage("Missing signal scale factors.", operation = "matchProject")
  TSL[, `:=`(s = s * scaleFactor, scaleFactor = NULL)]
  Products <- .signalProducts(TSL, units = params$units$target)
  PSW <- .buildRecordPSW(Products$TSW, XI = params$xi, Tn = target[Tn > 0, Tn],
    D50 = TRUE, D100 = TRUE, nTheta = params$nTheta)
  if (length(params$xi) == 1L && "xi" %in% names(PSW)) PSW[, xi := NULL]
  Compatibility <- assessCompatibility(PSW, target = target, scale = Stage$scale,
    OCID = params$ocid, factorMin = params$aMin, factorMax = params$aMax)
  Metrics <- copy(results$metrics)
  Metrics[Stage$scale, on = "RecordID", scaleFactor := i.scaleFactor]
  c(Products, list(PSA = PSA, PSW = PSW, scale = Stage$scale, stage = Stage$stage,
    shaping = Shaping, metrics = Metrics, compatibility = Compatibility))
}

.matchModalProducts <- function(source, products, coeff, target, params) {
  Records <- products$IMW[OCID == params$ocid & is.finite(PGA), .(RecordID, OCID, PGA)][order(-PGA, RecordID)]
  if (!nrow(Records)) .stopUsage("No final PGA rows for modal products.", operation = "matchProject")
  Records[, rank := .I]
  Factors <- coeff[products$scale, on = "RecordID", nomatch = 0L]
  Factors[, k := stageFactor * scaleFactor]
  COL <- paste("AT", params$ocid, sep = ".")
  Signal <- products$TSW[, .(RecordID, OCID = params$ocid, t, ID = "signal", AT = get(COL))]
  Modes <- unique(Factors$IMF)
  .requireColumns(source$IMF, Modes, "source IMF")
  Modal <- melt(source$IMF[RecordID %in% Records$RecordID & OCID == params$ocid,
    c("RecordID", "OCID", "t", Modes), with = FALSE],
    id.vars = c("RecordID", "OCID", "t"), measure.vars = Modes,
    variable.name = "ID", variable.factor = FALSE, value.name = "AT")
  Modal <- Modal[Factors[, .(RecordID, ID = IMF, k)], on = c("RecordID", "ID"), nomatch = 0L]
  Factor <- .resolveUnitFactor(unitsSource = params$units$source, unitsTarget = params$units$target, ID = "AT")
  Modal[, `:=`(AT = AT * k * Factor, k = NULL)]
  ITS <- rbindlist(list(Signal, Modal), use.names = TRUE)
  ITS <- ITS[Records[, .(RecordID, rank)], on = "RecordID", nomatch = 0L]
  ITS[, modeOrder := match(ID, c("signal", grep("^IMF[0-9]+$", names(source$IMF), value = TRUE), "residue"))]
  setorderv(ITS, c("rank", "OCID", "modeOrder", "ID", "t"))
  ITS[, c("rank", "modeOrder") := NULL]
  Groups <- unique(ITS[, .(RecordID, OCID, ID)])
  IPSA <- rbindlist(lapply(seq_len(nrow(Groups)), function(i) {
    DT <- ITS[Groups[i], on = c("RecordID", "OCID", "ID")]
    TSL <- DT[, .(OCID, ID = "AT", t, s = AT)]
    TSL <- do.call(buildTSL, c(list(TSL = TSL, by = character(), unitsTarget = params$units$target,
      fmax = params$fmax, kNyq = params$kNyq, isRaw = FALSE), params$signalControl))
    TSW <- widenTSL(TSL, by = character())
    PSA <- .matchSpectra(TSW, target = target,
      params = list(keys = character(), OCID = params$ocid, xi = params$xi))$PSA
    data.table(RecordID = Groups$RecordID[i], OCID = Groups$OCID[i],
      Tn = PSA$Tn, ID = Groups$ID[i], PSA = PSA$Sa)
  }), use.names = TRUE)
  IPSA[, ID := factor(ID, levels = unique(ITS$ID))]
  setorder(IPSA, RecordID, OCID, ID, Tn)
  IPSA[, ID := as.character(ID)]
  list(ITS = ITS[], IPSA = IPSA[])
}

.matchUpdateMeta <- function(data, intensity) {
  OUT <- copy(data)
  KeyColumns <- c("RecordID", "OwnerID", "EventID", "StationID")
  if (!"DIR" %in% names(OUT)) return(unique(OUT, by = KeyColumns))
  Columns <- setdiff(intersect(names(intensity), names(OUT)), c(KeyColumns, "OCID"))
  for (x in Columns) {
    if (is.numeric(intensity[[x]]) && is.numeric(OUT[[x]]) && !is.double(OUT[[x]])) {
      set(OUT, j = x, value = as.numeric(OUT[[x]]))
    }
  }
  if (length(Columns)) {
    OUT[intensity, on = c(KeyColumns, DIR = "OCID"), (Columns) := mget(paste0("i.", Columns))]
  }
  OUT[]
}

matchProject <- function(file = file.path("gmsp", "match.json"), root = getwd()) {
  root <- .projectRoot(root)
  FILE.config <- .resolvePath(file, root = root)
  tryCatch({
    Config <- .readProjectConfig(FILE.config)
    Identifier <- .targetText(.jsonRequire(Config, "id"), label = "id")
    Params <- .matchParams(Config)
    PATH.out <- .resolvePath(.jsonRequire(Config, c("path", "out")), root = root)
    Override <- .readScalarLogical(.jsonGet(Config, "override", default = FALSE), "override")
    if ((file.exists(PATH.out) && !dir.exists(PATH.out)) ||
        (!Override && length(list.files(PATH.out, all.files = TRUE, no.. = TRUE)))) {
      .stopUsage(PATH.out, " exists; override=true permits replacing known files.", operation = "matchProject")
    }
    Parallel <- .readScalarLogical(.jsonGet(Config, "parallel", default = FALSE), "parallel")
    Workers <- .readWorkers(.jsonGet(Config, "workers"), parallel = Parallel)
    LogEvery <- .jsonGet(Config, "logEvery", default = 30)
    if (!is.numeric(LogEvery) || length(LogEvery) != 1L || !is.finite(LogEvery) || LogEvery < 1 || LogEvery != floor(LogEvery)) {
      .stopUsage("logEvery must be a positive integer.", operation = "matchProject")
    }
    if (.jsonHas(Config, c("plot", "audit")) || .jsonHas(Config, c("plot", "standard"))) {
      .warnSignal("The keys plot.audit and plot.standard are ignored since gmsp 1.0.0 because ",
        "figures are produced in the reports scaffold.", operation = "matchProject", path = FILE.config)
    }
    Probabilities <- .jsonGet(Config, c("summary", "probabilities"), default = NULL)
    if (is.list(Probabilities) && length(Probabilities) && is.null(names(Probabilities)) &&
        all(vapply(Probabilities, function(x) is.numeric(x) && length(x) == 1L, logical(1L)))) {
      Probabilities <- unlist(Probabilities, use.names = FALSE)
    }
    if (!is.null(Probabilities) && (!is.numeric(Probabilities) || !length(Probabilities) ||
        any(!is.finite(Probabilities)) || any(Probabilities < 0 | Probabilities > 1) ||
        anyDuplicated(Probabilities))) {
      .stopUsage("summary.probabilities requires distinct finite numbers in [0,1].", operation = "matchProject")
    }
    Target <- .projectTarget(Config, units = Params$units$psa, root = root, envelope = TRUE)
    Criterion <- .matchCriterion(Params$criterion, target = Target, fmax = Params$fmax)
    Params$criterion <- Criterion$criterion
    DBMode <- any(vapply(list("processID", c("path", "database"), c("path", "records")),
      function(x) .jsonHas(Config, x), logical(1L)))
    Paths <- NULL
    if (DBMode) Paths <- .matchDBPaths(Config, root = root)
    Input <- .matchSource(Config, paths = Paths, root = root)
    Selection <- Input$selection
    Source <- Input$source
    Results <- .matchRecords(Selection, target = Target, source = Source, params = Params,
      parallel = Parallel, workers = Workers, logEvery = LogEvery)
    Products <- tryCatch(.matchProducts(Results, target = Target, params = Params), error = identity)
    if (inherits(Products, "error")) {
      if (!length(Results$failures)) stop(Products)
      tryCatch({
        .mkdir(file.path(PATH.out, "match"))
        fwrite(Results$metrics, file.path(PATH.out, "match", "metrics.csv"), eol = "\n")
      }, error = function(e) .stopPartialPublication(conditionMessage(e), operation = "matchProject",
        path = PATH.out, failures = c(Results$failures, list(Products, e)), result = Results))
      .stopPartialPublication("Match has incomplete records; stage B could not finish.", operation = "matchProject",
        path = PATH.out, failures = c(Results$failures, list(Products)), result = Results)
    }
    Modal <- .matchModalProducts(Source, products = Products, coeff = Results$coeff,
                                 target = Target, params = Params)
    Mean <- .summarizeSuite(Products$PSA[kind == "final", .(RecordID, Tn, value = Sa)],
                            by = "Tn", probabilities = Probabilities)
    Fields <- character()
    if (!is.null(Probabilities)) {
      Fields <- paste0("q", vapply(Probabilities, function(p) {
        x <- as.character(jsonlite::toJSON(p, auto_unbox = TRUE, digits = NA))
        if (grepl("e", x, fixed = TRUE)) x <- format(p, scientific = FALSE, trim = TRUE, digits = 17L)
        x
      }, character(1L)))
      if (anyDuplicated(Fields)) .stopUsage("Summary probability column names are not distinct.", operation = "matchProject")
      Mean[, field := Fields[match(p, Probabilities)]]
      Mean <- dcast(Mean, Tn + mean + recordCount ~ field, value.var = "value")
    }
    setnames(Mean, "mean", "Sa")
    setcolorder(Mean, c("Tn", "Sa", "recordCount", Fields))
    Metadata <- .matchUpdateMeta(Source$META[RecordID %in% Products$TSW$RecordID], intensity = Products$IMW)
    KeyColumns <- c("RecordID", "OwnerID", "EventID", "StationID")
    Manifest <- unique(Selection[RecordID %in% Products$TSW$RecordID, KeyColumns, with = FALSE])
    Manifest[, `:=`(status = "done", path = file.path(PATH.out, "data"))]
    Tables <- list("data/TSW.csv" = Products$TSW, "data/PSW.csv" = Products$PSW,
      "data/IMW.csv" = Products$IMW, "data/ITS.csv" = Modal$ITS, "data/IPSA.csv" = Modal$IPSA,
      "metadata/selection.csv" = Selection, "metadata/MasterIndex.csv" = Metadata,
      "metadata/OCIDMAP.csv" = Source$OCIDMAP[RecordID %in% Products$TSW$RecordID],
      "match/target.csv" = Target, "match/factors.csv" = Results$coeff,
      "match/scale.csv" = Products$scale, "match/stage.csv" = Products$stage,
      "match/shaping.csv" = Products$shaping, "match/recordCompatibility.csv" = Products$compatibility$record,
      "match/compatibility.csv" = Products$compatibility$summary, "match/metrics.csv" = Products$metrics,
      "match/eval.csv" = Results$eval, "match/mean.csv" = Mean)
    Written <- character()
    tryCatch({
      for (x in c("data", "metadata", "match")) .mkdir(file.path(PATH.out, x))
      FILE <- file.path(PATH.out, "data", "manifest.csv")
      if (file.exists(FILE) && unlink(FILE) != 0L) stop("Could not replace manifest: ", FILE)
      for (x in names(Tables)) {
        FILE <- file.path(PATH.out, x)
        fwrite(Tables[[x]], FILE, eol = "\n")
        Written <- c(Written, FILE)
      }
      Intensity <- .recordFread(file.path(PATH.out, "data", "IMW.csv"))
      Columns <- setdiff(names(Products$IMW), c(KeyColumns, "OCID"))
      Intensity[, (Columns) := lapply(.SD, as.double), .SDcols = Columns]
      Intensity <- melt(Intensity, id.vars = c(KeyColumns, "OCID"), measure.vars = Columns,
        variable.name = "IM", variable.factor = FALSE, value.name = "value")
      Intensity <- .summarizeSuite(Intensity, by = c("OCID", "IM"), probabilities = Probabilities)
      setcolorder(Intensity, c("OCID", "IM", "mean", "recordCount",
        if (!is.null(Probabilities)) c("p", "value")))
      FILE <- file.path(PATH.out, "match", "intensitySummary.csv")
      fwrite(Intensity, FILE, eol = "\n")
      Written <- c(Written, FILE)
      FILE <- file.path(PATH.out, "metadata", "criterion.json")
      .writeJSON(Criterion$metadata, FILE, auto_unbox = TRUE, pretty = TRUE,
                 null = "null", digits = NA)
      Written <- c(Written, FILE)
      Info <- list(id = Identifier,
        processID = .jsonGet(Config, "processID", default = NA_character_), sourceMode = "folder",
        path = .jsonGet(Config, "path"))
      if (DBMode) Info$sourceMode <- "database"
      FILE <- file.path(PATH.out, "metadata", "source.json")
      .writeJSON(Info, FILE, auto_unbox = TRUE, pretty = TRUE)
      Written <- c(Written, FILE)
      FILE <- file.path(PATH.out, "metadata", "runMatch.json")
      if (!file.copy(FILE.config, FILE, overwrite = TRUE)) stop("Could not copy contract: ", FILE)
      Written <- c(Written, FILE)
      FILE <- file.path(PATH.out, "data", "manifest.csv")
      fwrite(Manifest, FILE, eol = "\n")
      Written <- c(Written, FILE)
    }, error = function(e) .stopPartialPublication(conditionMessage(e), operation = "matchProject",
      path = PATH.out, failures = c(Results$failures, list(e)), result = Written))
    if (length(Results$failures)) {
      .stopPartialPublication("Match published completed records with failed records.", operation = "matchProject",
        path = PATH.out, failures = Results$failures, result = Written)
    }
    .projectMessage("Match published.", operation = "matchProject", phase = "complete", path = PATH.out)
    invisible(0L)
  }, error = function(e) .projectFailure(e, operation = "matchProject", path = FILE.config))
}
