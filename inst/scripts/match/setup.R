# scripts/match/setup.R -- match pipeline orchestration helpers.
# The algorithm core (modal shaping, record scaling, compatibility)
# lives in the gmsp exported API: fitModalFactor(), fitRecordFactor(),
# matchCompatibility().

KEY.record <- c("RecordID", "OwnerID", "EventID", "StationID")
IDS.signal <- c("AT", "VT", "DT")
IDS.cumulative <- c("AI", "CAV", "CAV5")
IDS.ts <- c(IDS.signal, IDS.cumulative)
IDS.ps <- c("PSA", "PSV", "SD")
DIRS.ts <- c("H1", "H2", "UP")
DIRS.ps <- c(DIRS.ts, "D50", "D100")
G <- 9806.65

.matchConfigPath <- function(path) {
  FILE <- path.expand(as.character(path))
  if (file.exists(FILE)) return(normalizePath(FILE, winslash = "/"))
  .resolvePath(path)
}

.matchPath <- function(path) {
  FILE <- path.expand(as.character(path))
  if (grepl("^(/|~)", FILE)) return(FILE)
  if (file.exists(FILE)) return(normalizePath(FILE, winslash = "/"))
  .resolvePath(FILE)
}

.matchReadTable <- function(path) {
  FILE <- .matchPath(path)
  if (!file.exists(FILE)) stop(sprintf("Missing table: %s", FILE),
                               call. = FALSE)
  if (grepl("[.]rds$", FILE, ignore.case = TRUE)) {
    return(as.data.table(readRDS(FILE)))
  }
  if (grepl("[.]csv$", FILE, ignore.case = TRUE)) return(.recordFread(FILE))
  stop(sprintf("Unsupported table extension: %s", FILE), call. = FALSE)
}

.matchConvertSa <- function(Sa, from, to) {
  From <- tolower(as.character(from))
  To <- tolower(as.character(to))
  if (identical(From, To)) return(Sa)
  if (From == "g" && To %chin% c("mm/s2", "mm/s^2")) return(Sa * G)
  if (From %chin% c("mm/s2", "mm/s^2") && To == "g") return(Sa / G)
  stop(sprintf("Unsupported Sa unit conversion: %s -> %s", from, to),
       call. = FALSE)
}

.matchP <- function(x) {
  x <- as.character(x)
  Map <- c(p05 = "0.05", p5 = "0.05", p10 = "0.10", p16 = "0.16",
           p50 = "0.50", p84 = "0.84", p90 = "0.90", p95 = "0.95",
           mean = "mean")
  ifelse(x %chin% names(Map), Map[x], x)
}

.matchParams <- function(config) {
  Shape <- .jsonRequire(config, c("shape"))
  Units <- .jsonRequire(config, c("units"))
  IMF <- .jsonGet(config, c("imf"), default = list())
  ModalFactor <- .jsonGet(Shape, c("modalFactor"), default = list())
  RecordFactor <- .jsonGet(Shape, c("recordFactor"), default = list())
  if (!is.list(IMF)) stop("imf must be a JSON object.", call. = FALSE)
  if (!is.list(ModalFactor)) {
    stop("shape.modalFactor must be a JSON object.", call. = FALSE)
  }
  if (!is.list(RecordFactor)) {
    stop("shape.recordFactor must be a JSON object.", call. = FALSE)
  }
  for (Name in c("source", "target", "psa")) {
    if (is.null(Units[[Name]]) || !nzchar(as.character(Units[[Name]]))) {
      stop(sprintf("units.%s must be defined.", Name), call. = FALSE)
    }
  }
  ModalMax <- .jsonGet(ModalFactor, c("max"), default = Inf)
  if (is.null(ModalMax) || !length(ModalMax)) ModalMax <- Inf

  OUT <- list(
    ocid = as.character(.jsonGet(config, c("ocid"), default = "H1")),
    xi = as.numeric(.jsonGet(config, c("xi"), default = 0.05)),
    fmax = as.numeric(.jsonGet(config, c("fmax"), default = 25)),
    kNyq = as.numeric(.jsonGet(config, c("kNyq"), default = 4)),
    bMin = as.numeric(.jsonRequire(ModalFactor, c("min"),
                                   path = "shape.modalFactor")),
    bMax = as.numeric(ModalMax),
    aMin = as.numeric(.jsonGet(RecordFactor, c("min"), default = 0)),
    aMax = as.numeric(.jsonGet(RecordFactor, c("max"), default = Inf)),
    bandWeight = as.numeric(.jsonGet(Shape, c("bandWeight"), default = 2)),
    deficitWeight = as.numeric(.jsonGet(Shape, c("deficitWeight"),
                                        default = 1)),
    imfRemove = unlist(.jsonGet(IMF, c("remove"), default = character()),
                       use.names = FALSE),
    includeResidue = .readScalarLogical(
      .jsonGet(IMF, c("includeResidue"),
               default = .jsonGet(Shape, c("includeResidue"),
                                  default = TRUE)),
      "imf.includeResidue"),
    units = Units)

  if (length(OUT$ocid) != 1L || !(OUT$ocid %chin% DIRS.ts)) {
    stop("ocid must be one of H1, H2, UP.", call. = FALSE)
  }
  if (!is.finite(OUT$xi) || OUT$xi <= 0) {
    stop("xi must be finite and positive.", call. = FALSE)
  }
  if (!is.finite(OUT$fmax) || OUT$fmax <= 0) {
    stop("fmax must be finite and positive.", call. = FALSE)
  }
  if (!is.finite(OUT$kNyq) || OUT$kNyq <= 0) {
    stop("kNyq must be finite and positive.", call. = FALSE)
  }
  if (!is.null(.jsonGet(Shape, c("bMin"), default = NULL))) {
    stop("shape.bMin is not part of the current runMatch contract.",
         call. = FALSE)
  }
  if (!is.null(.jsonGet(Shape, c("initial"), default = NULL))) {
    stop("shape.initial is not part of the current runMatch contract.",
         call. = FALSE)
  }
  if (!is.null(.jsonGet(Shape, c("modalRatio"), default = NULL))) {
    stop("shape.modalRatio is not part of the current runMatch V3 contract.",
         call. = FALSE)
  }
  if (!is.null(.jsonGet(Shape, c("maxIt"), default = NULL)) ||
      !is.null(.jsonGet(Shape, c("lambdaBand"), default = NULL)) ||
      !is.null(.jsonGet(Shape, c("lambdaMove"), default = NULL))) {
    stop("shape.maxIt, shape.lambdaBand, and shape.lambdaMove are internal constants.",
         call. = FALSE)
  }
  if (!is.finite(OUT$bMin) || OUT$bMin <= 0 || OUT$bMin > 1 ||
      is.na(OUT$bMax) || OUT$bMax < OUT$bMin ||
      (is.finite(OUT$bMax) && OUT$bMax < 1)) {
    stop("shape.modalFactor.min must be finite, positive, and <= 1; optional max must contain 1.",
         call. = FALSE)
  }
  if (is.na(OUT$aMin) || OUT$aMin < 0 || is.na(OUT$aMax) ||
      OUT$aMax < OUT$aMin) {
    stop("shape.recordFactor.min/max must define a non-negative interval.",
         call. = FALSE)
  }
  if (!is.finite(OUT$bandWeight) || OUT$bandWeight < 0) {
    stop("shape.bandWeight must be finite and non-negative.",
         call. = FALSE)
  }
  if (!is.finite(OUT$deficitWeight) || OUT$deficitWeight <= 0) {
    stop("shape.deficitWeight must be finite and positive.",
         call. = FALSE)
  }
  OUT
}

.matchTnRange <- function(config) {
  TnMin <- .jsonGet(config, c("Tn", "min"), default = NULL)
  TnMax <- .jsonGet(config, c("Tn", "max"), default = NULL)
  if (is.null(TnMin) && is.null(TnMax)) return(NULL)
  OUT <- c(as.numeric(if (is.null(TnMin)) -Inf else TnMin),
           as.numeric(if (is.null(TnMax))  Inf else TnMax))
  if (length(OUT) != 2L || any(is.na(OUT)) || OUT[1L] > OUT[2L]) {
    stop("Tn.min/Tn.max must define one valid interval.", call. = FALSE)
  }
  OUT
}

.matchReadTargetBand <- function(config, params) {
  Target <- .matchReadTable(.jsonRequire(config, c("path", "target")))
  Value <- as.character(.jsonGet(config, c("target", "value"),
                                 default = "SaF"))
  .requireColumns(Target, c("ID", "TR", "Vs30", "p", "Tn", Value),
                  "target table")

  TargetID <- as.character(.jsonRequire(config, c("target", "ID")))
  TargetTR <- as.numeric(.jsonRequire(config, c("target", "TR")))
  TargetVs30 <- as.numeric(.jsonRequire(config, c("target", "Vs30")))
  SiteID <- as.character(.jsonGet(config, c("target", "siteID"),
                                  default = ""))
  TargetUnits <- as.character(.jsonGet(config, c("target", "units"),
                                       default = "g"))
  TargetP <- .matchP(as.character(.jsonGet(config, c("target", "p"),
                                           default = "mean")))
  EnvelopeID <- as.character(.jsonGet(config, c("target", "envelope", "ID"),
                                      default = TargetID))
  LowP <- .matchP(as.character(.jsonRequire(config, c("target", "envelope",
                                                      "lower"))))
  HighP <- .matchP(as.character(.jsonRequire(config, c("target", "envelope",
                                                       "upper"))))

  Mean <- Target[
    ID %chin% TargetID &
      TR %in% TargetTR &
      Vs30 %in% TargetVs30 &
      p %chin% TargetP]
  Band <- Target[
    ID %chin% EnvelopeID &
      TR %in% TargetTR &
      Vs30 %in% TargetVs30 &
      p %chin% c(LowP, HighP)]
  if (nzchar(SiteID) && "siteID" %chin% names(Mean)) {
    Mean <- Mean[siteID %chin% SiteID]
  }
  if (nzchar(SiteID) && "siteID" %chin% names(Band)) {
    Band <- Band[siteID %chin% SiteID]
  }
  if (!nrow(Mean)) stop("target mean filters produced zero rows.",
                        call. = FALSE)
  if (!nrow(Band)) stop("target envelope filters produced zero rows.",
                        call. = FALSE)

  OUT <- rbindlist(list(Mean, Band), use.names = TRUE)
  OUT <- OUT[, .(Sa = max(get(Value), na.rm = TRUE)), by = .(Tn, p)]
  OUT[, Sa := .matchConvertSa(Sa, from = TargetUnits,
                              to = params$units$psa)]
  OUT <- dcast(OUT, Tn ~ p, value.var = "Sa")
  .requireColumns(OUT, c(LowP, TargetP, HighP), "target band")
  setnames(OUT, c(LowP, TargetP, HighP),
           c("Sa.low", "Sa.mean", "Sa.high"))

  Range <- .matchTnRange(config)
  if (!is.null(Range)) {
    OUT <- OUT[Tn == 0 | (Tn > 0 & Tn >= Range[1L] & Tn <= Range[2L])]
  }
  if (!nrow(OUT) || !any(OUT$Tn == 0) || !any(OUT$Tn > 0)) {
    stop("target band must include PGA and positive periods.", call. = FALSE)
  }
  setorder(OUT, Tn)
  OUT[]
}

.matchSelection <- function(path, config) {
  FILE <- file.path(path, "data", "manifest.csv")
  if (!file.exists(FILE)) stop(sprintf("Missing manifest: %s", FILE),
                               call. = FALSE)
  OUT <- .recordFread(FILE)
  .requireColumns(OUT, c(KEY.record, "status"), "manifest.csv")
  OUT <- unique(OUT[status == "done", KEY.record, with = FALSE],
                by = KEY.record)
  Records <- .readCharacterVector(.jsonGet(config, c("candidate",
                                                     "recordID"),
                                           default = NULL),
                                  "candidate.recordID", allowNull = TRUE)
  if (length(Records)) OUT <- OUT[RecordID %chin% Records]
  if (!nrow(OUT)) stop("candidate selection produced zero records.",
                       call. = FALSE)
  OUT[]
}

.matchDBPaths <- function(config) {
  PATH.database <- .jsonGet(config, c("path", "database"), default = NULL)
  PATH.records <- .jsonGet(config, c("path", "records"), default = NULL)
  PATH.index <- .jsonGet(config, c("path", "index"), default = NULL)
  PATH.process <- .jsonGet(config, c("path", "process"), default = NULL)
  if (is.null(PATH.database) && !is.null(PATH.records) &&
      identical(basename(path.expand(PATH.records)), "records")) {
    PATH.database <- dirname(path.expand(PATH.records))
  }
  .recordPaths(list(database = PATH.database, records = PATH.records,
                    index = PATH.index, process = PATH.process),
               need = c("records", "index", "process"))
}

.matchSelectionDB <- function(config, paths) {
  ProcessID <- as.character(.jsonRequire(config, c("processID")))
  if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
    stop("processID must be one non-empty string.", call. = FALSE)
  }
  FILE <- .jsonGet(config, c("path", "selection"), default = NULL)
  if (!is.null(FILE)) {
    FILE <- .matchPath(FILE)
    if (dir.exists(FILE)) FILE <- file.path(FILE, "selection.csv")
    OUT <- .recordFread(FILE)
    .requireColumns(OUT, KEY.record, "selection")
    set(OUT, j = "ProcessID", value = ProcessID)
  } else {
    Owners <- .readCharacterVector(.jsonGet(config, c("candidate", "ownerID"),
                                            default = "*"),
                                   "candidate.ownerID")
    OUT <- selectRecords(keys = list(ProcessID = ProcessID, OwnerID = Owners),
                         criteria = list(), singleRecord = FALSE,
                         paths = paths)
  }
  Records <- .readCharacterVector(.jsonGet(config, c("candidate",
                                                     "recordID"),
                                           default = NULL),
                                  "candidate.recordID", allowNull = TRUE)
  if (length(Records)) OUT <- OUT[RecordID %chin% Records]
  if (!nrow(OUT)) stop("candidate selection produced zero records.",
                       call. = FALSE)
  unique(OUT[, c("ProcessID", KEY.record), with = FALSE],
         by = c("ProcessID", KEY.record))[]
}

.matchSource <- function(path, records) {
  FILES <- c(
    TSW = file.path(path, "data", "TSW.csv"),
    IMF = file.path(path, "data", "IMF.csv"),
    MasterIndex = file.path(path, "metadata", "MasterIndex.csv"),
    OCIDMAP = file.path(path, "metadata", "OCIDMAP.csv"))
  Missing <- FILES[!file.exists(FILES)]
  if (length(Missing)) {
    stop(sprintf("Missing source files:\n%s", paste(Missing, collapse = "\n")),
         call. = FALSE)
  }
  list(
    TSW = .recordFread(FILES[["TSW"]])[RecordID %chin% records],
    META = .recordFread(FILES[["MasterIndex"]])[RecordID %chin% records],
    OCIDMAP = .recordFread(FILES[["OCIDMAP"]])[RecordID %chin% records],
    IMF = .recordFread(FILES[["IMF"]])[RecordID %chin% records])
}

.matchOCIDMap <- function(package) {
  if ("ComponentMapTable" %chin% names(package)) {
    OUT <- copy(package$ComponentMapTable)
    if (!"DIR" %chin% names(OUT) && "OCID" %chin% names(OUT)) {
      OUT[, DIR := as.character(OCID)]
    }
    return(OUT[])
  }
  OUT <- unique(package$IMW[, c(KEY.record, "OCID"), with = FALSE],
                by = c(KEY.record, "OCID"))
  OUT[, `:=`(DIR = as.character(OCID), theta = NA_real_)]
  OUT[]
}

.matchMeta <- function(package) {
  OUT <- copy(attr(package, "index"))
  if (!nrow(OUT)) {
    OUT <- unique(package$IMW[, c(KEY.record, "OCID"), with = FALSE],
                  by = c(KEY.record, "OCID"))
  }
  if (!"DIR" %chin% names(OUT) && "OCID" %chin% names(OUT)) {
    OUT[, DIR := as.character(OCID)]
  }
  OUT[]
}

.matchProductPaths <- function(path) {
  OUT <- list(
    data = file.path(path, "data"),
    metadata = file.path(path, "metadata"),
    match = file.path(path, "match"),
    audit = file.path(path, "audit")
  )
  for (DIR in OUT) .ensureDir(DIR)
  OUT
}

.matchSourceInfo <- function(config, dbMode) {
  OUT <- list(
    id = as.character(.jsonRequire(config, c("id"))),
    processID = as.character(.jsonGet(config, c("processID"),
                                      default = NA_character_)),
    sourceMode = if (dbMode) "database" else "folder",
    path = list(
      database = .jsonGet(config, c("path", "database"), default = NULL),
      records = .jsonGet(config, c("path", "records"), default = NULL),
      source = .jsonGet(config, c("path", "source"), default = NULL),
      selection = .jsonGet(config, c("path", "selection"), default = NULL),
      target = .jsonGet(config, c("path", "target"), default = NULL)
    )
  )
  OUT
}

.matchSourceDB <- function(config, paths, selection) {
  Contract <- .recordReadProcessContract(
    processID = as.character(selection$ProcessID[1L]),
    paths = paths,
    required = TRUE)
  Output <- c("TSW", "IMW", "IMF")
  if ("ComponentMapTable" %chin% Contract$process) {
    Output <- c(Output, "ComponentMapTable")
  }
  OUT <- getRecords(keys = selection, paths = paths, output = Output)
  .requireColumns(OUT$TSW, c(KEY.record, "t"), "TSW")
  .requireColumns(OUT$IMW, c(KEY.record, "OCID"), "IMW")
  .requireColumns(OUT$IMF, c(KEY.record, "OCID", "t"), "IMF")
  list(
    TSW = OUT$TSW,
    META = .matchMeta(OUT),
    OCIDMAP = .matchOCIDMap(OUT),
    IMF = OUT$IMF
  )
}

.matchCumulativeTSL <- function(TSL, params) {
  LIST <- list(
    AI = gmsp::TSL2AI(
      .x = TSL,
      units.source = params$units$target,
      units.target = params$units$target,
      output = "long"),
    CAV = gmsp::TSL2CAV(
      .x = TSL,
      units.source = params$units$target,
      units.target = params$units$target,
      output = "long"),
    CAV5 = gmsp::TSL2CAV5(
      .x = TSL,
      units.source = params$units$target,
      units.target = params$units$target,
      output = "long"))

  OUT <- rbindlist(lapply(names(LIST), function(IDValue) {
    DT <- LIST[[IDValue]]
    setnames(DT, "ratio", "s")
    DT[, ID := IDValue]
    DT[]
  }), use.names = TRUE)
  setcolorder(OUT, c(KEY.record, "OCID", "ID", "t", "s"))
  setorderv(OUT, c(KEY.record, "OCID", "ID", "t"), na.last = TRUE)
  OUT[]
}

.matchBuildPSA <- function(TSW, target, params) {
  TSL <- gmsp::TSW2TSL(TSW)
  TSL <- TSL[ID %chin% IDS.signal]
  Tn <- target[Tn > 0, Tn]
  PSW <- gmsp::TSL2PS(.x = TSL, xi = params$xi, Tn = Tn, output = "PSW",
                      D50 = FALSE, D100 = FALSE, nTheta = 180)
  COL <- paste("PSA", params$ocid, sep = ".")
  OUT <- rbindlist(list(
    data.table(Tn = 0,
               Sa = max(abs(TSW[[paste("AT", params$ocid, sep = ".")]]),
                        na.rm = TRUE)),
    PSW[, .(Tn, Sa = get(COL))]
  ), use.names = TRUE)
  OUT[, .(Sa = max(Sa, na.rm = TRUE)), by = Tn][order(Tn)]
}

.matchBuildPSW <- function(TSW, target, params) {
  TSL <- gmsp::TSW2TSL(TSW)
  TSL <- TSL[ID %chin% IDS.signal]
  PSW <- gmsp::TSL2PS(.x = TSL, xi = params$xi,
                      Tn = target[Tn > 0, Tn], output = "PSW",
                      D50 = TRUE, D100 = TRUE, nTheta = 180)
  setcolorder(PSW, c(intersect(c(KEY.record, "xi", "Tn"), names(PSW)),
                     setdiff(names(PSW), c(KEY.record, "xi", "Tn"))))
  PSW[]
}

.matchBuildIMW <- function(TSW, params) {
  TSL <- gmsp::TSW2TSL(TSW)
  TSL <- TSL[ID %chin% IDS.signal]
  gmsp::TSL2IM(.x = TSL,
               units.source = params$units$source,
               units.target = params$units$target,
               output = "IMW")
}

.matchModalRecords <- function(IMW, params) {
  .requireColumns(IMW, c(KEY.record, "OCID", "PGA"), "IMW")
  OUT <- IMW[OCID == params$ocid & is.finite(PGA),
             .(RecordID, OCID, PGA)][order(-PGA, RecordID)]
  if (!nrow(OUT)) {
    stop(sprintf("No final %s PGA rows found for modal products.",
                 params$ocid), call. = FALSE)
  }
  OUT[, rank := .I]
  setcolorder(OUT, c("RecordID", "rank", "PGA", "OCID"))
  OUT[]
}

.matchBuildITS <- function(imf, TSW, coeff, scale, records, params) {
  COL <- paste("AT", params$ocid, sep = ".")
  .requireColumns(TSW, c("RecordID", "t", COL), "final TSW")
  .requireColumns(imf, c("RecordID", "OCID", "t"), "source IMF")
  .requireColumns(coeff, c("RecordID", "IMF", "stageFactor"),
                  "match factors")
  .requireColumns(scale, c("RecordID", "scaleFactor"), "Stage B scale")
  .requireColumns(records, c("RecordID", "rank"), "modal records")

  Records <- records$RecordID
  Factors <- coeff[RecordID %chin% Records,
                   .(RecordID, IMF, stageFactor)]
  Factors <- Factors[scale, on = "RecordID", nomatch = 0L]
  Factors[, k := stageFactor * scaleFactor]
  Modes <- intersect(unique(Factors$IMF), names(imf))
  if (!length(Modes)) {
    stop("No retained IMF modes are available for ITS QA.", call. = FALSE)
  }

  Signal <- TSW[RecordID %chin% Records,
                .(RecordID, OCID = params$ocid, t, ID = "signal",
                  AT = get(COL))]
  Modal <- melt(
    imf[RecordID %chin% Records & OCID == params$ocid,
        c("RecordID", "OCID", "t", Modes), with = FALSE],
    id.vars = c("RecordID", "OCID", "t"),
    measure.vars = Modes,
    variable.name = "ID",
    value.name = "AT")
  Modal[Factors, on = .(RecordID, ID = IMF), AT := AT * i.k]
  OUT <- rbindlist(list(Signal, Modal), use.names = TRUE)
  OUT <- OUT[records[, .(RecordID, rank)], on = "RecordID", nomatch = 0L]
  OUT[, modeOrder := fifelse(ID == "signal", 0L,
                             suppressWarnings(as.integer(sub("^IMF", "", ID))))]
  OUT[ID == "residue", modeOrder := .Machine$integer.max]
  setorder(OUT, rank, OCID, modeOrder, ID, t)
  OUT[, c("rank", "modeOrder") := NULL]
  OUT[, .(RecordID, OCID, t, ID, AT)]
}

.matchBuildIPSA <- function(ITS, target, params) {
  .requireColumns(ITS, c("RecordID", "OCID", "t", "ID", "AT"), "ITS")
  Groups <- unique(ITS[, .(RecordID, OCID, ID)])
  OUT <- rbindlist(lapply(seq_len(nrow(Groups)), function(i) {
    DT <- ITS[Groups[i], on = .(RecordID, OCID, ID)]
    AT <- data.table(t = DT$t)
    AT[, (Groups$OCID[i]) := DT$AT]
    TSW <- gmsp::AT2TS(
      .x = AT,
      units.source = params$units$target,
      units.target = params$units$target,
      Fmax = params$fmax,
      kNyq = params$kNyq,
      output = "TSW",
      isRaw = FALSE,
      audit = FALSE)
    if ("ts" %chin% names(TSW)) setnames(TSW, "ts", "t")
    PSA <- .matchBuildPSA(TSW = TSW, target = target, params = params)
    data.table(RecordID = Groups$RecordID[i],
               OCID = Groups$OCID[i],
               Tn = PSA$Tn,
               ID = Groups$ID[i],
               PSA = PSA$Sa)
  }), use.names = TRUE)
  OUT[order(RecordID, OCID, ID, Tn)]
}

.matchModalQA <- function(imf, TSW, IMW, coeff, scale, target, params) {
  Records <- .matchModalRecords(IMW = IMW, params = params)
  ITS <- .matchBuildITS(imf = imf, TSW = TSW, coeff = coeff, scale = scale,
                        records = Records, params = params)
  IPSA <- .matchBuildIPSA(ITS = ITS, target = target, params = params)
  list(records = Records, ITS = ITS, IPSA = IPSA)
}

.matchScaleTSW <- function(TSW, scale, params) {
  COLS <- CJ(ID = IDS.signal, DIR = DIRS.ts, sorted = FALSE)[
    , paste(ID, DIR, sep = ".")]
  .requireColumns(TSW, c(KEY.record, "t", COLS), "Stage B TSW")
  OUT <- copy(TSW)
  OUT[scale, scaleFactor := i.scaleFactor, on = "RecordID"]
  if (any(!is.finite(OUT$scaleFactor))) {
    stop("Stage B scale factors are missing for at least one TSW row.",
         call. = FALSE)
  }
  for (COL in COLS) set(OUT, j = COL, value = OUT[[COL]] * OUT$scaleFactor)
  OUT[, scaleFactor := NULL]
  TSL <- gmsp::TSW2TSL(OUT[, c(KEY.record, "t", COLS), with = FALSE])
  TSL <- TSL[ID %chin% IDS.signal]
  Cumulative <- .matchCumulativeTSL(TSL = TSL, params = params)
  OUT <- gmsp::TSL2TSW(
    rbindlist(list(TSL, Cumulative), use.names = TRUE),
    ids = IDS.ts)
  if ("ts" %chin% names(OUT)) setnames(OUT, "ts", "t")
  COLS.out <- CJ(ID = IDS.ts, DIR = DIRS.ts, sorted = FALSE)[
    , paste(ID, DIR, sep = ".")]
  .requireColumns(OUT, c(KEY.record, "t", COLS.out), "Stage B final TSW")
  setcolorder(OUT, c(KEY.record, "t", COLS.out))
  OUT[]
}

.matchScalePSA <- function(PSA, scale) {
  OUT <- copy(PSA)
  OUT[scale, scaleFactor := i.scaleFactor, on = "RecordID"]
  if (any(OUT[kind == "final", !is.finite(scaleFactor)])) {
    stop("Stage B scale factors are missing for final PSA curves.",
         call. = FALSE)
  }
  OUT[kind == "final", Sa := Sa * scaleFactor]
  OUT[, scaleFactor := NULL]
  OUT[]
}

.matchShapingQA <- function(PSA) {
  DT <- dcast(PSA[kind %chin% c("source", "final") & Tn > 0],
              RecordID + Tn ~ kind, value.var = "Sa")
  .requireColumns(DT, c("RecordID", "Tn", "source", "final"),
                  "shaping QA table")
  DT <- DT[is.finite(source) & source > 0 & is.finite(final)]
  if (!nrow(DT)) {
    stop("Shaping QA found zero usable spectra rows.", call. = FALSE)
  }
  DT[, ratio := final / source]
  DT[, .(minRatio = min(ratio),
         TnAtMin = Tn[which.min(ratio)],
         medianRatio = median(ratio),
         maxRatio = max(ratio),
         nTn = .N), by = RecordID]
}

.matchWorkers <- function(x, parallel) {
  if (is.null(x) || !length(x)) {
    x <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  x <- suppressWarnings(as.integer(x))[1L]
  if (length(x) != 1L || is.na(x) || x < 1L) {
    x <- suppressWarnings(as.integer(parallel::detectCores(logical = FALSE)))[1L]
  }
  if (length(x) != 1L || is.na(x) || x < 1L) x <- 1L
  if (isTRUE(parallel)) x else 1L
}

.matchCompleteTSW <- function(source, final, params) {
  COLS.signal <- CJ(ID = IDS.signal, DIR = DIRS.ts, sorted = FALSE)[,
    paste(ID, DIR, sep = ".")]
  COLS.ts <- CJ(ID = IDS.ts, DIR = DIRS.ts, sorted = FALSE)[,
    paste(ID, DIR, sep = ".")]
  .requireColumns(source, c(KEY.record, "t", COLS.signal), "source TSW")
  Keep <- paste(IDS.signal, params$ocid, sep = ".")
  .requireColumns(final, c(KEY.record, "t", Keep), "match TSW")
  if (nrow(source) < nrow(final)) {
    stop(sprintf("Source TS shorter than matched TS for %s.",
                 final$RecordID[1L]), call. = FALSE)
  }
  OUT <- source[seq_len(nrow(final)),
                c(KEY.record, "t", COLS.signal), with = FALSE]
  OUT[, t := final$t]
  OUT[, (Keep) := final[, Keep, with = FALSE]]

  TSL <- gmsp::TSW2TSL(OUT)
  TSL <- TSL[ID %chin% IDS.signal]
  Cumulative <- .matchCumulativeTSL(TSL = TSL, params = params)
  OUT <- gmsp::TSL2TSW(
    rbindlist(list(TSL, Cumulative), use.names = TRUE),
    ids = IDS.ts)
  if ("ts" %chin% names(OUT)) setnames(OUT, "ts", "t")
  .requireColumns(OUT, c(KEY.record, "t", COLS.ts), "final TSW")
  setcolorder(OUT, c(KEY.record, "t", COLS.ts))
  OUT[]
}

.matchErrorMetrics <- function(row, error) {
  data.table(
    row[, ..KEY.record],
    status = "error",
    convergence = NA_integer_,
    message = NA_character_,
    error = as.character(error),
    evals = NA_integer_,
    loss.start = NA_real_,
    loss.final = NA_real_,
    energyFactor.start = NA_real_,
    energyFactor.final = NA_real_,
    RMSE.start = NA_real_,
    RMSE.final = NA_real_,
    inside.start = NA_real_,
    inside.final = NA_real_,
    bandLoss.start = NA_real_,
    bandLoss.final = NA_real_,
    maxViolation.start = NA_real_,
    maxViolation.final = NA_real_,
    bMax.start = NA_real_,
    bMax.final = NA_real_,
    bMedian.final = NA_real_,
    stageFactorMin.final = NA_real_,
    stageFactorMedian.final = NA_real_,
    stageFactorMax.final = NA_real_,
    ratioMin.final = NA_real_,
    ratioMax.final = NA_real_,
    ratioMedian.final = NA_real_)
}

matchRecord <- function(row, target, source, path, params, imf = NULL) {
  OUT <- gmsp::fitModalFactor(
    .x = imf, target = target, source = source, key = row,
    ocid = params$ocid, xi = params$xi, fmax = params$fmax,
    kNyq = params$kNyq, factorMin = params$bMin, factorMax = params$bMax,
    remove = params$imfRemove, includeResidue = params$includeResidue,
    unitsSource = params$units$source, unitsTarget = params$units$target)
  SourceTSW <- source[RecordID == row$RecordID[1L]][order(t)]
  OUT$TSW <- .matchCompleteTSW(source = SourceTSW,
                               final = OUT$TSW[order(t)],
                               params = params)
  OUT
}

.matchRecordWorker <- function(row, path, params, override) {
  invisible(override)
  OUT <- tryCatch(matchRecord(row = row, target = params$Target,
                              source = params$Source$TSW,
                              path = params$PATH.source,
                              params = params$Params,
                              imf = params$Source$IMF),
                  error = function(e) e)
  if (inherits(OUT, "error")) {
    return(list(
      RecordID = as.character(row$RecordID[1L]),
      status = "failed",
      error = conditionMessage(OUT),
      metrics = .matchErrorMetrics(row = row,
                                   error = conditionMessage(OUT))))
  }
  OUT
}

matchRecords <- function(selection, target, source, path, params,
                         parallel = FALSE, workers = 1L, logEvery = 30) {
  Work <- list(Target = target, Source = source, Params = params,
               PATH.source = path)
  LIST <- if (parallel && workers > 1L) {
    .runRecordsParallel(selection, path = path, params = Work, override = TRUE,
                        files = character(),
                        processRecord = .matchRecordWorker,
                        workers = workers, log.every = logEvery,
                        label = "match")
  } else {
    .runRecordsSequential(selection, path = path, params = Work,
                          override = TRUE, files = character(),
                          processRecord = .matchRecordWorker,
                          label = "match")
  }
  OK <- vapply(LIST, function(x) is.list(x) && !is.null(x$TSW),
               logical(1L))
  list(
    TSW = if (any(OK)) {
      rbindlist(lapply(LIST[OK], `[[`, "TSW"), use.names = TRUE)
    } else data.table(),
    PSA = if (any(OK)) {
      rbindlist(lapply(LIST[OK], `[[`, "PSA"), use.names = TRUE)
    } else data.table(),
    coeff = if (any(OK)) {
      rbindlist(lapply(LIST[OK], `[[`, "coeff"), use.names = TRUE)
    } else data.table(),
    metrics = rbindlist(lapply(LIST, `[[`, "metrics"), use.names = TRUE),
    eval = if (any(OK)) {
      rbindlist(lapply(LIST[OK], `[[`, "eval"), use.names = TRUE)
    } else data.table())
}

.matchUpdateMeta <- function(META, IMW) {
  if (!nrow(META)) return(META)
  if (!"DIR" %chin% names(META)) {
    return(unique(META, by = intersect(KEY.record, names(META)))[])
  }
  .requireColumns(META, c(KEY.record, "DIR"), "META")
  COLS <- setdiff(intersect(names(IMW), names(META)),
                  c(KEY.record, "OCID"))
  if (length(COLS)) {
    for (COL in COLS) {
      if (is.numeric(IMW[[COL]]) && is.numeric(META[[COL]]) &&
          !is.double(META[[COL]])) {
        set(META, j = COL, value = as.numeric(META[[COL]]))
      }
    }
    META[IMW, on = c(KEY.record, "DIR" = "OCID"),
         (COLS) := mget(paste0("i.", COLS))]
  }
  META[]
}

.matchManifest <- function(selection, metrics, path) {
  OUT <- selection[metrics[status == "done", .(RecordID)],
                   on = "RecordID", nomatch = 0L]
  OUT <- unique(OUT[, KEY.record, with = FALSE], by = KEY.record)
  OUT[, `:=`(status = "done",
             path = file.path(path, "data"))]
  OUT[]
}

.matchPlotAudit <- function(path, target, curves, source, final, params) {
  PATH.audit <- file.path(path, "audit")
  DIR <- file.path(PATH.audit, paste("PSA", params$ocid, sep = "-"))
  .ensureDir(PATH.audit)
  .ensureDir(DIR)
  Mean <- curves[kind == "final", .(Sa = mean(Sa, na.rm = TRUE)),
                 by = Tn][order(Tn)]
  DATA <- rbindlist(list(
    target[, .(ID = "target low", X = Tn, Y = Sa.low, size = 2.5)],
    target[, .(ID = "target mean", X = Tn, Y = Sa.mean, size = 2.5)],
    target[, .(ID = "target high", X = Tn, Y = Sa.high, size = 2.5)],
    Mean[, .(ID = "mean final", X = Tn, Y = Sa, size = 2.5)],
    curves[kind == "final",
           .(ID = paste("final", RecordID), X = Tn, Y = Sa, size = 1)]
  ), use.names = TRUE)[is.finite(X) & is.finite(Y)][order(X)]
  .saveLinePlot(DATA = DATA, path = DIR,
                file = sprintf("summary.PSA.%s.match.html", params$ocid),
                xLegend = "Tn [s]",
                yLegend = sprintf("PSA(%s) [%s]", params$ocid,
                                  params$units$psa),
                lineType = "spline", xLog = TRUE, hide = TRUE)

  Records <- sort(unique(curves$RecordID))
  for (Record in Records) {
    DATA <- rbindlist(list(
      target[, .(ID = "target low", X = Tn, Y = Sa.low, size = 1.5)],
      target[, .(ID = "target mean", X = Tn, Y = Sa.mean, size = 2.5)],
      target[, .(ID = "target high", X = Tn, Y = Sa.high, size = 1.5)],
      curves[RecordID == Record & kind == "start",
             .(ID = "start", X = Tn, Y = Sa, size = 1.2)],
      curves[RecordID == Record & kind == "final",
             .(ID = "final", X = Tn, Y = Sa, size = 1.5)]
    ), use.names = TRUE)[is.finite(X) & is.finite(Y)][order(X)]
    .saveLinePlot(DATA = DATA, path = DIR,
                  file = sprintf("%s.PSA.%s.match.html", Record, params$ocid),
                  xLegend = "Tn [s]",
                  yLegend = sprintf("PSA(%s) [%s]", params$ocid,
                                    params$units$psa),
                  lineType = "spline", xLog = TRUE)
  }

  DIR <- file.path(PATH.audit, paste("AT", params$ocid, sep = "-"))
  .ensureDir(DIR)
  COL <- paste("AT", params$ocid, sep = ".")
  for (Record in Records) {
    DATA <- rbindlist(list(
      source[RecordID == Record, .(ID = "source", X = t, Y = get(COL),
                               size = 0.75)],
      final[RecordID == Record, .(ID = "final", X = t, Y = get(COL),
                              size = 0.75)]
    ), use.names = TRUE)[is.finite(X) & is.finite(Y)][order(X)]
    .saveLinePlot(DATA = DATA, path = DIR,
                  file = sprintf("%s.AT.%s.match.html", Record, params$ocid),
                  xLegend = "t [s]",
                  yLegend = sprintf("AT(%s) [mm/s2]", params$ocid))
  }
  Mean
}

.matchPlotStandard <- function(path, TSW, PSW, target, params) {
  PATH.plot <- file.path(path, "plot")
  .ensureDir(PATH.plot)
  Records <- sort(unique(TSW$RecordID))
  Line <- .lineDefaults()
  plotTS(TSW = TSW, records = Records, path = PATH.plot,
         units = params$units$target, ids = IDS.ts, dirs = DIRS.ts,
         line = Line$TS)
  Target <- target[, .(X = Tn, Y = Sa.mean)]
  plotPS(PSW = PSW, records = Records, path = PATH.plot,
         units = params$units$target, ids = IDS.ps, dirs = DIRS.ps,
         line = Line$PS, target = Target)
  invisible(NULL)
}

.matchOutputReady <- function(path, override) {
  if (dir.exists(path) && length(list.files(path, all.files = FALSE)) &&
      !override) {
    stop(sprintf("%s exists; set override=true to overwrite known files.",
                 path), call. = FALSE)
  }
  .ensureDir(path)
}

.matchRun <- function(path) {
  PATH.config <- .matchConfigPath(path)
  Config <- .readJson(PATH.config, simplifyVector = FALSE)
  Params <- .matchParams(Config)
  PATH.out <- .matchPath(.jsonRequire(Config, c("path", "out"),
                                      PATH.config))
  DBMode <- .jsonHas(Config, c("processID")) ||
    .jsonHas(Config, c("path", "database")) ||
    .jsonHas(Config, c("path", "records"))
  PATH.source <- if (DBMode) NULL else
    .matchPath(.jsonRequire(Config, c("path", "source"), PATH.config))
  Override <- .readScalarLogical(.jsonGet(Config, c("override"),
                                          default = FALSE),
                                 "override")
  Parallel <- .readScalarLogical(.jsonGet(Config, c("parallel"),
                                          default = FALSE),
                                 "parallel")
  Workers <- .matchWorkers(.jsonGet(Config, c("workers"), default = NULL),
                           parallel = Parallel)
  LogEvery <- as.numeric(.jsonGet(Config, c("logEvery"), default = 30))
  PlotAudit <- .readScalarLogical(.jsonGet(Config, c("plot", "audit"),
                                           default = FALSE),
                                  "plot.audit")
  PlotStandard <- .readScalarLogical(.jsonGet(Config, c("plot", "standard"),
                                              default = FALSE),
                                     "plot.standard")
  .matchOutputReady(path = PATH.out, override = Override)

  Target <- .matchReadTargetBand(config = Config, params = Params)
  if (DBMode) {
    PATHS <- .matchDBPaths(Config)
    Selection <- .matchSelectionDB(config = Config, paths = PATHS)
    Source <- .matchSourceDB(config = Config, paths = PATHS,
                             selection = Selection)
  } else {
    Selection <- .matchSelection(path = PATH.source, config = Config)
    Source <- .matchSource(path = PATH.source, records = Selection$RecordID)
  }

  .log("match start: records=%s parallel=%s workers=%s",
       nrow(Selection), Parallel, Workers)
  Results <- matchRecords(selection = Selection, target = Target,
                          source = Source, path = PATH.source,
                          params = Params, parallel = Parallel,
                          workers = Workers, logEvery = LogEvery)
  Metrics <- Results$metrics
  Paths <- .matchProductPaths(PATH.out)
  fwrite(Target, file.path(Paths$match, "target.csv"))
  fwrite(Selection, file.path(Paths$metadata, "selection.csv"))
  if (!nrow(Results$TSW)) {
    stop("match produced zero completed records.", call. = FALSE)
  }

  TSW <- Results$TSW
  PSA <- Results$PSA
  Records <- Metrics[status == "done", RecordID]
  StageB <- gmsp::fitRecordFactor(.x = PSA, target = Target,
                                  records = Records,
                                  factorMin = Params$aMin,
                                  factorMax = Params$aMax,
                                  bandWeight = Params$bandWeight,
                                  deficitWeight = Params$deficitWeight)
  Shaping <- .matchShapingQA(PSA = PSA)
  PSA <- .matchScalePSA(PSA = PSA, scale = StageB$scale)
  TSW <- .matchScaleTSW(TSW = TSW, scale = StageB$scale, params = Params)
  PSW <- .matchBuildPSW(TSW = TSW, target = Target, params = Params)
  AUX <- gmsp::matchCompatibility(.x = PSW, target = Target,
                                  scale = StageB$scale,
                                  ocid = Params$ocid,
                                  factorMin = Params$aMin,
                                  factorMax = Params$aMax)
  Compatibility <- AUX$record
  CompatibilitySummary <- AUX$summary
  Metrics[StageB$scale, scaleFactor := i.scaleFactor, on = "RecordID"]
  Coeff <- Results$coeff
  Eval <- Results$eval
  IMW <- .matchBuildIMW(TSW = TSW, params = Params)
  META <- .matchUpdateMeta(Source$META[RecordID %chin% TSW$RecordID], IMW)
  OCIDMAP <- Source$OCIDMAP[RecordID %chin% TSW$RecordID]
  Manifest <- .matchManifest(selection = Selection, metrics = Metrics,
                             path = PATH.out)
  ModalQA <- .matchModalQA(imf = Source$IMF, TSW = TSW, IMW = IMW,
                           coeff = Coeff, scale = StageB$scale,
                           target = Target, params = Params)

  Mean <- PSA[kind == "final", .(Sa = mean(Sa, na.rm = TRUE)),
              by = Tn][order(Tn)]

  fwrite(Manifest, file.path(Paths$data, "manifest.csv"))
  fwrite(TSW, file.path(Paths$data, "TSW.csv"))
  fwrite(PSW, file.path(Paths$data, "PSW.csv"))
  fwrite(IMW, file.path(Paths$data, "IMW.csv"))
  fwrite(OCIDMAP, file.path(Paths$metadata, "OCIDMAP.csv"))
  if (nrow(META)) fwrite(META, file.path(Paths$metadata, "MasterIndex.csv"))
  fwrite(Coeff, file.path(Paths$match, "factors.csv"))
  fwrite(StageB$scale, file.path(Paths$match, "scale.csv"))
  fwrite(StageB$stage, file.path(Paths$match, "stage.csv"))
  fwrite(Shaping, file.path(Paths$match, "shaping.csv"))
  fwrite(Compatibility, file.path(Paths$match, "recordCompatibility.csv"))
  fwrite(CompatibilitySummary, file.path(Paths$match, "compatibility.csv"))
  fwrite(Metrics, file.path(Paths$match, "metrics.csv"))
  fwrite(Eval, file.path(Paths$match, "eval.csv"))
  fwrite(Mean, file.path(Paths$match, "mean.csv"))
  fwrite(ModalQA$ITS, file.path(Paths$data, "ITS.csv"))
  fwrite(ModalQA$IPSA, file.path(Paths$data, "IPSA.csv"))
  jsonlite::write_json(.matchSourceInfo(config = Config, dbMode = DBMode),
                       file.path(Paths$metadata, "source.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  if (PlotAudit) {
    .matchPlotAudit(path = PATH.out, target = Target, curves = PSA,
                    source = Source$TSW, final = TSW, params = Params)
  }
  if (PlotStandard) {
    .matchPlotStandard(path = PATH.out, TSW = TSW, PSW = PSW,
                       target = Target, params = Params)
  }

  invisible(file.copy(PATH.config, file.path(Paths$metadata, "runMatch.json"),
                      overwrite = TRUE))
  .log("match wrote %s records to %s", nrow(Manifest), PATH.out)
  invisible(PATH.out)
}
