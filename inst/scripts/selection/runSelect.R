#!/usr/bin/env Rscript

source(file.path(Sys.getenv("GMSP_SCRIPT_ROOT"), "tools", "setup.R"))

Args <- commandArgs(trailingOnly = TRUE)
PATH.config <- if (length(Args)) Args[1L] else
  "scripts/selection/runSelect.json"
PATH.config <- .resolvePath(PATH.config)
Config <- .readJson(PATH.config, simplifyVector = FALSE)

PATH.database <- .jsonGet(Config, c("path", "database"), default = NULL)
PATH.index <- .jsonGet(Config, c("path", "index"), default = NULL)
PATH.process <- .jsonGet(Config, c("path", "process"), default = NULL)
if (is.null(PATH.database) && !is.null(PATH.index) &&
    identical(basename(path.expand(PATH.index)), "index")) {
  PATH.database <- dirname(path.expand(PATH.index))
}
PATH.selection <- .resolvePath(.jsonRequire(Config, c("path", "selection"),
                                            PATH.config))

Owners <- unique(as.character(.jsonRequire(Config, c("owners"), PATH.config)))
ProcessID <- as.character(.jsonGet(Config, c("processID"), default = "raw"))
if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
  stop("processID must be one non-empty string.", call. = FALSE)
}
PATHS <- .recordPaths(list(database = PATH.database,
                           index = PATH.index,
                           process = PATH.process),
                      need = if (identical(ProcessID, "raw")) {
                        "index"
                      } else {
                        c("index", "process")
                      })
RequireIMF <- .readScalarLogical(.jsonGet(Config, c("requireIMF"),
                                          default = FALSE),
                                 "requireIMF")
if (RequireIMF && identical(ProcessID, "raw")) {
  stop(
    paste0(
      "requireIMF=TRUE is not supported for processID='raw'; ",
      "select a processed ProcessID."
    ),
    call. = FALSE
  )
}
OneRecordPerEvent <- isTRUE(.jsonGet(Config, c("oneRecordPerEvent"),
                                     default = TRUE))
Units.PGA <- tolower(as.character(.jsonGet(Config, c("units", "PGA"),
                                           default = "mm/s2")))
Exclude <- .readCharacterVector(.jsonGet(Config, c("exclude"), default = NULL),
                                "exclude", allowNull = TRUE)

.selectP <- function(x) {
  x <- as.character(x)
  Map <- c(p05 = "0.05", p5 = "0.05", p10 = "0.10", p16 = "0.16",
           p50 = "0.50", p84 = "0.84", p90 = "0.90", p95 = "0.95",
           mean = "mean")
  ifelse(x %chin% names(Map), Map[x], x)
}

.selectReadTable <- function(path) {
  FILE <- .resolvePath(path)
  if (!file.exists(FILE)) stop(sprintf("Missing table: %s", FILE),
                               call. = FALSE)
  if (grepl("[.]rds$", FILE, ignore.case = TRUE)) {
    return(as.data.table(readRDS(FILE)))
  }
  if (grepl("[.]csv$", FILE, ignore.case = TRUE)) return(fread(FILE))
  stop(sprintf("Unsupported table extension: %s", FILE), call. = FALSE)
}

.selectConvertPGA <- function(PGA, from, to) {
  From <- tolower(as.character(from))
  To <- tolower(as.character(to))
  if (identical(From, To)) return(PGA)
  if (From == "g" && To %chin% c("mm/s2", "mm/s^2")) return(PGA * 9806.65)
  if (From %chin% c("mm/s2", "mm/s^2") && To == "g") return(PGA / 9806.65)
  stop(sprintf("Unsupported PGA unit conversion: %s -> %s", from, to),
       call. = FALSE)
}

.deriveTargetPGA <- function(config, units) {
  FILE.config <- .jsonGet(config, c("path", "matchConfig"), default = NULL)
  if (is.null(FILE.config)) return(config)
  if (.jsonHas(config, c("PGA"))) {
    stop("Do not define PGA with path.matchConfig; PGA is derived from target.",
         call. = FALSE)
  }

  FILE.config <- .resolvePath(FILE.config)
  Match <- .readJson(FILE.config, simplifyVector = FALSE)
  Target <- .selectReadTable(.jsonRequire(Match, c("path", "target"),
                                          FILE.config))
  Value <- as.character(.jsonGet(Match, c("target", "value"),
                                 default = "SaF"))
  .requireColumns(Target, c("ID", "TR", "Vs30", "p", "Tn", Value),
                  "target table")

  TargetID <- as.character(.jsonRequire(Match, c("target", "ID"),
                                        FILE.config))
  TargetTR <- as.numeric(.jsonRequire(Match, c("target", "TR"),
                                      FILE.config))
  TargetVs30 <- as.numeric(.jsonRequire(Match, c("target", "Vs30"),
                                        FILE.config))
  TargetP <- .selectP(as.character(.jsonGet(Match, c("target", "p"),
                                            default = "mean")))
  SiteID <- as.character(.jsonGet(Match, c("target", "siteID"), default = ""))
  TargetUnits <- as.character(.jsonGet(Match, c("target", "units"),
                                       default = "g"))

  Row <- Target[
    ID %chin% TargetID &
      TR %in% TargetTR &
      Vs30 %in% TargetVs30 &
      p %chin% TargetP &
      Tn == 0]
  if (nzchar(SiteID) && "siteID" %chin% names(Row)) {
    Row <- Row[siteID %chin% SiteID]
  }
  if (!nrow(Row)) stop("target PGA filters produced zero rows.",
                       call. = FALSE)

  PGA <- max(Row[[Value]], na.rm = TRUE)
  PGA <- .selectConvertPGA(PGA = PGA, from = TargetUnits, to = units)

  Shape <- .jsonRequire(Match, c("shape"), FILE.config)
  Factor <- .jsonRequire(Shape, c("recordFactor"), FILE.config)
  Min <- as.numeric(.jsonRequire(Factor, c("min"), "shape.recordFactor"))
  Max <- as.numeric(.jsonRequire(Factor, c("max"), "shape.recordFactor"))
  if (!is.finite(PGA) || PGA <= 0 ||
      !is.finite(Min) || !is.finite(Max) || Min <= 0 || Max < Min) {
    stop("target PGA and shape.recordFactor must be finite and positive.",
         call. = FALSE)
  }

  config$PGA <- c(PGA / Max, PGA / Min)
  config
}

Config <- .deriveTargetPGA(Config, Units.PGA)

FilterFields <- unique(c(
  "PGA", "EventMagnitude", "EventMagnitudeType", "EventDepth",
  "StationVs30", "Repi", "Rhyp", "Year", "D0575", "D0595", "D2080",
  "Dmax"
))

.criteriaSpec <- function(spec) {
  OUT <- list()
  for (Field in FilterFields) {
    if (!.jsonHas(spec, c(Field))) next
    Value <- .jsonGet(spec, c(Field))
    if (identical(Field, "PGA") && Units.PGA == "g") {
      Value <- .range(Value) * 9806.65
    }
    OUT[[length(OUT) + 1L]] <- Value
    names(OUT)[length(OUT)] <- Field
  }
  OUT
}

.selectPart <- function(criteria, criterion) {
  DT <- selectRecords(
    keys = list(ProcessID = ProcessID, OwnerID = Owners),
    criteria = c(list(DIR = "H1"), criteria),
    singleRecord = FALSE,
    paths = PATHS
  )
  if ("DIR" %chin% names(DT)) DT <- DT[DIR == "H1"]
  DT[, Year := as.integer(substr(EventID, 1L, 4L))]
  if (length(Exclude)) DT <- DT[!(RecordID %chin% Exclude)]
  if (!nrow(DT)) return(DT)
  DT[, criterion := criterion]
  DT[]
}

.reduceOneRecord <- function(DT) {
  if (!OneRecordPerEvent) return(copy(DT))
  if (!"PGA" %chin% names(DT)) {
    stop("oneRecordPerEvent requires PGA in the selected index table.",
         call. = FALSE)
  }
  setorder(DT, EventID, -PGA, -StationID, RecordID)
  DT[, .SD[1L], by = EventID]
}

BaseCriteria <- .criteriaSpec(Config)

if (.jsonHas(Config, c("criteria"))) {
  Parts <- lapply(seq_along(Config$criteria), function(i) {
    Spec <- Config$criteria[[i]]
    Criteria <- c(BaseCriteria, .criteriaSpec(Spec))
    Criterion <- as.character(.jsonGet(Spec, c("criterion"),
                                       default = sprintf("C%d", i)))
    .selectPart(Criteria, criterion = Criterion)
  })
  DT <- rbindlist(Parts, use.names = TRUE)
  Keys <- setdiff(names(DT), "criterion")
  DT[, criterion := paste(sort(unique(criterion)), collapse = "+"), by = Keys]
  DT <- unique(DT, by = Keys)
} else {
  DT <- .selectPart(BaseCriteria, criterion = "all")
}

if (!nrow(DT)) stop("Selection filters produced zero candidates.", call. = FALSE)
Selection <- .reduceOneRecord(DT)
setorder(Selection, RecordID, -PGA, EventID, OwnerID, StationID)
Selection <- unique(Selection, by = "RecordID")

Order <- c("criterion", "OwnerID", "EventID", "EventID.USGS", "StationID",
           "NetworkID", "RecordID", "ProcessID", "EventMagnitude",
           "EventMagnitudeType", "EventDepth", "EventMechanism.owner",
           "EventMechanism.STREC", "EventTectonicRegion.owner",
           "EventTectonicRegion.STREC", "EventTectonicRegion.inferred",
           "Repi", "Rhyp", "StationVs30", "PGA", "AI", "CAV", "CAV5",
           "D0575", "D0595", "D2080", "Dmax", "DIR", "OCID", "Year")
setcolorder(Selection, c(intersect(Order, names(Selection)),
                         setdiff(names(Selection), Order)))
setcolorder(DT, c(intersect(Order, names(DT)), setdiff(names(DT), Order)))

if (dir.exists(PATH.selection)) {
  .clearGeneratedDir(PATH.selection, dirname(PATH.selection))
}
.mkdir(PATH.selection)
fwrite(Selection, file.path(PATH.selection, "selection.csv"))
fwrite(DT, file.path(PATH.selection, "candidates.csv"))

jsonlite::write_json(Config, file.path(PATH.selection, "selection.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null")
Path <- list(index = PATHS$index)
if (!is.null(PATHS$metadata)) Path$metadata <- PATHS$metadata
jsonlite::write_json(list(
  stage = "select",
  path = Path,
  processID = ProcessID,
  requireIMF = RequireIMF,
  candidates = nrow(DT),
  records = uniqueN(Selection$RecordID),
  events = uniqueN(Selection$EventID)
),
                     file.path(PATH.selection, "summary.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null")
message(sprintf("Wrote %s selected records from %s candidates to %s",
                format(uniqueN(Selection$RecordID), big.mark = ","),
                format(nrow(DT), big.mark = ","), PATH.selection))
