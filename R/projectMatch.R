# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.projectTarget <- function(config, units, root, envelope) {
  if (.jsonHas(config, c("path", "target"))) {
    .stopUsage("path.target is obsolete; use path.data.", operation = ".projectTarget")
  }
  Target <- .jsonRequire(config, "target")
  if (!is.list(Target) || is.null(names(Target)) || anyDuplicated(names(Target))) {
    .stopUsage("target must be a JSON object with distinct keys.", operation = ".projectTarget")
  }
  Target$value <- .jsonGet(Target, "value", default = "SaF")
  Target$units <- .jsonGet(Target, "units", default = "g")
  if (.jsonHas(Target, c("envelope", "ID"))) {
    .stopUsage("target.envelope.ID is obsolete; use lower and upper. ",
               "Allowed values: 0.<digits> strictly between 0 and 1 (for example 0.50), or mean.",
               operation = ".projectTarget")
  }
  if (envelope) .jsonRequire(Target, "envelope", path = "target")
  if (!envelope) Target$envelope <- NULL
  Range <- NULL
  if (.jsonHas(config, "Tn")) {
    Range <- c(.jsonGet(config, c("Tn", "min"), default = -Inf),
               .jsonGet(config, c("Tn", "max"), default = Inf))
    if (!is.numeric(Range) || length(Range) != 2L || anyNA(Range)) {
      .stopUsage("Tn.min/Tn.max must define a numeric interval.", operation = ".projectTarget")
    }
  }
  .readTargetBand(path.data = .resolvePath(.jsonRequire(config, c("path", "data")), root = root),
                  target = Target, unitsTarget = units, range = Range)
}

.matchDBPaths <- function(config, root) {
  Paths <- lapply(c("database", "records", "index", "process"), function(x) {
    Path <- .jsonGet(config, c("path", x))
    if (is.null(Path)) return(NULL)
    .resolvePath(Path, root = root)
  })
  names(Paths) <- c("database", "records", "index", "process")
  if (is.null(Paths$database) && !is.null(Paths$records) &&
      identical(basename(Paths$records), "records")) {
    Paths$database <- dirname(Paths$records)
  }
  .recordPaths(Paths, need = c("records", "index", "process"))
}

.matchSelectionDB <- function(config, paths, root) {
  ProcessID <- .targetText(.jsonRequire(config, "processID"), label = "processID")
  KeyColumns <- c("RecordID", "OwnerID", "EventID", "StationID")
  FILE <- .jsonGet(config, c("path", "selection"))
  if (!is.null(FILE)) {
    FILE <- .resolvePath(FILE, root = root)
    if (dir.exists(FILE)) FILE <- file.path(FILE, "selection.csv")
    OUT <- .recordFread(FILE)
    .requireColumns(OUT, KeyColumns, "selection")
    if ("ProcessID" %in% names(OUT) &&
        (anyNA(OUT$ProcessID) || any(OUT$ProcessID != ProcessID))) {
      .stopUsage("Selection ProcessID contradicts processID.", operation = ".matchSelectionDB")
    }
    OUT[, ProcessID := ProcessID]
  }
  if (is.null(FILE)) {
    Owners <- .readCharacterVector(.jsonRequire(config, c("candidate", "ownerID")), "candidate.ownerID")
    OUT <- .recordSelect(keys = list(ProcessID = ProcessID, OwnerID = Owners), criteria = list(), paths = paths)
  }
  Records <- .readCharacterVector(.jsonGet(config, c("candidate", "recordID")),
                                  "candidate.recordID", allowNull = TRUE)
  if (length(Records)) OUT <- OUT[RecordID %in% Records]
  if (!nrow(OUT)) .stopUsage("Candidate selection produced zero records.", operation = ".matchSelectionDB")
  unique(OUT[, c("ProcessID", KeyColumns), with = FALSE])
}

.matchParams <- function(config) {
  Shape <- .jsonRequire(config, "shape")
  Units <- .jsonRequire(config, "units")
  for (x in list(Shape, Units)) {
    if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x))) {
      .stopUsage("shape and units must be JSON objects with distinct keys.", operation = "matchProject")
    }
  }
  for (x in c("source", "target", "psa")) {
    Units[[x]] <- .targetText(.jsonRequire(Units, x, path = "units"), label = paste0("units.", x))
  }
  .resolveUnitFactor(unitsSource = Units$source, unitsTarget = Units$target, ID = "DT")
  if (!tolower(Units$psa) %in% paste0(tolower(Units$target), c("/s2", "/s^2"))) {
    .stopUsage("units.psa must be the acceleration unit of units.target.", operation = "matchProject")
  }
  for (x in c("bMin", "initial", "modalRatio", "maxIt", "lambdaBand", "lambdaMove")) {
    if (.jsonHas(Shape, x)) .stopUsage("Unsupported shape field: ", x, operation = "matchProject")
  }
  if (.jsonHas(config, c("imf", "remove")) || .jsonHas(config, "remove")) {
    .stopUsage("Mode names cannot define imf removal; supply imf.criterion.", operation = "matchProject")
  }
  OUT <- list(ocid = .targetText(.jsonGet(config, "ocid", default = "H1"), label = "ocid"),
    units = Units, signalControl = .jsonRequire(config, "signalControl"),
    criterion = .jsonRequire(config, c("imf", "criterion")),
    includeResidue = .readScalarLogical(.jsonGet(config, c("imf", "includeResidue"),
      default = .jsonGet(Shape, "includeResidue", default = TRUE)), "imf.includeResidue"))
  if (is.list(OUT$criterion) && identical(OUT$criterion$rule, "target")) {
    if (anyDuplicated(names(OUT$criterion)) ||
        !setequal(names(OUT$criterion), c("rule", "action")) ||
        !identical(OUT$criterion$action, "remove")) {
      .stopUsage("imf.criterion target requires exactly rule and action, with action remove.",
                 operation = "matchProject")
    }
  } else {
    .checkModeCriterion(OUT$criterion)
  }
  Fields <- list(xi = "xi", fmax = "fmax", kNyq = "kNyq", nTheta = "nTheta",
    bMin = c("shape", "modalFactor", "min"), bMax = c("shape", "modalFactor", "max"),
    aMin = c("shape", "recordFactor", "min"), aMax = c("shape", "recordFactor", "max"),
    bandWeight = c("shape", "bandWeight"), deficitWeight = c("shape", "deficitWeight"))
  for (x in names(Fields)) {
    OUT[[x]] <- .jsonRequire(config, Fields[[x]])
    if (x %in% c("bMax", "aMax") && identical(OUT[[x]], "Inf")) OUT[[x]] <- Inf
    if (!is.numeric(OUT[[x]]) || length(OUT[[x]]) != 1L || is.na(OUT[[x]])) {
      .stopUsage(paste(Fields[[x]], collapse = "."), " must be one number.", operation = "matchProject")
    }
  }
  if (!OUT$ocid %in% c("H1", "H2", "UP") || !is.finite(OUT$xi) || OUT$xi <= 0 || OUT$xi > 1 ||
      !is.finite(OUT$fmax) || OUT$fmax <= 0 || !is.finite(OUT$kNyq) || OUT$kNyq <= 0 ||
      !is.finite(OUT$nTheta) || OUT$nTheta < 2 || OUT$nTheta != floor(OUT$nTheta)) {
    .stopUsage("Invalid ocid, xi, fmax, kNyq or nTheta.", operation = "matchProject")
  }
  if (!is.finite(OUT$bMin) || OUT$bMin <= 0 || OUT$bMin > 1 || OUT$bMax < 1 ||
      !is.finite(OUT$aMin) || OUT$aMin < 0 || OUT$aMax < OUT$aMin ||
      !is.finite(OUT$bandWeight) || OUT$bandWeight < 0 ||
      !is.finite(OUT$deficitWeight) || OUT$deficitWeight <= 0) {
    .stopUsage("Invalid modal or record factor bounds or objective weights.", operation = "matchProject")
  }
  OUT
}

.matchCriterion <- function(criterion, target, fmax) {
  Metadata <- list(rule = criterion$rule, action = criterion$action,
    fMin = criterion$fMin, fMax = criterion$fMax,
    TnMax = NULL, origin = "explicit")
  if (identical(criterion$rule, "target")) {
    Periods <- target[Tn > 0, Tn]
    if (!length(Periods)) {
      .stopUsage("Target criterion requires positive target periods.", operation = "matchProject")
    }
    Metadata$TnMax <- max(Periods)
    Metadata$fMin <- 1 / Metadata$TnMax
    Metadata$fMax <- as.numeric(fmax)
    if (Metadata$fMin >= fmax) {
      .stopUsage("fmax must exceed 1/max(Tn) for the target criterion.", operation = "matchProject")
    }
    Metadata$origin <- "target"
    criterion <- list(rule = "band", fMin = Metadata$fMin, fMax = Metadata$fMax, action = "keep")
  }
  list(criterion = criterion, metadata = Metadata)
}
