# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
selectProject <- function(file = file.path("gmsp", "select.json"), root = getwd()) {
  root <- .projectRoot(root)
  FILE.config <- .resolvePath(file, root = root)
  tryCatch({
    Config <- .readProjectConfig(FILE.config)
    PATH.selection <- .resolvePath(.jsonRequire(Config, c("path", "selection"), FILE.config), root = root)
    Owners <- .readCharacterVector(.jsonRequire(Config, "owners", FILE.config), "owners")
    ProcessID <- .targetText(.jsonGet(Config, "processID", default = "raw"), label = "processID")
    Paths <- lapply(c("database", "index", "process"), function(x) {
      Path <- .jsonGet(Config, c("path", x))
      if (is.null(Path)) return(NULL)
      .resolvePath(Path, root = root)
    })
    names(Paths) <- c("database", "index", "process")
    if (is.null(Paths$database) && !is.null(Paths$index) &&
        identical(basename(Paths$index), "index")) {
      Paths$database <- dirname(Paths$index)
    }
    Need <- "index"
    if (ProcessID != "raw") Need <- c("index", "process")
    Paths <- .recordPaths(Paths, need = Need)
    RequireIMF <- .readScalarLogical(.jsonGet(Config, "requireIMF", default = FALSE), "requireIMF")
    if (RequireIMF && ProcessID == "raw") {
      .stopUsage("requireIMF=TRUE is not supported for processID='raw'.", operation = "selectProject")
    }
    OneRecordPerEvent <- .readScalarLogical(.jsonGet(Config, "oneRecordPerEvent", default = TRUE),
                                           "oneRecordPerEvent")
    Exclude <- .readCharacterVector(.jsonGet(Config, "exclude"), "exclude", allowNull = TRUE)
    Specs <- .jsonGet(Config, "criteria", default = list())
    if (!is.list(Specs)) .stopUsage("criteria must be a JSON array.", operation = "selectProject")
    HasPGA <- .jsonHas(Config, "PGA") || .jsonHas(Config, c("path", "matchConfig")) ||
      any(vapply(Specs, function(x) .jsonHas(x, "PGA"), logical(1L)))
    Units <- NULL
    if (HasPGA) {
      Units <- tolower(.targetText(.jsonRequire(Config, c("units", "PGA")), label = "units.PGA"))
      if (!Units %in% c("g", "mm/s2", "mm/s^2")) {
        .stopUsage("units.PGA must be g or mm/s2.", operation = "selectProject")
      }
    }
    if (.jsonHas(Config, c("path", "matchConfig"))) {
      if (.jsonHas(Config, "PGA")) {
        .stopUsage("Do not define PGA with path.matchConfig.", operation = "selectProject")
      }
      FILE <- .resolvePath(.jsonGet(Config, c("path", "matchConfig")), root = root)
      Match <- .readProjectConfig(FILE)
      Target <- .projectTarget(Match, units = Units, root = root, envelope = FALSE)
      PGA <- Target[Tn == 0, Sa]
      Bounds <- c(.jsonRequire(Match, c("shape", "recordFactor", "min")),
                  .jsonRequire(Match, c("shape", "recordFactor", "max")))
      if (!is.numeric(Bounds) || length(Bounds) != 2L || any(!is.finite(Bounds)) ||
          any(Bounds <= 0) || Bounds[2L] < Bounds[1L]) {
        .stopUsage("shape.recordFactor must be finite, positive and ordered.", operation = "selectProject")
      }
      Config$PGA <- c(PGA / Bounds[2L], PGA / Bounds[1L])
    }
    Fields <- c("PGA", "EventMagnitude", "EventMagnitudeType", "EventDepth", "StationVs30",
                "Repi", "Rhyp", "Year", "D0575", "D0595", "D2080", "Dmax")
    .criteriaSpec <- function(spec) {
      OUT <- list()
      for (x in intersect(Fields, names(spec))) {
        Value <- .jsonGet(spec, x)
        if (x != "EventMagnitudeType" && (!is.numeric(unlist(Value)) || anyNA(unlist(Value)))) {
          .stopUsage(x, " requires explicit numeric values.", operation = "selectProject")
        }
        if (x == "PGA" && Units == "g") Value <- .range(Value) * .getGravity("mm")
        OUT[[x]] <- Value
      }
      OUT
    }
    BaseCriteria <- .criteriaSpec(Config)
    Criteria <- list(list(criteria = BaseCriteria, criterion = "all"))
    if (length(Specs)) {
      Criteria <- lapply(seq_along(Specs), function(i) {
        list(criteria = c(BaseCriteria, .criteriaSpec(Specs[[i]])),
          criterion = .targetText(.jsonGet(Specs[[i]], "criterion", default = paste0("C", i)),
                                  label = "criterion"))
      })
    }
    Keys <- .recordNormalizeKeys(list(ProcessID = ProcessID, OwnerID = Owners))
    Index <- .recordReadSearchTable(processID = ProcessID, keys = Keys, paths = Paths)
    Results <- .selectCandidates(Index, keys = Keys, criteria = Criteria,
                                 exclude = Exclude, oneRecordPerEvent = OneRecordPerEvent)
    if (RequireIMF && !"IMF" %in% .recordDeclaredOutputs(ProcessID, Paths)) {
      .stopUsage("The selected process does not declare IMF.", operation = "selectProject")
    }
    Entries <- list.files(PATH.selection, all.files = TRUE, no.. = TRUE,
                          recursive = TRUE, include.dirs = TRUE)
    Foreign <- setdiff(Entries, c("selection.csv", "candidates.csv", "selection.json", "summary.json"))
    if (length(Foreign)) {
      .stopUsage("Refusing to clear ", PATH.selection, ": foreign entries: ",
                  paste(Foreign, collapse = ", "), operation = "selectProject")
    }
    if (length(Entries)) unlink(file.path(PATH.selection, Entries), force = TRUE)
    .mkdir(PATH.selection)
    Written <- character()
    tryCatch({
      for (x in c("selection", "candidates")) {
        FILE <- file.path(PATH.selection, paste0(x, ".csv"))
        fwrite(Results[[x]], FILE, eol = "\n")
        Written <- c(Written, FILE)
      }
      FILE <- file.path(PATH.selection, "selection.json")
      .writeJSON(Config, FILE, auto_unbox = TRUE, pretty = TRUE, null = "null")
      Written <- c(Written, FILE)
      Path <- list(index = Paths$index)
      if (!is.null(Paths$metadata)) Path$metadata <- Paths$metadata
      FILE <- file.path(PATH.selection, "summary.json")
      .writeJSON(list(stage = "select", path = Path, processID = ProcessID,
        requireIMF = RequireIMF, candidates = nrow(Results$candidates),
        records = uniqueN(Results$selection$RecordID), events = uniqueN(.eventIdentity(Results$selection))),
        FILE, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }, error = function(e) .stopPartialPublication(conditionMessage(e), operation = "selectProject",
        path = PATH.selection, failures = list(e), result = Written))
    .projectMessage("Selection published.", operation = "selectProject", phase = "complete", path = PATH.selection)
    invisible(0L)
  }, error = function(e) .projectFailure(e, operation = "selectProject", path = FILE.config))
}
