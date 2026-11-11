# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
rankProject <- function(file = file.path("gmsp", "match.json"), root = getwd()) {
  root <- .projectRoot(root)
  FILE.config <- .resolvePath(file, root = root)
  tryCatch({
    Config <- .readProjectConfig(FILE.config)
    RecordCount <- .jsonRequire(Config, c("stage0", "N"))
    if (!is.numeric(RecordCount) || length(RecordCount) != 1L || !is.finite(RecordCount) ||
        RecordCount < 2 || RecordCount != floor(RecordCount)) {
      .stopUsage("stage0.N must be an integer of at least two.", operation = "rankProject")
    }
    FILE.selection <- .resolvePath(.jsonRequire(Config, c("path", "selection")), root = root)
    if (dir.exists(FILE.selection)) FILE.selection <- file.path(FILE.selection, "selection.csv")
    ProcessID <- .targetText(.jsonRequire(Config, "processID"), label = "processID")
    if (ProcessID == "raw") .stopUsage("Ranking requires a processed base.", operation = "rankProject")
    Component <- .targetText(.jsonGet(Config, "ocid", default = "H1"), label = "ocid")
    Damping <- .jsonRequire(Config, "xi")
    if (!Component %in% c("H1", "H2", "UP") || !is.numeric(Damping) || length(Damping) != 1L ||
        !is.finite(Damping) || Damping <= 0 || Damping > 1) {
      .stopUsage("Invalid ocid or xi.", operation = "rankProject")
    }
    Units <- .targetText(.jsonRequire(Config, c("units", "psa")), label = "units.psa")
    Target <- .projectTarget(Config, units = Units, root = root, envelope = TRUE)
    Paths <- .matchDBPaths(Config, root = root)
    KeyColumns <- c("ProcessID", "RecordID", "OwnerID", "EventID", "StationID")
    Pool <- .jsonGet(Config, c("candidate", "pool"))
    Owners <- .jsonGet(Config, c("candidate", "ownerID"))
    if (is.null(Pool) && is.null(Owners)) .jsonRequire(Config, c("candidate", "ownerID"))
    if (!is.null(Owners)) {
      Owners <- .readCharacterVector(Owners, "candidate.ownerID")
      Selection <- .recordSelect(keys = list(ProcessID = ProcessID, OwnerID = Owners), criteria = list(), paths = Paths)
    }
    if (!is.null(Pool)) {
      Pool <- .resolvePath(Pool, root = root)
      if (dir.exists(Pool)) Pool <- file.path(Pool, "selection.csv")
      DT <- .recordFread(Pool)
      .requireColumns(DT, "RecordID", "candidate.pool")
      if ("ProcessID" %in% names(DT) &&
          (anyNA(DT$ProcessID) || any(DT$ProcessID != ProcessID))) {
        .stopUsage("candidate.pool ProcessID contradicts processID.", operation = "rankProject")
      }
      if (is.null(Owners)) {
        .requireColumns(DT, setdiff(KeyColumns, "ProcessID"), "candidate.pool")
        DT[, ProcessID := ProcessID]
        Selection <- .recordSelect(keys = DT[, KeyColumns, with = FALSE], criteria = list(), paths = Paths)
      }
      if (!is.null(Owners)) Selection <- .recordApplyKeys(Selection, keys = DT[, intersect(KeyColumns, names(DT)), with = FALSE])
    }
    Selection <- unique(Selection[, KeyColumns, with = FALSE])
    Records <- .readCharacterVector(.jsonGet(Config, c("candidate", "recordID")),
                                    "candidate.recordID", allowNull = TRUE)
    Exclude <- .readCharacterVector(.jsonGet(Config, c("candidate", "exclude")),
                                    "candidate.exclude", allowNull = TRUE)
    if (length(Records)) Selection <- Selection[RecordID %in% Records]
    if (length(Exclude)) Selection <- Selection[!RecordID %in% Exclude]
    FILE.meta <- .recordTableFile(ProcessID, paths = Paths, output = "MasterIndex")
    Metadata <- .recordFread(FILE.meta)
    # candidate.filter and the Mw column of stage0.spectral.csv use the moment magnitude of
    # select, as the owner decided on 2026-11-09.
    .setHomogeneousMagnitude(Metadata)
    Filter <- .jsonGet(Config, c("candidate", "filter"), default = list())
    if (!is.list(Filter) || (length(Filter) && is.null(names(Filter)))) {
      .stopUsage("candidate.filter must be a JSON object of ranges.", operation = "rankProject")
    }
    for (x in names(Filter)) {
      .requireColumns(Metadata, x, "MasterIndex")
      Bounds <- c(.jsonGet(Filter[[x]], "min", default = -Inf),
                  .jsonGet(Filter[[x]], "max", default = Inf))
      if (!is.numeric(Bounds) || length(Bounds) != 2L || anyNA(Bounds) || Bounds[1L] > Bounds[2L] ||
          !is.numeric(Metadata[[x]])) {
        .stopUsage("Invalid candidate.filter range for ", x, operation = "rankProject")
      }
      Selection <- Selection[RecordID %in% Metadata[
        is.finite(get(x)) & get(x) >= Bounds[1L] & get(x) <= Bounds[2L], RecordID]]
    }
    if (!nrow(Selection)) .stopUsage("Candidate filters produced zero records.", operation = "rankProject")
    .checkTableKeys(Selection, by = character(), columns = KeyColumns, keys = "RecordID", operation = "rankProject")
    FILE.intensity <- .recordTableFile(ProcessID, paths = Paths, output = "IntensityTable")
    Intensity <- .recordFread(FILE.intensity)
    .requireColumns(Intensity, c("RecordID", "OCID", "PGA", "AI"), "IntensityTable")
    Intensity <- Intensity[OCID == Component & is.finite(PGA) & PGA > 0,
      .(PGA = max(PGA), AI = max(AI, na.rm = TRUE)), by = "RecordID"]
    if (!nrow(Intensity)) .stopUsage("Ranking found zero finite PGA rows.", operation = "rankProject")
    .requireColumns(Metadata, c("RecordID", "EventName", "EventMagnitude", "Repi"), "MasterIndex")
    COLS <- c("RecordID", "EventName", "EventMagnitude", "Repi", intersect(USGSEventColumns, names(Metadata)))
    Metadata <- unique(Metadata[, ..COLS], by = "RecordID")
    OneRecordPerEvent <- .readScalarLogical(.jsonGet(Config, c("candidate", "oneRecordPerEvent"),
      default = FALSE), "candidate.oneRecordPerEvent")
    Contract <- .recordReadProcessContract(ProcessID, paths = Paths, required = TRUE)
    Declared <- .recordDeclaredOutputs(ProcessID, Paths)
    Output <- "TSW"
    if ("PSW" %in% Declared) Output <- "PSW"
    if (!Output %in% Declared) .stopUsage("Process declares neither PSW nor TSW.", operation = "rankProject")
    FILES <- .recordTableFile(ProcessID, paths = Paths, output = Output)
    if (!Output %in% Contract$process) {
      FILES <- vapply(seq_len(nrow(Selection)), function(i) {
        .recordLocalFile(Selection[i], pathRecords = Paths$records, output = Output)
      }, character(1L))
    }
    FILES <- unique(c(FILES, FILE.meta, FILE.intensity,
      file.path(Paths$process, ProcessID, "process.json")))
    if (any(!file.exists(FILES))) {
      .stopUsage("Missing rank input: ", paste(FILES[!file.exists(FILES)], collapse = ", "), operation = "rankProject")
    }
    Identity <- digest::digest(list(config = Config, root = root, paths = Paths, target = as.data.frame(Target),
      selection = as.data.frame(Selection), output = Output, xi = Damping, OCID = Component,
      files = stats::setNames(vapply(FILES, digest::digest, character(1L), algo = "sha256", file = TRUE), FILES)),
      algo = "sha256")
    DIR <- dirname(FILE.selection)
    FILE.psa <- file.path(DIR, "stage0.psa.csv")
    FILE.identity <- file.path(DIR, "stage0.identity.json")
    Done <- character()
    if (file.exists(FILE.psa) || file.exists(FILE.identity)) {
      if (!file.exists(FILE.psa) || !file.exists(FILE.identity)) {
        .stopUsage("Ranking checkpoint requires stage0.psa.csv and stage0.identity.json.", operation = "rankProject")
      }
      Checkpoint <- .readProjectConfig(FILE.identity)
      if (!identical(.jsonGet(Checkpoint, "identity"), Identity) ||
          !identical(.jsonGet(Checkpoint, "sha256"), digest::digest(FILE.psa, algo = "sha256", file = TRUE))) {
        .stopUsage("Ranking checkpoint identity differs from the resolved inputs or recipe.", operation = "rankProject")
      }
      PSA <- .recordFread(FILE.psa)
      .checkTableKeys(PSA, by = character(), columns = c("RecordID", "Tn", "Sa"),
                      keys = c("RecordID", "Tn"), operation = "rankProject")
      Done <- unique(PSA$RecordID)
      if (length(setdiff(Done, Selection$RecordID)) ||
          any(vapply(split(PSA$Tn, PSA$RecordID), function(x) !identical(as.numeric(x), Target[Tn > 0, Tn]), logical(1L)))) {
        .stopUsage("Ranking checkpoint population or grid is incomplete.", operation = "rankProject")
      }
    }
    .mkdir(DIR)
    Todo <- Selection[!RecordID %in% Done]
    Batches <- split(Todo, ceiling(seq_len(nrow(Todo)) / 200L))
    for (Batch in Batches) {
      Package <- .readRecordProducts(keys = Batch, paths = Paths, output = Output)
      PSW <- Package$PSW
      if (Output == "TSW") {
        TSL <- lengthenTSW(Package$TSW, by = intersect(KeyColumns, names(Package$TSW)))
        TSL <- TSL[ID %in% c("AT", "VT", "DT") & OCID == Component]
        PSL <- computeSpectraTSL(TSL, by = intersect(KeyColumns, names(TSL)), xi = Damping,
                                 Tn = Target[Tn > 0, Tn], includeD50 = FALSE, includeD100 = FALSE)
        PSW <- widenPSL(PSL, by = intersect(KeyColumns, names(PSL)))
      }
      COL <- paste("PSA", Component, sep = ".")
      .requireColumns(PSW, c("RecordID", "Tn", COL), "PSW")
      if ("xi" %in% names(PSW)) PSW <- PSW[xi == Damping]
      .checkTableKeys(PSW, by = character(), columns = c("RecordID", "Tn", COL),
                      keys = c("RecordID", "Tn"), operation = "rankProject")
      DT <- rbindlist(lapply(Batch$RecordID, function(x) {
        AUX <- PSW[RecordID == x]
        if (!nrow(AUX)) .stopUsage("Missing PSW for ", x, operation = "rankProject")
        data.table(RecordID = x, Tn = Target[Tn > 0, Tn],
          Sa = .interpSpectrum(AUX$Tn, y = AUX[[COL]], xout = Target[Tn > 0, Tn]))
      }), use.names = TRUE)
      tryCatch({
        fwrite(DT, FILE.psa, append = file.exists(FILE.psa), eol = "\n")
        .writeJSON(list(identity = Identity,
          sha256 = digest::digest(FILE.psa, algo = "sha256", file = TRUE)), FILE.identity, auto_unbox = TRUE)
      }, error = function(e) .stopPartialPublication(conditionMessage(e), operation = "rankProject",
        path = c(FILE.psa, FILE.identity), failures = list(e), result = Done))
      Done <- c(Done, Batch$RecordID)
    }
    PSA <- .recordFread(FILE.psa)
    setnames(PSA, "Sa", paste("PSA", Component, sep = "."))
    Results <- .rankCandidates(PSA, target = Target, keys = Selection, intensity = Intensity,
      metadata = Metadata, OCID = Component, recordCount = RecordCount, oneRecordPerEvent = OneRecordPerEvent)
    tryCatch({
      fwrite(Results$scores, file.path(DIR, "stage0.spectral.csv"), eol = "\n")
      fwrite(Results$selection[, KeyColumns, with = FALSE], FILE.selection, eol = "\n")
    }, error = function(e) .stopPartialPublication(conditionMessage(e), operation = "rankProject",
      path = DIR, failures = list(e), result = Results))
    .projectMessage("Ranking published.", operation = "rankProject", phase = "complete", path = DIR)
    invisible(0L)
  }, error = function(e) .projectFailure(e, operation = "rankProject", path = FILE.config))
}
