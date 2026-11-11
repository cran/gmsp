# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Process selected project records
#'
#' Reads a project JSON contract and publishes record products, process tables,
#' a signed processing recipe and execution evidence.
#'
#' @param file Path to the JSON contract, relative to root or absolute.
#'   The default is gmsp/process.json.
#' @param root Existing project directory used to resolve relative paths.
#'   The default is the caller's working directory.
#' @return Invisible integer 0 after complete publication.
#' @details The contract requires processID, ordered stages, path.selection,
#'   path.stage for execution evidence, and roots for records, index and process.
#'   path.database can supply these roots; explicit paths take precedence.
#'   Scientific controls are explicit in each stage section.
#'
#'   The stages are gmsp, trim, imf, imfTS and psa. Raw processing produces
#'   TSW and IMW; trim replaces these products within the same run, and psa
#'   writes PSW. imf writes IMF and IMFF through decomposeRecords and the
#'   xemd mode metrics. imfTS reads IMF, selects by its explicit criterion,
#'   reintegrates through reconstructRecords and writes TSW and IMW.
#'
#'   imf.method accepts vmd, eemd or a native xemd decomposition name.
#'   vmd requires k, alpha, tau, DC, init, tolerance and maxIterations in imf;
#'   eemd requires k, noise.amp, trials and sifting. Native methods take their
#'   controls in imf.control. Numeric JSON arrays become numeric vectors.
#'   imfTS requires criterion, fmax, kNyq, unitsTarget, windowLength,
#'   overlap, astop0, apass0, astopLP and
#'   apassLP. criterion uses band or energyFraction with an
#'   explicit action; rule target is resolved only by match and is rejected here.
#'   Removal by mode name is rejected. includeResidue defaults
#'   to TRUE. Optional signal controls follow reconstructRecords.
#'
#'   Stochastic imf requires imf.seed, parallel FALSE and workers 1 explicitly.
#'   The process sorts the record keys and calls set.seed once for a nonempty
#'   batch of new records. The caller's resulting random state is retained.
#'   A modal JSON file beside the execution evidence records the seed,
#'   RNGkind, states before and after processing, executed and skipped records,
#'   effective controls and xemd version. Reproducibility is limited to the
#'   same environment, seed and executed order.
#'
#'   Append compares the recipe identity before processing records and skips
#'   complete records already committed under that identity. Writer locks and
#'   metadata transactions protect publication. An incomplete rollback retains
#'   its recovery transaction and record products.
#'
#'   Input errors, processing failures and partial publication raise conditions
#'   instead of returning 0. Failures retain their causes; partial publication
#'   identifies the products or transaction retained for recovery. The operation
#'   preserves the caller's working directory and session options.
#' @examples
#' Root <- tempfile("processProject")
#' Raw <- file.path(Root, "database", "records", "A", "E1", "S1", "raw")
#' dir.create(Raw, recursive = TRUE)
#' Time <- seq(0, 10, by = 0.01)
#' Envelope <- sin(pi * Time / max(Time))^2
#' data.table::fwrite(data.table::data.table(
#'   HNE = 1200 * Envelope * sin(2 * pi * 1.5 * Time),
#'   HNN = 800 * Envelope * sin(2 * pi * 2.5 * Time + 0.4),
#'   HNZ = 400 * Envelope * sin(2 * pi * 4 * Time)), file.path(Raw, "AT.R1.csv"))
#' jsonlite::write_json(list(RecordID = "R1", OwnerID = "A", EventID = "E1",
#'   StationID = "S1", dt = 0.01), file.path(Raw, "AT.R1.json"), auto_unbox = TRUE)
#' data.table::fwrite(data.table::data.table(ProcessID = "raw", RecordID = "R1",
#'   OwnerID = "A", EventID = "E1", StationID = "S1"), file.path(Root, "selection.csv"))
#' # The gmsp stage spells its edge stop amplitude Astop0.
#' jsonlite::write_json(list(processID = "TEST", stages = "gmsp",
#'   parallel = FALSE, workers = 1,
#'   path = list(database = "database", selection = "selection.csv", stage = "stage"),
#'   gmsp = list(fmax = 25, kNyq = 4, flatZeros = FALSE, trimZeros = FALSE,
#'     Astop0 = 0.001, unitsSource = "mm", unitsTarget = "mm", windowLength = 128,
#'     overlap = 75, astopLP = 0.001, apass0 = 0.001, apassLP = 0.98)),
#'   file.path(Root, "process.json"), auto_unbox = TRUE)
#' processProject(file = "process.json", root = Root)
#' list.files(file.path(Root, "database", "process", "TEST"))
#' Intensity <- data.table::fread(file.path(Root, "database", "process", "TEST",
#'                                          "IntensityTable.csv"))
#' print(Intensity[, c("RecordID", "OCID", "PGA", "AI")])
#' unlink(Root, recursive = TRUE)
#' @importFrom data.table %chin% fsetdiff setkeyv
#' @export
processProject <- function(file = file.path("gmsp", "process.json"), root = getwd()) {
  root <- .projectRoot(root)
  Path <- .resolvePath(file, root = root)
  tryCatch({
    Config <- .readProjectConfig(Path)
    ProcessID <- .jsonRequire(Config, "processID", path = Path)
    if (!is.character(ProcessID) || length(ProcessID) != 1L || is.na(ProcessID) ||
        !nzchar(ProcessID) || ProcessID %in% c("raw", ".", "..") || grepl("[/\\\\]", ProcessID)) {
      .stopUsage("processID must identify one non-raw process directory.", operation = "processProject")
    }
    .requireWritableProcess(ProcessID)
    Stages <- .processStages(.jsonRequire(Config, "stages", path = Path))
    # Materialize the scientific recipe before creating evidence or locks.
    .processRecipe(Stages, config = Config)
    Paths <- lapply(c("database", "records", "index", "process"), function(x) {
      Value <- .jsonGet(Config, c("path", x))
      if (is.null(Value)) return(NULL)
      .resolvePath(Value, root = root)
    })
    names(Paths) <- c("database", "records", "index", "process")
    if (is.null(Paths$database) && !is.null(Paths$records) && basename(Paths$records) == "records") {
      Paths$database <- dirname(Paths$records)
    }
    if (is.null(Paths$database) && !is.null(Paths$index) && basename(Paths$index) == "index") {
      Paths$database <- dirname(Paths$index)
    }
    Paths <- .recordPaths(Paths, need = c("records", "index", "process"))
    SelectionPath <- .resolvePath(.jsonRequire(Config, c("path", "selection"), path = Path), root = root)
    Out <- list(stage = .resolvePath(.jsonRequire(Config, c("path", "stage"), path = Path), root = root))
    MaxRecords <- .jsonGet(Config, "maxRecords", Inf)
    if (is.null(MaxRecords)) MaxRecords <- Inf
    if (!is.numeric(MaxRecords) || length(MaxRecords) != 1L || is.na(MaxRecords) ||
        MaxRecords < 1 || (is.finite(MaxRecords) && MaxRecords != floor(MaxRecords))) {
      .stopUsage("maxRecords must be a positive integer or Inf.", operation = "processProject")
    }
    Records <- .readCharacterVector(.jsonGet(Config, "recordID"), "recordID", allowNull = TRUE)
    Parallel <- .readScalarLogical(.jsonGet(Config, "parallel", TRUE), "parallel")
    Workers <- .readWorkers(.jsonGet(Config, "workers"), parallel = Parallel, cap = 4L)
    .processModalExecution(Stages, Config, parallel = Parallel, workers = Workers)
    LogEvery <- .readScalarNumber(.jsonGet(Config, "logEvery", 30), "logEvery")
    if (!is.finite(LogEvery) || LogEvery <= 0) .stopUsage("logEvery must be positive and finite.", operation = "processProject")
    Override <- .readScalarLogical(.jsonGet(Config, "override", FALSE), "override")
    FILE <- if (dir.exists(SelectionPath)) file.path(SelectionPath, "selection.csv") else SelectionPath
    Selection <- .recordFread(FILE)
    .requireColumns(Selection, RecordKeys, "process selection")
    Selection <- .recordNormalizeKeys(Selection)
    .processValidateRecordMap(Selection, "process selection")
    AllowedProcess <- if (identical(Stages[1L], "gmsp")) c("raw", ProcessID) else ProcessID
    if ("ProcessID" %in% names(Selection) &&
        (anyNA(Selection$ProcessID) || any(!Selection$ProcessID %in% AllowedProcess))) {
      .stopUsage("Selection ProcessID contradicts processID.", operation = "processProject")
    }
    Selection <- unique(Selection, by = RecordKeys)
    if (length(Records)) Selection <- Selection[RecordID %chin% Records]
    if (is.finite(MaxRecords) && nrow(Selection) > MaxRecords) Selection <- Selection[seq_len(MaxRecords)]
    if (!nrow(Selection)) .stopUsage("No records selected for processProject.", operation = "processProject")
    DIR <- .processStageDir(out = Out, processID = ProcessID, stage = "process")
    .mkdir(DIR)
    RunID <- gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"))
    Evidence <- file.path(DIR, sprintf("runProcess.%s.%s.json", RunID, Sys.getpid()))
    if (!isTRUE(file.copy(Path, Evidence, overwrite = FALSE))) stop("Could not stage process evidence: ", Evidence, call. = FALSE)
    .projectMessage("Process ", ProcessID, " started", operation = "processProject",
      phase = "processing", count = 0L, total = nrow(Selection), path = Paths$records)
    OUT <- .processRecords(selection = Selection, paths = Paths, processID = ProcessID,
      stages = Stages, config = Config, out = Out, parallel = Parallel,
      workers = Workers, logEvery = LogEvery, override = Override, evidence = Evidence)
    .projectMessage("Process ", ProcessID, " committed ", OUT$appended,
      " new records and skipped ", OUT$skipped, operation = "processProject",
      phase = "complete", count = nrow(Selection), total = nrow(Selection), path = Paths$records)
    invisible(0L)
  }, error = function(e) .projectFailure(e, operation = "processProject", path = Path))
}
