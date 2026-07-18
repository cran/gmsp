#' Run a gmsp batch pipeline
#'
#' `gmsp` helpers such as [AT2TS()], [TSL2PS()], [TS2IMF()] and
#' [getIntensity()] work directly on R objects. The `run*()` functions are
#' optional batch runners for the same kind of work when the inputs and outputs
#' are files controlled by a project JSON file.
#'
#' Use the direct helpers for one record, one table, or interactive analysis.
#' Use a runner when the same operation must be repeated over a declared
#' selection of records and the output must be reproducible from a saved JSON
#' contract.
#'
#' @section Runner map:
#' \describe{
#'   \item{`runSelect()`}{Builds a record selection table. The helper-level
#'   contract is [selectRecords()] plus project-owned metadata.}
#'   \item{`runStage0()`}{Builds a spectrally compatible record selection from
#'   the processed candidate pool: scores PSA against the target
#'   compatibility band declared in the match JSON, applies optional
#'   seismological screens (`candidate.pool`, `candidate.filter`,
#'   `candidate.exclude`, `candidate.oneRecordPerEvent`), and writes the
#'   top-`stage0.N` selection to `path.selection` together with the score
#'   table and a reusable spectra cache.}
#'   \item{`runGMSP()`}{Processes raw acceleration, velocity, or displacement
#'   records into canonical time histories. The helper-level contracts are
#'   [AT2TS()], [VT2TS()] and [DT2TS()].}
#'   \item{`runTrim()`}{Applies an approved time-windowing contract to existing
#'   time histories.}
#'   \item{`runIMF()`}{Builds intrinsic-mode-function products from processed
#'   time histories. The helper-level contract is [TS2IMF()].}
#'   \item{`runPSA()`}{Builds elastic response spectra from processed time
#'   histories. The helper-level contract is [TSL2PS()].}
#'   \item{`runProcess()`}{Runs an approved composed processing contract for a
#'   project record set.}
#'   \item{`runMatch()`}{Runs an approved spectral-match contract for a selected
#'   record set. This is an advanced batch workflow, not a replacement for the
#'   response-spectrum helpers.}
#'   \item{`runPlot()`}{Builds optional raw QA/debug HTML widgets from existing
#'   products. It is not an SRK publication renderer.}
#'   \item{`runExport()`}{Packages existing products, metadata, and approved
#'   audit files into a delivery folder.}
#' }
#'
#' @section JSON and paths:
#' Each runner reads one JSON file. Paths inside that JSON are resolved relative
#' to `root`, normally the project root. The JSON file is a project input, not a
#' global package option.
#'
#' Installed template JSON files live under
#' `system.file("scripts", package = "gmsp")`. A project may keep simpler
#' names such as `gmsp/select.json` or pass an explicit template-style path such
#' as `gmsp/runSelect.json`.
#'
#' @section Plotting and SRK reports:
#' `runPlot()` writes raw self-contained QA/debug widgets. SRK client plots are
#' rendered outside `gmsp` by the report layer from materialized `gmsp` CSV
#' products. Do not use `runPlot()` output as final SRK report output.
#'
#' @param file character. Path to the pipeline JSON file.
#' @param root character. Project root used to resolve relative JSON paths.
#'
#' @return Invisibly returns the runner exit status.
#'
#' @examples
#' \dontrun{
#' # Direct helper use for one in-memory table:
#' TSL <- AT2TS(DT, units.source = "mm", Fmax = 25, output = "TSL")
#'
#' # Batch equivalent for a project-controlled record set:
#' runGMSP(file = "gmsp/gmsp.json", root = ".")
#'
#' # Compute spectra for an existing batch product:
#' runPSA(file = "gmsp/psa.json", root = ".")
#' }
#' @name pipelineRunners
NULL

.pipelineScriptRoot <- function() {
  PATH <- system.file("scripts", package = "gmsp", mustWork = FALSE)
  if (!nzchar(PATH) || !dir.exists(PATH)) {
    stop("Installed gmsp package does not contain pipeline scripts. Reinstall gmsp.",
         call. = FALSE)
  }
  normalizePath(PATH, winslash = "/", mustWork = TRUE)
}

.runPipelineScript <- function(stage, script, file, root) {
  FILE <- as.character(file)
  ROOT <- normalizePath(as.character(root), winslash = "/", mustWork = TRUE)
  if (length(FILE) != 1L || is.na(FILE) || !nzchar(FILE)) {
    stop("file must be one non-empty string.", call. = FALSE)
  }
  if (length(ROOT) != 1L || is.na(ROOT) || !nzchar(ROOT)) {
    stop("root must be one non-empty string.", call. = FALSE)
  }
  SCRIPT.root <- .pipelineScriptRoot()
  SCRIPT <- file.path(SCRIPT.root, stage, script)
  if (!file.exists(SCRIPT)) {
    stop(sprintf("Missing gmsp pipeline script: %s", SCRIPT), call. = FALSE)
  }

  WD <- getwd()
  setwd(ROOT)
  on.exit(setwd(WD), add = TRUE)
  ENV <- c(
    sprintf("GMSP_SCRIPT_ROOT=%s", SCRIPT.root),
    sprintf("GMSP_PROJECT_ROOT=%s", ROOT)
  )
  STATUS <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(SCRIPT, FILE),
    env = ENV
  )
  if (!identical(STATUS, 0L)) {
    stop(sprintf("gmsp %s pipeline failed with status %s.", stage, STATUS),
         call. = FALSE)
  }
  invisible(STATUS)
}

#' @rdname pipelineRunners
#' @export
runSelect <- function(file = file.path("gmsp", "select.json"),
                      root = getwd()) {
  .runPipelineScript(stage = "selection", script = "runSelect.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runGMSP <- function(file = file.path("gmsp", "gmsp.json"),
                    root = getwd()) {
  .runPipelineScript(stage = "gmsp", script = "runGMSP.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runTrim <- function(file = file.path("gmsp", "trim.json"),
                    root = getwd()) {
  .runPipelineScript(stage = "trim", script = "runTrim.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runIMF <- function(file = file.path("gmsp", "imf.json"),
                   root = getwd()) {
  .runPipelineScript(stage = "imf", script = "runIMF.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runPSA <- function(file = file.path("gmsp", "psa.json"),
                   root = getwd()) {
  .runPipelineScript(stage = "psa", script = "runPSA.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runProcess <- function(file = file.path("gmsp", "process.json"),
                       root = getwd()) {
  .runPipelineScript(stage = "process", script = "runProcess.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runStage0 <- function(file = file.path("gmsp", "match.json"),
                      root = getwd()) {
  .runPipelineScript(stage = "selection", script = "runStage0.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runMatch <- function(file = file.path("gmsp", "match.json"),
                     root = getwd()) {
  .runPipelineScript(stage = "match", script = "runMatch.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runPlot <- function(file = file.path("gmsp", "plot.json"),
                    root = getwd()) {
  .runPipelineScript(stage = "plot", script = "runPlot.R",
                     file = file, root = root)
}

#' @rdname pipelineRunners
#' @export
runExport <- function(file = file.path("gmsp", "export.json"),
                      root = getwd()) {
  .runPipelineScript(stage = "export", script = "runExport.R",
                     file = file, root = root)
}
