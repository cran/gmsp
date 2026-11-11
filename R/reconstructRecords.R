# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.checkModeCriterion <- function(criterion) {
  if (!is.list(criterion) || is.null(names(criterion)) ||
      anyNA(names(criterion)) || anyDuplicated(names(criterion)) ||
      !is.character(criterion$rule) || length(criterion$rule) != 1L ||
      is.na(criterion$rule) || !criterion$rule %in% c("band", "energyFraction")) {
    .stopUsage("criterion must declare rule band or energyFraction.",
               operation = "reconstructRecords")
  }
  Fields <- c("rule", "action")
  if (criterion$rule == "band") Fields <- c(Fields, "fMin", "fMax")
  if (criterion$rule == "energyFraction") Fields <- c(Fields, "minimum")
  if (!setequal(names(criterion), Fields) ||
      !is.character(criterion$action) || length(criterion$action) != 1L ||
      is.na(criterion$action) || !criterion$action %in% c("keep", "remove")) {
    .stopUsage("criterion requires action keep/remove and exactly the fields of its rule.",
               operation = "reconstructRecords")
  }
  for (j in setdiff(Fields, c("rule", "action"))) {
    x <- criterion[[j]]
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
      .stopUsage("criterion ", j, " must be finite and nonnegative.",
                 operation = "reconstructRecords")
    }
  }
  if (criterion$rule == "band" && criterion$fMin > criterion$fMax) {
    .stopUsage("criterion fMin must not exceed fMax.", operation = "reconstructRecords")
  }
  invisible(NULL)
}

.checkReconstructionControl <- function(signalControl) {
  Required <- c("unitsTarget", "fmax", "windowLength", "overlap",
    "astop0", "apass0", "astopLP", "apassLP")
  Logical <- c("flatZeros", "trimZeros", "removeMean", "forceRegularization",
    "includeWarnings")
  if (!is.list(signalControl) || is.null(names(signalControl)) ||
      anyNA(names(signalControl)) || anyDuplicated(names(signalControl)) ||
      !all(Required %in% names(signalControl)) ||
      !all(names(signalControl) %in% c(Required, "kNyq", Logical))) {
    .stopUsage("signalControl requires unitsTarget, fmax and signal window/filter controls, without duplicate or unknown fields.",
               operation = "reconstructRecords")
  }
  x <- signalControl$unitsTarget
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !tolower(x) %in% c("mm", "cm", "m")) {
    .stopUsage("signalControl unitsTarget must be mm, cm or m.", operation = "reconstructRecords")
  }
  Fields <- setdiff(Required, c("unitsTarget", "overlap"))
  if (!is.null(signalControl$kNyq)) Fields <- c(Fields, "kNyq")
  for (j in Fields) {
    x <- signalControl[[j]]
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
      .stopUsage("signalControl ", j, " must be finite and positive.",
                 operation = "reconstructRecords")
    }
  }
  if (signalControl$windowLength != floor(signalControl$windowLength)) {
    .stopUsage("signalControl windowLength must be an integer.", operation = "reconstructRecords")
  }
  x <- signalControl$overlap
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || x >= 100) {
    .stopUsage("signalControl overlap must be in [0,100).", operation = "reconstructRecords")
  }
  if (!is.null(signalControl$kNyq) &&
      (!is.finite(signalControl$kNyq * signalControl$fmax) ||
       round(signalControl$kNyq * signalControl$fmax) <= 0)) {
    .stopUsage("round(kNyq * fmax) must be finite and positive.", operation = "reconstructRecords")
  }
  for (j in intersect(Logical, names(signalControl))) {
    x <- signalControl[[j]]
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      .stopUsage("signalControl ", j, " must be one logical value.", operation = "reconstructRecords")
    }
  }
  invisible(NULL)
}

#' Reconstruct acceleration records from selected modes
#'
#' Measures and selects modes through xemd, reconstructs acceleration, and
#' reintegrates it with buildTSL using isRaw = FALSE.
#'
#' @param decomposition Native list returned by decomposeRecords, containing
#'   signal and modes tables with ID AT, OCID, t in seconds, s and metadata.
#'   Samples use the length basis declared by signalControl$unitsTarget.
#'   An optional residue or fitResidual supplies the remainder; both together
#'   are an error.
#' @param criterion Named list with exactly rule, action and the rule fields.
#'   Rule "band" requires finite fMin and fMax in Hz with 0 <= fMin <= fMax;
#'   "energyFraction" requires finite minimum >= 0.
#'   Action is "keep" or "remove"; selection by mode index is not admitted.
#' @param by Explicit character vector of metadata columns identifying a
#'   record; character() declares one record without grouping metadata.
#'   Metadata must be constant within each record/component/quantity series.
#'   Only the metadata selected by by is retained in the returned TSL.
#' @param signalControl Named list requiring unitsTarget, fmax, windowLength,
#'   overlap, astop0, apass0, astopLP and
#'   apassLP, with the units and conditions of buildTSL.
#'   Optional kNyq, flatZeros, trimZeros, removeMean, forceRegularization and
#'   includeWarnings retain their buildTSL defaults.
#'   Duplicate or other fields are errors; Units metadata, when present,
#'   must agree with unitsTarget.
#' @param includeResidue Logical scalar, TRUE by default, including residue
#'   or fitResidual when present; an absent remainder contributes nothing.
#'
#' @return A list with TSL, the reintegrated AT/VT/DT table; metrics, the
#'   freshly measured native mode metrics; and selection, the native decision
#'   table for that decomposition.
#'
#' @details Selection applies separately by OCID and retains native mode
#'   identities; the closed band compares frequency without bandwidth.
#'   A zero-energy mode has no frequency and is outside a frequency band.
#'   Reconstruction sums retained modes and the requested remainder before
#'   reintegration; meanNoise is not added again.
#'   Zero reconstructed acceleration energy is an error before buildTSL.
#'   Reintegration can taper, resample or remove means according to the
#'   supplied signal controls, so its histories need not equal the modal sum.
#'
#'   Invalid data or controls cause errors; dependency conditions retain
#'   their original classes, cause and group when supplied.
#'   Inputs are preserved; the operation reads or writes no files and uses
#'   no random numbers.
#'
#' @seealso [decomposeRecords()], [buildTSL()], [xemd::selectModesTSL()],
#'   [xemd::reconstructTSL()]
#' @examples
#' Time <- seq(0, 9, length.out = 1000)
#' TSL <- data.table::data.table(RecordID = "R1", OCID = "H1", ID = "AT", t = Time,
#'   s = sin(pi * Time) + sin(2 * pi * Time) + sin(6 * pi * Time) + 0.5 * Time,
#'   Units = "mm")
#' Decomposition <- decomposeRecords(TSL, method = "vmd",
#'   control = list(k = 3, alpha = 2000, tau = 0.5, DC = FALSE, init = 0,
#'                  tolerance = 1e-6, maxIterations = 98),
#'   by = "RecordID")
#' Control <- list(unitsTarget = "mm", fmax = 25, kNyq = 4, windowLength = 128,
#'                 overlap = 75, astop0 = 1e-3, apass0 = 1e-3, astopLP = 1e-3,
#'                 apassLP = 0.98, includeWarnings = FALSE)
#' Reconstruction <- reconstructRecords(Decomposition,
#'   criterion = list(rule = "band", fMin = 2, fMax = 15, action = "keep"),
#'   by = "RecordID", signalControl = Control)
#' print(Reconstruction$selection)
#' head(Reconstruction$TSL)
#' unique(Reconstruction$TSL$ID)
#' @export
reconstructRecords <- function(decomposition, criterion, by, signalControl,
                               includeResidue = TRUE) {
  tryCatch({
    .checkModeCriterion(criterion)
    .checkReconstructionControl(signalControl)
    .checkModalQuantity(decomposition)
    if (!is.logical(includeResidue) || length(includeResidue) != 1L || is.na(includeResidue)) {
      .stopUsage("includeResidue must be one logical value.", operation = "reconstructRecords")
    }
    Signal <- data.table::as.data.table(decomposition$signal)
    .checkTableKeys(Signal, by = by, columns = c("OCID", "ID", "t", "s"),
      keys = c(by, "OCID", "ID", "t"), operation = "reconstructRecords")
    if (!nrow(Signal)) .stopUsage("decomposition has no signal samples.", operation = "reconstructRecords")
    Keys <- setdiff(names(Signal), c("t", "s"))
    DT <- unique(Signal[, Keys, with = FALSE])
    if (anyDuplicated(DT, by = c(by, "OCID", "ID"))) {
      .stopUsage("Metadata must be constant within each by/OCID/ID series.",
                 operation = "reconstructRecords")
    }
    if ("Units" %in% names(Signal) &&
        (!is.character(Signal[["Units"]]) || anyNA(Signal[["Units"]]) ||
         any(tolower(Signal[["Units"]]) != tolower(signalControl$unitsTarget)))) {
      .stopUsage("Units metadata must agree with unitsTarget.", operation = "reconstructRecords")
    }
    Metrics <- xemd::measureModesTSL(decomposition)$metrics
    Selection <- xemd::selectModesTSL(decomposition, metrics = Metrics,
      criterion = criterion)$selection
    TSL <- xemd::reconstructTSL(decomposition, selection = Selection,
      includeResidue = includeResidue)$reconstruction
    Keys <- setdiff(names(TSL), c("t", "s"))
    DT <- TSL[, .(zero = all(s == 0)), by = Keys]
    if (any(DT[["zero"]])) {
      DT <- DT[zero == TRUE, Keys, with = FALSE]
      e <- structure(list(message = "Reconstructed acceleration has zero energy.",
        call = NULL, operation = "reconstructRecords", unit = DT),
        class = c("gmspZeroEnergy", "gmspFailure", "moduleFailure", "error", "condition"))
      stop(e)
    }
    TSL <- do.call(buildTSL, c(list(TSL = TSL, by = by, isRaw = FALSE), signalControl))
    list(TSL = TSL, metrics = Metrics, selection = Selection)
  }, error = function(e) {
    if (is.null(e$cause)) e$cause <- e
    if (is.null(e$operation)) e$operation <- "reconstructRecords"
    class(e) <- unique(c(class(e), "gmspFailure", "moduleFailure"))
    stop(e)
  })
}
