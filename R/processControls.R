# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.processSignalControl <- function(config, stopKey = "astop0") {
  Keys <- c("windowLength", "overlap", "astop0", "apass0",
            "astopLP", "apassLP")
  InputKeys <- Keys
  InputKeys[Keys == "astop0"] <- stopKey
  OUT <- lapply(InputKeys, function(x) .readScalarNumber(.jsonRequire(config, x), x))
  names(OUT) <- Keys
  if (any(!is.finite(unlist(OUT))) || OUT$windowLength <= 0 ||
      OUT$windowLength != floor(OUT$windowLength) || OUT$overlap < 0 ||
      OUT$overlap >= 100 || any(unlist(OUT[setdiff(Keys, "overlap")]) <= 0)) {
    .stopUsage("Invalid signal window/filter controls.", operation = "processProject")
  }
  if (OUT$apassLP >= 1 || OUT$astopLP >= OUT$apassLP) {
    .stopUsage("Signal controls require astopLP < apassLP < 1.",
               operation = "processProject")
  }
  OUT
}

.processLogical <- function(config, key) {
  Value <- .jsonRequire(config, key)
  if (!is.logical(Value) || length(Value) != 1L || is.na(Value)) {
    .stopUsage(key, " must be TRUE or FALSE.", operation = "processProject")
  }
  Value
}

.processSignalParams <- function(config) {
  Fmax <- .readScalarNumber(.jsonRequire(config, "fmax"), "fmax")
  Nyquist <- .jsonRequire(config, "kNyq")
  if (!is.null(Nyquist)) Nyquist <- .readScalarNumber(Nyquist, "kNyq")
  if (!is.finite(Fmax) || Fmax <= 0 || (!is.null(Nyquist) &&
      (!is.finite(Nyquist) || Nyquist <= 0 || !is.finite(Fmax * Nyquist) || round(Fmax * Nyquist) <= 0))) {
    .stopUsage("fmax and kNyq must define a finite positive sample rate.", operation = "processProject")
  }
  Units <- .jsonRequire(config, "unitsTarget")
  .resolveUnitFactor(unitsSource = Units, unitsTarget = Units, ID = "DT")
  list(Fmax = Fmax, kNyq = Nyquist, units = Units)
}

.processGMSPParams <- function(config, paths, processID) {
  Params <- .processSignalParams(config)
  SourceUnits <- .jsonRequire(config, "unitsSource")
  .resolveUnitFactor(unitsSource = SourceUnits, unitsTarget = Params$units, ID = "AT")
  if (.jsonHas(config, "astop0")) {
    .stopUsage("gmsp uses Astop0 for the edge stop amplitude; do not also declare astop0.",
               operation = "processProject")
  }
  Control <- .processSignalControl(config, stopKey = "Astop0")
  MaxRows <- .jsonGet(config, "maxF25Rows", Inf)
  if (!is.numeric(MaxRows) || length(MaxRows) != 1L || is.na(MaxRows) || MaxRows < 1 ||
      (is.finite(MaxRows) && MaxRows != floor(MaxRows))) {
    .stopUsage("maxF25Rows must be a positive integer or Inf.", operation = "processProject")
  }
  list(PATH.records = paths$records, PATHS = paths, ProcessID = processID,
    Fmax = Params$Fmax, kNyq = Params$kNyq,
    flatZeros = .processLogical(config, "flatZeros"),
    trimZeros = .processLogical(config, "trimZeros"),
    Astop0 = Control$astop0, Units.source = SourceUnits,
    Units.target = Params$units, MaxF25Rows = MaxRows, signalControl = Control)
}

.processTrimParams <- function(config) {
  Params <- .processSignalParams(config)
  Quantity <- .jsonRequire(config, "ID")
  if (!is.character(Quantity) || length(Quantity) != 1L || is.na(Quantity) ||
      !Quantity %in% c("AI", "CAV", "CAV5")) {
    .stopUsage("trim.ID must be AI, CAV or CAV5.", operation = "processProject")
  }
  Taper <- .readScalarNumber(.jsonRequire(config, "taper"), "trim.taper")
  Astop <- .readScalarNumber(.jsonRequire(config, "astop"), "trim.astop")
  Apass <- .readScalarNumber(.jsonRequire(config, "apass"), "trim.apass")
  if (!is.finite(Taper) || Taper < 0 || !is.finite(Astop) ||
      !is.finite(Apass) || Astop <= 0 || Astop >= Apass || Apass >= 1) {
    .stopUsage("Invalid trim taper or ramp amplitudes.", operation = "processProject")
  }
  list(ID = Quantity, DIR = .readCharacterVector(.jsonRequire(config, "DIR"), "trim.DIR"),
    range = .validTrimRange(.jsonRequire(config, "range"), "trim.range"),
    taper = Taper, astop = Astop, apass = Apass,
    Fmax = Params$Fmax, kNyq = Params$kNyq, units = Params$units,
    signalControl = .processSignalControl(config))
}

.processControlVectors <- function(control) {
  if (!is.list(control)) return(control)
  if (is.null(names(control)) && length(control) &&
      all(vapply(control, function(x) is.numeric(x) && !is.complex(x) && length(x) == 1L, logical(1L)))) {
    return(unlist(control, use.names = FALSE))
  }
  lapply(control, .processControlVectors)
}

.processIMFParams <- function(config) {
  Method <- .jsonRequire(config, "method")
  if (!is.character(Method) || length(Method) != 1L || is.na(Method) || !nzchar(Method)) {
    .stopUsage("imf.method must name one decomposition method.", operation = "processProject")
  }
  Keys <- switch(Method, vmd = c("k", "alpha", "tau", "DC", "init", "tolerance", "maxIterations"),
                 eemd = c("k", "noise.amp", "trials", "sifting"), "control")
  Unknown <- setdiff(names(config), c("method", Keys, "seed", "maxF25Rows"))
  if (length(Unknown)) .stopUsage("Unsupported imf field: ", paste(Unknown, collapse = ", "), operation = "processProject")
  Control <- if (identical(Keys, "control")) .jsonRequire(config, "control") else {
    stats::setNames(lapply(Keys, function(x) .jsonRequire(config, x)), Keys)
  }
  Control <- .processControlVectors(Control)
  Native <- .checkDecompositionControl(Method, Control)
  Operations <- xemd::listOperations()
  Entry <- Operations[vapply(Operations, function(x) identical(x$name, Native), logical(1L))]
  if (length(Entry) != 1L) .stopUsage("Decomposition method is absent from xemd operations.", operation = "processProject")
  Random <- any(c("noise", "noiseChannels") %in% Entry[[1L]]$arguments)
  Seed <- if (Random) .jsonRequire(config, "seed") else .jsonGet(config, "seed")
  if ((Random || .jsonHas(config, "seed")) &&
      (!is.numeric(Seed) || is.complex(Seed) || length(Seed) != 1L || !is.finite(Seed) ||
       Seed != floor(Seed) || abs(Seed) > .Machine$integer.max)) {
    .stopUsage("imf.seed must be one representable integer.", operation = "processProject")
  }
  MaxRows <- .jsonGet(config, "maxF25Rows", Inf)
  if (!is.numeric(MaxRows) || length(MaxRows) != 1L || is.na(MaxRows) || MaxRows < 1 ||
      (is.finite(MaxRows) && MaxRows != floor(MaxRows))) {
    .stopUsage("imf.maxF25Rows must be a positive integer or Inf.", operation = "processProject")
  }
  list(Method = Method, kIMF = if (Method %in% c("vmd", "eemd")) Control$k else NULL,
       control = Control, seed = Seed, stochastic = Random,
       RNGkind = if (Random) RNGkind() else NULL,
       xemd = as.character(utils::packageVersion("xemd")))
}

.processIMFTSParams <- function(config) {
  if (.jsonHas(config, "remove")) {
    .stopUsage("imfTS.remove is unsupported; supply imfTS.criterion.", operation = "processProject")
  }
  Params <- .processSignalParams(config)
  Criterion <- .jsonRequire(config, "criterion")
  if (is.list(Criterion) && identical(Criterion$rule, "target")) {
    .stopUsage("criterion rule target is resolved only by match; process requires band or energyFraction.",
               operation = "processProject")
  }
  .checkModeCriterion(Criterion)
  Control <- c(list(unitsTarget = Params$units, fmax = Params$Fmax, kNyq = Params$kNyq),
               .processSignalControl(config))
  Optional <- c("flatZeros", "trimZeros", "removeMean", "forceRegularization", "includeWarnings")
  for (Key in intersect(Optional, names(config))) Control[[Key]] <- .processLogical(config, Key)
  .checkReconstructionControl(Control)
  Include <- if (.jsonHas(config, "includeResidue")) .processLogical(config, "includeResidue") else TRUE
  list(criterion = Criterion, includeResidue = Include, Fmax = Params$Fmax,
       kNyq = Params$kNyq, units = Params$units, signalControl = Control)
}

.processPSAParams <- function(config) {
  Ratios <- unlist(.jsonRequire(config, "xi"), use.names = FALSE)
  if (!is.numeric(Ratios) || !length(Ratios) || any(!is.finite(Ratios)) || any(Ratios < 0 | Ratios > 1)) {
    .stopUsage("psa.xi must contain damping ratios in [0,1].", operation = "processProject")
  }
  D50 <- .processLogical(config, "D50")
  D100 <- .processLogical(config, "D100")
  Angles <- .jsonGet(config, "nTheta")
  if (D50 || D100 || !is.null(Angles)) {
    Angles <- .readScalarNumber(.jsonRequire(config, "nTheta"), "psa.nTheta")
    if (!is.finite(Angles) || Angles < 2 || Angles != floor(Angles)) {
      .stopUsage("psa.nTheta must be an integer of at least two.", operation = "processProject")
    }
  }
  list(XI = Ratios, Tn = .tnGrid(config, sourceJson = list()), D50 = D50,
       D100 = D100, nTheta = Angles)
}

.tnGrid <- function(config, sourceJson) {
  if (.jsonHas(config, c("Tn", "values"))) {
    OUT <- unlist(.jsonRequire(config, c("Tn", "values")), use.names = FALSE)
  } else {
    Start <- .readScalarNumber(.jsonRequire(config, c("Tn", "min")), "Tn.min")
    End <- .readScalarNumber(.jsonRequire(config, c("Tn", "max")), "Tn.max")
    n <- .readScalarNumber(.jsonRequire(config, c("Tn", "n")), "Tn.n")
    if (!is.finite(Start) || !is.finite(End) || Start <= 0 || Start >= End ||
        !is.finite(n) || n < 2 || n != floor(n)) {
      .stopUsage("Tn requires increasing positive bounds and an integer n >= 2.", operation = "processProject")
    }
    OUT <- 10^seq(log10(Start), log10(End), length.out = n)
  }
  if (!is.numeric(OUT) || !length(OUT) || any(!is.finite(OUT)) || any(OUT <= 0)) {
    .stopUsage("Tn.values must contain finite positive periods.", operation = "processProject")
  }
  OUT
}
