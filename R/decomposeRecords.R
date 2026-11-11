# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.checkDecompositionControl <- function(method, control) {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !method %in% c("vmd", "eemd", xemd::listDecompositions())) {
    .stopUsage("method must be vmd, eemd, or a name from xemd::listDecompositions().",
               operation = "decomposeRecords")
  }
  if (!is.list(control) || (length(control) &&
      (is.null(names(control)) || anyNA(names(control)) ||
       any(!nzchar(names(control))) || anyDuplicated(names(control))))) {
    .stopUsage("control must be a named list without duplicate names.",
               operation = "decomposeRecords")
  }
  if (length(intersect(names(control), c("signal", "samplingInterval", "noise", "noiseChannels")))) {
    .stopUsage("signal, samplingInterval, noise, and noiseChannels are derived from TSL.",
               operation = "decomposeRecords")
  }
  Required <- character()
  if (method == "vmd") Required <- c("k", "alpha", "tau", "DC", "init", "tolerance", "maxIterations")
  if (method == "eemd") Required <- c("k", "noise.amp", "trials", "sifting")
  if (length(Required)) {
    if (!setequal(names(control), Required)) {
      .stopUsage(method, " control requires exactly: ", paste(Required, collapse = ", "), ".",
                 operation = "decomposeRecords")
    }
    COLS <- "k"
    if (method == "vmd") COLS <- c(COLS, "alpha", "tau", "init", "tolerance", "maxIterations")
    if (method == "eemd") COLS <- c(COLS, "noise.amp", "trials")
    for (j in COLS) {
      x <- control[[j]]
      if (!is.numeric(x) || is.complex(x) || length(x) != 1L || !is.finite(x)) {
        .stopUsage(j, " must be one finite number.", operation = "decomposeRecords")
      }
    }
    COLS <- "k"
    if (method == "vmd") COLS <- c(COLS, "maxIterations")
    if (method == "eemd") COLS <- c(COLS, "trials")
    for (j in COLS) {
      if (control[[j]] < 1 || control[[j]] != floor(control[[j]])) {
        .stopUsage(j, " must be a positive integer.", operation = "decomposeRecords")
      }
    }
  }
  if (method == "vmd") {
    if (control$alpha <= 0 || control$tau < 0 || control$tolerance <= 0 || control$init != 0) {
      .stopUsage("vmd requires alpha > 0, tau >= 0, tolerance > 0, and init = 0.",
                 operation = "decomposeRecords")
    }
    if (!is.logical(control$DC) || length(control$DC) != 1L || is.na(control$DC)) {
      .stopUsage("DC must be one logical value.", operation = "decomposeRecords")
    }
    return("decomposeVMD")
  }
  if (method == "eemd") {
    if (control$noise.amp < 0) {
      .stopUsage("noise.amp must be nonnegative.", operation = "decomposeRecords")
    }
    Sifting <- control$sifting
    if (!is.list(Sifting) || is.null(names(Sifting)) || anyNA(names(Sifting)) ||
        any(!nzchar(names(Sifting))) || anyDuplicated(names(Sifting)) ||
        !is.character(Sifting$stopRule) || length(Sifting$stopRule) != 1L ||
        is.na(Sifting$stopRule) ||
        !Sifting$stopRule %in% c("SD", "Snumber", "doubleThreshold", "fixed")) {
      .stopUsage("sifting must explicitly declare an EEMD stopRule and named controls.",
                 operation = "decomposeRecords")
    }
    Required <- switch(Sifting$stopRule, SD = c("tolerance", "maxSift", "boundary"),
      Snumber = c("stableSift", "maxSift", "boundary"), doubleThreshold = character(),
      fixed = "fixedSift")
    Allowed <- c("stopRule", "maxModes", "boundary", Required)
    if (Sifting$stopRule == "doubleThreshold") Allowed <- c(Allowed, "thresholds", "fraction", "maxSift")
    if (!all(Required %in% names(Sifting)) || !all(names(Sifting) %in% Allowed)) {
      .stopUsage("sifting must contain the controls of its declared stopRule.",
                 operation = "decomposeRecords")
    }
    if ("boundary" %in% names(Sifting) &&
        (!is.character(Sifting$boundary) || length(Sifting$boundary) != 1L ||
         is.na(Sifting$boundary) || !Sifting$boundary %in% c("reflection", "extrapolation"))) {
      .stopUsage("sifting boundary must be reflection or extrapolation.", operation = "decomposeRecords")
    }
    if ("maxModes" %in% names(Sifting) &&
        (!is.numeric(Sifting$maxModes) || is.complex(Sifting$maxModes) ||
         length(Sifting$maxModes) != 1L || is.na(Sifting$maxModes) ||
         Sifting$maxModes != control$k)) {
      .stopUsage("sifting maxModes must equal k when supplied.", operation = "decomposeRecords")
    }
    return("decomposeEEMD")
  }
  method
}

.translateControl <- function(method, control, samplingInterval, signal) {
  Method <- .checkDecompositionControl(method, control)
  if (method == "vmd") {
    if (!is.numeric(samplingInterval) || is.complex(samplingInterval) ||
        length(samplingInterval) != 1L || !is.finite(samplingInterval) || samplingInterval <= 0) {
      .stopUsage("The effective samplingInterval must be finite and positive.",
                 operation = "decomposeRecords")
    }
    return(list(method = Method, control = list(initialCenters = rep(0, control$k),
      alpha = control$alpha * samplingInterval^2 / (8 * pi^2), tau = control$tau,
      fixedZeroCenter = control$DC, tolerance = control$tolerance,
      maxIterations = control$maxIterations)))
  }
  if (method == "eemd") {
    if (!is.numeric(signal) || is.complex(signal) || !is.null(dim(signal)) ||
        length(signal) < 2L || any(!is.finite(signal))) {
      .stopUsage("The effective signal must be a finite numeric vector.", operation = "decomposeRecords")
    }
    Deviation <- stats::sd(signal)
    if (!is.finite(Deviation) || Deviation <= 0) {
      .stopUsage("eemd requires positive finite sd of the effective signal.", operation = "decomposeRecords")
    }
    Sifting <- control$sifting
    Sifting$maxModes <- control$k
    return(list(method = Method, control = list(realizationCount = control$trials,
      amplitudeRatio = control$noise.amp / Deviation, divisor = "realizations", sifting = Sifting)))
  }
  list(method = Method, control = control)
}

decomposeRecords <- function(TSL, method, control, by) {
  tryCatch({
    Method <- .checkDecompositionControl(method, control)
    if (!data.table::is.data.table(TSL)) {
      .stopUsage("TSL must be a data.table.", operation = "decomposeRecords")
    }
    Columns <- c("t", "s", "ID", "OCID")
    if (!is.character(by) || anyNA(by) || any(!nzchar(by)) || anyDuplicated(by) ||
        length(intersect(by, Columns))) {
      .stopUsage("by must contain distinct metadata column names.", operation = "decomposeRecords")
    }
    if (anyDuplicated(names(TSL)) || !all(c(by, Columns) %in% names(TSL))) {
      .stopUsage("TSL requires unique column names and the by, t, s, ID, OCID columns.",
                 operation = "decomposeRecords")
    }
    for (j in c("ID", "OCID")) {
      if (!is.character(TSL[[j]]) || anyNA(TSL[[j]]) || any(!nzchar(TSL[[j]]))) {
        .stopUsage(j, " must contain nonempty character values.", operation = "decomposeRecords")
      }
    }
    Reserved <- c("mode", "iterations", "converged", "stopReason", "innerStopReason", "frequency", "boundary")
    if (Method == "decomposeEEMD") Reserved <- c(Reserved, "realization")
    if (Method == "decomposeCEEMD") Reserved <- c(Reserved, "realization", "sign")
    if (Method == "decomposeMaskedEMD") Reserved <- c(Reserved, "phase")
    if (Method == "decomposeBEMD") Reserved <- c(Reserved, "direction")
    if (length(intersect(names(TSL), Reserved))) {
      .stopUsage("TSL metadata uses a name reserved by xemd: ",
        paste(intersect(names(TSL), Reserved), collapse = ", "), ".", operation = "decomposeRecords")
    }
    DATA <- TSL[TSL[["ID"]] == "AT"]
    if (!nrow(DATA)) .stopUsage("TSL has no AT samples.", operation = "decomposeRecords")
    LIST <- list(DATA)
    if (length(by)) LIST <- split(DATA, by = by, keep.by = TRUE, sorted = TRUE)
    Intervals <- numeric(length(LIST))
    Metadata <- setdiff(names(DATA), c("t", "s", "ID", "OCID"))
    for (i in seq_along(LIST)) {
      DT <- LIST[[i]]
      Context <- "record without metadata"
      if (length(by)) Context <- paste(paste0(by, "=", vapply(DT[1L, by, with = FALSE],
        as.character, character(1L))), collapse = ", ")
      LIST[[i]] <- tryCatch({
        for (j in c("t", "s")) {
          if (!is.numeric(DT[[j]]) || is.complex(DT[[j]]) || any(!is.finite(DT[[j]]))) {
            .stopUsage(j, " must contain finite real numeric values.", operation = "decomposeRecords")
          }
        }
        data.table::setorderv(DT, c("OCID", "t"), na.last = TRUE)
        Channels <- unique(DT[["OCID"]])
        if (!all(Channels %in% c("H1", "H2", "UP"))) {
          .stopUsage("AT components must be H1, H2, or UP.", operation = "decomposeRecords")
        }
        .checkTableKeys(DT, by = by, columns = Columns,
                        keys = c(by, "OCID", "ID", "t"), operation = "decomposeRecords")
        Time <- DT[["t"]][DT[["OCID"]] == Channels[1L]]
        if (length(Time) < 3L || any(diff(Time) <= 0)) {
          .stopUsage("Each component requires at least three increasing times.", operation = "decomposeRecords")
        }
        if (nrow(DT) != length(Time) * length(Channels) ||
            !identical(DT[["t"]], rep(Time, length(Channels)))) {
          .stopUsage("Components must share one time grid.", operation = "decomposeRecords")
        }
        COLS <- setdiff(Metadata, by)
        if (length(COLS)) {
          AUX <- DT[, lapply(.SD, data.table::uniqueN), by = "OCID", .SDcols = COLS]
          if (any(as.matrix(AUX[, COLS, with = FALSE]) != 1L)) {
            .stopUsage("Metadata must be constant within each component.", operation = "decomposeRecords")
          }
        }
        Grid <- .regularizeSeries(t = Time,
          s = matrix(DT[["s"]], nrow = length(Time), dimnames = list(NULL, Channels)))
        Intervals[i] <- Grid$samplingInterval
        data.table::rbindlist(lapply(seq_along(Channels), function(j) {
          AUX <- DT[DT[["OCID"]] == Channels[j]][rep(1L, length(Grid$t)),
            c(Metadata, "OCID", "ID"), with = FALSE]
          data.table::set(AUX, j = "t", value = Grid$t)
          data.table::set(AUX, j = "s", value = Grid$s[, j])
          AUX
        }), use.names = TRUE)
      }, error = function(e) {
        e$message <- paste0(Context, ": ", conditionMessage(e))
        if (is.null(e$unit)) e$unit <- Context
        stop(e)
      })
    }
    Calls <- list()
    if (method == "vmd") {
      Calls <- lapply(sort(unique(Intervals)), function(x) {
        DT <- data.table::rbindlist(LIST[Intervals == x], use.names = TRUE)
        data.table::setorderv(DT, c(by, "OCID", "t"), na.last = TRUE)
        list(data = DT, parameters = .translateControl(method, control,
          samplingInterval = x, signal = NULL))
      })
    }
    if (method == "eemd") {
      DATA <- data.table::rbindlist(LIST, use.names = TRUE)
      Calls <- lapply(split(DATA, by = c(by, "OCID"), keep.by = TRUE, sorted = TRUE), function(x) {
        tryCatch(list(data = x, parameters = .translateControl(method, control,
          samplingInterval = x[["t"]][2L] - x[["t"]][1L], signal = x[["s"]])),
          error = function(e) {
            Context <- paste(paste0(c(by, "OCID"), "=", vapply(x[1L, c(by, "OCID"), with = FALSE],
              as.character, character(1L))), collapse = ", ")
            e$message <- paste0(Context, ": ", conditionMessage(e))
            if (is.null(e$unit)) e$unit <- Context
            stop(e)
          })
      })
    }
    if (!method %in% c("vmd", "eemd")) {
      Calls <- list(list(data = data.table::rbindlist(LIST, use.names = TRUE),
        parameters = .translateControl(method, control, samplingInterval = NULL, signal = NULL)))
    }
    OUT <- lapply(Calls, function(x) xemd::decomposeTSL(x$data,
      method = x$parameters$method, control = x$parameters$control, by = by))
    Fields <- unique(unlist(lapply(OUT, names), use.names = FALSE))
    OUT <- stats::setNames(lapply(Fields, function(j) {
      DT <- data.table::rbindlist(lapply(OUT, `[[`, j), use.names = TRUE)
      data.table::setorderv(DT, intersect(c(by, "OCID", "ID", "mode", "boundary", "t"), names(DT)),
                           na.last = TRUE)
      DT
    }), Fields)
    OUT
  }, error = function(e) {
    if (!inherits(e, "moduleFailure")) {
      Cause <- e
      class(e) <- unique(c("gmspFailure", "moduleFailure", class(e)))
      if (is.null(e$cause)) e$cause <- Cause
    }
    e$operation <- "decomposeRecords"
    stop(e)
  })
}
