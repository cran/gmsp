# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.processModalExecution <- function(stages, config, parallel, workers) {
  if (!"imf" %in% stages) return(NULL)
  Params <- .processIMFParams(.processConfig(config, "imf"))
  if (Params$stochastic) {
    RequestedParallel <- .jsonRequire(config, "parallel")
    RequestedWorkers <- .jsonRequire(config, "workers")
    if (!identical(RequestedParallel, FALSE) || !is.numeric(RequestedWorkers) ||
        length(RequestedWorkers) != 1L || is.na(RequestedWorkers) || RequestedWorkers != 1 ||
        !identical(parallel, FALSE) || !identical(as.numeric(workers), 1)) {
      .stopUsage("Stochastic imf requires explicit parallel FALSE and workers 1.", operation = "processProject")
    }
  }
  Params
}

.processModalStart <- function(params, run, skip) {
  if (is.null(params)) return(NULL)
  Seeded <- params$stochastic && nrow(run) > 0L
  if (Seeded) set.seed(params$seed)
  list(method = params$Method, control = params$control, seed = params$seed,
       RNGkind = params$RNGkind, xemd = params$xemd, seeded = Seeded,
       stateBefore = if (Seeded) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL,
       stateAfter = NULL, recordOrder = run[, ProcessKeys, with = FALSE],
       skipped = skip[, ProcessKeys, with = FALSE], status = "started", records = list())
}

.processModalEvidence <- function(modal, evidence) {
  if (is.null(modal) || is.null(evidence)) return(invisible(NULL))
  .recordWriteJSONAtomic(modal, paste0(evidence, ".modal.json"), override = TRUE, digits = NA)
}

.processModalFinish <- function(modal, records) {
  if (is.null(modal)) return(NULL)
  if (modal$seeded) modal$stateAfter <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  modal$records <- lapply(records, function(x) {
    list(key = x$row[, ProcessKeys, with = FALSE], status = x$status,
         error = x$error, effective = x$stages$imf$effective)
  })
  Failed <- vapply(records, function(x) identical(x$status, "failed"), logical(1L))
  modal$status <- if (any(Failed)) "failed" else "complete"
  modal
}

.processEffectiveControls <- function(decomposition, params) {
  Signal <- decomposition$signal
  Series <- unique(Signal[, c(ProcessKeys, "OCID", "ID"), with = FALSE])
  setorderv(Series, c(ProcessKeys, "OCID", "ID"))
  lapply(seq_len(nrow(Series)), function(i) {
    Key <- Series[i]
    DT <- Signal[Key, on = c(ProcessKeys, "OCID", "ID")]
    setorderv(DT, "t")
    Interval <- DT$t[2L] - DT$t[1L]
    Control <- .translateControl(params$Method, params$control,
                                 samplingInterval = Interval, signal = DT$s)
    list(key = Key, samplingInterval = Interval, method = Control$method, control = Control$control)
  })
}
