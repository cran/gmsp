# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.checkModalQuantity <- function(decomposition) {
  if (!is.list(decomposition) || is.null(names(decomposition)) ||
      anyNA(names(decomposition)) || any(!nzchar(names(decomposition))) ||
      anyDuplicated(names(decomposition)) ||
      !all(c("signal", "modes") %in% names(decomposition))) {
    .stopUsage("decomposition requires signal and modes tables with unique names.",
               operation = ".checkModalQuantity")
  }
  if (all(c("residue", "fitResidual") %in% names(decomposition))) {
    .stopUsage("decomposition cannot contain both residue and fitResidual.",
               operation = ".checkModalQuantity")
  }
  for (j in names(decomposition)) {
    DT <- decomposition[[j]]
    if (j %in% c("signal", "modes", "residue", "fitResidual", "meanNoise") &&
        (!is.data.frame(DT) || anyDuplicated(names(DT)) ||
         !all(c("OCID", "ID", "t", "s") %in% names(DT)))) {
      .stopUsage(j, " requires OCID, ID, t and s columns.", operation = ".checkModalQuantity")
    }
    if (is.data.frame(DT) && "ID" %in% names(DT) &&
        (!is.character(DT[["ID"]]) || anyNA(DT[["ID"]]) || any(DT[["ID"]] != "AT"))) {
      .stopUsage("decomposition must contain acceleration ID AT only.",
                 operation = ".checkModalQuantity")
    }
  }
  invisible(NULL)
}

.widenModes <- function(decomposition) {
  .checkModalQuantity(decomposition)
  Signal <- data.table::as.data.table(decomposition$signal)
  Keys <- setdiff(names(Signal), c("t", "s"))
  TimeKeys <- c(Keys, "t")
  if (any(c("signal", "residue") %in% Keys) || any(grepl("^IMF[0-9]+$", Keys))) {
    .stopUsage("Metadata conflicts with IMF product columns.", operation = ".widenModes")
  }
  Remainder <- intersect(c("residue", "fitResidual"), names(decomposition))
  Tables <- c("signal", "modes", Remainder)
  for (j in Tables) {
    DT <- data.table::as.data.table(decomposition[[j]])
    COLS <- c(TimeKeys, "s")
    if (j == "modes") COLS <- c(COLS, "mode")
    if (!setequal(names(DT), COLS)) {
      .stopUsage(j, " must retain the signal metadata and its native sample columns.",
                 operation = ".widenModes")
    }
    for (k in c("t", "s")) {
      if (!is.numeric(DT[[k]]) || any(!is.finite(DT[[k]]))) {
        .stopUsage(j, " ", k, " must contain finite numeric samples.", operation = ".widenModes")
      }
    }
    COLS <- TimeKeys
    if (j == "modes") COLS <- c(COLS, "mode")
    if (anyDuplicated(DT, by = COLS)) {
      .stopUsage(j, " contains duplicate sample keys.", operation = ".widenModes")
    }
    if (j != "signal" && nrow(DT[!Signal, on = TimeKeys])) {
      .stopUsage(j, " contains sample keys absent from signal.", operation = ".widenModes")
    }
  }
  Modes <- data.table::as.data.table(decomposition$modes)
  if (!is.numeric(Modes[["mode"]]) || any(!is.finite(Modes[["mode"]])) ||
      any(Modes[["mode"]] < 1 | Modes[["mode"]] > .Machine$integer.max |
          Modes[["mode"]] != floor(Modes[["mode"]]))) {
    .stopUsage("mode must contain positive integer indices.", operation = ".widenModes")
  }
  OUT <- data.table::copy(Signal)
  data.table::setnames(OUT, "s", "signal")
  Labels <- character()
  if (nrow(Modes)) {
    Labels <- paste0("IMF", unique(Modes[["mode"]]))
    Counts <- Signal[, .N, by = Keys]
    DT <- Modes[, .N, by = c(Keys, "mode")]
    if (any(Counts[DT, on = Keys, N != i.N])) {
      .stopUsage("Each present mode must cover its entire signal time grid.", operation = ".widenModes")
    }
    DT <- data.table::copy(Modes)
    DT[, mode := paste0("IMF", mode)]
    DT <- data.table::dcast(DT, ... ~ mode, value.var = "s")
    OUT <- DT[OUT, on = TimeKeys]
  }
  if (length(Remainder)) {
    DT <- data.table::as.data.table(decomposition[[Remainder]])
    if (nrow(DT) != nrow(Signal)) {
      .stopUsage("The remainder must cover the entire signal time grid.", operation = ".widenModes")
    }
    DT <- data.table::copy(DT)
    data.table::setnames(DT, "s", "residue")
    OUT <- DT[OUT, on = TimeKeys]
  }
  OUT[, ID := NULL]
  data.table::setcolorder(OUT, c(setdiff(Keys, "ID"), "t", "signal", Labels,
    intersect("residue", names(OUT))))
  OUT[]
}

.translateMetrics <- function(metrics) {
  Required <- c("OCID", "ID", "mode", "frequency", "bandwidth", "energyFraction")
  if (!is.data.frame(metrics) || anyDuplicated(names(metrics)) ||
      !all(Required %in% names(metrics))) {
    .stopUsage("metrics requires native mode metrics and OCID/ID columns.", operation = ".translateMetrics")
  }
  if (!is.character(metrics[["ID"]]) || anyNA(metrics[["ID"]]) || any(metrics[["ID"]] != "AT")) {
    .stopUsage("metrics must contain acceleration ID AT only.", operation = ".translateMetrics")
  }
  Fields <- c("mode", "frequency", "bandwidth", "energy", "energyFraction", "meanPeriod")
  Keys <- setdiff(names(metrics), c(Fields, "ID"))
  if (any(c("IMF", "Fo[Hz]", "BW[Hz]", "E[k]/Eo [%]") %in% Keys)) {
    .stopUsage("Metadata conflicts with IMFF product columns.", operation = ".translateMetrics")
  }
  if (!is.numeric(metrics[["mode"]]) || any(!is.finite(metrics[["mode"]])) ||
      any(metrics[["mode"]] < 1 | metrics[["mode"]] > .Machine$integer.max |
          metrics[["mode"]] != floor(metrics[["mode"]]))) {
    .stopUsage("mode must contain positive integer indices.", operation = ".translateMetrics")
  }
  for (j in c("frequency", "bandwidth", "energyFraction")) {
    if (!is.numeric(metrics[[j]])) .stopUsage(j, " must be numeric.", operation = ".translateMetrics")
  }
  OUT <- data.table::copy(data.table::as.data.table(metrics)[,
    c(Keys, "mode", "frequency", "bandwidth", "energyFraction"), with = FALSE])
  OUT[, `:=`(mode = paste0("IMF", mode, recycle0 = TRUE),
             energyFraction = 100 * energyFraction)]
  data.table::setnames(OUT, c("mode", "frequency", "bandwidth", "energyFraction"),
    c("IMF", "Fo[Hz]", "BW[Hz]", "E[k]/Eo [%]"))
  OUT[]
}
