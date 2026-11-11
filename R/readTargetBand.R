# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.checkTargetBand <- function(data, operation) {
  DT <- copy(as.data.table(data))
  COLS <- c("Tn", "Sa.low", "Sa.mean", "Sa.high")
  .checkTableKeys(DT, by = character(), columns = COLS, keys = "Tn",
                  operation = operation)
  if (!nrow(DT) || any(!vapply(DT[, COLS, with = FALSE], is.numeric, logical(1L)))) {
    .stopUsage("Target band must contain numeric rows.", operation = operation)
  }
  if (any(!is.finite(as.matrix(DT[, COLS, with = FALSE]))) ||
      any(DT[["Tn"]] < 0) || any(DT[["Sa.low"]] <= 0) ||
      any(DT[["Sa.low"]] > DT[["Sa.mean"]]) ||
      any(DT[["Sa.mean"]] > DT[["Sa.high"]])) {
    .stopUsage("Target requires finite nonnegative periods and positive ordered band values.",
               operation = operation)
  }
  setorderv(DT, "Tn")
  setcolorder(DT, COLS)
  DT[]
}

.targetText <- function(x, label) {
  if (is.list(x)) .stopUsage(label, " must be one non-empty string.", operation = ".readTargetBand")
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    .stopUsage(label, " must be one non-empty string.", operation = ".readTargetBand")
  }
  x
}

.targetQuantile <- function(x, label) {
  x <- .targetText(x, label = paste0(label,
    " (allowed: 0.<digits> strictly between 0 and 1, or mean)"))
  if (identical(x, "mean")) return(x)
  if (!grepl("^0[.][0-9]+$", x) || !is.finite(as.numeric(x)) ||
      as.numeric(x) <= 0 || as.numeric(x) >= 1) {
    .stopUsage(label, " must use a canonical decimal quantile. Allowed values: ",
               "0.<digits> strictly between 0 and 1 (for example 0.50), or mean; no percentages or p prefixes.",
               operation = ".readTargetBand")
  }
  x
}

.targetConvertSa <- function(Sa, from, to) {
  if (identical(from, to)) return(Sa)
  if (from == "g" && to %in% c("mm/s2", "mm/s^2")) return(Sa * .getGravity("mm"))
  if (from %in% c("mm/s2", "mm/s^2") && to == "g") return(Sa / .getGravity("mm"))
  .stopUsage("Unsupported target unit conversion: ", from, " -> ", to,
             operation = ".readTargetBand")
}

.readTargetBand <- function(path.data, target, unitsTarget, range = NULL) {
  if (!is.list(target) || is.null(names(target)) || anyDuplicated(names(target))) {
    .stopUsage("target must be a named list with distinct fields.", operation = ".readTargetBand")
  }
  COLS <- intersect(c("path", "ID", "TR"), names(target))
  if (length(COLS)) {
    .stopUsage("Obsolete target fields: ", paste(COLS, collapse = ", "), operation = ".readTargetBand")
  }
  Demand <- .targetText(target$demand, label = "target.demand")
  if (!Demand %in% c("MCE", "MDE")) {
    .stopUsage("target.demand must be exactly MCE or MDE.", operation = ".readTargetBand")
  }
  if (Demand == "MCE" && "modelID" %in% names(target)) {
    .stopUsage("target.modelID must be absent for MCE.", operation = ".readTargetBand")
  }
  ModelID <- "MCE"
  if (Demand == "MDE") ModelID <- .targetText(target$modelID, label = "target.modelID")
  SiteID <- .targetText(target$siteID, label = "target.siteID")
  if (is.list(target$Vs30)) {
    .stopUsage("target.Vs30 must be one finite positive number.", operation = ".readTargetBand")
  }
  Velocity <- as.numeric(target$Vs30)
  if (length(Velocity) != 1L || !is.finite(Velocity) || Velocity <= 0) {
    .stopUsage("target.Vs30 must be one finite positive number.", operation = ".readTargetBand")
  }
  Value <- .targetText(target$value, label = "target.value")
  Units <- tolower(.targetText(target$units, label = "target.units"))
  unitsTarget <- tolower(.targetText(unitsTarget, label = "unitsTarget"))
  Quantiles <- .targetQuantile(target$p, label = "target.p")
  HasEnvelope <- "envelope" %in% names(target)
  if (HasEnvelope) {
    if (!is.list(target$envelope) || anyDuplicated(names(target$envelope))) {
      .stopUsage("target.envelope must be a named list with distinct lower and upper keys. ",
                 "Allowed values: 0.<digits> strictly between 0 and 1 (for example 0.50), or mean.",
                 operation = ".readTargetBand")
    }
    if ("ID" %in% names(target$envelope)) {
      .stopUsage("target.envelope.ID is obsolete; use lower and upper. ",
                 "Allowed values: 0.<digits> strictly between 0 and 1 (for example 0.50), or mean.",
                 operation = ".readTargetBand")
    }
    Quantiles <- c(.targetQuantile(target$envelope$lower, label = "target.envelope.lower"),
      Quantiles, .targetQuantile(target$envelope$upper, label = "target.envelope.upper"))
    if (anyDuplicated(Quantiles)) {
      .stopUsage("target.envelope.lower, target.p and target.envelope.upper must be distinct. ",
                 "Allowed values: 0.<digits> strictly between 0 and 1 (for example 0.50), or mean.",
                 operation = ".readTargetBand")
    }
  }
  FILE <- file.path(.targetText(path.data, label = "path.data"),
                    switch(Demand, MCE = "MCETable.Rds", MDE = "UHSTable.Rds"))
  DT <- as.data.table(readRDS(FILE))
  COLS <- setdiff(c("ID", "siteID", "Vs30", "Tn", "p", Value,
                   if (Demand == "MDE") "TR"), names(DT))
  if (length(COLS) || anyDuplicated(names(DT))) {
    .stopUsage("Invalid target table columns: ", paste(COLS, collapse = ", "), operation = ".readTargetBand")
  }
  IDX <- as.character(DT[["ID"]]) == ModelID &
    as.character(DT[["siteID"]]) == SiteID & DT[["Vs30"]] == Velocity &
    as.character(DT[["p"]]) %in% Quantiles
  if (Demand == "MDE") IDX <- IDX & DT[["TR"]] == 10000
  DT <- DT[IDX]
  if (!nrow(DT)) .stopUsage("Target filters produced zero rows.", operation = ".readTargetBand")
  DT <- data.table(Tn = as.numeric(DT[["Tn"]]), p = as.character(DT[["p"]]),
                    Sa = as.numeric(DT[[Value]]))
  if (any(!is.finite(DT[["Tn"]]) | DT[["Tn"]] < 0) ||
      any(!is.finite(DT[["Sa"]]) | DT[["Sa"]] <= 0)) {
    .stopUsage("Target periods must be finite nonnegative and values finite positive.", operation = ".readTargetBand")
  }
  if (anyDuplicated(DT, by = c("p", "Tn"))) {
    .stopUsage("Target filters must identify one value for every p,Tn key.", operation = ".readTargetBand")
  }
  if (length(setdiff(Quantiles, DT[["p"]]))) {
    .stopUsage("Target is missing requested quantiles.", operation = ".readTargetBand")
  }
  Mesh <- split(DT[["Tn"]], DT[["p"]])
  Periods <- sort(Mesh[[Quantiles[1L]]])
  if (any(vapply(Mesh[Quantiles], function(x) !identical(sort(x), Periods), logical(1L)))) {
    .stopUsage("Target quantiles must use identical Tn meshes.", operation = ".readTargetBand")
  }
  if (sum(Periods == 0) != 1L || !any(Periods > 0)) {
    .stopUsage("Target must include one PGA and at least one positive period.", operation = ".readTargetBand")
  }
  if (!is.null(range)) {
    if (!is.numeric(range) || length(range) != 2L || anyNA(range) || range[1L] > range[2L]) {
      .stopUsage("range must define one valid interval.", operation = ".readTargetBand")
    }
    DT <- DT[Tn == 0 | (Tn > 0 & Tn >= range[1L] & Tn <= range[2L])]
    if (!any(DT[["Tn"]] > 0)) {
      .stopUsage("Target period range removed every positive period.", operation = ".readTargetBand")
    }
  }
  DT[, Sa := .targetConvertSa(Sa, from = Units, to = unitsTarget)]
  setorderv(DT, c("p", "Tn"))
  if (!HasEnvelope) return(DT[, .(Tn, Sa)])
  DT <- dcast(DT, Tn ~ p, value.var = "Sa")
  setnames(DT, Quantiles, c("Sa.low", "Sa.mean", "Sa.high"))
  .checkTargetBand(DT, operation = ".readTargetBand")
}
