# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.lengthenIMF <- function(IMF, key) {
  DT <- data.table::copy(data.table::as.data.table(IMF))
  Key <- data.table::as.data.table(key)
  Keys <- c("RecordID", "OwnerID", "EventID", "StationID")
  if ("ProcessID" %in% union(names(DT), names(Key))) Keys <- c(Keys, "ProcessID")
  if (nrow(Key) != 1L || anyDuplicated(names(Key)) ||
      !all(Keys %in% names(Key))) {
    .stopUsage("key must contain one row with the record context.", operation = ".lengthenIMF")
  }
  for (j in Keys) {
    x <- Key[[j]]
    if (!is.character(x) || anyNA(x) || !nzchar(x)) {
      .stopUsage("key requires a nonempty character ", j, ".", operation = ".lengthenIMF")
    }
  }
  Context <- paste(paste0(Keys, "=", unlist(Key[, Keys, with = FALSE])), collapse = ", ")
  tryCatch({
    if (!nrow(DT) || anyDuplicated(names(DT)) ||
        !all(c("OCID", "t", "signal") %in% names(DT))) {
      .stopUsage("IMF requires nonempty OCID, t and signal columns with unique names.",
                 operation = ".lengthenIMF")
    }
    for (j in Keys) {
      if (j %in% names(DT)) {
        if (anyNA(DT[[j]]) || any(DT[[j]] != Key[[j]])) {
          .stopUsage("IMF contradicts key column ", j, ".", operation = ".lengthenIMF")
        }
      } else data.table::set(DT, j = j, value = Key[[j]])
    }
    if (!is.character(DT[["OCID"]]) || anyNA(DT[["OCID"]]) ||
        any(!DT[["OCID"]] %in% c("H1", "H2", "UP"))) {
      .stopUsage("OCID must contain H1, H2 or UP.", operation = ".lengthenIMF")
    }
    COLS <- grep("^IMF", names(DT), value = TRUE)
    if (any(!grepl("^IMF[0-9]+$", COLS))) {
      .stopUsage("Mode columns must be named IMF followed by a positive integer.",
                 operation = ".lengthenIMF")
    }
    ModeIndex <- as.numeric(sub("^IMF", "", COLS))
    if (any(!is.finite(ModeIndex) | ModeIndex < 1 | ModeIndex > .Machine$integer.max) ||
        anyDuplicated(ModeIndex)) {
      .stopUsage("Mode indices must be distinct positive integers.", operation = ".lengthenIMF")
    }
    for (j in c("t", "signal", intersect("residue", names(DT)))) {
      if (!is.numeric(DT[[j]]) || any(!is.finite(DT[[j]]))) {
        .stopUsage(j, " must contain finite numeric samples.", operation = ".lengthenIMF")
      }
    }
    if (anyDuplicated(DT, by = c("OCID", "t"))) {
      .stopUsage("IMF has repeated component/time keys.", operation = ".lengthenIMF")
    }
    data.table::setorderv(DT, c("OCID", "t"))
    DT[, ID := "AT"]
    OUT <- list(signal = DT[, c(Keys, "OCID", "ID", "t", "signal"), with = FALSE])
    data.table::setnames(OUT$signal, "signal", "s")
    Modes <- vector("list", length(COLS))
    for (j in seq_along(COLS)) {
      if (!is.numeric(DT[[COLS[j]]]) && !all(is.na(DT[[COLS[j]]]))) {
        .stopUsage(COLS[j], " must contain numeric samples.", operation = ".lengthenIMF")
      }
      AUX <- DT[, c(Keys, "OCID", "ID", "t", COLS[j]), with = FALSE]
      data.table::setnames(AUX, COLS[j], "s")
      OK <- AUX[, .(absent = all(is.na(s)), invalid = any(!is.finite(s))), by = "OCID"]
      if (any(OK[["invalid"]] & !OK[["absent"]])) {
        .stopUsage(COLS[j], " has partial missing or non-finite samples.", operation = ".lengthenIMF")
      }
      AUX <- AUX[!OCID %in% OK[absent == TRUE, OCID]]
      AUX[, mode := as.integer(ModeIndex[j])]
      data.table::setcolorder(AUX, c(Keys, "OCID", "ID", "mode", "t", "s"))
      Modes[[j]] <- AUX
    }
    if (length(Modes)) {
      OUT$modes <- data.table::rbindlist(Modes, use.names = TRUE)
      if (!nrow(OUT$modes)) OUT$modes[, s := numeric()]
    } else {
      OUT$modes <- OUT$signal[0L]
      OUT$modes[, mode := integer()]
      data.table::setcolorder(OUT$modes, c(Keys, "OCID", "ID", "mode", "t", "s"))
    }
    if ("residue" %in% names(DT)) {
      OUT$residue <- DT[, c(Keys, "OCID", "ID", "t", "residue"), with = FALSE]
      data.table::setnames(OUT$residue, "residue", "s")
    }
    OUT
  }, error = function(e) {
    e$message <- paste0(Context, ": ", conditionMessage(e))
    e$unit <- Context
    stop(e)
  })
}
