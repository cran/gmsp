# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.recordApplyKeys <- function(data, keys) {
  if (!is.list(keys)) .stopUsage("keys must be a named list or table.", operation = ".selectCandidates")
  Keys <- as.data.table(keys)
  COLS <- intersect(c("ProcessID", "RecordID", "OwnerID", "EventID", "StationID"), names(Keys))
  if (!nrow(Keys) || !length(COLS) || anyDuplicated(names(Keys))) {
    .stopUsage("keys must contain rows with distinct canonical key fields.", operation = ".selectCandidates")
  }
  if (!is.data.frame(keys) && length(setdiff(names(Keys), COLS))) {
    .stopUsage("Unknown key fields.", operation = ".selectCandidates")
  }
  LIST <- lapply(seq_len(nrow(Keys)), function(i) {
    OK <- rep(TRUE, nrow(data))
    for (j in COLS) {
      x <- as.character(Keys[[j]][i])
      if (is.na(x) || !nzchar(x) || x == "*") next
      if (!j %in% names(data)) .stopUsage("Index table missing key column: ", j, operation = ".selectCandidates")
      OK <- OK & as.character(data[[j]]) == x
    }
    OK
  })
  data[Reduce(`|`, LIST)]
}

# Moment magnitude types of the owner's decision of 2026-11-09, compared in lower case.
MwTypes <- c("mw", "mwc", "mww", "mwr", "mwb")

.setHomogeneousMagnitude <- function(data) {
  if (!all(c("EventMagnitude", "EventMagnitudeType") %chin% names(data))) return(invisible(data))
  Type <- as.character(data[["EventMagnitudeType"]])
  Published <- tolower(Type) %chin% MwTypes
  Owner <- rep(FALSE, nrow(data))
  if (all(c("EventMagnitude.owner", "EventMagnitudeType.owner") %chin% names(data))) {
    Owner <- !Published & tolower(data[["EventMagnitudeType.owner"]]) %chin% MwTypes
  }
  SurfaceWave <- !Published & !Owner & tolower(Type) %chin% "ms"
  BodyWave <- !Published & !Owner & tolower(Type) %chin% "mb"
  if (any(Owner | SurfaceWave | BodyWave)) {
    Value <- as.numeric(data[["EventMagnitude"]])
    Value[Owner] <- data[["EventMagnitude.owner"]][Owner]
    # ISC-GEM 2013 relation as reproduced by Villalon-Semanat and Palau-Clares (2018, eq. 10),
    # published for Ms <= 6.47 and applied to every Ms by the owner's decision of 2026-11-09.
    Value[SurfaceWave] <- 0.67 * Value[SurfaceWave] + 2.13
    # Scordilis (2006, eq. 22); outside [3.5, 6.2] the owner chose Mw = mb,
    # without extrapolation (2026-11-10).
    IDX <- BodyWave & is.finite(Value) & Value >= 3.5 & Value <= 6.2
    Value[IDX] <- 0.85 * Value[IDX] + 1.03
    set(data, j = "EventMagnitude", value = Value)
  }
  Type[Owner | SurfaceWave | BodyWave | (!Published & tolower(Type) %chin% "ml")] <- "Mw"
  set(data, j = "EventMagnitudeType", value = Type)
  invisible(data)
}

.recordApplyCriteria <- function(data, criteria) {
  if (is.null(criteria) || !length(criteria)) return(data)
  if (!is.list(criteria) || is.null(names(criteria))) {
    .stopUsage("criteria must be a named list.", operation = ".selectCandidates")
  }
  for (j in names(criteria)) {
    if (is.na(j) || !nzchar(j) || !j %in% names(data)) {
      .stopUsage("Selection field not found in index table: ", j, operation = ".selectCandidates")
    }
    AUX <- unlist(criteria[[j]], use.names = FALSE)
    if (is.numeric(data[[j]])) {
      # Historical numeric criteria treat missing bounds as open limits.
      AUX <- suppressWarnings(as.numeric(AUX))
      if (length(AUX) == 2L) {
        if (is.na(AUX[1L])) AUX[1L] <- -Inf
        if (is.na(AUX[2L])) AUX[2L] <- Inf
        if (AUX[1L] > AUX[2L]) AUX <- rev(AUX)
        IDX <- !is.na(data[[j]]) & data[[j]] >= AUX[1L] & data[[j]] <= AUX[2L]
      }
      if (length(AUX) != 2L) IDX <- data[[j]] %in% AUX
    }
    if (!is.numeric(data[[j]])) IDX <- as.character(data[[j]]) %in% as.character(AUX)
    data <- data[IDX]
  }
  data
}

.selectCandidates <- function(data, keys, criteria, exclude, oneRecordPerEvent) {
  DT <- copy(as.data.table(data))
  KeyColumns <- c("ProcessID", "RecordID", "OwnerID", "EventID", "StationID")
  COLS <- setdiff(c(KeyColumns, "DIR", "PGA"), names(DT))
  if (length(COLS) || anyDuplicated(names(DT))) {
    .stopUsage("Invalid selection columns: ", paste(COLS, collapse = ", "), operation = ".selectCandidates")
  }
  if (!is.logical(oneRecordPerEvent) || length(oneRecordPerEvent) != 1L || is.na(oneRecordPerEvent)) {
    .stopUsage("oneRecordPerEvent must be a logical scalar.", operation = ".selectCandidates")
  }
  if (!is.list(criteria) || !length(criteria)) {
    .stopUsage("criteria must contain at least one criterion.", operation = ".selectCandidates")
  }
  DT <- .recordApplyKeys(DT, keys = keys)
  .setHomogeneousMagnitude(DT)
  DT <- rbindlist(lapply(criteria, function(x) {
    if (!is.list(x) || !is.character(x$criterion) || length(x$criterion) != 1L ||
        is.na(x$criterion) || !nzchar(x$criterion)) {
      .stopUsage("Each criterion requires one non-empty label.", operation = ".selectCandidates")
    }
    AUX <- .recordApplyCriteria(DT, criteria = c(list(DIR = "H1"), x$criteria))
    if (!nrow(AUX)) .stopUsage("Selection produced zero records.", operation = ".selectCandidates")
    setorderv(AUX, c(KeyColumns, intersect(c("DIR", "OCID"), names(AUX))), na.last = TRUE)
    AUX <- unique(AUX, by = KeyColumns)
    AUX <- AUX[DIR == "H1"]
    AUX[, Year := as.integer(substr(EventID, 1L, 4L))]
    if (length(exclude)) AUX <- AUX[!RecordID %in% exclude]
    if (!nrow(AUX)) return(AUX)
    AUX[, criterion := x$criterion]
    AUX[]
  }), use.names = TRUE)
  COLS <- setdiff(names(DT), "criterion")
  if (!nrow(DT)) .stopUsage("Selection filters produced zero candidates.", operation = ".selectCandidates")
  DT[, criterion := paste(sort(unique(criterion), method = "radix"), collapse = "+"), by = COLS]
  DT <- unique(DT, by = COLS)
  if (!oneRecordPerEvent) Selection <- copy(DT)
  if (oneRecordPerEvent) {
    # candidates keep their EventID order; the event identity decides the selection.
    setorderv(DT, c("EventID", "PGA", "StationID", "RecordID"), order = c(1L, -1L, -1L, 1L))
    AUX <- data.table(Event = .eventIdentity(DT), PGA = DT$PGA, StationID = DT$StationID,
                      RecordID = DT$RecordID, IDX = seq_len(nrow(DT)))
    setorderv(AUX, c("Event", "PGA", "StationID", "RecordID"), order = c(1L, -1L, -1L, 1L))
    Selection <- DT[AUX$IDX[!duplicated(AUX$Event)]]
  }
  setorderv(Selection, c("RecordID", "PGA", "EventID", "OwnerID", "StationID"),
            order = c(1L, -1L, 1L, 1L, 1L))
  Selection <- unique(Selection, by = "RecordID")
  Order <- c("criterion", "OwnerID", "EventID", "EventID.USGS", "StationID",
    "NetworkID", "RecordID", "ProcessID", "EventMagnitude", "EventMagnitudeType",
    "EventDepth", "EventMechanism.owner", "EventMechanism.STREC",
    "EventTectonicRegion.owner", "EventTectonicRegion.STREC", "EventTectonicRegion.inferred",
    "Repi", "Rhyp", "StationVs30", "PGA", "AI", "CAV", "CAV5", "D0575", "D0595",
    "D2080", "Dmax", "DIR", "OCID", "Year")
  setcolorder(Selection, c(intersect(Order, names(Selection)), setdiff(names(Selection), Order)))
  setcolorder(DT, c(intersect(Order, names(DT)), setdiff(names(DT), Order)))
  list(selection = Selection[], candidates = DT[])
}
