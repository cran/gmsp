# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.workRoot <- function(path) {
    file.path(path, ".work")
}

.signalProducts <- function(tsl, units) {
    Keys <- setdiff(names(tsl), c("OCID", "ID", "t", "s"))
    LIST <- lapply(c("AI", "CAV", "CAV5"), function(id) {
        accumulateIntensityTSL(tsl, by = Keys, ID = id,
                               unitsSource = units, unitsTarget = units)
    })
    TSW <- widenTSL(rbindlist(c(list(tsl), LIST), use.names = TRUE),
      by = Keys, ids = c("AT", "VT", "DT", "AI", "CAV", "CAV5"))
    IMW <- widenIML(measureIntensitiesTSL(tsl, by = Keys,
      unitsSource = units, unitsTarget = units), by = Keys)
    list(TSW = TSW[], IMW = IMW[])
}

.buildRawTSL <- function(at, params) {
    OUT <- at[, {
        Mapped <- mapComponents(.SD[, .(t, OCID, s)], rotate = TRUE, output = "long")
        if (is.null(Mapped)) stop("Could not classify and rotate raw components.", call. = FALSE)
        Map <- copy(attr(Mapped, "componentMap", exact = TRUE))
        Map[, theta := attr(Mapped, "theta", exact = TRUE)]
        Mapped[, `:=`(OCID = DIR, ID = "AT", DIR = NULL)]
        TSL <- do.call(buildTSL, c(list(TSL = Mapped, by = character(),
          unitsSource = params$Units.source, unitsTarget = params$Units.target,
          fmax = params$Fmax, kNyq = params$kNyq, flatZeros = params$flatZeros,
          trimZeros = params$trimZeros, isRaw = TRUE), params$signalControl))
        list(TSL = list(TSL), OCIDMAP = list(Map))
    }, by = RecordKeys]
    TSL <- OUT[, TSL[[1L]], by = RecordKeys]
    Map <- OUT[, OCIDMAP[[1L]], by = RecordKeys]
    setcolorder(TSL, c(RecordKeys, "OCID", "ID", "t", "s"))
    setcolorder(Map, c(RecordKeys, "OCID", "DIR", "theta"))
    list(TSL = TSL[], OCIDMAP = Map[])
}

.buildTrimmedTS <- function(tsw, params) {
    Keys <- intersect(c(ProcessKeys, "IMF", "Units"), names(tsw))
    TSL <- lengthenTSW(tsw, by = Keys, ids = c("AT", "VT", "DT", "AI", "CAV", "CAV5"))
    OUT <- do.call(.trimTSL, c(list(TSL = TSL, by = Keys, ID = params$ID,
      OCID = params$DIR, range = params$range, taper = params$taper,
      astop = params$astop, apass = params$apass, fmax = params$Fmax,
      kNyq = params$kNyq, unitsTarget = params$units), params$signalControl))
    if ("ProcessID" %in% names(OUT$WINDOW)) OUT$WINDOW[, ProcessID := NULL]
    c(.signalProducts(OUT$TSL, units = params$units), list(WINDOW = OUT$WINDOW))
}

.buildRecordPSW <- function(TSW, XI, Tn, D50, D100, nTheta) {
    Keys <- intersect(c(ProcessKeys, "IMF", "Units"), names(TSW))
    TSL <- lengthenTSW(TSW, by = Keys)
    TSL <- TSL[ID %chin% c("AT", "VT", "DT")]
    PSL <- computeSpectraTSL(TSL, by = Keys, xi = XI, Tn = Tn[Tn > 0],
      includeD50 = D50, includeD100 = D100, nTheta = nTheta)
    if (length(XI) == 1L) PSL[, xi := XI]
    OUT <- widenPSL(PSL, by = c(Keys, "xi"))
    setkeyv(OUT, NULL)
    Order <- c(RecordKeys, "IMF", "Tn", "xi")
    setcolorder(OUT, c(intersect(Order, names(OUT)), setdiff(names(OUT), Order)))
    OUT[]
}

.processGMSPRecord <- function(row, path, params, override) {
    RecordID <- as.character(row$RecordID[1L])
    PATH.record <- .workPath(path, RecordID)
    ROW <- copy(row[, .(RecordID, OwnerID, EventID, StationID)])
    ROW[, `:=`(ProcessID, params$ProcessID)]
    setcolorder(ROW, c("ProcessID", "RecordID", "OwnerID", "EventID", "StationID"))
    FILES.db <- file.path(params$PATHS$records, ROW$OwnerID, ROW$EventID, ROW$StationID, ROW$ProcessID, 
        sprintf("%s.%s.csv", c("TSW", "IMW"), RecordID))
    if (!override && .recordDone(PATH.record, files = c("TSW.csv", "IMW.csv", "OCIDMAP.csv", "META.csv")) && all(file.exists(FILES.db))) {
        return(list(RecordID = RecordID, status = "done", skipped = TRUE))
    }
    if (dir.exists(PATH.record)) 
        unlink(PATH.record, recursive = TRUE)
    .mkdir(PATH.record)
    Step <- "readAT"
    T0 <- proc.time()[["elapsed"]]
    .writeRecordStatus(PATH.record, row, "running", list(step = Step, pid = Sys.getpid()))
    tryCatch({
        AT <- readRecordTSL(records = row[, .(RecordID, OwnerID, EventID, StationID)],
                            path = path.expand(params$PATH.records), ID = "AT")
        if (!nrow(AT)) {
            stop("readRecordTSL returned zero rows for selected record.", call. = FALSE)
        }
        Step <- "buildTS"
        .writeRecordStatus(PATH.record, row, "running", list(step = Step, pid = Sys.getpid()))
        Raw <- .buildRawTSL(AT, params = params)
        F25Rows <- Raw$TSL[ID == "AT", .N, by = OCID][1L, N]
        if (is.finite(params$MaxF25Rows) && F25Rows > params$MaxF25Rows) {
            stop("F25 record has ", F25Rows, " rows; MaxF25Rows is ", params$MaxF25Rows, ".", call. = FALSE)
        }
        Products <- .signalProducts(Raw$TSL, units = params$Units.target)
        TSW <- Products$TSW
        IMW <- Products$IMW
        Step <- "write"
        .writeRecordStatus(PATH.record, row, "running", list(step = Step, pid = Sys.getpid()))
        fwrite(TSW, file.path(PATH.record, "TSW.csv"), eol = "\n")
        fwrite(IMW, file.path(PATH.record, "IMW.csv"), eol = "\n")
        fwrite(Raw$OCIDMAP, file.path(PATH.record, "OCIDMAP.csv"), eol = "\n")
        fwrite(row, file.path(PATH.record, "META.csv"), eol = "\n")
        .saveRecordTables(row = ROW, paths = params$PATHS, tables = list(TSW = TSW, IMW = IMW), override = override)
        Seconds <- proc.time()[["elapsed"]] - T0
        .writeRecordStatus(PATH.record, row, "done", list(n.TSW = nrow(TSW), n.IMW = nrow(IMW), n.OCIDMAP = nrow(Raw$OCIDMAP), 
            n.META = 1L, n.F25Rows = F25Rows, seconds = round(Seconds, 3)))
        list(RecordID = RecordID, status = "done", seconds = Seconds)
    }, error = function(e) {
        StatusFailure <- tryCatch(
            .writeRecordStatus(PATH.record, row, "failed", list(step = Step,
              error = conditionMessage(e), pid = Sys.getpid())), error = identity)
        if (inherits(StatusFailure, "error")) e$statusFailure <- StatusFailure
        list(RecordID = RecordID, status = "failed", error = conditionMessage(e), cause = e)
    })
}

.workPath <- function(path, recordID) {
    file.path(.workRoot(path), recordID)
}

.recordDone <- function(path, files) {
    FILE <- file.path(path, "status.json")
    if (!file.exists(FILE)) 
        return(FALSE)
    if (!all(file.exists(file.path(path, files)))) 
        return(FALSE)
    Status <- tryCatch(jsonlite::read_json(FILE, simplifyVector = TRUE), error = function(e) NULL)
    is.list(Status) && identical(Status[["status"]], "done")
}

.writeRecordStatus <- function(path, row, status, extra = list()) {
    OUT <- list(status = status, RecordID = as.character(row$RecordID[1L]), OwnerID = as.character(row$OwnerID[1L]), 
        EventID = as.character(row$EventID[1L]), StationID = as.character(row$StationID[1L]))
    OUT[names(extra)] <- extra
    .writeJSON(OUT, file.path(path, "status.json"), auto_unbox = TRUE, pretty = TRUE)
}

.validTrimRange <- function(range, label) {
    range <- .range(unlist(range))
    if (any(!is.finite(range)) || range[1L] < 0 || range[2L] > 1 || range[1L] >= range[2L]) {
        .stopUsage(label, " must be two increasing values in [0, 1].", operation = "processProject")
    }
    range
}

.readScalarNumber <- function(x, label) {
    Value <- unlist(x, use.names = FALSE)
    if (!is.numeric(Value) || length(Value) != 1L || is.na(Value)) {
        .stopUsage(label, " must be one number.", operation = ".readScalarNumber")
    }
    Value
}
