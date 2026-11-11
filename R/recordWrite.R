# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.recordAcquireLock <- function(path, label) {
    .mkdir(dirname(path))
    if (!isTRUE(dir.create(path, showWarnings = FALSE))) {
        OWNER <- file.path(path, "owner.json")
        Detail <- if (file.exists(OWNER)) {
            paste(readLines(OWNER, warn = FALSE), collapse = " ")
        }
        else {
            "owner unknown"
        }
        stop(label, " lock is already held: ", path, " (", Detail, ")", call. = FALSE)
    }
    OK <- FALSE
    on.exit(if (!OK && dir.exists(path)) unlink(path, recursive = TRUE), add = TRUE)
    Owner <- list(pid = Sys.getpid(), host = unname(Sys.info()[["nodename"]]), startedUTC = format(Sys.time(), 
        tz = "UTC", usetz = TRUE))
    .writeJSON(Owner, file.path(path, "owner.json"), auto_unbox = TRUE, pretty = TRUE)
    OK <- TRUE
    invisible(path)
}

.recordIndexLockPath <- function(paths) {
    file.path(paths$index, ".spt-locks", "process-index.lock")
}

.recordReleaseLock <- function(path) {
    if (length(path) == 1L && !is.na(path) && dir.exists(path)) {
        OWNER <- file.path(path, "owner.json")
        Owned <- FALSE
        if (file.exists(OWNER)) {
            Data <- tryCatch(jsonlite::read_json(OWNER, simplifyVector = TRUE), error = function(e) NULL)
            Owned <- is.list(Data) && identical(as.integer(Data[["pid"]]), as.integer(Sys.getpid())) && 
                identical(as.character(Data[["host"]]), as.character(unname(Sys.info()[["nodename"]])))
        }
        if (!Owned) {
            .warnSignal("Refusing to release lock not owned by this process: ", path,
                operation = ".recordReleaseLock", path = path)
            return(invisible(NULL))
        }
        unlink(path, recursive = TRUE)
        if (dir.exists(path)) {
            .warnSignal("Could not remove released lock: ", path,
                operation = ".recordReleaseLock", path = path)
        }
    }
    invisible(NULL)
}

.recordProcessLockPath <- function(paths, processID) {
    Name <- URLencode(as.character(processID), reserved = TRUE)
    file.path(paths$process, ".spt-locks", sprintf("process.%s.lock", Name))
}

.recordRequireNoTransaction <- function(paths) {
    Existing <- .recordTransactionDirs(paths)
    if (length(Existing)) {
        stop("Unresolved SPT metadata transaction: ", Existing[1L], call. = FALSE)
    }
    invisible(NULL)
}

.recordTransactionDirs <- function(paths) {
    ROOT <- file.path(paths$process, ".spt-transactions")
    if (!dir.exists(ROOT)) 
        return(character())
    Existing <- list.files(ROOT, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    Existing[file.info(Existing)$isdir %in% TRUE]
}

.writeProcessTables <- function(index, intensity, paths, processID, record = c("TSW", "IMW"), table = c("MasterIndex", 
    "IntensityTable"), extra = list(), params = list(), override = TRUE, signature = NULL, recipe = NULL, 
    contractMD5 = NULL, reseal = NULL, provenance = NULL, lockHeld = FALSE) {
    .requireWritableProcess(processID)
    paths <- .recordPaths(paths, need = c("index", "process"))
    ProcessID <- as.character(processID)
    if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
        stop("processID must be one non-empty string.", call. = FALSE)
    }
    if (!is.list(extra) || (length(extra) && is.null(names(extra)))) {
        stop("extra must be a named list.", call. = FALSE)
    }
    if (length(extra) && any(!nzchar(names(extra)))) {
        stop("extra table names must be non-empty.", call. = FALSE)
    }
    Reserved <- intersect(names(extra), c("MasterIndex", "IntensityTable"))
    if (length(Reserved)) {
        stop("extra cannot replace reserved tables: ", paste(Reserved, collapse = ", "), call. = FALSE)
    }
    ProcessLock <- NULL
    IndexLock <- NULL
    if (!isTRUE(lockHeld)) {
        IndexLock <- .recordAcquireLock(.recordIndexLockPath(paths), "global process index")
        on.exit(.recordReleaseLock(IndexLock), add = TRUE)
        ProcessLock <- .recordAcquireLock(.recordProcessLockPath(paths = paths, processID = ProcessID), 
            sprintf("process %s", ProcessID))
        on.exit(.recordReleaseLock(ProcessLock), add = TRUE)
    }
    else {
        .recordRequireOwnedLock(.recordIndexLockPath(paths), "global process index")
        .recordRequireOwnedLock(.recordProcessLockPath(paths = paths, processID = ProcessID), sprintf("process %s", 
            ProcessID))
    }
    .recordRequireNoTransaction(paths)
    IntensityTable <- .recordPrepareProcessIntensity(intensity, processID = ProcessID)
    MasterIndex <- .recordPrepareProcessIndex(index = index, intensity = IntensityTable, processID = ProcessID)
    Incoming <- list(MasterIndex = .recordNormalizeProcessTable(MasterIndex, processID = ProcessID, label = "incoming MasterIndex"), 
        IntensityTable = .recordNormalizeProcessTable(IntensityTable, processID = ProcessID, label = "incoming IntensityTable"))
    for (COL in names(extra)) {
        Incoming[[COL]] <- .recordNormalizeProcessTable(extra[[COL]], processID = ProcessID, label = sprintf("incoming %s", 
            COL))
    }
    DIR <- .recordTableDir(processID = ProcessID, paths)
    ContractFile <- .recordProcessContractFile(processID = ProcessID, paths = paths)
    ContractExists <- file.exists(ContractFile)
    TableFiles <- list.files(DIR, pattern = "[.]csv$", full.names = TRUE)
    if (!isTRUE(override) && xor(ContractExists, length(TableFiles) > 0L)) {
        stop("Process ", ProcessID, " has an incomplete contract/table baseline.", call. = FALSE)
    }
    ProposedTable <- unique(c("MasterIndex", "IntensityTable", as.character(table), names(extra)))
    Append <- !isTRUE(override) && ContractExists
    AppendInfo <- NULL
    ContractData <- NULL
    Candidates <- Incoming
    if (Append) {
        AppendInfo <- .recordRequireAppendContract(processID = ProcessID, paths = paths, record = record, 
            table = ProposedTable, signature = signature, recipe = recipe, contractMD5 = contractMD5,
            reseal = reseal, provenance = provenance)
        Declared <- AppendInfo$contract$process
        Required <- c("MasterIndex", "IntensityTable")
        if (!all(Required %chin% Declared)) {
            stop("Process ", ProcessID, " does not declare append index tables.", call. = FALSE)
        }
        Missing <- setdiff(Declared, names(Incoming))
        if (length(Missing)) {
            stop("Append is missing declared tables for ", ProcessID, ": ", paste(Missing, collapse = ", "), 
                call. = FALSE)
        }
        Candidates <- Incoming[Declared]
        .recordValidateTableMaps(Candidates, processID = ProcessID, label = "incoming process tables")
        for (COL in Declared) {
            FILE <- file.path(DIR, sprintf("%s.csv", COL))
            if (!file.exists(FILE)) {
                stop("Missing declared process table: ", FILE, call. = FALSE)
            }
            Candidates[[COL]] <- .recordMergeProcessTable(existing = .recordFread(FILE), incoming = Candidates[[COL]], 
                processID = ProcessID, label = COL)
        }
        .recordValidateTableMaps(Candidates, processID = ProcessID, label = "merged process tables")
    }
    else {
        Missing <- setdiff(ProposedTable, names(Incoming))
        if (length(Missing)) {
            stop("Missing process table data for ", ProcessID, ": ", paste(Missing, collapse = ", "), 
                call. = FALSE)
        }
        .recordValidateTableMaps(Incoming, processID = ProcessID, label = "incoming process tables")
        ExistingJSON <- if (ContractExists) {
            jsonlite::read_json(ContractFile, simplifyVector = FALSE)
        }
        else {
            NULL
        }
        ExistingContract <- .recordReadProcessContract(processID = ProcessID, paths = paths, required = FALSE)
        Record <- unique(c(if (is.null(ExistingContract)) character() else ExistingContract$record, as.character(record)))
        Table <- unique(c(if (is.null(ExistingContract)) character() else ExistingContract$process, ProposedTable))
        ContractData <- .recordContractData(processID = ProcessID, record = Record, table = Table, params = params, 
            existing = ExistingJSON)
    }
    Staged <- data.table(source = character(), target = character())
    PreserveStaged <- FALSE
    on.exit({
        Ambiguous <- length(.recordTransactionDirs(paths)) > 0L
        if (!PreserveStaged && !Ambiguous) {
            Left <- Staged$source[file.exists(Staged$source)]
            if (length(Left)) unlink(Left, force = TRUE)
        }
    }, add = TRUE)
    ProcessSources <- list()
    for (COL in names(Candidates)) {
        Target <- file.path(DIR, sprintf("%s.csv", COL))
        Source <- if (Append) {
            .recordStageAppendTable(existing = Target, incoming = Incoming[[COL]], target = Target)
        }
        else {
            .recordStageTable(Candidates[[COL]], Target)
        }
        ProcessSources[[COL]] <- Source
        Staged <- rbind(Staged, data.table(source = Source, target = Target))
    }
    for (COL in names(ProcessSources)) {
        Target <- file.path(paths$index, sprintf("%s.csv", COL))
        Source <- .recordStageIndexTable(paths = paths, output = COL, target = Target, processID = ProcessID, 
            candidate = ProcessSources[[COL]])
        Staged <- rbind(Staged, data.table(source = Source, target = Target))
    }
    if (!is.null(ContractData)) {
        Source <- .recordStageJSON(ContractData, ContractFile)
        Staged <- rbind(Staged, data.table(source = Source, target = ContractFile))
    }
    if (!is.null(AppendInfo$ledgerCandidate)) {
        Target <- .recordAppendContractFile(processID = ProcessID, paths = paths)
        Source <- .recordStageJSON(AppendInfo$ledgerCandidate, Target)
        Staged <- rbind(Staged, data.table(source = Source, target = Target))
    }
    tryCatch(.recordCommitFiles(Staged, paths = paths), spt_ambiguous_commit = function(e) {
        PreserveStaged <<- TRUE
        stop(e)
    })
    invisible(Candidates)
}

.updateProcessContract <- function(paths, processID, record = character(), table = character(), params = list()) {
    .requireWritableProcess(processID)
    paths <- .recordPaths(paths, need = "process")
    ProcessID <- as.character(processID)
    if (length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
        stop("processID must be one non-empty string.", call. = FALSE)
    }
    Contract <- .recordReadProcessContract(processID = ProcessID, paths = paths, required = FALSE)
    Record <- unique(c(if (is.null(Contract)) character() else Contract$record, as.character(record)))
    Table <- unique(c(if (is.null(Contract)) character() else Contract$process, as.character(table)))
    Record <- Record[nzchar(Record) & !is.na(Record)]
    Table <- Table[nzchar(Table) & !is.na(Table)]
    .recordWriteProcessContract(processID = ProcessID, paths = paths, record = Record, table = Table, 
        params = params)
}

.recordAppendContractFile <- function(processID, paths) {
    file.path(.recordProcessDir(processID = processID, paths = paths), "append.json")
}

.recordRequireAppendContract <- function(processID, paths, record, table, signature, recipe = NULL, contractMD5 = NULL,
    reseal = NULL, provenance = NULL) {
    Contract <- .recordReadProcessContract(processID = processID, paths = paths, required = TRUE)
    JSON <- .recordReadProcessJSON(processID = processID, paths = paths, required = TRUE)
    ActualMD5 <- .recordContractMD5(processID = processID, paths = paths)
    ExpectedMD5 <- .recordAppendMD5(contractMD5, "append.contractMD5")
    if (!is.null(ExpectedMD5) && !identical(ExpectedMD5, ActualMD5)) {
        stop("append.contractMD5 mismatch for ", processID, ": expected ", ExpectedMD5, ", found ", ActualMD5, 
            ".", call. = FALSE)
    }
    Record <- unique(as.character(record))
    Table <- unique(as.character(table))
    if (!identical(Contract$record, Record)) {
        stop("Append record inventory does not match process.json for ", processID, ".", call. = FALSE)
    }
    if (!all(Contract$process %chin% Table)) {
        Missing <- setdiff(Contract$process, Table)
        stop("Append cannot produce declared tables for ", processID, ": ", paste(Missing, collapse = ", "), 
            call. = FALSE)
    }
    Signature <- as.character(signature)
    if (length(Signature) != 1L || is.na(Signature) || !nzchar(Signature)) {
        stop("append signature must be one non-empty string.", call. = FALSE)
    }
    LedgerFile <- .recordAppendContractFile(processID = processID, paths = paths)
    Ledger <- if (file.exists(LedgerFile)) jsonlite::read_json(LedgerFile, simplifyVector = FALSE)
    Output <- list(record = Contract$record, table = Contract$process)
    # The seal is the ledger when it exists, else the signature kept in process.json.
    Sealed <- NULL
    if (!is.null(Ledger)) {
        LedgerID <- as.character(.jsonGet(Ledger, c("processID"), default = ""))
        LedgerMD5 <- tolower(as.character(.jsonGet(Ledger, c("baselineContractMD5"), default = "")))
        if (!identical(LedgerID, processID) || !identical(LedgerMD5, ActualMD5)) {
            stop("append.json does not belong to process ", processID, " or to its current process.json.",
                call. = FALSE)
        }
        Sealed <- list(source = "append.json", signature = as.character(.jsonGet(Ledger, c("recipeMD5"), default = "")),
            recipe = .jsonGet(Ledger, c("recipe")))
        Output <- .jsonGet(Ledger, c("output"), default = Output)
    }
    ExistingSignature <- as.character(.jsonGet(JSON, c("params", "sptAppendSignature"), default = character()))
    if (is.null(Ledger) && length(ExistingSignature)) {
        if (length(ExistingSignature) != 1L) {
            stop("process.json holds more than one append signature for ", processID, ".", call. = FALSE)
        }
        Sealed <- list(source = "process.json", signature = ExistingSignature,
            recipe = .jsonGet(JSON, c("params", "appendRecipe")))
    }
    LedgerCandidate <- NULL
    if (is.null(Sealed)) {
        if (is.null(ExpectedMD5)) {
            stop("Legacy process ", processID, " requires append.contractMD5 before append; current contract MD5 is ", 
                ActualMD5, ".", call. = FALSE)
        }
        LedgerCandidate <- list(processID = processID, mode = "append-only", baselineContractMD5 = ActualMD5, 
            recipeMD5 = Signature, recipe = recipe, provenance = provenance, output = Output,
            compatibility = "user-asserted-against-pinned-legacy-contract",
            createdUTC = format(Sys.time(), tz = "UTC", usetz = TRUE))
    }
    if (!is.null(Sealed) && !identical(Sealed$signature, Signature)) {
        Reseal <- .recordAppendMD5(reseal, "append.reseal")
        if (is.null(Reseal)) {
            stop(.recordSealMismatch(processID = processID, sealed = Sealed, signature = Signature, recipe = recipe),
                call. = FALSE)
        }
        if (!identical(Reseal, Sealed$signature)) {
            stop("append.reseal does not name the recipe that seals ", processID, ": it is sealed by ",
                Sealed$signature, ".", call. = FALSE)
        }
        Previous <- if (is.null(Ledger)) {
            list(source = "process.json", recipeMD5 = Sealed$signature, recipe = Sealed$recipe)
        }
        else Ledger[setdiff(names(Ledger), "history")]
        LedgerCandidate <- list(processID = processID, mode = "append-only", baselineContractMD5 = ActualMD5,
            recipeMD5 = Signature, recipe = recipe, provenance = provenance, output = Output,
            compatibility = "user-asserted-reseal", createdUTC = format(Sys.time(), tz = "UTC", usetz = TRUE),
            history = c(.jsonGet(Ledger, c("history"), default = list()), list(Previous)))
    }
    if (identical(Sealed$source, "process.json") && !setequal(Contract$process, Table)) {
        stop("Append table inventory does not match process.json for ", processID, ".", call. = FALSE)
    }
    list(contract = Contract, json = JSON, md5 = ActualMD5, ledger = Ledger, ledgerCandidate = LedgerCandidate)
}

.recordSealMismatch <- function(processID, sealed, signature, recipe) {
    Differences <- if (is.null(sealed$recipe)) {
        "the sealed recipe was not stored, so its differences cannot be listed"
    }
    else paste(.recipeDifferences(sealed = sealed$recipe, current = recipe), collapse = "; ")
    paste0("Processing recipe does not match ", sealed$source, " for ", processID, ": sealed by ", sealed$signature,
        ", this producer computes ", signature, ". Differences (sealed -> current): ", Differences,
        ". After verifying that both producers yield the same products, accept this one by setting append.reseal to \"",
        sealed$signature, "\".")
}

.recipeDifferences <- function(sealed, current) {
    Sealed <- unlist(sealed)
    Current <- unlist(current)
    Names <- sort(union(names(Sealed), names(Current)), method = "radix")
    OUT <- vapply(Names, function(Name) {
        Old <- if (Name %in% names(Sealed)) Sealed[[Name]] else "absent"
        New <- if (Name %in% names(Current)) Current[[Name]] else "absent"
        if (identical(Old, New)) NA_character_ else paste0(Name, ": ", Old, " -> ", New)
    }, character(1L))
    OUT <- unname(OUT[!is.na(OUT)])
    if (!length(OUT)) return("no leaf differs; the serialized structure does")
    OUT
}

.recordRequireOwnedLock <- function(path, label) {
    OWNER <- file.path(path, "owner.json")
    Data <- if (dir.exists(path) && file.exists(OWNER)) {
        tryCatch(jsonlite::read_json(OWNER, simplifyVector = TRUE), error = function(e) NULL)
    }
    else {
        NULL
    }
    Owned <- is.list(Data) && identical(as.integer(Data[["pid"]]), as.integer(Sys.getpid())) && identical(as.character(Data[["host"]]), 
        as.character(unname(Sys.info()[["nodename"]])))
    if (!Owned) {
        stop(label, " lock is not owned by this process: ", path, call. = FALSE)
    }
    invisible(path)
}

.recordPrepareProcessIntensity <- function(intensity, processID) {
    OUT <- copy(as.data.table(intensity))
    .requireColumns(OUT, RecordKeys, "process intensity")
    OUT[, `:=`(ProcessID, processID)]
    if (!"DIR" %chin% names(OUT) && "OCID" %chin% names(OUT)) {
        OUT[, `:=`(DIR, as.character(OCID))]
    }
    COLS <- c(ProcessKeys, intersect(c("DIR", "OCID"), names(OUT)))
    setcolorder(OUT, c(COLS, setdiff(names(OUT), COLS)))
    OUT[]
}

.recordPrepareProcessIndex <- function(index, intensity, processID) {
    OUT <- .recordPrepareProcessIntensity(intensity, processID = processID)
    AUX <- copy(as.data.table(index))
    .requireColumns(AUX, RecordKeys, "process index source")
    AUX[, `:=`((RecordKeys), lapply(.SD, as.character)), .SDcols = RecordKeys]
    OUT[, `:=`((RecordKeys), lapply(.SD, as.character)), .SDcols = RecordKeys]
    AUX <- unique(AUX, by = RecordKeys)
    MISS <- OUT[!AUX, on = RecordKeys, unique(RecordID)]
    if (length(MISS)) {
        stop("Missing index metadata for RecordID: ", paste(MISS, collapse = ", "), call. = FALSE)
    }
    COLS <- setdiff(names(AUX), c(names(OUT), "ProcessID"))
    if (length(COLS)) {
        OUT[AUX, on = RecordKeys, `:=`((COLS), mget(paste0("i.", COLS)))]
    }
    COLS <- c(ProcessKeys, "DIR", "OCID", setdiff(names(OUT), c(ProcessKeys, "DIR", "OCID")))
    setcolorder(OUT, intersect(COLS, names(OUT)))
    OUT[]
}

.recordNormalizeProcessTable <- function(data, processID, label) {
    OUT <- copy(as.data.table(data))
    .requireColumns(OUT, RecordKeys, label)
    KeyColumns <- c(if ("ProcessID" %chin% names(OUT)) "ProcessID", RecordKeys)
    OUT[, `:=`((KeyColumns), lapply(.SD, as.character)), .SDcols = KeyColumns]
    for (COL in KeyColumns) {
        if (any(is.na(OUT[[COL]]) | !nzchar(OUT[[COL]]))) {
            stop(label, " has empty ", COL, " values.", call. = FALSE)
        }
    }
    if ("ProcessID" %chin% names(OUT) && any(OUT$ProcessID != processID)) {
        stop(label, " contains a ProcessID other than ", processID, ".", call. = FALSE)
    }
    Map <- unique(OUT[, .SD, .SDcols = RecordKeys])
    Ambiguous <- Map[, .N, by = RecordID][N > 1L, RecordID]
    if (length(Ambiguous)) {
        stop(label, " maps RecordID to multiple record keys: ", paste(Ambiguous, collapse = ", "), call. = FALSE)
    }
    OUT[]
}

.recordTableDir <- function(processID, paths) {
    .recordProcessDir(processID = processID, paths = paths)
}

.recordValidateTableMaps <- function(tables, processID, label) {
    if (!length(tables)) 
        stop(label, " has no tables.", call. = FALSE)
    Maps <- lapply(names(tables), function(Name) {
        DT <- .recordNormalizeProcessTable(tables[[Name]], processID = processID, label = sprintf("%s %s", 
            label, Name))
        unique(DT[, .SD, .SDcols = RecordKeys])
    })
    names(Maps) <- names(tables)
    ReferenceName <- if ("MasterIndex" %chin% names(Maps)) {
        "MasterIndex"
    }
    else {
        names(Maps)[1L]
    }
    Reference <- Maps[[ReferenceName]]
    for (Name in names(Maps)) {
        Missing <- fsetdiff(Reference, Maps[[Name]])
        Extra <- fsetdiff(Maps[[Name]], Reference)
        if (nrow(Missing) || nrow(Extra)) {
            stop(label, " ", Name, " record-key map differs from ", ReferenceName, ".", call. = FALSE)
        }
    }
    invisible(Reference)
}

.recordMergeProcessTable <- function(existing, incoming, processID, label) {
    OLD <- .recordNormalizeProcessTable(existing, processID = processID, label = sprintf("existing %s", 
        label))
    NEW <- .recordNormalizeProcessTable(incoming, processID = processID, label = sprintf("incoming %s", 
        label))
    if (!setequal(names(OLD), names(NEW))) {
        stop(label, " schema mismatch before append.", call. = FALSE)
    }
    NEW <- NEW[, names(OLD), with = FALSE]
    .recordRequireCompatibleTypes(list(OLD, NEW), label = label)
    Overlap <- intersect(unique(OLD$RecordID), unique(NEW$RecordID))
    if (length(Overlap)) {
        stop(label, " append overlaps committed RecordID: ", paste(Overlap, collapse = ", "), call. = FALSE)
    }
    OUT <- rbindlist(list(OLD, NEW), use.names = TRUE)
    .recordNormalizeProcessTable(OUT, processID = processID, label = label)
}

.recordContractData <- function(processID, record, table, params, existing = NULL) {
    ExistingOutput <- .recordNull(if (is.null(existing)) 
        NULL
    else existing[["output"]], list())
    ExistingRecord <- as.character(.recordNull(ExistingOutput[["record"]], character()))
    ExistingTable <- as.character(.recordNull(ExistingOutput[["table"]], ExistingOutput[["process"]]))
    ExistingParams <- .recordNull(if (is.null(existing)) 
        NULL
    else existing[["params"]], list())
    if (!is.list(ExistingParams)) 
        ExistingParams <- list()
    if (length(params)) 
        ExistingParams <- utils::modifyList(ExistingParams, params)
    list(processID = processID, output = list(record = unique(c(ExistingRecord, as.character(record))), 
        table = unique(c(ExistingTable, as.character(table)))), params = ExistingParams)
}

.recordStageAppendTable <- function(existing, incoming, target) {
    Header <- names(fread(existing, nrows = 0L, showProgress = FALSE))
    if (!setequal(Header, names(incoming))) {
        stop(basename(target), " schema mismatch before append.", call. = FALSE)
    }
    NEW <- incoming[, ..Header]
    .mkdir(dirname(target))
    FILE <- tempfile(pattern = paste0(".", basename(target), ".spt."), tmpdir = dirname(target), fileext = ".tmp")
    OK <- FALSE
    on.exit(if (!OK && file.exists(FILE)) unlink(FILE, force = TRUE), add = TRUE)
    if (!isTRUE(file.copy(existing, FILE, overwrite = TRUE))) {
        stop("Could not stage existing table: ", existing, call. = FALSE)
    }
    .recordEnsureTrailingNewline(FILE)
    fwrite(NEW, FILE, append = TRUE, col.names = FALSE, eol = "\n")
    OK <- TRUE
    FILE
}

.recordStageTable <- function(data, target) {
    .mkdir(dirname(target))
    FILE <- tempfile(pattern = paste0(".", basename(target), ".spt."), tmpdir = dirname(target), fileext = ".tmp")
    OK <- FALSE
    on.exit(if (!OK && file.exists(FILE)) unlink(FILE, force = TRUE), add = TRUE)
    fwrite(data, FILE, eol = "\n")
    OK <- TRUE
    FILE
}

.recordStageIndexTable <- function(paths, output, target, processID = NULL, candidate = NULL) {
    Files <- .recordProcessTableSources(paths = paths, output = output, processID = processID, candidate = candidate)
    .recordValidateIndexSources(Files, output = output)
    .mkdir(dirname(target))
    FILE <- tempfile(pattern = paste0(".", basename(target), ".spt."), tmpdir = dirname(target), fileext = ".tmp")
    OK <- FALSE
    on.exit(if (!OK && file.exists(FILE)) unlink(FILE, force = TRUE), add = TRUE)
    if (!isTRUE(file.copy(Files[1L], FILE, overwrite = TRUE))) {
        stop("Could not stage process index source: ", Files[1L], call. = FALSE)
    }
    if (length(Files) > 1L) {
        for (Source in Files[-1L]) {
            .recordAppendCSVFile(Source, FILE, header = FALSE)
        }
    }
    OK <- TRUE
    FILE
}

.recordStageJSON <- function(data, target) {
    .mkdir(dirname(target))
    FILE <- tempfile(pattern = paste0(".", basename(target), ".spt."), tmpdir = dirname(target), fileext = ".tmp")
    OK <- FALSE
    on.exit(if (!OK && file.exists(FILE)) unlink(FILE, force = TRUE), add = TRUE)
    .writeJSON(data, FILE, auto_unbox = TRUE, pretty = TRUE, null = "null")
    OK <- TRUE
    FILE
}

.recordCommitFiles <- function(staged, paths) {
    Files <- copy(as.data.table(staged))
    .requireColumns(Files, c("source", "target"), "staged files")
    if (!nrow(Files)) 
        return(invisible(NULL))
    if (anyDuplicated(Files$target)) {
        stop("Transaction has duplicate target files.", call. = FALSE)
    }
    if (!all(file.exists(Files$source))) {
        stop("Transaction is missing staged source files.", call. = FALSE)
    }
    ROOT <- file.path(paths$process, ".spt-transactions")
    .mkdir(ROOT)
    Existing <- list.files(ROOT, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    Existing <- Existing[file.info(Existing)$isdir %in% TRUE]
    if (length(Existing)) {
        stop("Unresolved SPT metadata transaction: ", Existing[1L], call. = FALSE)
    }
    TX <- file.path(ROOT, "active")
    if (!isTRUE(dir.create(TX, showWarnings = FALSE))) {
        stop("Could not create metadata transaction: ", TX, call. = FALSE)
    }
    Files[, `:=`(existed, file.exists(target))]
    Files[, `:=`(backup, file.path(TX, sprintf("backup.%04d", .I)))]
    for (i in which(Files$existed)) {
        Linked <- isTRUE(suppressWarnings(file.link(Files$target[i], Files$backup[i])))
        if (!Linked && !isTRUE(file.copy(Files$target[i], Files$backup[i], overwrite = FALSE))) {
            unlink(TX, recursive = TRUE)
            stop("Could not snapshot metadata file: ", Files$target[i], call. = FALSE)
        }
    }
    Manifest <- list(state = "prepared", pid = Sys.getpid(), host = unname(Sys.info()[["nodename"]]), 
        startedUTC = format(Sys.time(), tz = "UTC", usetz = TRUE), files = as.data.frame(Files))
    ManifestFile <- file.path(TX, "manifest.json")
    Prepared <- tryCatch({
        .writeJSON(Manifest, ManifestFile, pretty = TRUE, auto_unbox = TRUE)
        TRUE
    }, error = function(e) e)
    if (inherits(Prepared, "error")) {
        unlink(TX, recursive = TRUE)
        stop(Prepared)
    }
    PromotionError <- NULL
    PromotionInterrupt <- NULL
    tryCatch({
        for (i in seq_len(nrow(Files))) {
            .recordReplaceFile(Files$source[i], Files$target[i])
        }
    }, error = function(e) PromotionError <<- e, interrupt = function(e) PromotionInterrupt <<- e)
    PromotionCondition <- if (!is.null(PromotionInterrupt)) {
        PromotionInterrupt
    }
    else {
        PromotionError
    }
    if (!is.null(PromotionCondition)) {
        RollbackOK <- .recordRollbackFiles(Files)
        if (RollbackOK) {
            unlink(TX, recursive = TRUE)
            Left <- Files$source[file.exists(Files$source)]
            if (length(Left)) 
                unlink(Left, force = TRUE)
            if (!is.null(PromotionInterrupt)) {
                stop(PromotionInterrupt)
            }
            stop(PromotionError)
        }
        stop(.recordAmbiguousCommit(paste0("Metadata promotion failed and rollback was incomplete; ",
            "preserving outputs and recovery transaction: ", TX), transaction = TX,
            cause = PromotionCondition, files = Files))
    }
    unlink(TX, recursive = TRUE)
    if (dir.exists(TX)) {
        .warnSignal("Metadata committed but transaction cleanup failed: ", TX,
            operation = ".recordCommitFiles", path = TX, phase = "cleanup")
    }
    invisible(Files$target)
}

.recordWriteProcessContract <- function(processID, paths, record, table, params = list()) {
    .requireWritableProcess(processID)
    FILE <- .recordProcessContractFile(processID, paths)
    if (!is.list(params)) 
        stop("params must be a list.", call. = FALSE)
    LIST <- if (file.exists(FILE)) {
        jsonlite::read_json(FILE, simplifyVector = FALSE)
    }
    else {
        list()
    }
    AUX <- .recordNull(LIST$params, list())
    if (!is.list(AUX)) 
        AUX <- list()
    if (length(params)) 
        AUX <- utils::modifyList(AUX, params)
    OUT <- list(processID = processID, output = list(record = unique(as.character(record)), table = unique(as.character(table))), 
        params = AUX)
    .recordWriteJSONAtomic(OUT, FILE, override = TRUE)
    invisible(FILE)
}

.recordWriteAtomic <- function(data, file, override = FALSE) {
    DIR <- dirname(file)
    .mkdir(DIR)
    if (file.exists(file) && !isTRUE(override)) {
        stop("Refusing to overwrite existing file: ", file, call. = FALSE)
    }
    FILE <- tempfile(pattern = paste0(".", basename(file), "."), tmpdir = DIR, fileext = ".tmp")
    on.exit(if (file.exists(FILE)) unlink(FILE, force = TRUE), add = TRUE)
    fwrite(data, FILE, eol = "\n")
    .recordReplaceFile(FILE, file)
    invisible(file)
}

.recordReplaceFile <- function(source, target) {
    if (!file.exists(source)) {
        stop("Replacement source does not exist: ", source, call. = FALSE)
    }
    .mkdir(dirname(target))
    if (isTRUE(suppressWarnings(file.rename(source, target)))) {
        return(invisible(target))
    }
    SWAP <- tempfile(pattern = paste0(".", basename(target), ".old."), tmpdir = dirname(target))
    HadTarget <- file.exists(target)
    if (HadTarget && !isTRUE(suppressWarnings(file.rename(target, SWAP)))) {
        stop("Could not preserve existing file: ", target, call. = FALSE)
    }
    OK <- isTRUE(suppressWarnings(file.rename(source, target)))
    if (!OK) {
        Restored <- !HadTarget || isTRUE(suppressWarnings(file.rename(SWAP, target)))
        if (!Restored) {
            stop("Could not replace ", target, " and could not restore its original; preserved original: ", 
                SWAP, call. = FALSE)
        }
        stop("Could not replace file: ", target, call. = FALSE)
    }
    if (HadTarget && file.exists(SWAP)) 
        unlink(SWAP, force = TRUE)
    invisible(target)
}

.saveRecordTables <- function(row, paths, tables, override = FALSE, replace = character()) {
    .requireWritableProcess(row$ProcessID)
    paths <- .recordPaths(paths, need = "records")
    Row <- as.data.table(row)
    .requireColumns(Row, ProcessKeys, "record row")
    if (nrow(Row) != 1L) 
        stop("row must describe one record.", call. = FALSE)
    if (identical(as.character(Row$ProcessID[1L]), "raw")) {
        stop("saveRecordTables() does not write raw records.", call. = FALSE)
    }
    if (!is.list(tables) || is.null(names(tables)) || any(!nzchar(names(tables)))) {
        stop("tables must be a named list.", call. = FALSE)
    }
    Bad <- setdiff(names(tables), c("AT", "TS", "TSW", "IMW", "IMF", "IMFF", "PSW"))
    if (length(Bad)) {
        stop("Unsupported record output: ", paste(Bad, collapse = ", "), call. = FALSE)
    }
    Replace <- unique(as.character(replace))
    Replace <- Replace[nzchar(Replace) & !is.na(Replace)]
    Bad <- setdiff(Replace, names(tables))
    if (length(Bad)) {
        stop("replace names are not being written: ", paste(Bad, collapse = ", "), call. = FALSE)
    }
    OUT <- vector("list", length(tables))
    names(OUT) <- names(tables)
    for (COL in names(tables)) {
        DT <- .recordSubsetOutput(as.data.table(tables[[COL]]), row = Row, output = COL)
        FILE <- .recordLocalFile(Row, pathRecords = paths$records, output = COL)
        .recordWriteAtomic(data = DT, file = FILE, override = isTRUE(override) || COL %chin% Replace)
        OUT[[COL]] <- data.table(ProcessID = as.character(Row$ProcessID[1L]), RecordID = as.character(Row$RecordID[1L]), 
            OwnerID = as.character(Row$OwnerID[1L]), EventID = as.character(Row$EventID[1L]), StationID = as.character(Row$StationID[1L]), 
            output = COL, FILE = FILE)
    }
    rbindlist(OUT, use.names = TRUE)
}

.recordTextMD5 <- function(text) {
    FILE <- tempfile(pattern = "spt-md5-")
    on.exit(unlink(FILE, force = TRUE), add = TRUE)
    writeBin(charToRaw(enc2utf8(paste(text, collapse = ""))), FILE)
    OUT <- unname(tools::md5sum(FILE))
    as.character(OUT)
}

.recordContractMD5 <- function(processID, paths) {
    FILE <- .recordProcessContractFile(processID = processID, paths = paths)
    if (!file.exists(FILE)) {
        stop("Missing process contract: ", FILE, call. = FALSE)
    }
    as.character(unname(tools::md5sum(FILE)))
}

.recordAppendMD5 <- function(value, label) {
    if (is.null(value) || !length(value)) 
        return(NULL)
    OUT <- tolower(as.character(value)[1L])
    if (is.na(OUT) || !grepl("^[0-9a-f]{32}$", OUT)) {
        stop(label, " must be one 32-character MD5 string.", call. = FALSE)
    }
    OUT
}

.recordRequireCompatibleTypes <- function(tables, label) {
    if (length(tables) < 2L) 
        return(invisible(NULL))
    COLS <- names(tables[[1L]])
    for (i in seq_along(tables)[-1L]) {
        for (COL in COLS) {
            A <- tables[[1L]][[COL]]
            B <- tables[[i]][[COL]]
            Integer64 <- inherits(A, "integer64") || inherits(B, "integer64")
            Compatible <- if (Integer64) {
                identical(class(A), class(B))
            }
            else {
                (identical(typeof(A), typeof(B)) && identical(class(A), class(B))) || (is.numeric(A) && 
                  is.numeric(B)) || all(is.na(A)) || all(is.na(B))
            }
            if (!Compatible) {
                stop(label, " type mismatch for column ", COL, ".", call. = FALSE)
            }
        }
    }
    invisible(NULL)
}

.recordEnsureTrailingNewline <- function(file) {
    Size <- file.info(file)$size
    if (is.na(Size) || Size == 0L) 
        return(invisible(file))
    CON <- file(file, open = "rb")
    on.exit(close(CON), add = TRUE)
    seek(CON, where = Size - 1L, origin = "start")
    Last <- readBin(CON, what = "raw", n = 1L)
    close(CON)
    on.exit(NULL, add = FALSE)
    if (!identical(Last, as.raw(10L))) {
        CON <- file(file, open = "ab")
        on.exit(close(CON), add = TRUE)
        writeBin(as.raw(10L), CON)
    }
    invisible(file)
}

.recordProcessTableSources <- function(paths, output, processID = NULL, candidate = NULL) {
    DIRS <- .recordProcessDirs(paths)
    OUT <- character()
    IDs <- character()
    for (DIR in DIRS) {
        ID <- basename(DIR)
        FILE <- if (!is.null(processID) && identical(ID, processID) && !is.null(candidate)) {
            candidate
        }
        else {
            file.path(DIR, sprintf("%s.csv", output))
        }
        if (file.exists(FILE)) {
            OUT <- c(OUT, FILE)
            IDs <- c(IDs, ID)
        }
    }
    if (!is.null(processID) && !processID %chin% IDs && !is.null(candidate)) {
        OUT <- c(OUT, candidate)
        IDs <- c(IDs, processID)
    }
    names(OUT) <- IDs
    OUT
}

.recordValidateIndexSources <- function(files, output) {
    if (!length(files)) {
        stop("process ", output, " has no source tables.", call. = FALSE)
    }
    Tables <- lapply(files, .recordFread)
    COLS <- names(Tables[[1L]])
    if (!all(vapply(Tables, function(x) identical(names(x), COLS), logical(1L)))) {
        stop("process ", output, " schema mismatch before binding.", call. = FALSE)
    }
    .recordRequireCompatibleTypes(Tables, label = sprintf("process %s", output))
    Maps <- lapply(seq_along(Tables), function(i) {
        x <- Tables[[i]]
        Expected <- names(files)[i]
        .requireColumns(x, RecordKeys, sprintf("process %s", output))
        if ("ProcessID" %chin% names(x)) {
            IDs <- unique(as.character(x$ProcessID))
        }
        else {
            IDs <- Expected
            x[, `:=`(ProcessID, Expected)]
        }
        if (length(IDs) != 1L || is.na(IDs) || !nzchar(IDs) || !identical(IDs, Expected)) {
            stop("process ", output, " source does not match its ProcessID: ", files[i], call. = FALSE)
        }
        unique(x[, .SD, .SDcols = ProcessKeys])
    })
    Map <- rbindlist(Maps, use.names = TRUE)
    Duplicate <- Map[, .N, by = .(ProcessID, RecordID)][N > 1L]
    if (nrow(Duplicate)) {
        Label <- paste0(Duplicate$ProcessID, "/", Duplicate$RecordID)
        stop("process ", output, " has record-key collisions: ", paste(Label, collapse = ", "), call. = FALSE)
    }
    invisible(NULL)
}

.recordAppendCSVFile <- function(source, target, header = FALSE) {
    .recordEnsureTrailingNewline(target)
    IN <- file(source, open = "rb")
    OUT <- file(target, open = "ab")
    on.exit({
        close(IN)
        close(OUT)
    }, add = TRUE)
    SkipHeader <- !isTRUE(header)
    repeat {
        Bytes <- readBin(IN, what = "raw", n = 1024L * 1024L)
        if (!length(Bytes)) 
            break
        if (SkipHeader) {
            Newline <- which(Bytes == as.raw(10L))[1L]
            if (is.na(Newline)) 
                next
            Bytes <- if (Newline < length(Bytes)) {
                Bytes[(Newline + 1L):length(Bytes)]
            }
            else {
                raw()
            }
            SkipHeader <- FALSE
        }
        writeBin(Bytes, OUT)
    }
    if (SkipHeader) {
        stop("CSV source has no header newline: ", source, call. = FALSE)
    }
    invisible(target)
}

.recordRollbackFiles <- function(files) {
    OK <- TRUE
    for (i in rev(seq_len(nrow(files)))) {
        Target <- files$target[i]
        if (isTRUE(files$existed[i])) {
            Restore <- tempfile(pattern = paste0(".", basename(Target), ".restore."), tmpdir = dirname(Target), 
                fileext = ".tmp")
            Copied <- isTRUE(suppressWarnings(file.link(files$backup[i], Restore))) || isTRUE(file.copy(files$backup[i], 
                Restore, overwrite = TRUE))
            if (!Copied) {
                OK <- FALSE
                next
            }
            Replaced <- tryCatch({
                .recordReplaceFile(Restore, Target)
                TRUE
            }, error = function(e) FALSE)
            if (!Replaced) 
                OK <- FALSE
            if (file.exists(Restore)) 
                unlink(Restore, force = TRUE)
        }
        else if (file.exists(Target)) {
            if (!isTRUE(unlink(Target, force = TRUE) == 0L)) 
                OK <- FALSE
        }
    }
    OK
}

.recordAmbiguousCommit <- function(message, transaction, cause = NULL, files = NULL) {
    structure(list(message = message, call = NULL, transaction = transaction,
        operation = "processProject", phase = "publication", path = transaction,
        cause = cause, failures = list(list(cause = cause)), result = files),
        class = c("gmspPartialPublication", "modulePartialPublication", "moduleFailure",
                  "spt_ambiguous_commit", "error", "condition"))
}

.recordWriteJSONAtomic <- function(data, file, override = FALSE, digits = 4) {
    DIR <- dirname(file)
    .mkdir(DIR)
    if (file.exists(file) && !isTRUE(override)) {
        stop("Refusing to overwrite existing file: ", file, call. = FALSE)
    }
    FILE <- tempfile(pattern = paste0(".", basename(file), "."), tmpdir = DIR, fileext = ".tmp")
    on.exit(if (file.exists(FILE)) unlink(FILE, force = TRUE), add = TRUE)
    .writeJSON(data, FILE, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = digits)
    .recordReplaceFile(FILE, file)
    invisible(file)
}

.recordSubsetOutput <- function(data, row, output) {
    .requireColumns(data, RecordKeys, output)
    OUT <- copy(data)
    if (identical(output, "IMFF") && nrow(OUT) == 0L) {
        for (COL in setdiff(ProcessKeys, names(OUT))) set(OUT, j = COL, value = character())
        return(.recordAddKeyColumns(data = OUT, row = row))
    }
    for (COL in RecordKeys) {
        OUT <- OUT[as.character(get(COL)) == as.character(row[[COL]][1L])]
    }
    if (!nrow(OUT)) {
        stop(output, " has zero rows for ", row$RecordID, ".", call. = FALSE)
    }
    .recordAddKeyColumns(data = OUT, row = row)
}

.recordProcessDirs <- function(paths) {
    DIRS <- list.files(paths$process, full.names = TRUE, recursive = FALSE)
    DIRS[file.info(DIRS)$isdir %in% TRUE]
}
