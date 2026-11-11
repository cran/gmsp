# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.processRecords <- function(selection, paths, processID, stages, config = list(), out = list(), parallel = FALSE, 
    workers = 1L, logEvery = 30, override = FALSE, evidence = NULL) {
    .requireWritableProcess(processID)
    paths <- .recordPaths(paths, need = c("records", "index", "process"))
    Selection <- copy(as.data.table(selection))
    .requireColumns(Selection, RecordKeys, "processRecords selection")
    if (anyDuplicated(Selection$RecordID)) {
        Duplicate <- unique(Selection$RecordID[duplicated(Selection$RecordID)])
        .stopUsage("processRecords selection has duplicate RecordID: ", paste(Duplicate, collapse = ", "),
                   operation = "processProject")
    }
    Selection <- unique(Selection, by = RecordKeys)
    Selection[, `:=`((RecordKeys), lapply(.SD, as.character)), .SDcols = RecordKeys]
    Selection[, `:=`(ProcessID, processID)]
    setcolorder(Selection, c("ProcessID", RecordKeys, setdiff(names(Selection), c("ProcessID", RecordKeys))))
    Stages <- .processStages(stages)
    ModalParams <- .processModalExecution(Stages, config, parallel = parallel, workers = workers)
    if (!is.null(ModalParams) && ModalParams$stochastic) setorderv(Selection, RecordKeys)
    IndexLock <- .recordAcquireLock(.recordIndexLockPath(paths), "global process index")
    on.exit(.recordReleaseLock(IndexLock), add = TRUE)
    ProcessLock <- .recordAcquireLock(.recordProcessLockPath(paths = paths, processID = processID), sprintf("process %s", 
        processID))
    on.exit(.recordReleaseLock(ProcessLock), add = TRUE)
    .recordRequireNoTransaction(paths)
    Plan <- .processAppendPlan(selection = Selection, paths = paths, processID = processID, stages = Stages, 
        config = config, override = override)
    Todo <- Plan$run
    CreatedFiles <- if (isTRUE(override)) 
        character()
    else .processOutputFiles(Todo, paths = paths, outputs = Plan$outputs)
    Committed <- isTRUE(nrow(Todo) == 0L)
    CleanupSafe <- TRUE
    on.exit({
        Ambiguous <- length(.recordTransactionDirs(paths)) > 0L
        if (!isTRUE(override) && !Committed && CleanupSafe && !Ambiguous) {
            .processCleanupFiles(CreatedFiles)
        }
    }, add = TRUE)
    Modal <- .processModalStart(ModalParams, run = Todo, skip = Plan$skip)
    .processModalEvidence(Modal, evidence)
    .worker <- function(row) {
        tryCatch(.processRecord(row = row, paths = paths, processID = processID,
                                stages = Stages, config = config, out = out,
                                override = override),
                 error = function(e) list(row = row, RecordID = as.character(row$RecordID[1L]),
                                          status = "failed", error = conditionMessage(e), cause = e,
                                          stages = list(), IMW = NULL, tables = list()))
    }
    LIST <- if (!nrow(Todo)) {
        list()
    }
    else if (isTRUE(parallel) && workers > 1L) {
        .runRecordsParallel(Todo, processRecord = .worker, workers = workers,
                            logEvery = logEvery, label = "process")
    }
    else {
        .runRecordsSequential(Todo, processRecord = .worker, label = "process")
    }
    Modal <- .processModalFinish(Modal, LIST)
    .processModalEvidence(Modal, evidence)
    Status <- if (length(LIST)) {
        vapply(LIST, `[[`, character(1L), "status")
    }
    else {
        character()
    }
    if (length(Status) && any(Status == "failed")) {
        BAD <- vapply(LIST[Status == "failed"], function(x) {
            paste0(x[["RecordID"]], ": ", x[["error"]])
        }, character(1L))
        if (isTRUE(override)) {
            Files <- .processOutputFiles(Todo, paths = paths, outputs = Plan$outputs)
            .stopPartialPublication("Processing failed after replacement was enabled: ",
              paste(BAD, collapse = "\n"), operation = "processProject", path = paths$records,
              failures = LIST[Status == "failed"], result = Files[file.exists(Files)])
        }
        stop(structure(list(message = paste0("processRecord failed for ", length(BAD),
            " records:\n", paste(BAD, collapse = "\n")), call = NULL,
            operation = "processProject", phase = "processing", failures = LIST[Status == "failed"]),
            class = c("gmspFailure", "moduleFailure", "error", "condition")))
    }
    Outputs <- Plan$outputs
    IMWList <- lapply(LIST, `[[`, "IMW")
    HasIMW <- vapply(IMWList, Negate(is.null), logical(1L))
    Extra <- list()
    ComponentMap <- lapply(LIST, function(x) x$tables$ComponentMapTable)
    ComponentMap <- ComponentMap[!vapply(ComponentMap, is.null, logical(1L))]
    if (length(ComponentMap)) {
        Extra$ComponentMapTable <- rbindlist(ComponentMap, use.names = TRUE)
    }
    Trim <- lapply(LIST, function(x) x$tables$TrimWindowTable)
    Trim <- Trim[!vapply(Trim, is.null, logical(1L))]
    if (length(Trim)) {
        Extra$TrimWindowTable <- rbindlist(Trim, use.names = TRUE)
    }
    ContractParams <- .processStageParams(config = config, stages = Stages)
    if (!is.null(Modal)) ContractParams$modalExecution <- Modal
    if (!isTRUE(override)) {
        ContractParams$sptAppendSignature <- Plan$signature
        ContractParams$appendRecipe <- Plan$recipe
        ContractParams$appendProvenance <- Plan$provenance
    }
    if (nrow(Todo) && length(Outputs) && any(HasIMW)) {
        suspendInterrupts(tryCatch({
            .writeProcessTables(index = Plan$index, intensity = rbindlist(IMWList[HasIMW], use.names = TRUE), 
                paths = paths, processID = processID, record = Outputs, extra = Extra, params = ContractParams, 
                override = override, signature = Plan$signature, recipe = Plan$recipe, contractMD5 = Plan$contractMD5, 
                reseal = Plan$reseal, provenance = Plan$provenance, lockHeld = TRUE)
            Committed <- TRUE
        }, spt_ambiguous_commit = function(e) {
            CleanupSafe <<- FALSE
            stop(e)
        }))
    }
    else if (nrow(Todo) && length(Outputs) && isTRUE(override)) {
        .updateProcessContract(paths = paths, processID = processID, record = Outputs, table = names(Extra), 
            params = ContractParams)
        Committed <- TRUE
    }
    else if (nrow(Todo) && length(Outputs)) {
        stop("Append produced no IMW rows; no records were committed.", call. = FALSE)
    }
    SelectionDir <- tryCatch({
        DIR <- .processWriteSelection(selection = Selection, out = out)
        .processPublishEvidence(source = evidence, out = out, processID = processID)
        DIR
    }, error = function(e) {
        .stopPartialPublication("Process products committed but execution evidence failed: ",
          conditionMessage(e), operation = "processProject", path = paths$process,
          failures = list(list(cause = e, processID = processID)),
          result = list(appended = nrow(Todo), skipped = nrow(Plan$skip),
            products = .processOutputFiles(Selection, paths = paths, outputs = Outputs)))
    })
    Skipped <- lapply(seq_len(nrow(Plan$skip)), function(i) {
        list(row = Plan$skip[i], status = "skipped", stages = list(), IMW = NULL, tables = list())
    })
    Records <- c(LIST, Skipped)
    if (length(Records)) {
        IDs <- vapply(Records, function(x) as.character(x$row$RecordID[1L]), character(1L))
        Records <- Records[match(Selection$RecordID, IDs)]
    }
    OUT <- list(records = Records, selection = SelectionDir, appended = nrow(Todo), skipped = nrow(Plan$skip), 
        process = .recordProcessContractFile(processID = processID, paths = paths), modal = Modal)
    OUT
}

.processRecord <- function(row, paths, processID, stages, config = list(), out = list(), override = FALSE) {
    paths <- .recordPaths(paths, need = c("records", "index", "process"))
    Stages <- .processStages(stages)
    Row <- .processRow(row = row, processID = processID)
    Outputs <- .processRecordOutputs(Stages)
    if (!isTRUE(override)) {
        Files <- vapply(Outputs, function(Output) {
            .processOutputFile(row = Row, paths = paths, output = Output)
        }, character(1L))
        Existing <- Files[file.exists(Files)]
        if (length(Existing)) {
            stop("Refusing unplanned pre-existing record output: ", Existing[1L], call. = FALSE)
        }
    }
    OUT <- list()
    Tables <- list()
    IMW <- NULL
    Produced <- character()
    for (Stage in Stages) {
        if (identical(Stage, "gmsp")) {
            OUT$gmsp <- .processRunGMSP(row = Row, paths = paths, processID = processID, config = .processConfig(config, 
                "gmsp"), out = out, override = override)
            IMW <- .processReadOutput(row = Row, paths = paths, output = "IMW")
            Tables$ComponentMapTable <- OUT$gmsp$ComponentMapTable
            Produced <- unique(c(Produced, "TSW", "IMW"))
        }
        if (identical(Stage, "trim")) {
            OUT$trim <- .processRunTrim(row = Row, paths = paths, config = .processConfig(config,
                "trim"), override = override, replace = intersect(c("TSW", "IMW"), Produced))
            IMW <- OUT$trim$IMW
            Tables$TrimWindowTable <- OUT$trim$TrimWindowTable
            Produced <- unique(c(Produced, "TSW", "IMW"))
        }
        if (identical(Stage, "imf")) {
            OUT$imf <- .processRunIMF(row = Row, paths = paths, config = .processConfig(config,
                "imf"), override = override)
            Produced <- unique(c(Produced, "IMF", "IMFF"))
        }
        if (identical(Stage, "imfts")) {
            OUT$imfTS <- .processRunIMFTS(row = Row, paths = paths, config = .processConfig(config, "imfTS"), 
                override = override, replace = intersect(c("TSW", "IMW"), Produced))
            IMW <- OUT$imfTS$IMW
            Produced <- unique(c(Produced, "TSW", "IMW"))
        }
        if (identical(Stage, "psa")) {
            OUT$psa <- .processRunPSA(row = Row, paths = paths, config = .processConfig(config,
                "psa"), override = override)
            Produced <- unique(c(Produced, "PSW"))
        }
    }
    list(row = Row, status = "done", stages = OUT, IMW = IMW, tables = Tables)
}

.processStages <- function(stages) {
    OUT <- unique(tolower(gsub("[._-]", "", as.character(stages))))
    OUT <- OUT[nzchar(OUT) & !is.na(OUT)]
    if (!length(OUT)) 
        .stopUsage("stages must contain at least one stage.", operation = "processProject")
    Bad <- setdiff(OUT, c("gmsp", "trim", "imf", "imfts", "psa"))
    if (length(Bad)) {
        .stopUsage("Unsupported process stage: ", paste(Bad, collapse = ", "), operation = "processProject")
    }
    OUT
}

.processStageDir <- function(out, processID, stage) {
    file.path(out$stage, sprintf("%s.%s", processID, stage))
}

.processStageLabel <- function(stages) {
    OUT <- stages
    OUT[OUT == "imfts"] <- "imfTS"
    OUT
}

.processAppendPlan <- function(selection, paths, processID, stages, config, override = FALSE) {
    Outputs <- .processRecordOutputs(stages)
    Tables <- .processTableOutputs(stages)
    Recipe <- .processRecipe(stages = stages, config = config)
    Signature <- .processSignature(Recipe)
    Provenance <- .processProvenance()
    Reseal <- .jsonGet(config, c("append", "reseal"), default = NULL)
    if (isTRUE(override)) {
        ContractFile <- .recordProcessContractFile(processID = processID, paths = paths)
        LedgerFile <- .recordAppendContractFile(processID = processID, paths = paths)
        AppendEnabled <- file.exists(LedgerFile)
        if (file.exists(ContractFile)) {
            JSON <- .recordReadProcessJSON(processID = processID, paths = paths, required = TRUE)
            AppendEnabled <- AppendEnabled || length(.jsonGet(JSON, c("params", "sptAppendSignature"), 
                default = character())) > 0L
        }
        if (AppendEnabled) {
            stop("override=true is disabled for append-pinned process ", processID, "; use a new ProcessID for a different recipe.", 
                call. = FALSE)
        }
        return(list(run = selection, skip = selection[0L], index = selection, outputs = Outputs, tables = Tables, 
            signature = Signature, recipe = Recipe, provenance = Provenance, contractMD5 = NULL,
            reseal = NULL, existing = FALSE))
    }
    if (!"IMW" %chin% Outputs) {
        stop(paste0("override=false append requires an IMW-producing stage so ", "MasterIndex and IntensityTable can define committed records."), 
            call. = FALSE)
    }
    SelectionMap <- .processValidateRecordMap(selection[, RecordKeys, with = FALSE], "process selection")
    ContractFile <- .recordProcessContractFile(processID = processID, paths = paths)
    MasterFile <- .recordTableFile(processID = processID, paths = paths, output = "MasterIndex")
    HasContract <- file.exists(ContractFile)
    HasMaster <- file.exists(MasterFile)
    ProcessDir <- .recordProcessDir(processID = processID, paths = paths)
    HasTables <- length(list.files(ProcessDir, pattern = "[.]csv$")) > 0L
    if (xor(HasContract, HasMaster) || xor(HasContract, HasTables)) {
        stop("Process ", processID, " has an incomplete contract/table baseline.", call. = FALSE)
    }
    ContractMD5 <- .jsonGet(config, c("append", "contractMD5"), default = NULL)
    if (!HasContract) {
        Files <- .processOutputFiles(selection, paths = paths, outputs = Outputs)
        Existing <- Files[file.exists(Files)]
        if (length(Existing)) {
            stop("Uncommitted output already exists: ", Existing[1L], call. = FALSE)
        }
        return(list(run = selection, skip = selection[0L], index = selection, outputs = Outputs, tables = Tables, 
            signature = Signature, recipe = Recipe, provenance = Provenance, contractMD5 = ContractMD5,
            reseal = Reseal, existing = FALSE))
    }
    Info <- .recordRequireAppendContract(processID = processID, paths = paths, record = Outputs, table = Tables, 
        signature = Signature, recipe = Recipe, contractMD5 = ContractMD5, reseal = Reseal,
        provenance = Provenance)
    Master <- .processReadKeyTable(MasterFile, columns = RecordKeys, label = sprintf("%s MasterIndex", 
        processID))
    ExistingMap <- .processValidateRecordMap(Master, sprintf("%s MasterIndex", processID))
    At <- match(SelectionMap$RecordID, ExistingMap$RecordID)
    Hit <- !is.na(At)
    if (any(Hit)) {
        for (COL in setdiff(RecordKeys, "RecordID")) {
            Bad <- Hit & SelectionMap[[COL]] != ExistingMap[[COL]][At]
            if (any(Bad)) {
                stop("RecordID key collision in ", processID, ": ", SelectionMap$RecordID[which(Bad)[1L]], 
                  call. = FALSE)
            }
        }
    }
    TableMaps <- lapply(Info$contract$process, function(Table) {
        FILE <- .recordTableFile(processID = processID, paths = paths, output = Table)
        .processValidateRecordMap(.processReadKeyTable(FILE, columns = RecordKeys, label = sprintf("%s %s", 
            processID, Table)), sprintf("%s %s", processID, Table))
    })
    names(TableMaps) <- Info$contract$process
    for (Table in names(TableMaps)) {
        Missing <- fsetdiff(ExistingMap, TableMaps[[Table]])
        Extra <- fsetdiff(TableMaps[[Table]], ExistingMap)
        if (nrow(Missing) || nrow(Extra)) {
            stop(processID, " ", Table, " record-key map differs from MasterIndex.", call. = FALSE)
        }
    }
    NewSelection <- selection[!Hit]
    IndexSelection <- if (nrow(NewSelection)) {
        .processProjectLegacyIndex(selection = NewSelection, paths = paths, processID = processID)
    }
    else {
        NewSelection
    }
    for (i in which(Hit)) {
        ID <- SelectionMap$RecordID[i]
        Files <- .processOutputFiles(selection[i], paths = paths, outputs = Outputs)
        MissingFile <- Files[!file.exists(Files) | file.info(Files)$size <= 0]
        if (length(MissingFile)) {
            stop("Committed RecordID ", ID, " has missing output: ", MissingFile[1L], call. = FALSE)
        }
    }
    for (i in which(!Hit)) {
        Files <- .processOutputFiles(selection[i], paths = paths, outputs = Outputs)
        Existing <- Files[file.exists(Files)]
        if (length(Existing)) {
            stop("Uncommitted output already exists for ", SelectionMap$RecordID[i], ": ", Existing[1L], 
                call. = FALSE)
        }
    }
    list(run = NewSelection, skip = selection[Hit], index = IndexSelection, outputs = Outputs, tables = Tables, 
        signature = Signature, recipe = Recipe, provenance = Provenance, contractMD5 = ContractMD5,
        reseal = Reseal, existing = TRUE)
}

.processOutputFiles <- function(selection, paths, outputs) {
    if (!nrow(selection) || !length(outputs)) 
        return(character())
    unlist(lapply(seq_len(nrow(selection)), function(i) {
        vapply(outputs, function(Output) {
            .processOutputFile(row = selection[i], paths = paths, output = Output)
        }, character(1L))
    }), use.names = FALSE)
}

.processCleanupFiles <- function(files) {
    Files <- unique(as.character(files))
    Files <- Files[nzchar(Files) & !is.na(Files) & file.exists(Files)]
    if (length(Files)) {
        unlink(Files, force = TRUE)
        Left <- Files[file.exists(Files)]
        if (length(Left)) {
            .warnSignal("Could not clean uncommitted outputs: ", paste(Left, collapse = ", "),
                operation = ".processCleanupFiles", path = Left, phase = "cleanup")
        }
    }
    invisible(NULL)
}

.processStageParams <- function(config, stages) {
    OUT <- list()
    Label <- .processStageLabel(stages)
    for (i in seq_along(stages)) OUT[[Label[i]]] <- .processConfig(config, Label[i])
    list(stages = OUT)
}

.processWriteSelection <- function(selection, out) {
    DIR <- file.path(out$stage, sprintf("%s.selection", selection$ProcessID[1L]))
    .mkdir(DIR)
    .recordWriteAtomic(selection, file.path(DIR, "selection.csv"), override = TRUE)
    DIR
}

.processPublishEvidence <- function(source, out, processID) {
    if (is.null(source) || !length(source)) 
        return(invisible(NULL))
    Source <- path.expand(as.character(source)[1L])
    if (!file.exists(Source)) {
        stop("Process evidence source does not exist: ", Source, call. = FALSE)
    }
    DIR <- .processStageDir(out = out, processID = processID, stage = "process")
    .mkdir(DIR)
    Target <- file.path(DIR, "runProcess.json")
    Staged <- tempfile(pattern = ".runProcess.", tmpdir = DIR, fileext = ".tmp")
    on.exit(if (file.exists(Staged)) unlink(Staged, force = TRUE), add = TRUE)
    Published <- {
        if (!isTRUE(file.copy(Source, Staged, overwrite = FALSE))) {
            stop("copy failed", call. = FALSE)
        }
        .recordReplaceFile(Staged, Target)
        TRUE
    }
    invisible(Published)
}

.processRow <- function(row, processID) {
    OUT <- copy(as.data.table(row))
    .requireColumns(OUT, RecordKeys, "record row")
    if (nrow(OUT) != 1L) 
        .stopUsage("processRecord() row must have one record.", operation = "processProject")
    OUT[, `:=`((RecordKeys), lapply(.SD, as.character)), .SDcols = RecordKeys]
    OUT[, `:=`(ProcessID, processID)]
    setcolorder(OUT, c("ProcessID", RecordKeys, setdiff(names(OUT), c("ProcessID", RecordKeys))))
    OUT[]
}

.processRecordOutputs <- function(stages) {
    unique(c(if (any(c("gmsp", "trim", "imfts") %chin% stages)) c("TSW", "IMW"), if ("imf" %chin% stages) c("IMF", 
        "IMFF"), if ("psa" %chin% stages) "PSW"))
}

.processOutputFile <- function(row, paths, output) {
    file.path(paths$records, row$OwnerID, row$EventID, row$StationID, row$ProcessID, sprintf("%s.%s.csv", 
        output, row$RecordID))
}

.processRunGMSP <- function(row, paths, processID, config, out, override) {
    DIR <- .processStageDir(out = out, processID = processID, stage = "gmsp")
    .mkdir(.workRoot(DIR))
    OUT <- .processGMSPRecord(row = row[, RecordKeys, with = FALSE], path = DIR, params = .processGMSPParams(config = config, 
        paths = paths, processID = processID), override = override)
    if (identical(OUT$status, "failed")) 
        stop(OUT$cause)
    ComponentMapTable <- .recordFread(file.path(.workPath(DIR, row$RecordID), "OCIDMAP.csv"))
    ComponentMapTable[, `:=`(ProcessID, processID)]
    setcolorder(ComponentMapTable, c("ProcessID", RecordKeys, "OCID", "DIR", "theta", setdiff(names(ComponentMapTable), 
        c("ProcessID", RecordKeys, "OCID", "DIR", "theta"))))
    list(status = OUT, ComponentMapTable = ComponentMapTable)
}

.processConfig <- function(config, stage) {
    .jsonGet(config, stage, list())
}

.processReadOutput <- function(row, paths, output) {
    FILE <- .processOutputFile(row = row, paths = paths, output = output)
    if (!file.exists(FILE)) {
        stop("Missing ", output, " product for ", row$RecordID, ": ", FILE, call. = FALSE)
    }
    .recordFread(FILE)
}

.processRunTrim <- function(row, paths, config, override, replace = character()) {
    Params <- .processTrimParams(config)
    TSW <- .processReadOutput(row = row, paths = paths, output = "TSW")
    OUT <- .buildTrimmedTS(tsw = TSW, params = Params)
    .saveRecordTables(row = row, paths = paths, tables = list(TSW = OUT$TSW, IMW = OUT$IMW), override = override, 
        replace = replace)
    list(status = list(RecordID = row$RecordID, status = "done", n.TSW = nrow(OUT$TSW), n.IMW = nrow(OUT$IMW)), 
        IMW = OUT$IMW, TrimWindowTable = OUT$WINDOW)
}

.processRunIMF <- function(row, paths, config, override) {
    Params <- .processIMFParams(config)
    TSW <- .recordAddKeyColumns(.processReadOutput(row, paths, "TSW"), row)
    Columns <- intersect(c(ProcessKeys, "IMF", "Units"), names(TSW))
    TSL <- lengthenTSW(TSW, by = Columns)
    Decomposition <- decomposeRecords(TSL, method = Params$Method, control = Params$control, by = ProcessKeys)
    IMF <- .widenModes(Decomposition)
    Metrics <- xemd::measureModesTSL(Decomposition)$metrics
    IMFF <- .translateMetrics(Metrics)
    .saveRecordTables(row = row, paths = paths, tables = list(IMF = IMF, IMFF = IMFF), override = override)
    list(RecordID = row$RecordID, status = "done", n.IMF = nrow(IMF), n.IMFF = nrow(IMFF),
         effective = .processEffectiveControls(Decomposition, Params))
}

.processRunIMFTS <- function(row, paths, config, override, replace = character()) {
    Params <- .processIMFTSParams(config)
    IMF <- .processReadOutput(row, paths, "IMF")
    if ("Units" %in% names(IMF) &&
        (!is.character(IMF$Units) || anyNA(IMF$Units) || any(tolower(IMF$Units) != tolower(Params$units)))) {
        .stopUsage("IMF Units contradict imfTS.unitsTarget.", operation = "processProject")
    }
    Decomposition <- .lengthenIMF(IMF, key = row[, ProcessKeys, with = FALSE])
    Reconstructed <- reconstructRecords(Decomposition, criterion = Params$criterion,
      by = ProcessKeys, signalControl = Params$signalControl, includeResidue = Params$includeResidue)
    OUT <- .signalProducts(Reconstructed$TSL, units = Params$units)
    .saveRecordTables(row = row, paths = paths, tables = OUT, override = override, replace = replace)
    list(status = list(RecordID = row$RecordID, status = "done", n.TSW = nrow(OUT$TSW), n.IMW = nrow(OUT$IMW)),
         IMW = OUT$IMW, metrics = Reconstructed$metrics, selection = Reconstructed$selection)
}

.processRunPSA <- function(row, paths, config, override) {
    Params <- .processPSAParams(config)
    TSW <- .processReadOutput(row = row, paths = paths, output = "TSW")
    PSW <- .buildRecordPSW(TSW = TSW, XI = Params$XI, Tn = Params$Tn, D50 = Params$D50, D100 = Params$D100, 
        nTheta = Params$nTheta)
    .saveRecordTables(row = row, paths = paths, tables = list(PSW = PSW), override = override)
    list(RecordID = row$RecordID, status = "done", n.PSW = nrow(PSW))
}

.processTableOutputs <- function(stages) {
    Outputs <- .processRecordOutputs(stages)
    unique(c(if ("IMW" %chin% Outputs) c("MasterIndex", "IntensityTable"), if ("gmsp" %chin% stages) "ComponentMapTable", 
        if ("trim" %chin% stages) "TrimWindowTable"))
}

# The signed producer is a gmsp release on an R minor series. R patch releases
# and the installed bytes are provenance: recorded beside the seal, never compared.
.processProducer <- function() {
    list(gmsp = as.character(utils::packageVersion("gmsp")),
         R = paste(R.version$major, sub("[.].*$", "", R.version$minor), sep = "."))
}

.processProvenance <- function() {
    Path <- getNamespaceInfo(asNamespace("gmsp"), "path")
    Files <- c(file.path(Path, "NAMESPACE"),
               list.files(file.path(Path, c("R", "data", "libs", "extdata")),
                          recursive = TRUE, full.names = TRUE, all.files = TRUE))
    # Radix order ignores the session collation.
    Files <- sort(Files, method = "radix")
    Hashes <- vapply(Files, digest::digest, character(1L), algo = "md5", file = TRUE)
    names(Hashes) <- substring(Files, nchar(Path) + 2L)
    list(R = paste(R.version$major, R.version$minor, sep = "."), packageMD5 = as.list(Hashes))
}

.processRecipe <- function(stages, config) {
    Stages <- .processStages(stages)
    Params <- list()
    for (Stage in Stages) {
        if (identical(Stage, "gmsp")) {
            AUX <- .processGMSPParams(config = .processConfig(config, "gmsp"), paths = list(records = "<records>"), 
                processID = "<process>")
            AUX[c("PATH.records", "PATHS", "ProcessID")] <- NULL
            Params$gmsp <- AUX
        }
        if (identical(Stage, "trim")) {
            # These fields belong to the signed recipe even when the calculation does not use them.
            Params$trim <- c(list(PATH.source = NULL, PATHS = NULL, ProcessID = NULL),
                             .processTrimParams(.processConfig(config, "trim")),
                             list(files = c("TSW.csv", "IMW.csv", "WINDOW.csv")))
        }
        if (identical(Stage, "imf")) {
            Config <- .processConfig(config, "imf")
            Params$imf <- c(list(PATH.source = NULL, PATHS = NULL, ProcessID = NULL),
                            .processIMFParams(Config),
                            list(MaxF25Rows = as.numeric(.jsonGet(Config, "maxF25Rows", Inf))))
        }
        if (identical(Stage, "imfts")) {
            Params$imfTS <- .processIMFTSParams(.processConfig(config, "imfTS"))
        }
        if (identical(Stage, "psa")) {
            Params$psa <- .processPSAParams(.processConfig(config, "psa"))
        }
    }
    list(protocol = "gmsp-process-append-v3", producer = .processProducer(),
         stages = .processStageLabel(Stages), params = Params)
}

.processSignature <- function(recipe) {
    Recipe <- recipe
    JSON <- jsonlite::toJSON(Recipe, auto_unbox = TRUE, null = "null", na = "string", digits = NA, pretty = FALSE)
    .recordTextMD5(JSON)
}

.processValidateRecordMap <- function(data, label) {
    .requireColumns(data, RecordKeys, label)
    OUT <- unique(data[, RecordKeys, with = FALSE])
    for (COL in RecordKeys) {
        if (any(is.na(OUT[[COL]]) | !nzchar(OUT[[COL]]))) {
            .stopUsage(label, " has empty ", COL, " values.", operation = "processProject")
        }
    }
    Bad <- OUT[, .N, by = RecordID][N > 1L, RecordID]
    if (length(Bad)) {
        .stopUsage(label, " maps RecordID to multiple record keys: ", paste(Bad, collapse = ", "),
                   operation = "processProject")
    }
    OUT[]
}

.processReadKeyTable <- function(file, columns, label) {
    if (!file.exists(file)) {
        stop("Missing ", label, ": ", file, call. = FALSE)
    }
    Header <- names(fread(file, nrows = 0L, showProgress = FALSE))
    Missing <- setdiff(columns, Header)
    if (length(Missing)) {
        stop(label, " missing columns: ", paste(Missing, collapse = ", "), call. = FALSE)
    }
    OUT <- fread(file, select = columns, colClasses = "character", showProgress = FALSE)
    OUT[, `:=`((columns), lapply(.SD, as.character)), .SDcols = columns]
    OUT[]
}

.processProjectLegacyIndex <- function(selection, paths, processID) {
    MasterFile <- .recordTableFile(processID = processID, paths = paths, output = "MasterIndex")
    IntensityFile <- .recordTableFile(processID = processID, paths = paths, output = "IntensityTable")
    MasterHeader <- names(fread(MasterFile, nrows = 0L, showProgress = FALSE))
    IntensityHeader <- names(fread(IntensityFile, nrows = 0L, showProgress = FALSE))
    Metadata <- setdiff(MasterHeader, IntensityHeader)
    Required <- unique(c(RecordKeys, Metadata))
    OUT <- copy(selection)
    Aliases <- list(usgsEventId = "EventID.USGS")
    for (Target in names(Aliases)) {
        if (!Target %chin% names(OUT)) {
            Source <- Aliases[[Target]]
            if (Source %chin% names(OUT)) 
                OUT[, `:=`((Target), get(Source))]
        }
    }
    Missing <- setdiff(Required, names(OUT))
    if (length(Missing)) {
        stop("Legacy ", processID, " MasterIndex cannot be projected before processing; missing metadata: ", 
            paste(Missing, collapse = ", "), call. = FALSE)
    }
    OUT <- OUT[, ..Required]
    KeyClasses <- stats::setNames(rep("character", length(RecordKeys)), RecordKeys)
    MasterTypes <- fread(MasterFile, showProgress = FALSE, colClasses = KeyClasses)
    MasterProbe <- MasterTypes[1L]
    if (!nrow(MasterProbe)) {
        stop("Legacy ", processID, " MasterIndex has no rows.", call. = FALSE)
    }
    for (COL in Metadata) {
        Template <- MasterProbe[[COL]]
        Value <- OUT[[COL]]
        Coerced <- if (inherits(Template, "POSIXct")) {
            as.POSIXct(Value, tz = attr(Template, "tzone")[1L])
        }
        else if (inherits(Template, "Date")) {
            as.Date(Value)
        }
        else if (inherits(Template, "integer64")) {
            if (!requireNamespace("bit64", quietly = TRUE)) {
                stop("Legacy ", processID, " requires bit64 for metadata column ", COL, ".", call. = FALSE)
            }
            bit64::as.integer64(Value)
        }
        else {
            switch(typeof(Template), character = as.character(Value), integer = as.integer(Value), double = as.numeric(Value), 
                logical = as.logical(Value), stop("Legacy ", processID, " has unsupported metadata type for ", 
                  COL, ".", call. = FALSE))
        }
        Lost <- !is.na(Value) & is.na(Coerced)
        Changed <- if (typeof(Template) == "integer") {
            !is.na(Value) & suppressWarnings(as.numeric(Value)) != Coerced
        }
        else if (inherits(Template, "integer64")) {
            Text <- as.character(Value)
            !is.na(Value) & (!grepl("^[+-]?[0-9]+$", Text) | as.character(Coerced) != Text)
        }
        else if (typeof(Template) == "logical") {
            Text <- tolower(as.character(Value))
            !is.na(Value) & !Text %chin% c("true", "false", "t", "f", "1", "0")
        }
        else {
            rep(FALSE, length(Value))
        }
        if (any(Lost | Changed, na.rm = TRUE)) {
            stop("Legacy ", processID, " cannot coerce metadata column ", COL, ".", call. = FALSE)
        }
        OUT[, `:=`((COL), Coerced)]
    }
    Probe <- fread(IntensityFile, nrows = 1L, colClasses = KeyClasses, showProgress = FALSE)
    if (!nrow(Probe)) {
        stop("Legacy ", processID, " IntensityTable has no rows.", call. = FALSE)
    }
    for (COL in RecordKeys) Probe[, `:=`((COL), OUT[[COL]][1L])]
    Prepared <- .recordPrepareProcessIndex(index = OUT[1L], intensity = Probe, processID = processID)
    if (!identical(names(Prepared), MasterHeader)) {
        stop("Legacy ", processID, " MasterIndex schema cannot be reproduced.", call. = FALSE)
    }
    .recordRequireCompatibleTypes(list(MasterProbe[, ..MasterHeader], Prepared[, ..MasterHeader]), label = sprintf("legacy %s MasterIndex", 
        processID))
    OUT[]
}

