library(gmsp)
library(data.table)

# Safe-append contract of runProcess(): append-only classification, same-run
# stage replacement, legacy contract pinning, idempotent reruns, locks,
# metadata prevalidation, cleanup and rollback. Stage doubles stand in for the
# numerical stages; no real record is processed.

.checkAppend <- function() {
  # Stage doubles and induced failures replace private functions. The checks
  # therefore run on a copy of the namespace whose closures resolve names here.
  Test <- environment()
  Namespace <- getOption("gmsp.testNamespace", asNamespace("gmsp"))
  for (Name in setdiff(ls(Namespace, all.names = TRUE),
                       c(".__global__", ".__NAMESPACE__.", ".__S3MethodsTable__.", ".packageName"))) {
    Value <- get(Name, envir = Namespace)
    if (is.function(Value)) environment(Value) <- Test
    assign(Name, Value, envir = Test)
  }

  Pass <- 0L
  Fail <- 0L
  Fixtures <- list()
  on.exit(for (Fixture in Fixtures) unlink(Fixture$root, recursive = TRUE), add = TRUE)

  .ok <- function(label) {
    Pass <<- Pass + 1L
    message("  PASS: ", label)
  }

  .bad <- function(label, detail = NULL) {
    Fail <<- Fail + 1L
    message("  FAIL: ", label, if (!is.null(detail)) paste0(" - ", detail))
  }

  .expect <- function(value, label) {
    if (isTRUE(value)) .ok(label) else .bad(label)
  }

  .expectError <- function(expr, pattern, label) {
    Message <- tryCatch({
      force(expr)
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(Message) && grepl(pattern, Message, ignore.case = TRUE)) {
      .ok(label)
    } else {
      .bad(label, if (is.null(Message)) "no error" else Message)
    }
  }

  .expectClassError <- function(expr, class, label) {
    Error <- tryCatch({
      force(expr)
      NULL
    }, error = function(e) e)
    if (!is.null(Error) && inherits(Error, class)) {
      .ok(label)
    } else {
      .bad(label, if (is.null(Error)) "no error" else class(Error)[1L])
    }
  }

  .fixture <- function() {
    Root <- tempfile("gmsp-process-append-")
    for (DIR in c("records", "index", "process", "stage")) {
      dir.create(file.path(Root, DIR), recursive = TRUE, showWarnings = FALSE)
    }
    OUT <- list(root = Root, paths = list(database = Root),
                out = list(stage = file.path(Root, "stage")))
    Fixtures[[length(Fixtures) + 1L]] <<- OUT
    OUT
  }

  .row <- function(id, owner) {
    data.table(RecordID = id, OwnerID = owner, EventID = paste0("E-", id),
               StationID = paste0("S-", id), ProviderTag = owner)
  }

  .local <- function(row, producer, value = 1) {
    data.table(RecordID = row$RecordID, OwnerID = row$OwnerID,
               EventID = row$EventID, StationID = row$StationID,
               OCID = "H1", ID = "AT", t = 0, s = value, producer = producer)
  }

  .imw <- function(row, producer = "trim", value = 1) {
    data.table(RecordID = row$RecordID, OwnerID = row$OwnerID,
               EventID = row$EventID, StationID = row$StationID,
               DIR = "H1", OCID = "H1", PGA = value, producer = producer)
  }

  .component <- function(row) {
    data.table(ProcessID = row$ProcessID, RecordID = row$RecordID,
               OwnerID = row$OwnerID, EventID = row$EventID,
               StationID = row$StationID, OCID = "H1", DIR = "H1", theta = 0)
  }

  .window <- function(row) {
    data.table(ProcessID = row$ProcessID, RecordID = row$RecordID,
               OwnerID = row$OwnerID, EventID = row$EventID,
               StationID = row$StationID, range.start = 0.005, range.end = 0.99)
  }

  .recordFiles <- function(row, paths, outputs = c("TSW", "IMW", "IMF", "IMFF")) {
    Row <- copy(row)
    Row[, ProcessID := "TEST"]
    stats::setNames(vapply(outputs, function(Output) {
      .processOutputFile(row = Row, paths = .recordPaths(paths, need = "records"),
                         output = Output)
    }, character(1L)), outputs)
  }

  .hash <- function(files) {
    OUT <- as.character(tools::md5sum(files))
    names(OUT) <- names(files)
    OUT
  }

  .bytes <- function(file) {
    readBin(file, what = "raw", n = file.info(file)$size)
  }

  .metadataSnapshot <- function(root) {
    FILES <- unlist(lapply(file.path(root, c("process", "index")), function(DIR) {
      list.files(DIR, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    }), use.names = FALSE)
    FILES <- FILES[file.info(FILES)$isdir %in% FALSE]
    OUT <- as.character(tools::md5sum(FILES))
    names(OUT) <- substring(FILES, nchar(root) + 2L)
    OUT[order(names(OUT), method = "radix")]
  }

  .config <- function(contractMD5 = NULL, trimRange = c(0.005, 0.99), reseal = NULL) {
    # Explicit historical controls; stage doubles below perform no numerics.
    OUT <- list(
      gmsp = list(fmax = 25, kNyq = 4, flatZeros = FALSE, trimZeros = FALSE,
        Astop0 = 0.001, unitsSource = "mm", unitsTarget = "mm", windowLength = 128,
        overlap = 75, astopLP = 0.001, apass0 = 0.001,
        apassLP = 0.98),
      trim = list(ID = "AI", DIR = c("H1", "H2", "UP"), range = trimRange,
        taper = 2, astop = 0.01, apass = 0.95, fmax = 25, kNyq = 4,
        unitsTarget = "mm", windowLength = 128, overlap = 75,
        astop0 = 0.001, apass0 = 0.001,
        astopLP = 0.001, apassLP = 0.98),
      imf = list(method = "vmd", k = 10L))
    if (!is.null(contractMD5)) OUT$append$contractMD5 <- contractMD5
    if (!is.null(reseal)) OUT$append$reseal <- reseal
    OUT
  }

  Calls <- character()
  FailTrim <- FALSE

  # Append identity is tested with stage doubles; modal execution has its own test.
  .processModalExecution <- function(...) NULL
  .processIMFParams <- function(config) {
    list(Method = .jsonRequire(config, "method"), kIMF = .jsonRequire(config, "k"))
  }

  .processRunGMSP <- function(row, paths, processID, config, out, override) {
    Calls <<- c(Calls, paste0("gmsp:", row$RecordID))
    .saveRecordTables(row = row, paths = paths,
                      tables = list(TSW = .local(row, "gmsp"), IMW = .imw(row, "gmsp")),
                      override = override)
    list(status = list(RecordID = row$RecordID, status = "done"),
         ComponentMapTable = .component(row))
  }

  .processRunTrim <- function(row, paths, config, override, replace = character()) {
    Calls <<- c(Calls, paste0("trim:", row$RecordID))
    if (FailTrim) stop("induced trim failure", call. = FALSE)
    IMW <- .imw(row, "trim", 2)
    .saveRecordTables(row = row, paths = paths,
                      tables = list(TSW = .local(row, "trim", 2), IMW = IMW),
                      override = override, replace = replace)
    list(status = list(RecordID = row$RecordID, status = "done"),
         IMW = IMW, TrimWindowTable = .window(row))
  }

  .processRunIMF <- function(row, paths, config, override) {
    Calls <<- c(Calls, paste0("imf:", row$RecordID))
    .saveRecordTables(row = row, paths = paths,
                      tables = list(IMF = .local(row, "imf", 3), IMFF = .local(row, "imf", 4)),
                      override = override)
    list(RecordID = row$RecordID, status = "done")
  }

  .run <- function(fixture, selection, config = .config(), override = FALSE,
                   parallel = FALSE, workers = 1L) {
    .processRecords(selection = selection, paths = fixture$paths, processID = "TEST",
                    stages = c("gmsp", "trim", "imf"), config = config,
                    out = fixture$out, parallel = parallel, workers = workers,
                    override = override)
  }

  .writeLegacy <- function(fixture, row, index = row) {
    Row <- copy(row)
    Row[, ProcessID := "TEST"]
    .saveRecordTables(row = Row, paths = fixture$paths,
                      tables = list(TSW = .local(Row, "trim", 2), IMW = .imw(Row, "trim", 2),
                                    IMF = .local(Row, "imf", 3), IMFF = .local(Row, "imf", 4)),
                      override = FALSE)
    .writeProcessTables(index = index, intensity = .imw(row, "trim", 2),
                        paths = fixture$paths, processID = "TEST",
                        record = c("TSW", "IMW", "IMF", "IMFF"),
                        table = c("MasterIndex", "IntensityTable"),
                        params = list(), override = TRUE)
    .recordContractMD5(processID = "TEST",
                       paths = .recordPaths(fixture$paths, need = c("process", "index")))
  }

  .processTable <- function(fixture, processID, table) {
    .recordFread(file.path(fixture$root, "process", processID, paste0(table, ".csv")))
  }

  # A new multistage process may replace only outputs produced earlier in its run.
  F1 <- .fixture()
  R1 <- .row("R1", "OLD")
  OUT <- .run(F1, R1)
  .expect(identical(OUT$appended, 1L) && identical(OUT$skipped, 0L),
          "new append commits one record")
  .expect(identical(Calls, c("gmsp:R1", "trim:R1", "imf:R1")),
          "multistage chain runs once in order")
  .expect(identical(unique(.recordFread(.recordFiles(R1, F1$paths)[["TSW"]])$producer), "trim"),
          "trim replaces only same-run gmsp products")
  .expect(all(c("MasterIndex.csv", "IntensityTable.csv", "ComponentMapTable.csv",
                "TrimWindowTable.csv", "process.json") %chin%
                list.files(file.path(F1$root, "process", "TEST"))),
          "new process publishes its complete table inventory")

  BeforeIdempotent <- .metadataSnapshot(F1$root)
  OldProductHash <- .hash(.recordFiles(R1, F1$paths))
  Calls <- character()
  OUT <- .run(F1, R1)
  .expect(identical(OUT$appended, 0L) && identical(OUT$skipped, 1L),
          "committed record is skipped")
  .expect(!length(Calls), "idempotent rerun executes no stages")
  .expect(identical(OldProductHash, .hash(.recordFiles(R1, F1$paths))),
          "idempotent rerun preserves record bytes")
  .expect(identical(BeforeIdempotent, .metadataSnapshot(F1$root)),
          "idempotent rerun preserves process and index bytes")
  Calls <- character()
  .expectError(.run(F1, R1, override = TRUE), "disabled.*append-pinned",
               "override rebuild is rejected for an append-pinned process")
  .expect(!length(Calls), "rejected override rebuild executes no stage")

  .expectError(.run(F1, rbindlist(list(R1, R1), use.names = TRUE)), "duplicate RecordID",
               "duplicate selection is rejected before classification")
  .expect(identical(OldProductHash, .hash(.recordFiles(R1, F1$paths))),
          "duplicate selection cannot delete committed signals")

  # Forked workers inherit these stage doubles; socket workers load the
  # installed package instead, so the parallel path is checked on Unix only.
  if (.Platform$OS.type == "unix") {
    FParallel <- .fixture()
    ParallelRows <- rbindlist(list(.row("PARALLEL-001", "NEW"), .row("PARALLEL-002", "NEW")))
    OUT <- .run(FParallel, ParallelRows, parallel = TRUE, workers = 2L)
    .expect(identical(OUT$appended, 2L) &&
              setequal(.processTable(FParallel, "TEST", "MasterIndex")$RecordID,
                       ParallelRows$RecordID),
            "parallel append commits every new record")
    ParallelFiles <- unlist(lapply(seq_len(nrow(ParallelRows)), function(i) {
      .recordFiles(ParallelRows[i], FParallel$paths)
    }), use.names = FALSE)
    ParallelHashes <- .hash(ParallelFiles)
    OUT <- .run(FParallel, ParallelRows, parallel = TRUE, workers = 2L)
    .expect(identical(OUT$appended, 0L) && identical(OUT$skipped, 2L) &&
              identical(ParallelHashes, .hash(ParallelFiles)),
            "parallel append rerun is byte-idempotent")
  }

  # Evidence staging failure after DB publication must not delete committed data.
  FSelection <- .fixture()
  RSelection <- .row("SELECTION-001", "NEW")
  WriteSelection <- .processWriteSelection
  .processWriteSelection <- function(selection, out) {
    stop("induced selection evidence failure", call. = FALSE)
  }
  .expectError(.run(FSelection, RSelection), "selection evidence failure",
               "post-commit selection evidence error is reported")
  .processWriteSelection <- WriteSelection
  .expect(all(file.exists(.recordFiles(RSelection, FSelection$paths))),
          "post-commit evidence error preserves committed signals")
  .expect("SELECTION-001" %chin% .processTable(FSelection, "TEST", "MasterIndex")$RecordID,
          "post-commit evidence error preserves committed metadata")

  # An ambiguous metadata outcome preserves outputs for operator recovery.
  FAmbiguous <- .fixture()
  RAmbiguous <- .row("AMBIGUOUS-001", "NEW")
  CommitFiles <- .recordCommitFiles
  AmbiguousSources <- character()
  .recordCommitFiles <- function(staged, paths) {
    AmbiguousSources <<- staged$source
    stop(.recordAmbiguousCommit("induced ambiguous commit", "test-transaction"))
  }
  .expectClassError(.run(FAmbiguous, RAmbiguous), "spt_ambiguous_commit",
                    "ambiguous commit is reported distinctly")
  .recordCommitFiles <- CommitFiles
  .expect(all(file.exists(.recordFiles(RAmbiguous, FAmbiguous$paths))),
          "ambiguous commit preserves new signals")
  .expect(length(AmbiguousSources) > 0L && all(file.exists(AmbiguousSources)),
          "ambiguous commit preserves staged metadata for recovery")

  # Interrupts after at least one metadata promotion roll back cleanly and keep
  # the append retryable.
  FInterrupt <- .fixture()
  TI <- file.path(FInterrupt$root, c("process", "index"), c("interrupt-one.csv", "interrupt-two.csv"))
  SI <- file.path(FInterrupt$root, c("process", "index"), c("interrupt-one.new", "interrupt-two.new"))
  writeLines("old-one", TI[1L])
  writeLines("old-two", TI[2L])
  writeLines("new-one", SI[1L])
  writeLines("new-two", SI[2L])
  ReplaceFile <- .recordReplaceFile
  ReplaceCount <- 0L
  .recordReplaceFile <- function(source, target) {
    ReplaceCount <<- ReplaceCount + 1L
    if (ReplaceCount == 2L) {
      signalCondition(structure(list(message = "induced interrupt", call = NULL),
                                class = c("interrupt", "condition")))
    }
    ReplaceFile(source, target)
  }
  Interrupt <- tryCatch({
    .recordCommitFiles(staged = data.table(source = SI, target = TI),
                       paths = .recordPaths(FInterrupt$paths, need = c("process", "index")))
    NULL
  }, interrupt = function(e) e)
  .expect(inherits(Interrupt, "interrupt"), "metadata interrupt is propagated after rollback")
  .recordReplaceFile <- ReplaceFile
  .expect(identical(readLines(TI[1L]), "old-one") && identical(readLines(TI[2L]), "old-two") &&
            !length(.recordTransactionDirs(.recordPaths(FInterrupt$paths,
                                                        need = c("process", "index")))),
          "metadata interrupt restores targets and removes its journal")

  # A legacy F25 baseline accepts a pinned recipe and keeps old signal files.
  F2 <- .fixture()
  OLD <- .row("OLD-001", "OLD")
  NEW <- .row("NEW-001", "NEW")
  OLD[, `:=`(EventID = "000123", StationID = "000045")]
  LegacyIndex <- copy(OLD)
  LegacyIndex[, `:=`(usgsEventId = "USGS-OLD-001", FixedCode = "00123")]
  LegacyMD5 <- .writeLegacy(F2, OLD, index = LegacyIndex)
  LegacyConfig <- .config(contractMD5 = LegacyMD5)
  OLDSelection <- copy(OLD)
  OLDSelection[, `:=`(EventID.USGS = "USGS-OLD-001", FixedCode = "00123",
                      Year = 2001L, criterion = "legacy-current")]
  NEW[, `:=`(EventID.USGS = "USGS-NEW-001", Year = 2002L, FixedCode = 456L,
             criterion = "legacy-current")]
  OldFiles <- .recordFiles(OLD, F2$paths)
  OldHash <- .hash(OldFiles)
  ContractFile <- file.path(F2$root, "process", "TEST", "process.json")
  ContractHash <- as.character(tools::md5sum(ContractFile))
  LegacyMasterFile <- file.path(F2$root, "process", "TEST", "MasterIndex.csv")
  LegacyMasterBytes <- .bytes(LegacyMasterFile)
  Calls <- character()
  OUT <- .run(F2, NEW, config = LegacyConfig)
  .expect(identical(OUT$appended, 1L), "delta-only provider append commits")
  .expect(identical(Calls, c("gmsp:NEW-001", "trim:NEW-001", "imf:NEW-001")),
          "delta append processes only the new provider")
  .expect(identical(OldHash, .hash(OldFiles)), "legacy record products remain byte-identical")
  .expect(identical(ContractHash, as.character(tools::md5sum(ContractFile))),
          "legacy process.json remains byte-identical")
  .expect(identical(.bytes(LegacyMasterFile)[seq_along(LegacyMasterBytes)], LegacyMasterBytes),
          "legacy MasterIndex bytes remain an exact prefix")
  .expect(file.exists(file.path(F2$root, "process", "TEST", "append.json")),
          "first legacy append pins the accepted recipe")
  .expect(setequal(list.files(file.path(F2$root, "process", "TEST"), pattern = "[.]csv$"),
                   c("MasterIndex.csv", "IntensityTable.csv")),
          "legacy append does not invent partial extra tables")
  Master <- .processTable(F2, "TEST", "MasterIndex")
  .expect(setequal(Master$RecordID, c("NEW-001", "OLD-001")), "process MasterIndex is old union new")
  .expect(setequal(.recordFread(file.path(F2$root, "index", "MasterIndex.csv"))$RecordID,
                   Master$RecordID),
          "global index is old union new")
  .expect(identical(unique(Master[RecordID == "NEW-001"]$usgsEventId), "USGS-NEW-001") &&
            !any(c("EventID.USGS", "Year", "criterion") %chin% names(Master)),
          "legacy schema alias and projection are explicit")
  .expect(identical(unique(Master[RecordID == "OLD-001"]$EventID), "000123") &&
            identical(unique(Master[RecordID == "OLD-001"]$StationID), "000045"),
          "append preserves leading-zero record keys")

  BeforeFull <- .metadataSnapshot(F2$root)
  Calls <- character()
  OUT <- .run(F2, rbindlist(list(OLDSelection, NEW), use.names = TRUE), config = LegacyConfig)
  .expect(identical(OUT$appended, 0L) && identical(OUT$skipped, 2L),
          "full selection skips both committed records")
  .expect(!length(Calls), "full rerun does not reprocess old or new records")

  BeforeMinimal <- .metadataSnapshot(F2$root)
  Calls <- character()
  OUT <- .run(F2, OLD[, RecordKeys, with = FALSE], config = LegacyConfig)
  .expect(identical(OUT$appended, 0L) && identical(OUT$skipped, 1L),
          "key-only selection can skip a committed legacy record")
  .expect(!length(Calls) && identical(BeforeMinimal, .metadataSnapshot(F2$root)),
          "key-only no-op neither computes nor republishes metadata")
  .expect(identical(BeforeFull, .metadataSnapshot(F2$root)), "full rerun is a metadata no-op")

  Calls <- character()
  .expectError(.run(F2, .row("NEW-002", "NEW"),
                    config = .config(contractMD5 = LegacyMD5, trimRange = c(0.05, 0.95))),
               "recipe|append[.]json", "changed recipe is rejected before processing")
  .expect(!length(Calls), "recipe mismatch executes no stage")

  Collision <- copy(OLD)
  Collision[, OwnerID := "OTHER"]
  Calls <- character()
  .expectError(.run(F2, Collision, config = LegacyConfig), "collision",
               "RecordID collision across providers is rejected")
  .expect(!length(Calls), "RecordID collision executes no stage")

  Orphan <- .row("ORPHAN-001", "NEW")
  Orphan[, `:=`(EventID.USGS = "USGS-ORPHAN-001", FixedCode = 789L, Year = 2003L,
                criterion = "legacy-current")]
  OrphanProcess <- copy(Orphan)
  OrphanProcess[, ProcessID := "TEST"]
  .saveRecordTables(row = OrphanProcess, paths = F2$paths,
                    tables = list(TSW = .local(OrphanProcess, "orphan")), override = FALSE)
  Calls <- character()
  .expectError(.run(F2, Orphan, config = LegacyConfig), "Uncommitted output",
               "unindexed pre-existing output fails closed")
  .expect(!length(Calls), "orphan output executes no stage")

  # The legacy seal written above belongs to this producer. Another producer
  # extends the process only after naming that seal in append.reseal.
  Producer <- .processProducer
  .expect(grepl("^[0-9]+[.][0-9]+$", Producer()$R), "signed R version has no patch level")
  LedgerFile <- file.path(F2$root, "process", "TEST", "append.json")
  LegacySeal <- jsonlite::read_json(LedgerFile)$recipeMD5
  NEW3 <- .row("NEW-003", "NEW")
  NEW3[, `:=`(EventID.USGS = "USGS-NEW-003", Year = 2003L, FixedCode = 789L,
              criterion = "legacy-current")]
  .processProducer <- function() list(gmsp = "99.0.0", R = "9.9")
  Calls <- character()
  .expectError(.run(F2, NEW3, config = LegacyConfig),
               "does not match append[.]json.*append[.]reseal",
               "another producer is rejected on a sealed legacy process")
  .expect(!length(Calls), "rejected producer executes no stage")
  OUT <- .run(F2, NEW3, config = .config(contractMD5 = LegacyMD5, reseal = LegacySeal))
  Ledger <- jsonlite::read_json(LedgerFile)
  .expect(identical(OUT$appended, 1L) && identical(Ledger$compatibility, "user-asserted-reseal") &&
            !identical(Ledger$recipeMD5, LegacySeal) && length(Ledger$history) == 1L &&
            identical(Ledger$history[[1L]]$recipeMD5, LegacySeal) &&
            identical(Ledger$history[[1L]]$compatibility,
                      "user-asserted-against-pinned-legacy-contract"),
          "legacy reseal appends and keeps the previous seal in history")
  .expect(identical(ContractHash, as.character(tools::md5sum(ContractFile))) &&
            identical(.bytes(LegacyMasterFile)[seq_along(LegacyMasterBytes)], LegacyMasterBytes),
          "legacy reseal keeps process.json and the legacy table bytes")
  .processProducer <- Producer

  # Every declared legacy table must share the complete MasterIndex key map,
  # including records not present in a delta-only selection.
  FBaseline <- .fixture()
  BaselineMD5 <- .writeLegacy(FBaseline, .row("BASELINE-OLD", "OLD"))
  BadIntensity <- file.path(FBaseline$root, "process", "TEST", "IntensityTable.csv")
  DT <- .recordFread(BadIntensity)
  DT[, OwnerID := "WRONG"]
  fwrite(DT, BadIntensity)
  Calls <- character()
  .expectError(.run(FBaseline, .row("BASELINE-NEW", "NEW"),
                    config = .config(contractMD5 = BaselineMD5)),
               "record-key map differs", "unselected baseline key drift blocks append")
  .expect(!length(Calls), "baseline key drift is detected before stages")

  # A declared incoming table may not omit a Todo RecordID.
  FIncoming <- .fixture()
  IncomingRow <- .row("INCOMING-001", "NEW")
  IncomingProcess <- copy(IncomingRow)
  IncomingProcess[, ProcessID := "TEST"]
  .expectError(.writeProcessTables(index = IncomingRow, intensity = .imw(IncomingRow),
                                   paths = FIncoming$paths, processID = "TEST",
                                   record = c("TSW", "IMW"),
                                   extra = list(TrimWindowTable = .window(IncomingProcess)[0L]),
                                   override = TRUE),
               "record-key map differs", "partial incoming table is rejected")
  .expect(!file.exists(file.path(FIncoming$root, "process", "TEST", "MasterIndex.csv")),
          "partial incoming table publishes no metadata")

  FStale <- .fixture()
  dir.create(file.path(FStale$root, "process", ".spt-transactions", "stale"), recursive = TRUE)
  Calls <- character()
  .expectError(.run(FStale, .row("STALE-001", "NEW")), "Unresolved SPT metadata transaction",
               "stale metadata transaction blocks before processing")
  .expect(!length(Calls), "stale transaction executes no stage")

  FLock <- .fixture()
  ForeignLock <- file.path(FLock$root, "index", ".spt-locks", "process-index.lock")
  dir.create(ForeignLock, recursive = TRUE)
  jsonlite::write_json(list(pid = 999999L, host = "foreign"),
                       file.path(ForeignLock, "owner.json"), auto_unbox = TRUE)
  Calls <- character()
  .expectError(.run(FLock, .row("LOCK-001", "NEW")), "lock is already held",
               "global writer lock blocks before processing")
  .expect(!length(Calls), "global writer lock executes no stage")

  # An ordinary stage error removes only outputs claimed by this attempt.
  F3 <- .fixture()
  PARTIAL <- .row("PARTIAL-001", "NEW")
  FailTrim <- TRUE
  .expectError(.run(F3, PARTIAL), "induced trim failure", "stage failure is reported")
  FailTrim <- FALSE
  .expect(!any(file.exists(.recordFiles(PARTIAL, F3$paths))),
          "failed attempt removes its newly-created final outputs")
  OUT <- .run(F3, PARTIAL)
  .expect(identical(OUT$appended, 1L), "clean retry commits after ordinary failure")

  # A sibling schema error is discovered before publishing any metadata.
  F4 <- .fixture()
  .run(F4, .row("BASE-001", "OLD"))
  P2 <- .row("P2-001", "P2")
  P2Process <- copy(P2)
  P2Process[, ProcessID := "P2"]
  .writeProcessTables(index = P2, intensity = .imw(P2), paths = F4$paths, processID = "P2",
                      record = c("TSW", "IMW", "IMF", "IMFF"),
                      extra = list(ComponentMapTable = .component(P2Process),
                                   TrimWindowTable = .window(P2Process)),
                      params = list(sptAppendSignature = "p2-signature"),
                      override = TRUE, signature = "p2-signature")

  # A RecordID may legitimately occur in more than one ProcessID.
  Shared <- .row("BASE-001", "OLD")
  .writeProcessTables(index = Shared, intensity = .imw(Shared), paths = F4$paths,
                      processID = "P3", record = c("TSW", "IMW"), params = list(),
                      override = TRUE)
  DT <- .recordFread(file.path(F4$root, "index", "MasterIndex.csv"))[RecordID == "BASE-001"]
  .expect(nrow(DT) == 2L && setequal(DT$ProcessID, c("TEST", "P3")),
          "global index permits one RecordID in distinct ProcessIDs")

  BadSibling <- file.path(F4$root, "process", "P2", "TrimWindowTable.csv")
  DT <- .recordFread(BadSibling)
  DT[, SiblingOnly := "drift"]
  fwrite(DT, BadSibling)
  BeforeMismatch <- .metadataSnapshot(F4$root)
  NEW2 <- .row("NEW-002", "NEW")
  .expectError(.run(F4, NEW2), "schema mismatch", "sibling schema mismatch aborts append")
  .expect(identical(BeforeMismatch, .metadataSnapshot(F4$root)),
          "schema mismatch publishes no process or index metadata")
  .expect(!any(file.exists(.recordFiles(NEW2, F4$paths))),
          "schema mismatch cleans new record outputs")

  # A caught promotion failure rolls all metadata targets back.
  F5 <- .fixture()
  TP <- file.path(F5$root, c("process", "index"), c("one.csv", "two.csv"))
  SP <- file.path(F5$root, c("process", "index"), c("one.new", "two.new"))
  writeLines("old-one", TP[1L])
  writeLines("old-two", TP[2L])
  writeLines("new-one", SP[1L])
  writeLines("new-two", SP[2L])
  ReplaceCount <- 0L
  .recordReplaceFile <- function(source, target) {
    ReplaceCount <<- ReplaceCount + 1L
    if (ReplaceCount == 2L) stop("induced promotion failure", call. = FALSE)
    ReplaceFile(source, target)
  }
  .expectError(.recordCommitFiles(staged = data.table(source = SP, target = TP),
                                  paths = .recordPaths(F5$paths, need = c("process", "index"))),
               "induced promotion failure", "metadata promotion failure is reported")
  .recordReplaceFile <- ReplaceFile
  .expect(identical(readLines(TP[1L]), "old-one") && identical(readLines(TP[2L]), "old-two"),
          "metadata promotion failure restores every old target")

  # If rollback cannot prove restoration, the real transaction remains available
  # for operator recovery together with any unpromoted candidates.
  FUnproven <- .fixture()
  TA <- file.path(FUnproven$root, c("process", "index"), c("ambiguous-one.csv", "ambiguous-two.csv"))
  SA <- file.path(FUnproven$root, c("process", "index"), c("ambiguous-one.new", "ambiguous-two.new"))
  writeLines("old-one", TA[1L])
  writeLines("old-two", TA[2L])
  writeLines("new-one", SA[1L])
  writeLines("new-two", SA[2L])
  RollbackFiles <- .recordRollbackFiles
  ReplaceCount <- 0L
  .recordReplaceFile <- function(source, target) {
    ReplaceCount <<- ReplaceCount + 1L
    if (ReplaceCount == 2L) stop("induced ambiguous promotion", call. = FALSE)
    ReplaceFile(source, target)
  }
  .recordRollbackFiles <- function(files) FALSE
  .expectClassError(.recordCommitFiles(staged = data.table(source = SA, target = TA),
                                       paths = .recordPaths(FUnproven$paths,
                                                            need = c("process", "index"))),
                    "spt_ambiguous_commit", "incomplete rollback is classified as ambiguous")
  .recordReplaceFile <- ReplaceFile
  .recordRollbackFiles <- RollbackFiles
  Recovery <- .recordTransactionDirs(.recordPaths(FUnproven$paths, need = c("process", "index")))
  .expect(length(Recovery) == 1L && file.exists(file.path(Recovery, "manifest.json")) &&
            length(list.files(Recovery, pattern = "^backup[.]")) == 2L && file.exists(SA[2L]),
          "ambiguous transaction preserves manifest, backups, and candidate")

  # A process sealed in process.json by another producer needs an explicit reseal.
  FSeal <- .fixture()
  .run(FSeal, .row("SEAL-001", "OLD"))
  SealContract <- file.path(FSeal$root, "process", "TEST", "process.json")
  SealContractHash <- as.character(tools::md5sum(SealContract))
  SealedBy <- jsonlite::read_json(SealContract)$params$sptAppendSignature
  SealMaster <- file.path(FSeal$root, "process", "TEST", "MasterIndex.csv")
  SealMasterBytes <- .bytes(SealMaster)
  SealLedger <- file.path(FSeal$root, "process", "TEST", "append.json")
  .processProducer <- function() list(gmsp = "99.0.0", R = "9.9")
  Calls <- character()
  Message <- tryCatch({
    .run(FSeal, .row("SEAL-002", "NEW"))
    ""
  }, error = function(e) conditionMessage(e))
  .expect(grepl("does not match process.json", Message, fixed = TRUE) && !length(Calls),
          "producer change without reseal is rejected before any stage")
  .expect(grepl(SealedBy, Message, fixed = TRUE) && grepl("producer.gmsp", Message, fixed = TRUE) &&
            grepl("append.reseal", Message, fixed = TRUE),
          "rejection names the seal, the differing leaves and the reseal key")
  .expectError(.run(FSeal, .row("SEAL-002", "NEW"), config = .config(reseal = strrep("0", 32L))),
               "append[.]reseal does not name", "reseal naming another recipe is rejected")

  # A failed publication leaves the previous seal and tables in place.
  ReplaceCount <- 0L
  .recordReplaceFile <- function(source, target) {
    ReplaceCount <<- ReplaceCount + 1L
    if (ReplaceCount == 2L) stop("induced reseal promotion failure", call. = FALSE)
    ReplaceFile(source, target)
  }
  .expectError(.run(FSeal, .row("SEAL-002", "NEW"), config = .config(reseal = SealedBy)),
               "induced reseal promotion failure", "failed reseal publication is reported")
  .recordReplaceFile <- ReplaceFile
  .expect(!file.exists(SealLedger) && identical(.bytes(SealMaster), SealMasterBytes),
          "failed reseal publication leaves the previous seal and tables")

  OUT <- .run(FSeal, .row("SEAL-002", "NEW"), config = .config(reseal = SealedBy))
  .expect(identical(OUT$appended, 1L), "reseal naming the sealed recipe appends")
  Ledger <- jsonlite::read_json(SealLedger)
  .expect(identical(Ledger$compatibility, "user-asserted-reseal") &&
            !identical(Ledger$recipeMD5, SealedBy) && length(Ledger$history) == 1L &&
            identical(Ledger$history[[1L]]$recipeMD5, SealedBy) &&
            identical(Ledger$history[[1L]]$source, "process.json"),
          "reseal records the new seal and keeps the previous one in history")
  .expect(identical(SealContractHash, as.character(tools::md5sum(SealContract))) &&
            identical(.bytes(SealMaster)[seq_along(SealMasterBytes)], SealMasterBytes),
          "reseal keeps process.json and the old table bytes")
  BeforeRerun <- .metadataSnapshot(FSeal$root)
  Calls <- character()
  OUT <- .run(FSeal, rbindlist(list(.row("SEAL-001", "OLD"), .row("SEAL-002", "NEW"))),
              config = .config(reseal = SealedBy))
  .expect(identical(OUT$appended, 0L) && identical(OUT$skipped, 2L) && !length(Calls) &&
            identical(BeforeRerun, .metadataSnapshot(FSeal$root)),
          "the same contract after a reseal is a no-op")
  .processProducer <- Producer

  message("runAppend: ", Pass, " passed, ", Fail, " failed")
  if (Fail > 0L) stop(Fail, " safe-append checks failed.", call. = FALSE)
}
environment(.checkAppend) <- asNamespace("gmsp")
.checkAppend()
