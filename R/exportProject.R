# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.copyHtmlTree <- function(path, out, label) {
  if (!dir.exists(path)) return(0L)
  REL <- .htmlRelFiles(path)
  if (!length(REL)) return(0L)
  FILES <- file.path(path, REL)
  .mkdir(out)
  OUT <- file.path(out, REL)
  DIRS <- unique(dirname(OUT))
  for (DIR in DIRS) .mkdir(DIR)
  OK <- file.copy(FILES, OUT, overwrite = TRUE)
  if (!all(OK)) {
    stop("Could not copy ", label, " files: ", paste(REL[!OK], collapse = ", "),
         call. = FALSE)
  }
  length(FILES)
}

.htmlRelFiles <- function(path) {
  if (!dir.exists(path)) return(character())
  list.files(path, pattern = "[.]html$", recursive = TRUE, full.names = FALSE)
}

.checkHtmlInputs <- function(paths, label) {
  if (length(paths) <= 1L) return(invisible(NULL))
  REL <- unlist(lapply(paths, .htmlRelFiles), use.names = FALSE)
  AUX <- REL[duplicated(REL)]
  if (length(AUX)) {
    .stopUsage(label, " has duplicate relative HTML files: ",
         paste(unique(AUX), collapse = ", "),
                operation = "exportProject")
  }
  invisible(NULL)
}


#' Export project products
#'
#' Writes selected time series and product tables, and copies available metadata
#' and HTML files to a delivery directory.
#'
#' @param file Path to the JSON export contract, resolved against \code{root}.
#' @param root Existing project directory.
#' @details
#' The contract requires \code{path.out} and selects \code{path.source} or database
#' paths with an explicit selection.
#' A source directory requires \code{TSW.csv} and \code{IMW.csv}; \code{PSW.csv}
#' is optional.
#' \code{ID} defaults to \code{AT} and \code{DIR} to \code{H1}.
#' A raw database supports \code{AT}.
#' An explicit \code{path.index} identifies a MasterIndex CSV file.
#' HTML copies preserve subdirectories and reject duplicate relative paths
#' within the plot or audit inputs.
#' A nonempty destination requires \code{override=true}, which preserves
#' unrelated files.
#' The working directory is restored after completion or failure.
#' @return Invisible integer zero on completion; usage, execution or partial
#'   publication failures signal conditions.
#' @examples
#' Root <- tempfile("exportProject")
#' dir.create(file.path(Root, "source"), recursive = TRUE)
#' dir.create(file.path(Root, "gmsp"))
#' data.table::fwrite(data.table::data.table(RecordID = "001", t = c(0, 0.1, 0.2),
#'   AT.H1 = c(0, 1, -1), AT.H2 = c(1, 0, -1)), file.path(Root, "source", "TSW.csv"))
#' data.table::fwrite(data.table::data.table(RecordID = "001", OCID = c("H1", "H2"),
#'   PGA = c(100, 200)), file.path(Root, "source", "IMW.csv"))
#' jsonlite::write_json(list(path = list(source = "source", out = "delivery"),
#'   DIR = c("H1", "H2")), file.path(Root, "gmsp", "export.json"), auto_unbox = TRUE)
#' exportProject(root = Root)
#' list.files(file.path(Root, "delivery"), recursive = TRUE)
#' data.table::fread(file.path(Root, "delivery", "TSW.001.AT.H1.csv"))
#' unlink(Root, recursive = TRUE)
#' @export
exportProject <- function(file = file.path("gmsp", "export.json"), root = getwd()) {
  Published <- FALSE
  PATH.out <- NULL
  tryCatch({
    root <- .projectRoot(root)
    Directory <- getwd()
    on.exit(setwd(Directory), add = TRUE)
    setwd(root)
    PATH.config <- .resolvePath(file, root = root)
    Config <- .readProjectConfig(PATH.config)
    PATH.source <- .jsonGet(Config, c("path", "source"), default = NULL)
    PATH.source <- if (is.null(PATH.source)) NULL else .resolvePath(PATH.source, root = root)
    PATH.out <- .resolvePath(.jsonRequire(Config, c("path", "out"),
                                          PATH.config), root = root)
    PATH.database <- .jsonGet(Config, c("path", "database"), default = NULL)
    PATH.records <- .jsonGet(Config, c("path", "records"), default = NULL)
    PATH.index <- .jsonGet(Config, c("path", "index"), default = NULL)
    IndexUser <- !is.null(PATH.index)
    PATH.index <- if (is.null(PATH.index)) NULL else .resolvePath(PATH.index, root = root)
    if (IndexUser && dir.exists(PATH.index)) {
      .stopUsage("path.index must point to a MasterIndex CSV file, not an index directory.",
                  operation = "exportProject")
    }
    PATH.process <- .jsonGet(Config, c("path", "process"), default = NULL)
    if (is.null(PATH.database) && !is.null(PATH.records) &&
        identical(basename(path.expand(PATH.records)), "records")) {
      PATH.database <- dirname(path.expand(PATH.records))
    }
    DBMode <- !is.null(PATH.database) || !is.null(PATH.records)
    PATHS.db <- if (DBMode) {
      .recordPaths(list(database = PATH.database, records = PATH.records,
                        index = PATH.index, process = PATH.process))
    } else {
      list(records = NULL, index = PATH.index, process = PATH.process)
    }
    PATH.records <- if (DBMode) PATHS.db$records else NULL
    PATH.index <- PATHS.db$index
    PATH.selection <- .jsonGet(Config, c("path", "selection"), default = NULL)
    PATH.selection.file <- NULL
    if (!is.null(PATH.selection)) {
      PATH.selection <- .resolvePath(PATH.selection, root = root)
      PATH.selection.file <- if (dir.exists(PATH.selection)) {
        file.path(PATH.selection, "selection.csv")
      } else {
        PATH.selection
      }
    }
    ProcessID <- .jsonGet(Config, c("processID"), default = "raw")
    if (!is.character(ProcessID) || length(ProcessID) != 1L || is.na(ProcessID) || !nzchar(ProcessID)) {
      .stopUsage("processID must be one non-empty string.", operation = "exportProject")
    }
    Override <- .readScalarLogical(.jsonGet(Config, c("override"),
                                            default = FALSE),
                                   "override")
    if (!Override && dir.exists(PATH.out) && length(list.files(PATH.out, all.files = TRUE, no.. = TRUE))) {
      .stopUsage(PATH.out, " already exists. Set override=true to rebuild.",
                  operation = "exportProject")
    }
    PATH.metadata <- file.path(PATH.out, "metadata")
    
    Records <- .readCharacterVector(.jsonGet(Config, c("recordID"),
                                             default = NULL),
                                    "recordID", allowNull = TRUE)
    
    RawDB <- !is.null(PATH.records) && identical(ProcessID, "raw")
    
    if (!is.null(PATH.records)) {
      if (RawDB) {
        Outputs <- "AT"
      } else {
        Outputs <- intersect(c("TSW", "IMW", "PSW"),
                             .recordDeclaredOutputs(ProcessID, PATHS.db))
        Missing <- setdiff(c("TSW", "IMW"), Outputs)
        if (length(Missing)) {
          .stopUsage("ProcessID ", ProcessID, " does not declare required export outputs: ",
               paste(Missing, collapse = ", "),
                      operation = "exportProject")
        }
      }
      Keys <- .recordReadSelectionKeys(path = PATH.selection,
                                       processID = ProcessID,
                                       recordID = Records,
                                       label = "selection",
                                       allowAll = FALSE)
      Package <- .readRecordProducts(keys = Keys, paths = PATHS.db, output = Outputs)
      TSW <- if (RawDB) Package$AT else Package$TSW
      IMW <- if (RawDB) NULL else Package$IMW
      PSW <- if (RawDB) NULL else Package$PSW
      INDEX <- attr(Package, "index")
      Selection <- attr(Package, "keys")
    } else {
      if (is.null(PATH.source)) {
        .stopUsage("path.source is required when path.records is not provided.",
                    operation = "exportProject")
      }
      FILES <- vapply(c("TSW.csv", "IMW.csv"), function(FILE) {
        .recordSourceDataFile(PATH.source, FILE)
      }, character(1L))
      Missing <- FILES[!file.exists(FILES)]
      if (length(Missing)) {
        .stopUsage("Missing gmsp source files:\n", paste(Missing, collapse = "\n"),
                    operation = "exportProject")
      }
      TSW <- .recordFread(FILES[["TSW.csv"]])
      IMW <- .recordFread(FILES[["IMW.csv"]])
      FILE <- .recordSourceDataFile(PATH.source, "PSW.csv")
      PSW <- if (file.exists(FILE)) .recordFread(FILE) else NULL
      INDEX <- NULL
      Selection <- NULL
    }
    PATH.plot <- .jsonGet(Config, c("path", "plot"),
                          default = if (is.null(PATH.source)) NULL else
                            file.path(PATH.source, "plot"))
    PATH.plot <- if (is.null(PATH.plot)) character() else
      vapply(as.list(PATH.plot), .resolvePath, character(1L), root = root)
    PATH.audit <- .jsonGet(Config, c("path", "audit"),
                           default = if (is.null(PATH.source)) NULL else
                             file.path(PATH.source, "audit"))
    PATH.audit <- if (is.null(PATH.audit)) character() else
      vapply(as.list(PATH.audit), .resolvePath, character(1L), root = root)
    .checkHtmlInputs(PATH.plot, "path.plot")
    .checkHtmlInputs(PATH.audit, "path.audit")
    
    if (!length(Records)) Records <- sort(unique(TSW$RecordID), method = "radix")
    
    ID <- .readCharacterVector(.jsonGet(Config, c("ID"), default = "AT"), "ID")
    DIR <- .readCharacterVector(.jsonGet(Config, c("DIR"), default = "H1"), "DIR")
    
    DT <- TSW[RecordID %chin% Records]
    if (!nrow(DT)) .stopUsage("No TSW rows selected for export.", operation = "exportProject")
    
    if (RawDB && any(ID != "AT")) {
      .stopUsage("Raw database export only supports ID = AT.", operation = "exportProject")
    }
    
    OUT <- CJ(RecordID = sort(unique(DT$RecordID)), ID = ID, DIR = DIR)
    COLS <- if (RawDB) OUT$DIR else paste(OUT$ID, OUT$DIR, sep = ".")
    if (RawDB) {
      for (Record in unique(OUT$RecordID)) {
        # A literal provider channel keeps priority over a canonical direction.
        IDX <- which(OUT$RecordID == Record & !COLS %chin% names(DT))
        if (!length(IDX)) next
        FILE <- attr(Package, "files")[RecordID == Record, FILE]
        Map <- jsonlite::read_json(sub("[.]csv$", ".json", FILE), simplifyVector = TRUE)
        AUX <- match(COLS[IDX], Map$DIR)
        if (length(Map$DIR) != length(Map$OCID) || anyNA(AUX)) {
          .stopUsage("Raw record ", Record, " has no DIR/OCID mapping for: ",
                     paste(COLS[IDX], collapse = ", "), operation = "exportProject")
        }
        COLS[IDX] <- Map$OCID[AUX]
      }
    }
    .requireColumns(DT, c("RecordID", "t", COLS), if (RawDB) "AT" else "TSW")
    if (RawDB) {
      OUT[, FILE := sprintf("AT.%s.%s.csv", RecordID, DIR)]
    } else {
      OUT[, FILE := sprintf("TSW.%s.%s.%s.csv", RecordID, ID, DIR)]
    }
  
    FILE.index <- if (IndexUser) {
      PATH.index
    } else if (is.null(PATH.source)) {
      NA_character_
    } else {
      .recordSourceMetadataFile(PATH.source, "MasterIndex.csv")
    }
    if (!is.na(FILE.index) && file.exists(FILE.index)) {
      INDEX <- .recordFread(FILE.index)
      .requireColumns(INDEX, "RecordID", "MasterIndex")
      INDEX <- INDEX[RecordID %chin% Records]
      if (!nrow(INDEX)) .stopUsage("MasterIndex has zero rows for selected records.", operation = "exportProject")
    } else if (IndexUser) {
      .stopUsage("Missing MasterIndex table: ", PATH.index, operation = "exportProject")
    }
    if (!is.null(INDEX)) setorder(INDEX, RecordID)
    if (!is.null(PATH.selection.file)) {
      if (!file.exists(PATH.selection.file)) {
        .stopUsage("Missing selection table: ", PATH.selection.file, operation = "exportProject")
      }
      Selection <- .recordFread(PATH.selection.file)
      .requireColumns(Selection, "RecordID", "selection")
      Selection <- Selection[RecordID %chin% Records]
      if (!nrow(Selection)) .stopUsage("selection has zero rows for selected records.", operation = "exportProject")
    }
    if (!is.null(Selection)) setorder(Selection, RecordID)
  
    .mkdir(PATH.out)
    Published <- TRUE
    .mkdir(PATH.metadata)
    .projectMessage("export start: source=", PATH.source, " out=", PATH.out,
      " records=", length(Records), operation = "exportProject", phase = "export", path = PATH.out)
    
    for (i in seq_len(nrow(OUT))) {
      .projectMessage("Export ", i, "/", nrow(OUT), " ", OUT$FILE[i],
        operation = "exportProject", phase = "export", count = i,
        total = nrow(OUT), unit = OUT$RecordID[i], path = PATH.out)
      FILE <- file.path(PATH.out, OUT$FILE[i])
      fwrite(DT[RecordID == OUT$RecordID[i], .(t, s = get(COLS[i]))], FILE, eol = "\n")
    }
    
    fwrite(OUT, file.path(PATH.out, "files.csv"), eol = "\n")
    
    AUX <- OUT[, .(
      nFiles = .N,
      ID = paste(sort(unique(ID), method = "radix"), collapse = ","),
      DIR = paste(sort(unique(DIR), method = "radix"), collapse = ",")
    ), by = RecordID][order(RecordID)]
    fwrite(AUX, file.path(PATH.out, "unique.csv"), eol = "\n")
    
    if (!is.null(IMW)) {
      IMW <- IMW[RecordID %chin% Records]
      if ("OCID" %chin% names(IMW)) IMW <- IMW[OCID %chin% unique(OUT$DIR)]
      setcolorder(IMW, c(intersect(c("RecordID", "OwnerID", "EventID",
                                     "StationID", "OCID"), names(IMW)),
                         setdiff(names(IMW), c("RecordID", "OwnerID", "EventID",
                                               "StationID", "OCID"))))
      fwrite(IMW, file.path(PATH.out, "IMW.csv"), eol = "\n")
    }
    
    if (!is.null(PSW)) {
      PSW <- PSW[RecordID %chin% Records]
      setcolorder(PSW, c(intersect(c("RecordID", "OwnerID", "EventID",
                                     "StationID", "xi", "Tn"), names(PSW)),
                         setdiff(names(PSW), c("RecordID", "OwnerID", "EventID",
                                               "StationID", "xi", "Tn"))))
      fwrite(PSW, file.path(PATH.out, "PSW.csv"), eol = "\n")
    }
    
    if (!is.null(PATH.source)) {
      FILES.meta <- c("OCIDMAP.csv", "source.json", "runMatch.json")
      if (!IndexUser) FILES.meta <- c(FILES.meta, "MasterIndex.csv")
      FILES.metadata <- c(
        vapply(FILES.meta, function(FILE) {
          .recordSourceMetadataFile(PATH.source, FILE)
        }, character(1L)),
        .recordSourceDataFile(PATH.source, "manifest.csv")
      )
      FILES.metadata <- FILES.metadata[file.exists(FILES.metadata)]
      if (length(FILES.metadata)) {
        OK <- file.copy(FILES.metadata, PATH.metadata, overwrite = TRUE)
        if (!all(OK)) {
          stop("Could not copy metadata files: ",
               paste(basename(FILES.metadata[!OK]), collapse = ", "),
               call. = FALSE)
        }
      }
    }
    
    if (!is.null(INDEX)) {
      fwrite(INDEX, file.path(PATH.metadata, "MasterIndex.csv"), eol = "\n")
    }
    if (!is.null(Selection)) {
      fwrite(Selection, file.path(PATH.metadata, "selection.csv"), eol = "\n")
    }
    
    for (Plot in PATH.plot) {
      if (dir.exists(Plot)) {
        .copyHtmlTree(path = Plot, out = file.path(PATH.out, "plot"),
                      label = "plot")
      }
    }
    
    for (Audit in PATH.audit) {
      if (dir.exists(Audit)) {
        .copyHtmlTree(path = Audit, out = file.path(PATH.out, "audit"),
                      label = "audit")
      }
    }
    
    OK <- file.copy(PATH.config, file.path(PATH.out, "runExport.json"),
                    overwrite = TRUE)
    if (!OK) stop("Could not copy runExport.json.", call. = FALSE)
    
    .projectMessage("Exported ", format(nrow(OUT), big.mark = ","), " files to ", PATH.out,
      operation = "exportProject", phase = "complete", count = nrow(OUT), path = PATH.out)
    invisible(0L)
  }, error = function(e) {
    if (Published) .stopPartialPublication(conditionMessage(e), operation = "exportProject",
      path = PATH.out, failures = list(e), result = list.files(PATH.out, recursive = TRUE, full.names = TRUE))
    .projectFailure(e, operation = "exportProject", path = PATH.out)
  })
}
