#' Build the per-owner RawFileTable CSV (provider file inventory, post-archive safe).
#'
#' For each station, reads `raw.owner/record.json` and emits one row
#' per provider file (one per ComponentID x FileID). Handles both states:
#'   - `raw.owner/record.json` present on disk (not yet archived).
#'   - `raw.owner.tar.gz` archive (extracts record.json via streaming
#'     `tar -xzOf` to avoid touching disk).
#'
#' Schema:
#' ```
#' OwnerID, EventID, StationID, ComponentID, FileID,
#'   NP, dt, Fs, Units, HP, LP, isArray
#' ```
#' `PGA` is intentionally not emitted here. Pre-parse PGAs from
#' `record.json` are in heterogeneous provider units; canonical
#' post-parse PGAs (in mm/s^2) live in `RawIntensityTable.<Owner>.csv`.
#' Missing provider fields in the canonical schema are emitted as typed
#' `NA` columns instead of changing the output schema.
#'
#' `isArray = nComponentID > 3` (heuristic per legacy convention).
#'
#' @param path.records Absolute path to the records root. Required -- no default.
#' @param path.index Absolute path to the index root where per-owner CSVs are
#'   written. Required -- no default.
#' @param owners     Character vector of `OwnerID`s. `NULL` = scan all.
#' @return Invisibly, the per-owner row counts.
#' @examples
#' root <- file.path(tempdir(), "gmsp-raw-file-example")
#' index <- file.path(tempdir(), "gmsp-raw-file-index")
#' unlink(c(root, index), recursive = TRUE)
#' dir.create(file.path(root, "AAA", "E1", "S1", "raw.owner"),
#'            recursive = TRUE)
#' dir.create(index)
#' record <- list(
#'   Event = list(EventID = "E1"),
#'   Station = list(StationID = "S1"),
#'   Record = list(
#'     list(ComponentID = "H1", FileID = "H1.txt", NP = 4, dt = 0.01,
#'          Fs = 100, Units = "cm", HP = NA, LP = NA),
#'     list(ComponentID = "H2", FileID = "H2.txt", NP = 4, dt = 0.01,
#'          Fs = 100, Units = "cm", HP = NA, LP = NA),
#'     list(ComponentID = "UP", FileID = "UP.txt", NP = 4, dt = 0.01,
#'          Fs = 100, Units = "cm", HP = NA, LP = NA)
#'   )
#' )
#' jsonlite::write_json(
#'   record,
#'   file.path(root, "AAA", "E1", "S1", "raw.owner", "record.json"),
#'   auto_unbox = TRUE
#' )
#' suppressMessages(buildRawFileTable(root, index, owners = "AAA"))
#' data.table::fread(file.path(index, "RawFileTable.AAA.csv"))
#'
#' @importFrom data.table data.table rbindlist fwrite setorder setcolorder
#' @importFrom jsonlite read_json fromJSON
#' @export
buildRawFileTable <- function(path.records, path.index, owners = NULL) {
  path.records <- path.expand(path.records)
  path.index   <- path.expand(path.index)
  if (is.null(owners))
    owners <- basename(list.dirs(path.records, recursive = FALSE))

  OUT <- list()
  for (O in owners) {
    EVS <- list.dirs(file.path(path.records, O), recursive = FALSE)
    if (length(EVS) == 0L) next
    STA <- unlist(lapply(EVS, list.dirs, recursive = FALSE))
    if (length(STA) == 0L) next

    T0   <- Sys.time()
    FAIL <- 0L
    LIST <- lapply(STA, function(SD) {
      tryCatch({
        F_PLAIN <- file.path(SD, "raw.owner", "record.json")
        F_TGZ   <- file.path(SD, "raw.owner.tar.gz")
        J <- if (file.exists(F_PLAIN)) {
          jsonlite::read_json(F_PLAIN, simplifyVector = TRUE)
        } else if (file.exists(F_TGZ)) {
          txt <- system2("tar",
                         args = c("-xzOf", shQuote(F_TGZ), "raw.owner/record.json"),
                         stdout = TRUE, stderr = FALSE)
          jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = TRUE)
        } else NULL
        if (is.null(J) || is.null(J$Record) || length(J$Record) == 0L) return(NULL)
        R <- as.data.table(J$Record)
        n <- nrow(R)
        R[, OwnerID   := O]
        R[, EventID   := if (!is.null(J$Event$EventID))     J$Event$EventID     else NA_character_]
        R[, StationID := if (!is.null(J$Station$StationID)) J$Station$StationID else NA_character_]
        R[, isArray   := n > 3L]
        .canonicalRawFileTable(R)
      }, error = function(e) { FAIL <<- FAIL + 1L; NULL })
    })
    LIST <- LIST[!vapply(LIST, is.null, logical(1L))]
    if (!length(LIST)) next
    .assertRawFileSchema(LIST)
    DT <- rbindlist(LIST, use.names = TRUE)
    if (nrow(DT) == 0L) next

    DT <- .canonicalRawFileTable(DT)
    setorder(DT, EventID, StationID, ComponentID)
    fwrite(DT, file.path(path.index, sprintf("RawFileTable.%s.csv", O)))
    OUT[[O]] <- nrow(DT)
    message(sprintf("[buildRawFileTable] %-7s  rows=%-7d stations=%-6d  fail=%d  %.1f sec",
                    O, nrow(DT), length(STA), FAIL,
                    as.numeric(difftime(Sys.time(), T0, units = "secs"))))
  }
  invisible(OUT)
}

#' @noRd
.rawFileCols <- function() {
  c("OwnerID", "EventID", "StationID", "ComponentID", "FileID",
    "NP", "dt", "Fs", "Units", "HP", "LP", "isArray")
}

#' @noRd
.canonicalRawFileTable <- function(DT) {
  COLS <- .rawFileCols()
  MISS <- setdiff(COLS, names(DT))
  for (nm in MISS) {
    DT[, (nm) := NA]
  }
  EXTRA <- setdiff(names(DT), COLS)
  if (length(EXTRA)) {
    DT[, (EXTRA) := NULL]
  }
  setcolorder(DT, COLS)

  COLS.character <- c("OwnerID", "EventID", "StationID",
                      "ComponentID", "FileID", "Units")
  COLS.integer <- "NP"
  COLS.numeric <- c("dt", "Fs", "HP", "LP")
  DT[, (COLS.character) := lapply(.SD, as.character),
     .SDcols = COLS.character]
  DT[, (COLS.integer) := lapply(.SD, as.integer),
     .SDcols = COLS.integer]
  DT[, (COLS.numeric) := lapply(.SD, as.numeric),
     .SDcols = COLS.numeric]
  DT[, isArray := as.logical(isArray)]
  DT[]
}

#' @noRd
.assertRawFileSchema <- function(LIST) {
  if (is.null(names(LIST))) {
    names(LIST) <- sprintf("table%d", seq_along(LIST))
  }
  SCHEMA <- lapply(LIST, function(DT) {
    if (!data.table::is.data.table(DT)) {
      stop("buildRawFileTable() contains a non-data.table before binding",
           call. = FALSE)
    }
    paste(names(DT),
          vapply(DT, function(col) paste(class(col), collapse = "/"),
                 character(1L)),
          sep = ":")
  })
  SAME <- vapply(SCHEMA, identical, logical(1L), SCHEMA[[1L]])
  if (!all(SAME)) {
    stop(sprintf("buildRawFileTable() schema mismatch before binding: %s",
                 paste(names(SAME)[!SAME], collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}
