#' Build the canonical RawIntensityTable for one or more owners (WIDE).
#'
#' For each station with `raw/AT.<RID>.csv` / `.json`, calls
#' [getRawIntensities()] to compute the 20 AT-derivable scalars per
#' direction via [getIntensity()]. Emits one row per
#' `(RecordID, DIR)` - three rows per record.
#'
#' Schema:
#' ```
#' RecordID, DIR, OCID,
#'   AI, AId, AIu, ARMS, ATn, ATo, AZC, CAV, CAV5,
#'   D0575, D0595, D2080, Dmax, EPI, Fs, NP, PDI,
#'   PGA, TmA, dt
#' ```
#'
#' All amplitude scalars are in `TARGET_UNITS = "mm"`-derived units
#' (`mm/s2`, `mm/s`, etc. per `getIntensity()` contract).
#'
#' @param path.records Absolute path to the records root. Required -- no default.
#' @param path.index Absolute path to the index root where per-owner CSVs are
#'   written. Required -- no default.
#' @param owners     Character vector of `OwnerID`s. `NULL` = scan all.
#' @param incremental Logical. If `TRUE`, skip records already present in
#'                    `RawIntensityTable.<O>.csv`. Default: `TRUE`.
#' @param force      Logical. If `TRUE`, ignore incremental cache and
#'                   recompute everything. Default: `FALSE`.
#' @return Invisibly, the per-owner row counts.
#' @examples
#' root <- file.path(tempdir(), "gmsp-raw-intensity-table-example")
#' index <- file.path(tempdir(), "gmsp-raw-intensity-table-index")
#' unlink(c(root, index), recursive = TRUE)
#' raw <- file.path(root, "AAA", "E1", "S1", "raw")
#' dir.create(raw, recursive = TRUE)
#' dir.create(index)
#' t <- seq(0, 1, by = 0.01)
#' data.table::fwrite(
#'   data.table::data.table(
#'     H1 = sin(2 * pi * t),
#'     H2 = 0.5 * cos(2 * pi * t),
#'     UP = 0.25 * sin(4 * pi * t)
#'   ),
#'   file.path(raw, "AT.R1.csv")
#' )
#' jsonlite::write_json(
#'   list(RecordID = "R1", OwnerID = "AAA", EventID = "E1",
#'        StationID = "S1", NetworkID = "NW",
#'        DIR = c("H1", "H2", "UP"), OCID = c("H1", "H2", "UP"),
#'        NP = rep(length(t), 3), dt = 0.01, Fs = 100, Units = "mm"),
#'   file.path(raw, "AT.R1.json"), auto_unbox = TRUE
#' )
#' suppressMessages(buildRawIntensityTable(root, index, owners = "AAA",
#'                                         incremental = FALSE))
#' data.table::fread(file.path(index, "RawIntensityTable.AAA.csv"))
#'
#' @importFrom data.table data.table rbindlist fwrite setorder setcolorder
#' @export
buildRawIntensityTable <- function(path.records, path.index, owners = NULL,
                                   incremental = TRUE, force = FALSE) {
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

    IT_PATH <- file.path(path.index, sprintf("RawIntensityTable.%s.csv", O))
    EXIST   <- if (incremental && !force && file.exists(IT_PATH))
                 .canonicalRawIntensityTable(fread(IT_PATH),
                                             "existing intensity table")
               else NULL
    DONE    <- if (!is.null(EXIST)) unique(EXIST$RecordID) else character(0L)

    if (length(DONE) > 0L) {
      STA_DONE <- vapply(STA, function(sd) {
        F <- list.files(file.path(sd, "raw"),
                        pattern = "^(AT|VT|DT)\\.(.+)\\.json$")
        if (length(F) == 1L)
          sub("^(AT|VT|DT)\\.(.+)\\.json$", "\\2", F) else NA_character_
      }, character(1L))
      STA <- STA[!STA_DONE %in% DONE]
    }
    if (length(STA) == 0L) {
      message(sprintf("[buildRawIntensityTable] %-7s  all up-to-date (skip)", O))
      next
    }

    T0   <- Sys.time()
    FAIL <- 0L
    LIST <- lapply(STA, function(sd) {
      tryCatch(getRawIntensities(path = sd),
               error = function(e) {
                 FAIL <<- FAIL + 1L
                 message(sprintf("[%s] FAIL %s: %s", O, basename(sd), e$message))
                 NULL
               })
    })
    LIST <- LIST[!vapply(LIST, is.null, logical(1L))]
    if (!length(LIST)) next
    LIST <- lapply(seq_along(LIST), function(i) {
      .canonicalRawIntensityTable(LIST[[i]],
                                  sprintf("new intensity table %d", i))
    })
    .assertRawIntensitySchema(LIST, "new intensity tables")
    NEW <- rbindlist(LIST, use.names = TRUE)
    if (nrow(NEW) == 0L) next
    # Provider aliases (same signal under multiple (E,S)) result in repeated
    # station dirs that share AT.<RID>.csv/.json. Each gets processed
    # independently above, producing byte-identical duplicate rows.
    # Intensities are a function of the signal alone - dedup to (RID, DIR).
    NEW <- unique(NEW)
    NEW <- .canonicalRawIntensityTable(NEW, "new intensity table")

    DT <- if (is.null(EXIST)) {
      NEW
    } else {
      .assertRawIntensitySchema(list(EXIST = EXIST, NEW = NEW),
                                "incremental intensity tables")
      rbind(EXIST[!RecordID %in% NEW$RecordID], NEW, use.names = TRUE)
    }
    setorder(DT, RecordID, DIR)
    fwrite(DT, IT_PATH)
    OUT[[O]] <- nrow(DT)
    message(sprintf("[buildRawIntensityTable] %-7s  rows=%-7d records=%-6d  new=%-5d  fail=%d  %.1f sec",
                    O, nrow(DT), nrow(DT) %/% 3L, nrow(NEW) %/% 3L, FAIL,
                    as.numeric(difftime(Sys.time(), T0, units = "secs"))))
  }
  invisible(OUT)
}

#' @noRd
.rawIntensityCols <- function() {
  c("RecordID", "DIR", "OCID",
    "AI", "AId", "AIu", "ARMS", "ATn", "ATo", "AZC", "CAV", "CAV5",
    "D0575", "D0595", "D2080", "Dmax", "EPI", "Fs", "NP", "PDI",
    "PGA", "TmA", "dt")
}

#' @noRd
.canonicalRawIntensityTable <- function(DT, context) {
  COLS <- .rawIntensityCols()
  MISS <- setdiff(COLS, names(DT))
  EXTRA <- setdiff(names(DT), COLS)
  if (length(MISS) || length(EXTRA)) {
    stop(sprintf("buildRawIntensityTable() %s schema mismatch: missing=%s extra=%s",
                 context,
                 paste(MISS, collapse = ", "),
                 paste(EXTRA, collapse = ", ")), call. = FALSE)
  }

  setcolorder(DT, COLS)
  COLS.character <- c("RecordID", "DIR", "OCID")
  COLS.integer <- c("AI", "AId", "AIu", "ARMS", "ATn", "ATo", "AZC",
                    "CAV", "CAV5", "EPI", "Fs", "NP", "PDI", "PGA")
  COLS.numeric <- c("D0575", "D0595", "D2080", "Dmax", "TmA", "dt")

  DT[, (COLS.character) := lapply(.SD, as.character),
     .SDcols = COLS.character]
  DT[, (COLS.integer) := lapply(.SD, as.integer),
     .SDcols = COLS.integer]
  DT[, (COLS.numeric) := lapply(.SD, as.numeric),
     .SDcols = COLS.numeric]
  DT[]
}

#' @noRd
.assertRawIntensitySchema <- function(LIST, context) {
  if (is.null(names(LIST))) {
    names(LIST) <- sprintf("table%d", seq_along(LIST))
  }
  SCHEMA <- lapply(LIST, function(DT) {
    if (!data.table::is.data.table(DT)) {
      stop(sprintf("buildRawIntensityTable() %s contains a non-data.table",
                   context), call. = FALSE)
    }
    paste(names(DT),
          vapply(DT, function(col) paste(class(col), collapse = "/"),
                 character(1L)),
          sep = ":")
  })
  SAME <- vapply(SCHEMA, identical, logical(1L), SCHEMA[[1L]])
  if (!all(SAME)) {
    stop(sprintf("buildRawIntensityTable() %s schema mismatch before binding: %s",
                 context, paste(names(SAME)[!SAME], collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}
