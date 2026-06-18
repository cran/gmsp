#' Extract one record to `raw/<KIND>.<RecordID>.csv` + `<KIND>.<RecordID>.json`.
#'
#' Pipeline: `parseRecord` -> `mapComponents(rotate = FALSE)` -> capture pre-align NP and
#' `DIR`/`OCID` mapping -> `alignComponents` -> pivot WIDE by provider
#' `OCID` -> md5-16 hash -> write. The function returns `NULL` and writes
#' nothing in three
#' skip paths:
#' 1. Component classification cannot map the record (arrays with > 3
#'    OCIDs, 2-component records, or records with no vertical channel
#'    in the vertical-component vocabulary).
#' 2. The provider `Units` string cannot be normalised (`.parseUnits`
#'    returns `NA_character_`, so the scale factor `K` is `NULL`).
#' 3. `kind` is left at its default `NULL` and `.parseKind(Key$Units)`
#'    returns `NA_character_` (kind cannot be derived from `Units`).
#'
#' `KIND` is one of `"AT"` (acceleration), `"VT"` (velocity), `"DT"`
#' (displacement). By default it is derived from `Key$Units` via
#' `.parseKind()`. Pass `kind` explicitly to override -- e.g.
#' `kind = "VT"` for blasting records whose `Units` may not be
#' machine-parseable.
#'
#' Old contents of `raw/` are unlinked before writing (idempotent).
#'
#' JSON sidecar schema:
#' ```
#' RecordID, OwnerID, EventID, StationID, NetworkID,
#' FileID            (scalar = "<KIND>.<RID>.csv"),
#' DIR               (array, ["H1","H2","UP"]),
#' OCID              (array of 3, provider channels in DIR order),
#' NP                (array of 3, pre-align NP per DIR),
#' PGA / PGV / PGD   (array of 3, peak |s| per DIR, post-align;
#'                    field name derived from KIND[1] -> PGA/PGV/PGD),
#' dt, Fs, Units     (scalars).
#' ```
#'
#' @param .x `data.table` subset for ONE record.
#' @param path Absolute path to the records root. The function writes outputs
#'   under `<path>/<OwnerID>/<EventID>/<StationID>/raw/`. Required -- no
#'   default.
#' @param align `"max"` (pad to longest, default) or `"min"` (truncate).
#' @param kind Optional `"AT"|"VT"|"DT"` override. When `NULL` (default)
#'             KIND is derived from `Key$Units` via `.parseKind()`.
#' @return Absolute path to the written `<KIND>.<RecordID>.csv`, or `NULL` (skip).
#' @examples
#' root <- file.path(tempdir(), "gmsp-extract-example")
#' unlink(root, recursive = TRUE)
#' raw <- file.path(root, "ESM", "E1", "S1", "raw.owner")
#' dir.create(raw, recursive = TRUE)
#' writeLines(c("0 1", "0.01 2", "0.02 3"), file.path(raw, "N_acc.txt"))
#' writeLines(c("0 2", "0.01 3", "0.02 4"), file.path(raw, "E_acc.txt"))
#' writeLines(c("0 0", "0.01 1", "0.02 0"), file.path(raw, "Z_acc.txt"))
#' rows <- data.table::data.table(
#'   OwnerID = "ESM", EventID = "E1", StationID = "S1",
#'   NetworkID = "NW", Units = "cm",
#'   FileID = c("N_acc.txt", "E_acc.txt", "Z_acc.txt")
#' )
#' path <- extractRecord(rows, path = root, kind = "AT")
#' basename(path)
#'
#' @importFrom data.table data.table fwrite
#' @importFrom openssl md5
#' @importFrom jsonlite write_json
#' @export
extractRecord <- function(.x, path, align = "max",
                          kind = NULL) {
  PATH.records <- path.expand(path)
  Key          <- .x[1L]

  DT <- parseRecord(.x = .x, path = PATH.records)
  DT <- mapComponents(DT, rotate = FALSE)
  if (is.null(DT)) return(NULL)

  K <- switch(.parseUnits(Key$Units), mm = 1, cm = 10, m = 1000, g = 9806.650)
  if (is.null(K)) return(NULL)
  KIND <- if (is.null(kind)) .parseKind(Key$Units) else kind
  if (is.na(KIND)) return(NULL)
  DT[, s := s * K]

  # All checks passed -- only now create raw/ and clear stale outputs
  RawDir <- file.path(PATH.records, Key$OwnerID, Key$EventID, Key$StationID, "raw")
  dir.create(RawDir, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(RawDir, full.names = TRUE))

  MAP <- DT[, .(NP = .N, OCID = OCID[1L]), by = DIR]
  MAP <- MAP[match(c("H1", "H2", "UP"), DIR)]
  if (anyNA(MAP$DIR) || anyNA(MAP$OCID)) return(NULL)

  DT <- alignComponents(DT, align = align)$DT
  AUX <- DT[, .N, by = .(t, OCID)]
  if (any(AUX[["N"]] > 1L))
    stop("extractRecord() cannot dcast: duplicate (t, OCID) rows")

  dt <- round(
    DT[OCID == MAP$OCID[1L]][2L, t] - DT[OCID == MAP$OCID[1L]][1L, t],
    9L
  )

  PEAK       <- vapply(MAP$OCID, function(o) max(abs(DT[OCID == o, s])), numeric(1L))
  PEAK_FIELD <- sprintf("PG%s", substr(KIND, 1L, 1L))

  # CSV: wide with original OCID names as columns (e.g. L/T/V for ISEE)
  WIDE <- dcast(DT, t ~ OCID, value.var = "s")[, -"t"]
  FILE <- tempfile(tmpdir = RawDir, fileext = ".csv")
  fwrite(WIDE, FILE)
  AUX      <- file(FILE, "rb"); on.exit(try(close(AUX), silent = TRUE), add = TRUE)
  RecordID <- substr(paste(openssl::md5(AUX)), 1L, 16L)
  close(AUX)
  CSV  <- sprintf("%s.%s.csv", KIND, RecordID)
  PATH <- file.path(RawDir, CSV)
  file.rename(FILE, PATH)

  SIDECAR <- list(RecordID  = RecordID,
                  OwnerID   = Key$OwnerID,
                  EventID   = Key$EventID,
                  StationID = Key$StationID,
                  NetworkID = Key$NetworkID,
                  FileID    = CSV,
                  DIR       = MAP$DIR,
                  OCID      = MAP$OCID,
                  NP        = MAP$NP)
  SIDECAR[[PEAK_FIELD]] <- PEAK
  SIDECAR$dt    <- dt
  SIDECAR$Fs    <- round(1 / dt, 6L)
  SIDECAR$Units <- TARGET_UNITS
  jsonlite::write_json(
    SIDECAR,
    file.path(RawDir, sprintf("%s.%s.json", KIND, RecordID)),
    auto_unbox = TRUE, pretty = TRUE
  )

  PATH
}
