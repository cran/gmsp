#' Compute KIND-derivable intensities for one raw record.
#'
#' Reads `raw/<KIND>.<RID>.csv` (WIDE provider `OCID` columns) plus
#' `raw/<KIND>.<RID>.json`, builds a long TSL with `ID` set to the
#' file's KIND (`AT`, `VT`, or `DT`), and calls [getIntensity()] to
#' compute the per-direction intensity scalars.
#'
#' Output (WIDE): one row per `(RecordID, DIR)` with the KIND-derivable
#' intensity columns.
#'
#' `DIR` (`H1`/`H2`/`UP`) is recovered from the JSON sidecar's explicit
#' `DIR`/`OCID` mapping. The CSV column names remain provider `OCID`
#' values such as `HHE`, `HNZ`, or `L/T/V`.
#'
#' Assumes signal already in `TARGET_UNITS = "mm"` (per `extractRecord`).
#'
#' @param path Absolute path to a station folder containing `raw/`.
#' @return Wide `data.table` with intensity columns
#'   (e.g. `RecordID, DIR, OCID, AI, AIu, AId, PGA, ARMS, AZC, ATo, ATn,
#'    D0595, D2080, D0575, TmA, CAV, CAV5, NP, dt, Fs, Dmax, EPI, PDI`
#'   for AT inputs).
#'   Returns `NULL` if the station has no `raw/<KIND>.<RID>.csv` / `.json`.
#' @examples
#' station <- file.path(tempdir(), "gmsp-raw-intensity-example")
#' unlink(station, recursive = TRUE)
#' raw <- file.path(station, "raw")
#' dir.create(raw, recursive = TRUE)
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
#' getRawIntensities(station)
#'
#' @importFrom data.table data.table fread melt dcast setnames setcolorder setorder
#' @importFrom jsonlite read_json
#' @export
getRawIntensities <- function(path) {
  RAW <- file.path(path.expand(path), "raw")
  CSV <- list.files(RAW, pattern = "^(AT|VT|DT)\\..+\\.csv$",  full.names = TRUE)
  JSN <- list.files(RAW, pattern = "^(AT|VT|DT)\\..+\\.json$", full.names = TRUE)
  if (length(CSV) != 1L || length(JSN) != 1L) return(NULL)
  KIND <- sub("^([ADV]T)\\..+$", "\\1", basename(CSV[1L]))

  DT <- fread(CSV[1L])
  J  <- jsonlite::read_json(JSN[1L], simplifyVector = TRUE)
  if (is.null(J$DIR) || is.null(J$OCID))
    stop("raw sidecar must contain DIR and OCID arrays")

  MAP <- data.table(DIR = as.character(J$DIR), OCID = as.character(J$OCID))
  if (anyNA(MAP$DIR) || anyNA(MAP$OCID) ||
      anyDuplicated(MAP$DIR) || anyDuplicated(MAP$OCID))
    stop("raw sidecar has invalid DIR/OCID mapping")

  CSV.OCID <- names(DT)
  if (!setequal(CSV.OCID, MAP$OCID))
    stop("raw CSV columns do not match sidecar OCID values")

  DT[, t := (seq_len(.N) - 1L) * J$dt]
  TSL <- melt(DT, id.vars = "t", variable.name = "OCID", value.name = "s")
  TSL[, RSN  := J$RecordID]
  TSL[, ID   := KIND]
  TSL[, OCID := as.character(OCID)]

  IM <- suppressWarnings(
    getIntensity(TSL, units.source = TARGET_UNITS, units.target = TARGET_UNITS)
  )

  AUX <- IM[, .N, by = .(RSN, OCID, IM)]
  if (any(AUX[["N"]] > 1L))
    stop("getRawIntensities() cannot dcast: duplicate (RSN, OCID, IM) rows")
  W <- dcast(IM, RSN + OCID ~ IM, value.var = "value")
  setnames(W, "RSN", "RecordID")

  INT  <- intersect(names(W),
                    c("PGA","ARMS","AI","AIu","AId","ATo","ATn",
                      "CAV","CAV5","EPI","PDI","NP","Fs","AZC"))
  DUR  <- intersect(names(W),
                    c("TmA","D0595","D2080","D0575","Dmax"))
  RATE <- intersect(names(W), "dt")
  for (c in INT)  set(W, j = c, value = as.integer(round(W[[c]])))
  for (c in DUR)  set(W, j = c, value = round(W[[c]], 1L))
  for (c in RATE) set(W, j = c, value = round(W[[c]], 4L))

  W <- MAP[W, on = "OCID"]
  W[, DIR := factor(DIR, levels = c("H1", "H2", "UP"))]
  setorder(W, RecordID, DIR)
  W[, DIR := as.character(DIR)]
  setcolorder(W, c("RecordID", "DIR", "OCID"))
  W[]
}
