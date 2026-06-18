#' Convert canonical long time-series tables to wide form.
#'
#' `TSL2TSW()` casts a canonical long `TSL` table with `t`, `s`, `ID`, and
#' `OCID` columns to wide `TSW` form with columns such as `AT.H1`, `VT.H1`,
#' and `DT.H1`.
#'
#' @param .x Canonical `TSL` `data.table` with columns `t`, `s`, `ID`, and
#'   `OCID`, plus optional metadata columns.
#' @param by Metadata columns to keep as row keys. The default `"auto"` uses all
#'   columns except `t`, `s`, `ID`, and `OCID`.
#' @param ids Preferred order for signal IDs in the output columns. Other IDs
#'   present in `.x` are kept after these values.
#' @return A wide `data.table` keyed by `<metadata>, t`.
#' @examples
#' tsl <- data.table::data.table(RecordID = "R1", OCID = "H1",
#'                               ID = "AT", t = c(0, 0.01), s = c(1, 2))
#' TSL2TSW(tsl)
#' @importFrom data.table as.data.table copy dcast melt setcolorder setorderv
#' @export
TSL2TSW <- function(.x, by = "auto", ids = c("AT", "VT", "DT")) {
  DT <- copy(as.data.table(.x))
  REQ <- c("t", "s", "ID", "OCID")
  .requireTableColumns(DT, REQ, "TSL2TSW")
  META <- .resolveTableBy(DT, dataCols = REQ, by = by, label = "TSL2TSW")

  KEY <- c(META, "t", "ID", "OCID")
  .checkUniqueKeys(DT, KEY, "TSL2TSW")

  ROW <- c(META, "t")
  FORMULA <- stats::as.formula(paste(paste(ROW, collapse = " + "), "~ ID + OCID"))
  OUT <- dcast(DT, FORMULA, value.var = "s", sep = ".")

  IDValues <- .preferValues(unique(as.character(DT[["ID"]])), ids)
  OCIDValues <- unique(as.character(DT[["OCID"]]))
  SIGNAL <- unlist(lapply(IDValues, function(IDValue) {
    paste(IDValue, OCIDValues, sep = ".")
  }), use.names = FALSE)
  SIGNAL <- intersect(SIGNAL, names(OUT))

  setcolorder(OUT, c(ROW, SIGNAL, setdiff(names(OUT), c(ROW, SIGNAL))))
  setorderv(OUT, ROW, na.last = TRUE)
  OUT[]
}

#' Convert wide time-series tables to canonical long form.
#'
#' `TSW2TSL()` melts wide time-series columns named `<ID>.<OCID>` back to
#' canonical `TSL` rows.
#'
#' @inheritParams TSL2TSW
#' @param .x Wide `TSW` `data.table` with a `t` column, or a legacy constructor
#'   `ts` time column, plus signal columns named `<ID>.<OCID>`.
#' @return A canonical long `data.table` with metadata columns, `OCID`, `ID`,
#'   `t`, and `s`.
#' @examples
#' tsw <- data.table::data.table(RecordID = "R1", t = c(0, 0.01),
#'                               AT.H1 = c(1, 2))
#' TSW2TSL(tsw)
#' @export
TSW2TSL <- function(.x, by = "auto", ids = c("AT", "VT", "DT")) {
  DT <- copy(as.data.table(.x))
  SHAPE <- .resolveWideShape(DT, by = by, label = "TSW2TSL")

  OUT <- melt(
    DT,
    id.vars = c(SHAPE$meta, SHAPE$time),
    measure.vars = SHAPE$measure,
    variable.name = "key",
    value.name = "s",
    variable.factor = FALSE
  )
  OUT[, `:=`(
    ID = sub("[.].*$", "", key),
    OCID = sub("^[^.]+[.]", "", key)
  )]
  OUT[, key := NULL]
  if (SHAPE$time != "t") setnames(OUT, SHAPE$time, "t")

  IDValues <- .preferValues(unique(as.character(OUT[["ID"]])), ids)
  OUT[, ID := factor(ID, levels = IDValues)]
  setorderv(OUT, c(SHAPE$meta, "OCID", "ID", "t"), na.last = TRUE)
  OUT[, ID := as.character(ID)]
  setcolorder(OUT, c(SHAPE$meta, "OCID", "ID", "t", "s"))
  OUT[]
}

#' Convert long intensity tables to wide form.
#'
#' `IML2IMW()` casts long intensity output from [TSL2IM()] / [getIntensity()]
#' to one row per metadata and `OCID`, with intensity measures as columns.
#'
#' @param .x Long intensity table with columns `OCID`, `IM`, and `value`.
#' @param by Metadata columns to keep as row keys. The default `"auto"` uses all
#'   columns except `OCID`, `ID`, `IM`, `value`, and `units`.
#' @return A wide intensity `data.table`.
#' @examples
#' iml <- data.table::data.table(RecordID = "R1", OCID = "H1",
#'                               ID = "AT", IM = "PGA",
#'                               value = 1, units = "mm /s2")
#' IML2IMW(iml)
#' @export
IML2IMW <- function(.x, by = "auto") {
  DT <- copy(as.data.table(.x))
  REQ <- c("OCID", "IM", "value")
  .requireTableColumns(DT, REQ, "IML2IMW")
  META <- .resolveTableBy(
    DT,
    dataCols = c("OCID", "ID", "IM", "value", "units"),
    by = by,
    label = "IML2IMW"
  )

  ROW <- c(META, "OCID")
  KEY <- c(ROW, "IM")
  .checkUniqueKeys(DT, KEY, "IML2IMW")

  FORMULA <- stats::as.formula(paste(paste(ROW, collapse = " + "), "~ IM"))
  OUT <- dcast(DT, FORMULA, value.var = "value")
  IMCols <- intersect(unique(as.character(DT[["IM"]])), names(OUT))

  setcolorder(OUT, c(ROW, IMCols, setdiff(names(OUT), c(ROW, IMCols))))
  setorderv(OUT, ROW, na.last = TRUE)
  OUT[]
}

#' Convert long response spectra to wide form.
#'
#' `PSL2PSW()` casts canonical long spectra rows to wide columns such as
#' `PSA.H1`, `PSV.H1`, and `SD.H1`.
#'
#' @param .x Long spectra table with columns `OCID`, `Tn`, `ID`, and `S`.
#' @param by Metadata columns to keep as row keys. The default `"auto"` uses all
#'   columns except `OCID`, `Tn`, `ID`, and `S`.
#' @return A wide spectra `data.table`.
#' @examples
#' psl <- data.table::data.table(RecordID = "R1", OCID = "H1",
#'                               Tn = 0.1, ID = "PSA", S = 1)
#' PSL2PSW(psl)
#' @export
PSL2PSW <- function(.x, by = "auto") {
  DT <- copy(as.data.table(.x))
  REQ <- c("OCID", "Tn", "ID", "S")
  .requireTableColumns(DT, REQ, "PSL2PSW")
  META <- .resolveTableBy(DT, dataCols = REQ, by = by, label = "PSL2PSW")

  ROW <- c(META, "Tn")
  KEY <- c(ROW, "ID", "OCID")
  .checkUniqueKeys(DT, KEY, "PSL2PSW")

  FORMULA <- stats::as.formula(paste(paste(ROW, collapse = " + "), "~ ID + OCID"))
  OUT <- dcast(DT, FORMULA, value.var = "S", sep = ".")

  OCIDS <- .preferValues(unique(as.character(DT[["OCID"]])),
                         c("H1", "H2", "UP", "D50", "D100"))
  IDS <- .preferValues(unique(as.character(DT[["ID"]])), c("PSA", "PSV", "SD"))
  SPECTRA <- unlist(lapply(OCIDS, function(OCIDValue) {
    paste(IDS, OCIDValue, sep = ".")
  }), use.names = FALSE)
  SPECTRA <- intersect(SPECTRA, names(OUT))

  setcolorder(OUT, c(ROW, SPECTRA, setdiff(names(OUT), c(ROW, SPECTRA))))
  setorderv(OUT, ROW, na.last = TRUE)
  OUT[]
}

#' Convert wide response spectra to long form.
#'
#' `PSW2PSL()` melts spectra columns named `<ID>.<OCID>` back to canonical
#' long spectra rows. Derived RotD components such as `D50` and `D100` are
#' returned as ordinary `OCID` values.
#'
#' @param .x Wide spectra table with a `Tn` column and spectra columns named
#'   `<ID>.<OCID>`.
#' @param by Metadata columns to keep as row keys. The default `"auto"` uses all
#'   non-spectra columns except `Tn`.
#' @param ids Preferred order for spectra IDs. Other IDs present in `.x` are
#'   kept after these values.
#' @return A canonical long spectra `data.table`.
#' @examples
#' psw <- data.table::data.table(RecordID = "R1", Tn = 0.1, PSA.H1 = 1)
#' PSW2PSL(psw)
#' @export
PSW2PSL <- function(.x, by = "auto", ids = c("PSA", "PSV", "SD")) {
  DT <- copy(as.data.table(.x))
  .requireTableColumns(DT, "Tn", "PSW2PSL")
  SHAPE <- .resolveWideShape(DT, by = by, label = "PSW2PSL", time = "Tn")

  OUT <- melt(
    DT,
    id.vars = c(SHAPE$meta, "Tn"),
    measure.vars = SHAPE$measure,
    variable.name = "key",
    value.name = "S",
    variable.factor = FALSE
  )
  OUT[, `:=`(
    ID = sub("[.].*$", "", key),
    OCID = sub("^[^.]+[.]", "", key)
  )]
  OUT[, key := NULL]

  IDValues <- .preferValues(unique(as.character(OUT[["ID"]])), ids)
  OUT[, ID := factor(ID, levels = IDValues)]
  setorderv(OUT, c(SHAPE$meta, "OCID", "ID", "Tn"), na.last = TRUE)
  OUT[, ID := as.character(ID)]
  setcolorder(OUT, c(SHAPE$meta, "OCID", "Tn", "ID", "S"))
  OUT[]
}

.resolveTableBy <- function(DT, dataCols, by, label) {
  if (identical(by, "auto")) return(setdiff(names(DT), dataCols))
  if (is.null(by)) return(character())
  if (!is.character(by) || anyNA(by)) {
    stop(sprintf("%s by must be \"auto\", NULL, or a character vector.", label),
         call. = FALSE)
  }
  Missing <- setdiff(by, names(DT))
  if (length(Missing)) {
    stop(sprintf("%s by columns are missing: %s.",
                 label, paste(Missing, collapse = ", ")), call. = FALSE)
  }
  Bad <- intersect(by, dataCols)
  if (length(Bad)) {
    stop(sprintf("%s by cannot include data columns: %s.",
                 label, paste(Bad, collapse = ", ")), call. = FALSE)
  }
  by
}

.resolveWideShape <- function(DT, by, label, time = NULL) {
  if (is.null(time)) {
    if ("t" %chin% names(DT)) {
      time <- "t"
    } else if ("ts" %chin% names(DT)) {
      time <- "ts"
    } else {
      stop(sprintf("%s requires a t or ts time column.", label), call. = FALSE)
    }
  }
  .requireTableColumns(DT, time, label)

  CAND <- setdiff(names(DT), time)
  if (identical(by, "auto")) {
    MEASURE <- grep("^[^.]+[.].+$", CAND, value = TRUE)
    META <- setdiff(CAND, MEASURE)
  } else {
    META <- .resolveTableBy(DT, dataCols = time, by = by, label = label)
    MEASURE <- setdiff(names(DT), c(time, META))
  }

  if (!length(MEASURE)) {
    stop(sprintf("%s found no <ID>.<OCID> columns.", label), call. = FALSE)
  }
  BAD <- MEASURE[!grepl("^[^.]+[.].+$", MEASURE)]
  if (length(BAD)) {
    stop(sprintf("%s measure columns must be named <ID>.<OCID>: %s.",
                 label, paste(BAD, collapse = ", ")), call. = FALSE)
  }
  list(time = time, meta = META, measure = MEASURE)
}

.requireTableColumns <- function(DT, cols, label) {
  Missing <- setdiff(cols, names(DT))
  if (length(Missing)) {
    stop(sprintf("%s missing required columns: %s.",
                 label, paste(Missing, collapse = ", ")), call. = FALSE)
  }
  invisible(NULL)
}

.checkUniqueKeys <- function(DT, key, label) {
  Counts <- DT[, .(RowCount = .N), by = key]
  if (any(Counts[["RowCount"]] > 1L)) {
    stop(sprintf("%s cannot cast duplicated keys.", label), call. = FALSE)
  }
  invisible(NULL)
}

.preferValues <- function(values, preferred) {
  values <- as.character(values)
  preferred <- as.character(preferred)
  c(intersect(preferred, values), setdiff(values, preferred))
}
