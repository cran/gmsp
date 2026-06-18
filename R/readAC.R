#' Read a 3D-COL acceleration record (ACA, ACB, LIS).
#'
#' One file holds 3 components as parallel columns:
#'   - ACA (IGP Peru): cols `Z N E` after a header row.
#'   - ACB (CISMID Peru): cols `T EW NS UD`; the time column `T` is dropped.
#'   - LIS (UCR Costa Rica): cols `N00E UPDO N90E` after a `===DATA===` line.
#'
#' OCIDs come from the file's column header line.
#'
#' @param file Path to the file.
#' @param type One of `"ACA"`, `"ACB"`, `"LIS"`.
#' @return LONG `data.table(t, OCID, s)`.
#' @examples
#' file <- tempfile()
#' writeLines(c(
#'   "MUESTREO : 100",
#'   "  Z N E",
#'   "1 2 3",
#'   "4 5 6"
#' ), file)
#' readAC(file, type = "ACA")
#'
#' @importFrom data.table fread data.table melt
#' @export
readAC <- function(file, type) {
  DATA <- .readClean(file)
  AUX  <- switch(type,
    ACA = list(rowPat = "\\s+Z\\s+N",  inv = TRUE,
               key = "MUESTREO",
               pat = ".*MUESTREO\\s*:\\s*([0-9.]+).*"),
    ACB = list(rowPat = "\\s+T\\s+EW", inv = TRUE,
               key = "SAMPLING FREQUENCY",
               pat = ".*\\(Hz\\)\\s*:\\s*([0-9.]+).*"),
    LIS = list(rowPat = "={2,}DATA",   inv = FALSE,
               key = "Delta",
               pat = ".*Delta\\s*t\\s*:\\s*([0-9.]+).*"))
  i  <- grep(AUX$rowPat, DATA, perl = TRUE)
  stopifnot(length(i) == 1L)
  j  <- if (type == "LIS") i else i - 1L
  dt <- as.numeric(sub(AUX$pat, "\\1",
                       grep(AUX$key, DATA[1L:j], value = TRUE, perl = TRUE)[1L],
                       perl = TRUE))
  stopifnot(length(dt) == 1L, !is.na(dt), dt > 0)
  if (AUX$inv) dt <- 1 / dt
  DT <- fread(text = DATA[(j + 1L):length(DATA)])
  if (type == "ACB") {
    AUX <- names(DT)[nchar(names(DT)) == 1L][1L]
    if (!is.na(AUX)) DT[, (AUX) := NULL]
  }
  DT[, t := seq.int(0, by = dt, length.out = .N)]
  melt(DT, id.vars = "t", variable.name = "OCID", value.name = "s",
       variable.factor = FALSE)
}
