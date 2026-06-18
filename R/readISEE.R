#' Read a Micromate ISEE blasting record.
#'
#' Parses the TXT output of an ISEE-compliant blasting seismograph
#' (Micromate, Vibra-Tech, GeoSonics). The format is the
#' *International Society of Explosives Engineers* (ISEE) Performance
#' Specifications for Blasting Seismographs and is declared in the
#' header line `Version : V 10-90 Micromate ISEE`.
#'
#' Two firmware variants are supported transparently:
#' * **v10 / `.TXT`**: header lines `"<key> : <value>"`
#'   (space-colon-space), body tab-separated `Tran Vert Long MicL`.
#' * **v11 / `.CSV`**: header lines `"<key>","<value>"` (CSV pair),
#'   body comma-separated quoted `"Tran","Vert","Long"` (no MicL
#'   present in the body).
#'
#' Variant is auto-detected from the file content -- the dispatch
#' is not by extension. Sample rate is read from a line matching
#' `Sample\\s*Rate\\s*[: ,"]+<Fs>\\s*sps`, which captures both
#' header styles. The body header is located by the regex
#' `Tran[^A-Za-z]+Vert[^A-Za-z]+Long`, also style-agnostic.
#'
#' Column-to-OCID mapping (ISEE convention):
#' * `Tran` (transverse) -> `T`
#' * `Vert` (vertical)   -> `V`
#' * `Long` (longitudinal, radial blast-to-station) -> `L`
#' * `MicL` (microphone, dB(L)) -> dropped when present (only in v10).
#'
#' Velocity is in mm/s; `dt = 1 / Fs` is derived from the header.
#' No metadata other than `Fs` is read here -- per-event scalars
#' (charge, distance to centroid, location) live in the blast
#' flatfile, not in the parser's contract.
#'
#' To process an ISEE record end-to-end:
#'
#' ```r
#' DT <- readISEE("UM12780_23_10_2025_15_22_8.TXT")
#' # DT now has columns (t, OCID, s) with OCIDs T/V/L; s in mm/s
#' ```
#'
#' From `parseRecord()` (the canonical entry point), the dispatch
#' happens via `.OWNER_FORMAT["ISEE"] = "ISEE"`. To get a sidecar
#' record (`raw/VT.<RID>.csv` + JSON), call
#' `extractRecord(.x, path)` -- the provider-string parser
#' (`.parseUnits` / `.parseKind`) accepts `"mm/s"` on the master row and
#' derives `KIND = "VT"` automatically, or pass `kind = "VT"` explicitly
#' to bypass derivation.
#'
#' @param file Path to the ISEE TXT.
#' @return LONG `data.table(t, OCID, s)` with `OCID` in `T`/`V`/`L`
#'         and `s` in mm/s. Three rows per sample, `3 * NP` rows
#'         total where `NP` is the per-channel sample count.
#' @seealso [parseRecord()], [extractRecord()], [readAT2()],
#'   [readV2()], [readAC()].
#' @examples
#' file <- tempfile(fileext = ".TXT")
#' writeLines(c(
#'   "Version : V 10-90 Micromate ISEE",
#'   "Sample Rate : 100 sps",
#'   "Tran\tVert\tLong\tMicL",
#'   "1\t2\t3\t90",
#'   "4\t5\t6\t91"
#' ), file)
#' readISEE(file)
#'
#' @importFrom data.table fread melt setnames
#' @export
readISEE <- function(file) {
  LINES <- readLines(file, warn = FALSE)
  SR    <- grep('Sample\\s*Rate', LINES, perl = TRUE, value = TRUE)[1L]
  Fs    <- as.numeric(sub('.*?([0-9]+(?:\\.[0-9]+)?)\\s*sps.*',
                          '\\1', SR, perl = TRUE))
  stopifnot(length(Fs) == 1L, !is.na(Fs), Fs > 0)
  dt <- 1 / Fs

  i  <- grep('Tran[^A-Za-z]+Vert[^A-Za-z]+Long', LINES, perl = TRUE)[1L]
  stopifnot(!is.na(i))

  DT <- fread(text = LINES[i:length(LINES)],
              header = TRUE,
              select = c("Tran", "Vert", "Long"))
  setnames(DT, c("Tran", "Vert", "Long"), c("T", "V", "L"))
  DT[, t := seq.int(0, by = dt, length.out = .N)]
  melt(DT, id.vars = "t", variable.name = "OCID", value.name = "s",
       variable.factor = FALSE)
}
