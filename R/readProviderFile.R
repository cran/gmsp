# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
#' Read a provider signal file
#'
#' Returns provider channels and native amplitudes without unit conversion.
#' Time is in seconds; TWO_COL preserves its time column and the other formats
#' construct time from zero using the header sampling interval.
#'
#' @param file Path to one provider file.
#' @param format One of AT2, TWO_COL, TRA, TRZ, TRB, TRC, ACA, ACB, LIS,
#'   V2, V2A or ISEE. V2 and ISEE variants are recognized within the format.
#' @return A data.table with columns \code{t, OCID, s}. An NPTS block with
#'   insufficient samples or NA within its declared samples returns NULL;
#'   multi-block readers omit such blocks. ISEE returns T, V and L velocities
#'   in mm/s.
#' @details Unknown formats, unreadable files and invalid required headers
#'   raise errors with file and format context. Files are read without writes.
#' @examples
#' Root <- tempfile("readProviderFile")
#' dir.create(Root)
#' File <- file.path(Root, "record.AT2")
#' writeLines(c("header", "Event, date, station, H1", "units",
#'              "NPTS= 4, DT= 0.01 SEC", "1.0 2.0 3.0 4.0"), File)
#' readProviderFile(File, format = "AT2")
#' unlink(Root, recursive = TRUE)
#' @importFrom data.table data.table fread melt rbindlist setnames
#' @export
readProviderFile <- function(file, format) {
  if (!is.character(format) || length(format) != 1L || is.na(format) ||
      !format %in% c("AT2", "TWO_COL", "TRA", "TRZ", "TRB", "TRC",
                     "ACA", "ACB", "LIS", "V2", "V2A", "ISEE")) {
    .stopUsage("Unknown provider format: ", paste(format, collapse = ", "),
               "; file=", paste(file, collapse = ", "), operation = "readProviderFile")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .stopUsage("file must be one nonempty path; format=", format,
               operation = "readProviderFile")
  }
  tryCatch({
    if (!file.exists(file) || dir.exists(file) || file.access(file, 4L) != 0L) {
      .stopUsage("File is not readable.", operation = "readProviderFile")
    }
    if (format == "AT2") return(.readAT2(file))
    if (format == "TWO_COL") return(.readTwoCol(file))
    if (format %in% c("TRA", "TRZ", "TRB", "TRC")) {
      return(.readTR(file, format = format))
    }
    if (format %in% c("ACA", "ACB", "LIS")) return(.readAC(file, format = format))
    if (format == "V2") return(.readV2(file))
    if (format == "V2A") return(.readV2A(file))
    if (format == "ISEE") return(.readISEE(file))
  }, error = function(e) {
    e$message <- paste0("readProviderFile [", format, "] ", file, ": ",
                        conditionMessage(e))
    e$file <- file
    e$format <- format
    stop(e)
  })
}

.readClean <- function(file) {
  DATA <- readLines(file, warn = FALSE)
  DATA[DATA != ""]
}

.scanN <- function(text, sampleCount) {
  s <- tryCatch(suppressWarnings(scan(
    text = gsub("(?<=[0-9])-(?=[0-9])", " -", text, perl = TRUE),
    what = double(), quiet = TRUE)), error = function(e) NULL)
  if (is.null(s) || length(s) < sampleCount || anyNA(s[seq_len(sampleCount)])) {
    return(NULL)
  }
  s[seq_len(sampleCount)]
}

.toLong <- function(samplingInterval, OCID, signal) {
  data.table(t = seq.int(0, by = samplingInterval, length.out = length(signal)),
             OCID = OCID, s = signal)
}

.checkProviderHeader <- function(samplingInterval, sampleCount, OCID) {
  if (length(samplingInterval) != 1L || !is.finite(samplingInterval) ||
      samplingInterval <= 0 || length(sampleCount) != 1L ||
      !is.finite(sampleCount) || sampleCount <= 0 ||
      sampleCount != trunc(sampleCount) || length(OCID) != 1L ||
      is.na(OCID) || !nzchar(OCID)) {
    .stopUsage("Header requires a positive sampling interval, NPTS and channel.",
               operation = "readProviderFile")
  }
  invisible(NULL)
}
