# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readV2 <- function(file) {
  DATA <- .readClean(file)
  if (!length(DATA)) .stopUsage("Empty V2 file.", operation = "readProviderFile")
  if (grepl("^Corrected accelerogram", DATA[1L])) return(.readV2multi(DATA))
  if (grepl("^Corrected acceleration", DATA[1L])) return(.readV2c(DATA))
  .stopUsage("Unknown V2 marker: ", substr(DATA[1L], 1L, 60L), operation = "readProviderFile")
}

.readV2multi <- function(data) {
  IDX <- c(grep("^Corrected accelerogram", data), length(data) + 1L)
  rbindlist(lapply(seq_len(length(IDX) - 1L), function(k) {
    DATA <- data[seq.int(IDX[k], IDX[k + 1L] - 1L)]
    OCID <- sub("^Chan[[:space:]]+[[:digit:]]*:[[:space:]]*([^[:space:]]+).*", "\\1",
      grep("^Chan[[:space:]]+[[:digit:]]*:", DATA, value = TRUE)[1L])
    i <- grep("points of accel data", DATA)[1L]
    j <- grep("points of veloc data", DATA)[1L]
    if (is.na(i)) .stopUsage("V2 acceleration header is missing.", operation = "readProviderFile")
    if (is.na(j)) j <- length(DATA) + 1L
    SampleCount <- suppressWarnings(as.integer(sub("\\s*([0-9]+)\\s+points.*", "\\1", DATA[i], perl = TRUE)))
    SamplingInterval <- suppressWarnings(as.numeric(sub(".*spaced at\\s+([0-9.]+)\\s+sec.*", "\\1", DATA[i], perl = TRUE)))
    .checkProviderHeader(SamplingInterval, sampleCount = SampleCount, OCID = OCID)
    if (j <= i + 1L) return(NULL)
    s <- .scanN(DATA[seq.int(i + 1L, j - 1L)], sampleCount = SampleCount)
    if (is.null(s)) return(NULL)
    .toLong(SamplingInterval, OCID = OCID, signal = s)
  }))
}

.readV2c <- function(data) {
  i <- grep("acceleration pts", data)[1L]
  if (is.na(i)) .stopUsage("V2c acceleration header is missing.", operation = "readProviderFile")
  SampleCount <- suppressWarnings(as.integer(sub("\\s*([0-9]+)\\s+acceleration pts.*", "\\1", data[i], perl = TRUE)))
  AUX <- grep("samples/sec", data, value = TRUE)
  SamplingInterval <- NULL
  if (!length(AUX)) {
    SamplingInterval <- suppressWarnings(as.numeric(sub(".*approx\\s+([0-9.]+)\\s+secs.*", "\\1", data[i], perl = TRUE))) / SampleCount
  }
  if (length(AUX)) {
    SamplingInterval <- 1 / suppressWarnings(as.numeric(sub(".*?([0-9.]+)\\s+samples/sec.*", "\\1", tail(AUX, 1L), perl = TRUE)))
  }
  OCID <- sub(".*Chan[[:space:]]+[^:]+:[[:space:]]*([^[:space:]]+).*", "\\1",
              grep("^Sta Chan", data, value = TRUE)[1L])
  .checkProviderHeader(SamplingInterval, sampleCount = SampleCount, OCID = OCID)
  IDX <- grep("End-of-data", data)
  j <- length(data)
  if (length(IDX)) j <- IDX[1L] - 1L
  if (j <= i) return(NULL)
  s <- .scanN(data[seq.int(i + 1L, j)], sampleCount = SampleCount)
  if (is.null(s)) return(NULL)
  .toLong(SamplingInterval, OCID = OCID, signal = s)
}
