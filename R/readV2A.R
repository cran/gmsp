# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readV2A <- function(file) {
  DATA <- .readClean(file)
  IDX <- grep("^Corrected [Aa]ccelerogram", DATA)
  if (length(IDX) < 3L) .stopUsage("V2A requires three blocks.", operation = "readProviderFile")
  IDX <- c(IDX[seq_len(3L)], length(DATA) + 1L)
  SamplingInterval <- suppressWarnings(as.numeric(sub(".*at\\s+([0-9.]+)\\s*sec intervals.*", "\\1",
    grep("sec intervals", DATA[seq.int(IDX[1L], IDX[2L] - 1L)], value = TRUE, perl = TRUE)[1L],
    perl = TRUE)))
  rbindlist(lapply(seq_len(3L), function(k) {
    AUX <- DATA[seq.int(IDX[k], IDX[k + 1L] - 1L)]
    OCID <- sub(".*Component\\s+(\\S+).*", "\\1",
      grep("^Component\\s", AUX, value = TRUE, perl = TRUE)[1L], perl = TRUE)
    SampleCount <- suppressWarnings(as.integer(sub(".*Number of points\\s+([0-9]+).*", "\\1",
      grep("Number of points", AUX, value = TRUE, perl = TRUE)[1L], perl = TRUE)))
    .checkProviderHeader(SamplingInterval, sampleCount = SampleCount, OCID = OCID)
    i <- grep("Displacement", AUX)[1L]
    if (is.na(i)) .stopUsage("V2A displacement header is missing.", operation = "readProviderFile")
    if (i + 11L > length(AUX)) return(NULL)
    s <- .scanN(AUX[seq.int(i + 11L, length(AUX))], sampleCount = SampleCount)
    if (is.null(s)) return(NULL)
    .toLong(SamplingInterval, OCID = OCID, signal = s)
  }))
}
