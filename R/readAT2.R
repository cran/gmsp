# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readAT2 <- function(file) {
  DATA <- .readClean(file)
  i <- grep("NPTS", DATA)
  if (length(i) != 1L || i < 3L) {
    .stopUsage("AT2 header requires one NPTS line and channel line.",
               operation = "readProviderFile")
  }
  SamplingInterval <- suppressWarnings(as.numeric(sub(".*DT=\\s*([0-9.]+).*", "\\1", DATA[i], perl = TRUE)))
  SampleCount <- suppressWarnings(as.integer(sub(".*NPTS=\\s*([0-9]+).*", "\\1", DATA[i], perl = TRUE)))
  AUX <- gsub("\\s", "", strsplit(DATA[i - 2L], ",", fixed = TRUE)[[1L]], perl = TRUE)
  OCID <- tail(AUX, 1L)
  if (length(AUX) > 1L && grepl("\\.AT2$", OCID, ignore.case = TRUE)) {
    OCID <- AUX[length(AUX) - 1L]
  }
  .checkProviderHeader(SamplingInterval, sampleCount = SampleCount, OCID = OCID)
  s <- .scanN(DATA[-seq_len(i)], sampleCount = SampleCount)
  if (is.null(s)) return(NULL)
  .toLong(SamplingInterval, OCID = OCID, signal = s)
}
