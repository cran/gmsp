# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readTR <- function(file, format) {
  DATA <- .readClean(file)
  Header <- switch(format,
    TRA = list(end = "END_HEADER", skip = 3L, column = "last",
               pattern = "(?<=rate:)\\s*[0-9.]+", inverse = TRUE,
               channel = "(?<=Component:)\\s*\\S+"),
    TRZ = list(end = "END_HEADER", skip = 3L, column = "last",
               pattern = "(?<=rate:)\\s*[0-9.]+", inverse = TRUE,
               channel = "(?<=Component:)\\s*\\S+"),
    TRB = list(end = "Unidades", skip = 1L, column = "first",
               pattern = "(?<=muestreo:)\\s*[0-9.]+", inverse = TRUE,
               channel = "(?<=Componente:)\\s*\\S+"),
    TRC = list(end = "USER5", skip = 1L, column = "first",
               pattern = "(?<=SAMPLING_INTERVAL_S:)\\s*[0-9.]+", inverse = FALSE,
               channel = "(?<=STREAM:)\\s*\\S+"))
  i <- grep(Header$end, DATA)
  if (length(i) != 1L) {
    .stopUsage("TR header terminator is missing or repeated.", operation = "readProviderFile")
  }
  SamplingInterval <- suppressWarnings(as.numeric(regmatches(DATA[seq_len(i)],
    regexpr(Header$pattern, DATA[seq_len(i)], perl = TRUE))))
  if (length(SamplingInterval) != 1L || !is.finite(SamplingInterval) || SamplingInterval <= 0) {
    .stopUsage("TR header requires a positive sampling value.", operation = "readProviderFile")
  }
  if (Header$inverse) SamplingInterval <- 1 / SamplingInterval
  OCID <- gsub("\\s", "", regmatches(DATA[seq_len(i)],
    regexpr(Header$channel, DATA[seq_len(i)], perl = TRUE))[1L])
  if (is.na(OCID) || !nzchar(OCID)) {
    .stopUsage("TR header requires a channel.", operation = "readProviderFile")
  }
  DT <- suppressWarnings(fread(text = DATA[seq.int(i + Header$skip, length(DATA))]))
  s <- DT[[1L]]
  if (Header$column == "last") s <- DT[[ncol(DT)]]
  .toLong(SamplingInterval, OCID = OCID, signal = as.vector(s))
}
