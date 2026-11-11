# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readAC <- function(file, format) {
  DATA <- .readClean(file)
  Header <- switch(format,
    ACA = list(row = "\\s+Z\\s+N", inverse = TRUE, key = "MUESTREO",
               pattern = ".*MUESTREO\\s*:\\s*([0-9.]+).*"),
    ACB = list(row = "\\s+T\\s+EW", inverse = TRUE, key = "SAMPLING FREQUENCY",
               pattern = ".*\\(Hz\\)\\s*:\\s*([0-9.]+).*"),
    LIS = list(row = "={2,}DATA", inverse = FALSE, key = "Delta",
               pattern = ".*Delta\\s*t\\s*:\\s*([0-9.]+).*"))
  i <- grep(Header$row, DATA, perl = TRUE)
  if (length(i) != 1L) {
    .stopUsage("AC header row is missing or repeated.", operation = "readProviderFile")
  }
  j <- i - 1L
  if (format == "LIS") j <- i
  SamplingInterval <- suppressWarnings(as.numeric(sub(Header$pattern, "\\1",
    grep(Header$key, DATA[seq_len(j)], value = TRUE, perl = TRUE)[1L], perl = TRUE)))
  if (length(SamplingInterval) != 1L || !is.finite(SamplingInterval) || SamplingInterval <= 0) {
    .stopUsage("AC header requires a positive sampling value.", operation = "readProviderFile")
  }
  if (Header$inverse) SamplingInterval <- 1 / SamplingInterval
  DT <- fread(text = DATA[seq.int(j + 1L, length(DATA))])
  if (format == "ACB") {
    COLS <- names(DT)[nchar(names(DT)) == 1L][1L]
    if (!is.na(COLS)) DT[, (COLS) := NULL]
  }
  DT[, t := seq.int(0, by = SamplingInterval, length.out = .N)]
  melt(DT, id.vars = "t", variable.name = "OCID", value.name = "s", variable.factor = FALSE)
}
