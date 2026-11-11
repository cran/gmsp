# SPDX-FileCopyrightText: 2026 Alejandro Daniel Verri <averri@fi.uba.ar>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Part of gmsp: free software under the GNU Affero General Public License, version 3 or later, see <https://www.gnu.org/licenses/>.
# Text and data mining reserved for any use not licensed under the AGPL-3: see <https://kanameishi.github.io/tdm-policy.html>.
.readISEE <- function(file) {
  DATA <- readLines(file, warn = FALSE)
  SamplingFrequency <- suppressWarnings(as.numeric(sub('.*?([0-9]+(?:\\.[0-9]+)?)\\s*sps.*', '\\1',
    grep('Sample\\s*Rate', DATA, perl = TRUE, value = TRUE)[1L], perl = TRUE)))
  if (length(SamplingFrequency) != 1L || !is.finite(SamplingFrequency) || SamplingFrequency <= 0) {
    .stopUsage("ISEE header requires a positive sample rate.", operation = "readProviderFile")
  }
  i <- grep('Tran[^A-Za-z]+Vert[^A-Za-z]+Long', DATA, perl = TRUE)[1L]
  if (is.na(i)) .stopUsage("ISEE channel header is missing.", operation = "readProviderFile")
  DT <- fread(text = DATA[seq.int(i, length(DATA))], header = TRUE,
              select = c("Tran", "Vert", "Long"))
  setnames(DT, c("Tran", "Vert", "Long"), c("T", "V", "L"))
  DT[, t := seq.int(0, by = 1 / SamplingFrequency, length.out = .N)]
  melt(DT, id.vars = "t", variable.name = "OCID", value.name = "s", variable.factor = FALSE)
}
