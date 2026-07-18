# gmsp

**Ground Motion Signal Processing**

<!-- badges: start -->
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue)](https://www.r-project.org/)
[![CRAN status](https://www.r-pkg.org/badges/version/gmsp)](https://CRAN.R-project.org/package=gmsp)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/gmsp)](https://CRAN.R-project.org/package=gmsp)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue)](https://averriK.github.io/gmsp/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
<!-- badges: end -->

`gmsp` implements STFT-based processing of strong-motion time
series: time-grid regularisation via monotone-Hermite interpolation,
STFT-window and anti-alias-resampling strategy selection, edge
tapering of low-amplitude pre/post segments, and frequency-domain
integration and differentiation. A single input (acceleration,
velocity, or displacement) is mapped into a consistent `(AT, VT, DT)`
triplet under one chosen `Fmax` analysis bandwidth.

On top of that pipeline the package provides

* IMF decomposition through one of three engines — EMD, EEMD, or VMD
  (default) — with optional band-rule mode filtering and per-IMF
  metrics (center frequency, bandwidth, relative energy);
* elastic SDOF domain-normalised spectra (PSA / PSV / SD) from the independent
  acceleration / velocity / displacement rows of canonical `TSL` through
  `TSL2PS()`, by exact state-space integration of the 2D oscillator equation
  with `expm(A·dt)` over a user-supplied period grid, including optional D50
  and D100 horizontal spectra;
* a per-record table of intensity measures from a long TSL input —
  PGA / PGV / PGD, RMS, zero-crossings, Arias intensity and its
  positive / negative variants AI / AIu / AId, three significant-
  duration measures D5–95 / D5–75 / D20–80, CAV and CAV5, mean
  period Tm, and the derived indices EPI and PDI.

A provider-format indexing layer parses six raw record formats —
PEER NGA-W2 AT2, CESMD V2 / V2c, NWZ V2A, Geological Survey of
Canada TR, Peruvian and Costa Rican AC variants (ACA / LIS), and
generic two-column ASCII — normalises the three physical components
to canonical mm, classifies them as H1 / H2 / UP, equalises NP across
channels, and writes a per-record CSV/JSON pair under
`<recordsDir>/<OwnerID>/<EventID>/<StationID>/raw/`. The same layer
builds per-owner index CSVs (file / record / intensity tables) and
joins them with event and station metadata into an in-memory master
record `data.table` keyed at `(RecordID, DIR)`. Every function that
touches disk takes its base path as a required argument; the package
does not assume a specific filesystem root.

## Installation

Requires **R >= 4.1.0**.

```r
# Stable release:
install.packages("gmsp")

# Development version:
remotes::install_github("averriK/gmsp")
```

## Exported API

Signal-processing core: `AT2TS()`, `VT2TS()`, `DT2TS()`, `TS2IMF()` for
single-series IMF decomposition,
`TSL2PS()` for canonical TSL response spectra including D50, `getIntensity()`.

Indexing layer: `readAT2()`, `readV2()`, `readV2A()`, `readAC()`,
`readTR()`, `readTwoCol()`, `parseRecord()`, `mapComponents()`,
`alignComponents()`, `extractRecord()`, `archiveRawOwner()`,
`buildRawFileTable()`, `buildRawRecordTable()`,
`buildRawIntensityTable()`, `selectRecords()`,
`writeSelection()`, `readAT()`, `getRawIntensities()`,
`auditSite()`, `auditDistances()`, `auditParsers()`.

Project pipeline wrappers: `runSelect()`, `runGMSP()`, `runProcess()`,
`runTrim()`, `runIMF()`, `runPSA()`, `runPlot()`, `runMatch()`, and
`runExport()` execute the installed JSON-driven pipeline runners from a
project root.

Full reference: `help(package = "gmsp")` or
<https://averriK.github.io/gmsp/reference/>.

## Dependencies

R (>= 4.1.0).

**Imports:** data.table, digest, EMD, expm, hht, jsonlite, openssl,
pracma, purrr, seewave, signal, spectral, stats, stringr, utils,
VMDecomp.

**Suggests:** knitr, rmarkdown, testthat (>= 3.0.0).

## Documentation

Six vignettes ship with the package:

```r
vignette("gmsp-quickstart",    package = "gmsp")  # runnable hello world
vignette("signal-processing",  package = "gmsp")  # AT2TS / VT2TS / DT2TS math
vignette("imfs",               package = "gmsp")  # TS2IMF (EMD / EEMD / VMD)
vignette("spectra",            package = "gmsp")  # TSL2PS elastic SDOF spectra
vignette("intensity-measures", package = "gmsp")  # getIntensity output
vignette("database",           package = "gmsp")  # provider-format indexing
```

Rendered documentation: <https://averriK.github.io/gmsp/>.

## Citation

```r
citation("gmsp")
```

## License

MIT - see [LICENSE](LICENSE).

## Author

**Alejandro Verri Kozlowski**
Email: averri@fi.uba.ar
[ORCID 0000-0002-8535-1170](https://orcid.org/0000-0002-8535-1170)
Universidad de Buenos Aires, Facultad de Ingeniería
