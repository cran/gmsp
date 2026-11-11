# R library

`gmsp` processes ground-motion time histories, computes response spectra and
intensity measures, selects and matches records, and composes modal
decomposition and reconstruction through `xemd`.

```r
library(gmsp)
describeModule()
help(package = "gmsp")
```

`describeModule()` returns the installed package name and version.
The function help documents arguments, returned objects and effects.

## Public functions

| Work | Entry points | Result |
| --- | --- | --- |
| Time histories | `buildTSL()`, `normalizeTSL()` | Acceleration, velocity and displacement histories; scaling by a reference peak |
| Spectra and intensities | `computeSpectraTSL()`, `measureIntensitiesTSL()` | Response spectra and scalar intensity measures |
| Provider records | `parseRecord()`, `selectRecords()` | Native-unit samples and selected record identities |
| Modal analysis | `decomposeRecords()`, `reconstructRecords()` | Native `xemd` tables, mode selection and reintegrated histories |
| Project products | `selectProject()`, `rankProject()`, `processProject()`, `matchProject()`, `exportProject()` | Selection, processing, matching and delivery files from JSON contracts |

Time-history tables use `OCID`, `ID`, `t` and `s` for component, quantity,
time in seconds and sample value. The explicit `by` argument identifies
record metadata; `character()` declares one record without metadata.
`buildTSL()` returns `AT`, `VT` and `DT` on a common grid per record.
Its unit arguments declare the source and target length bases.

`computeSpectraTSL()` requires damping ratios and positive periods;
rotated D50 or D100 components also require `nTheta`.
`reconstructRecords()` selects modes by a frequency band or energy fraction
with an explicit keep/remove action.
The modal results retain native diagnostics and termination fields,
including iteration limits and non-convergence.
Stochastic `decomposeRecords()` calls consume the caller's random stream.

Project functions read their JSON contract relative to `root` and publish
the products described in their help. `processProject()` supports the
`gmsp`, `trim`, `imf`, `imfTS` and `psa` stages with explicit scientific
controls. Its stochastic modal stage requires an explicit seed and serial
execution settings and records execution evidence.

## Documentation

The [function manuals](https://kanameishi.github.io/gmsp/reference/index.html) describe arguments, data requirements and
publication behavior. The documentation site,
<https://kanameishi.github.io/gmsp/>, publishes the function reference and
the books
[Mathematical Basis](https://kanameishi.github.io/gmsp/books/model-en/index.html)
and
[Numerical Implementation](https://kanameishi.github.io/gmsp/books/implementation-en/index.html).
