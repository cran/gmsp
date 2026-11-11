# gmsp 1.1.1

## Files and documentation

* The inaccessible `BugReports` URL is removed. `URL` points to the public
  documentation site.
* The source package omits `LICENSE.note`. Files in `R/` carry copyright
  and SPDX licence headers for GNU AGPL, version 3 or later.
* The function manuals are linked through the public reference index.

# gmsp 1.1.0

## Licence

* The licence changes from MIT to GNU AGPL, version 3 or later.

## Interface by families

* The library is rebuilt by families, with new function names and
  arguments. The 0.7.1 functions listed below as retired have no aliases.
* The functions that keep their 0.7.1 names take renamed arguments, except
  `archiveRawOwner()`, whose only argument is still `path`; `fitModalFactor()`,
  `fitRecordFactor()` and `mapComponents()` also require arguments that had
  defaults in 0.7.1. `fitModalFactor()` requires `OCID`, `xi`, `fmax`,
  `kNyq`, `factorMax`, `unitsSource` and `unitsTarget`, and takes `criterion`
  in place of `remove` and the new `signalControl`; `fitRecordFactor()`
  requires `factorMin`, `factorMax`, `bandWeight` and `deficitWeight`;
  `mapComponents()` requires `rotate`.
* `describeModule()` returns the installed package name and version.

## Signal

* `buildTSL()` builds acceleration, velocity and displacement histories on a
  common grid per record; `normalizeTSL()` normalizes all quantities by a
  reference peak; `widenTSL()` widens a long time-series table and
  `lengthenTSW()` lengthens wide time-series columns.
* Retired: `AT2TS()`, `VT2TS()`, `DT2TS()`, `normalizeTS()`, `TSL2TSW()` and
  `TSW2TSL()`.

## Intensity measures and spectra

* `measureIntensitiesTSL()` measures intensities from time histories,
  `accumulateIntensityTSL()` accumulates normalized intensity histories and
  `computeSpectraTSL()` computes response spectra from canonical time series.
  `widenIML()` and `widenPSL()` widen long intensity measures and response
  spectra, and `lengthenPSW()` lengthens wide response spectra.
* Retired: `TSL2IM()`, `getIntensity()`, `TSL2AI()`, `TSL2CAV()`,
  `TSL2CAV5()`, `TSL2PS()`, `IML2IMW()`, `PSL2PSW()` and `PSW2PSL()`.

## Selection and matching

* `assessCompatibility()` assesses the compatibility of a scaled suite.
  `fitModalFactor()` fits modal factors against a response-spectrum band and
  `fitRecordFactor()` fits suite record-scaling factors; both keep their
  names.
* Retired: `matchCompatibility()`.

## Providers

* `readProviderFile()` reads a provider signal file and `readRecordTSL()`
  reads raw records as long time histories. `assessDistances()`,
  `assessParsers()` and `assessSite()` assess coordinate and distance
  metadata, provider parsing by station and station velocity metadata.
* `parseRecord()`, `mapComponents()`, `alignComponents()`, `extractRecord()`,
  `buildRawFileTable()`, `buildRawRecordTable()`, `buildRawIntensityTable()`,
  `archiveRawOwner()`, `selectRecords()` and `writeSelection()` keep their
  names.
* Retired: `readAC()`, `readAT2()`, `readISEE()`, `readTR()`, `readTwoCol()`,
  `readV2()`, `readV2A()`, `readTS()`, `readAT()`, `readVT()`, `readDT()`,
  `getRawIntensities()`, `auditDistances()`, `auditParsers()` and
  `auditSite()`.

## Project products

* `selectProject()`, `rankProject()`, `processProject()`, `matchProject()` and
  `exportProject()` read a JSON project contract relative to `root` and
  publish the products described in their help. Their default contracts are
  `gmsp/select.json`, `gmsp/match.json` for ranking and matching,
  `gmsp/process.json` and `gmsp/export.json`.
* `processProject()` runs the stages `gmsp`, `trim`, `imf`, `imfTS` and `psa`
  with the scientific controls that each stage section of its contract
  declares explicitly. Its stochastic `imf` stage requires `imf.seed`,
  `parallel` FALSE and `workers` 1 explicitly.
* `matchProject()` writes no plot or audit folder and ignores the contract
  keys `plot.audit` and `plot.standard` with a warning.
* Retired: `runSelect()`, `runStage0()`, `runGMSP()`, `runTrim()`, `runIMF()`,
  `runPSA()`, `runProcess()`, `runMatch()`, `runPlot()` and `runExport()`.

## Modal decomposition

* `decomposeRecords()` decomposes acceleration records into modes and
  returns the native tables of the selected xemd method.
* `reconstructRecords()` measures and selects modes through xemd,
  reconstructs acceleration and reintegrates it with `buildTSL()`. Its
  criterion is a frequency band or an energy fraction with an explicit keep
  or remove action; selection by mode index is not admitted.
* Retired: `TS2IMF()`.

## Event identity, magnitude and target band

* `selectProject()` keeps one record per earthquake by default
  (`oneRecordPerEvent` TRUE). Records belong to the same earthquake when their
  `EventID.USGS` is equal (`usgsEventId` in legacy processed indexes); a
  record without it is identified by its `EventID`. The record with the
  largest PGA is kept, ties going to the larger `StationID` and then to the
  smaller `RecordID`. The events count in `summary.json` uses the same
  identity.
* With `candidate.oneRecordPerEvent` TRUE (default FALSE), `rankProject()`
  keeps the best-ranked record of each earthquake, with the same event
  identity read from the MasterIndex.
* Before the criteria are applied, `selectProject()` takes a moment magnitude
  where one is published or an Ms or mb is converted: `EventMagnitude` is kept when
  `EventMagnitudeType` is Mw, mwc, mww, mwr or mwb in any letter case;
  otherwise `EventMagnitude.owner` is taken when `EventMagnitudeType.owner` is
  one of those types; otherwise an Ms becomes `0.67 Ms + 2.13`, the ISC-GEM
  catalogue relation cited in the help, published for `Ms <= 6.47` and
  extrapolated to every larger Ms; otherwise an mb in `3.5 <= mb <= 6.2`
  becomes `0.85 mb + 1.03`, and an mb outside that interval is taken as Mw
  without conversion; otherwise an ML is taken as Mw without conversion.
  The `EventMagnitude` and `EventMagnitudeType` criteria and the published
  tables use the resulting values. The help of `selectProject()` gives the
  complete rule and its references.
* Before `candidate.filter` is applied, `rankProject()` gives the
  `EventMagnitude` of the MasterIndex the same moment magnitude; a
  `candidate.filter` range on `EventMagnitude` and the `Mw` column of
  `stage0.spectral.csv` use it.
* The help of `rankProject()` and `matchProject()` documents
  `target.envelope`, a required object with `lower` and `upper` fields:
  `lower`, `target.p` and `upper` select p labels of the target table, whose
  curves become `Sa.low`, `Sa.mean` and `Sa.high`. At positive target
  periods, ranking uses `Sa.low` and `Sa.high` for `insideFraction` and the
  band violations, and `Sa.mean` for `relaxedRMSE`. Record scaling in
  `matchProject()` weights the record band violations with
  `shape.bandWeight` and the suite-mean deficit below `Sa.mean` with
  `shape.deficitWeight`.

## Documentation

* The vignette "Introduction" (`vignette("gmsp", package = "gmsp")`) builds
  acceleration, velocity and displacement histories, response spectra and
  intensity measures from a synthetic record, and runs a selection project
  in a temporary directory.
* Every help page except that of `describeModule()` has examples; those
  that write files create them under `tempdir()` and remove them.
* The vignettes of 0.7.1, `database`, `gmsp-quickstart`, `imfs`,
  `intensity-measures`, `pipeline-runners`, `signal-processing`, `spectra`
  and `spectral-matching`, are withdrawn.

## Files and dependencies

* The CSV and JSON files that the package generates end their lines with LF
  on every system; copies of supplied files, such as `metadata/runMatch.json`,
  keep the bytes of their source.
* Modal decomposition goes through xemd, which the package imports; EMD,
  hht, VMDecomp, openssl, purrr, spectral and stringr are no longer imported;
  Matrix and osqp move from Suggests to Imports, testthat leaves Suggests and
  bit64 enters it.
