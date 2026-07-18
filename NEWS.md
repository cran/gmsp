# gmsp 0.7.1

## Corrected domain normalisation in `TSL2PS()`

* `TSL2PS()` now applies the normalised SDOF operator
  `omega_n^2 H_d` independently to acceleration (`AT`), velocity (`VT`), and
  displacement (`DT`). `PSA`, `PSV`, and `SD` therefore retain acceleration,
  velocity, and displacement units, respectively.
* PSA is bit-for-bit unchanged. The positive-period PSV and SD ordinates are
  corrected incompatibly; `Tn = 0` PGA/PGV/PGD anchors, table schemas,
  metadata, D50/D100, and scalar/vector damping interfaces are unchanged.
* The spectral-matching selection, modal loss, and suite scaling continue to
  consume PSA only and therefore produce the same decisions and factors.
  Existing full `PSW.csv` products must be regenerated before republishing
  PSV or SD values. No legacy mode or fallback is provided.

## Spectral-match algorithm graduated to the exported API

* New `fitModalFactor()`: per-record modal shaping (released modal
  amplitudes fitted against the target band, energy restoration after
  the modal objective) — the algorithm behind `shape.modalFactor` in
  the match JSON contract.
* New `fitRecordFactor()`: suite-level record scaling (bounded QP
  solve, one scale coefficient per record, global band-conditioned
  objective) — the algorithm behind `shape.recordFactor`.
* New `matchCompatibility()`: per-record and suite compatibility
  diagnostics of the scaled spectra against the target band.
* `scripts/match/setup.R` now delegates the algorithm core to these
  functions and keeps only orchestration and product materialization.
  Pipeline products are byte-identical to 0.6.0 (Tier-2 identity
  harness: per-record, suite, and full-pipeline stages).
* No JSON contract change; `stage.csv` keeps its frozen product labels.

# gmsp 0.6.0

## runStage0: spectral record selection pipeline

* New `runStage0()` runner and `scripts/selection/runStage0.R`: builds a
  spectrally compatible selection from the processed candidate pool,
  scoring PSA against the target compatibility band declared in the
  match JSON. Driven by the same site JSON as `runMatch()` via a new
  `stage0.N` key; writes `selection.csv`, the score table, and a
  reusable per-pool spectra cache into `dirname(path.selection)`.
* Seismological screening surface on the `candidate` block:
  `pool` (a broad `runSelect` output as candidate pool), `filter`
  (numeric min/max ranges over MasterIndex columns), `exclude`
  (RecordID blacklist), and `oneRecordPerEvent` (keeps the best
  band-ranked record per event).
* Removed the IM-based candidate ranking surface from `runMatch`
  (`candidate.rank`, `candidate.rankOCID`, `candidate.maxRecords`) and
  the `shape.recordFactor.batchSize` key: selection belongs to
  `runSelect`/`runStage0`, and the V4 global solve has no batches.

## runMatch V4: band-conditioned suite scaling

* Stage B now solves one global quadratic objective that combines the
  target-normalized mean spectral residual with a compatibility-envelope
  containment term over the scaled per-record spectra
  (`shape.bandWeight`, default 2). The batched box-only solve is retired:
  the band term makes the global system well-posed, so
  `shape.recordFactor.batchSize` is accepted but ignored.
* `shape.recordFactor.min`/`max` are now optional backstop bounds
  (defaults 0 and unbounded). Record-scale coefficients are conditioned
  by the envelope term instead of artificial box saturation.
* New `shape.deficitWeight` (default 1 = symmetric): weights suite-mean
  deficit residuals more than exceedance when the acceptance criterion
  requires the mean spectrum not to fall below the target.
* `match/stage.csv` publishes `bandWeight`, `insideFraction`,
  `meanViolation`, and `maxViolation` with `mode = global`. The batch
  products `match/preflight.csv` and `match/blocks.csv` are removed
  (no readers existed; `scale.csv` and `stage.csv` carry the contract).
* New Stage A QA product `match/shaping.csv`: per-record shaped/source
  PSA ratio summary at the target grid (`minRatio`, `TnAtMin`,
  `medianRatio`, `maxRatio`). This exposes spectral-content loss caused
  by modal-set truncation (for example removing the lowest-frequency VMD
  mode), which previously surfaced only as Stage B bound saturation.
* Per-record match spectra now include the source-signal spectrum
  (`kind = "source"`) alongside `start` and `final`.

# gmsp 0.5.1

## Parallel process architecture

* Planning track opened for the 0.5.1 architecture change: singular helpers
  process one record serially, plural helpers orchestrate record sets and own
  parallel execution.
* Scope includes the future `processRecord()` / `processRecords()` producer
  flow and parallelized PSA, plot, and match consumers.
* Added the first consumer-side implementation cut:
  `buildPSARecords()` orchestrates PSA records, `matchRecords()` orchestrates
  match records, and `runPlot.R` can parallelize explicit plot units without
  changing output names or product folders.
* Added `examples/scripts/process/runProcess.R`, a composed producer runner for
  raw-to-`ProcessID` suites. The serial `processRecord()` unit writes only
  record-local products; `processRecords()` owns parallel orchestration and
  shared process table publication.
* Added canonical `examples/scripts/psa/runPSA.R` and `runPSA.json` entry
  points. `runBuildPSA.R` / `runBuildPSA.json` remain compatible for existing
  scripts.
* Added exported project runner wrappers: `runSelect()`, `runGMSP()`,
  `runProcess()`, `runTrim()`, `runIMF()`, `runPSA()`, `runPlot()`,
  `runMatch()`, and `runExport()`. These resolve project JSON paths from the
  supplied project root while executing the installed `gmsp` runner scripts.
* Clarified the plot boundary: `runPlot()` is an optional raw QA/debug
  htmlwidget runner. SRK publication plots are rendered outside `gmsp` from
  materialized `gmsp` CSV products.
* Removed worker counts from JSON contracts. Runners own worker-count policy;
  JSON keeps only processing parameters and `parallel`.
* `runSelect` can now derive its PGA selection window from a `runMatch`
  target and `shape.recordFactor` bounds through `path.matchConfig`, avoiding
  duplicated absolute PGA thresholds in selection JSON.
* `runMatch` Stage B now solves bounded record-scale factors in a
  batch/preflight mode. It writes `match/scale.csv`, `match/stage.csv`,
  `match/preflight.csv`, and `match/blocks.csv` alongside modal diagnostics.
* Promoted component mapping metadata to durable `ComponentMapTable.csv`
  process/global tables. Legacy `OCIDMAP.csv` remains only for stage
  checkpoints and folder-mode match/export packages.
* Closed the plot product classification: `TS` and `PS` are suite-level plots;
  `TSI` and `PSI` are per-record plots.
* Removed the fossil `cropRecord()` export and its generated documentation.
* Removed non-production helpers from the exported surface:
  `hydrateEventNameUSGS()`, `auditEventNameUSGS()`, `TSL2ND()`, and
  `rotateComponents()`. Event-name hydration moved to operational
  database metadata scripts; Newmark-displacement work remains outside the
  exported package surface; production component rotation is owned by
  `mapComponents()`.

# gmsp 0.5.0

## Database process products

* Added the current database process-product contract for scripts:
  `process/<ProcessID>/process.json`, `process/<ProcessID>/MasterIndex.csv`,
  `process/<ProcessID>/IntensityTable.csv`, record-local products under
  `records/<OwnerID>/<EventID>/<StationID>/<ProcessID>/`, and regenerated
  global process tables under `index/`.
* Added record-access helpers used by the example pipelines:
  `selectRecords()`, `getRecord()`, `getRecords()`, `writeProcessTables()`,
  `updateProcessContract()`, and `buildProcessIndex()`.
* Updated the selection, GMSP, trim, IMF, PSA, plot, match, and export example
  runners to consume the database/process contract instead of legacy source
  folders.
* `runMatch` now writes a clean product tree with `data/`, `metadata/`,
  `match/`, `plot/`, and `audit/`; it no longer writes per-record `.work`
  products or `META.csv`.
* `runExport` remains a packager, not a producer. It exports flat time-series
  CSV files, preserves `metadata/MasterIndex.csv` and
  `metadata/selection.csv`, and copies official `plot/` and `audit/` HTML
  trees without regenerating them.
* `buildMaster()` / `writeMaster()` now support the current raw master index
  workflow and preserve available event metadata such as `EventName` for
  downstream selection and export products.

# gmsp 0.4.7

## Time-series crop helpers

* `cropRecord(.x, t0, t1, reset = TRUE)` crops canonical `TSL` or `TSW`
  records by explicit time bounds and returns the same table shape as the
  input. It does not inspect signal amplitudes, does not trim literal zeros,
  and does not compute cumulative-intensity windows.

## Constructor cleanup

* `AT2TS()`, `VT2TS()`, and `DT2TS()` no longer use `match.call()` and the
  local `HasKNyq` flag to distinguish automatic STFT selection from explicit
  `kNyq`. The required branch is preserved directly with `missing(kNyq)`.
* `AT2TS(flatZeros = TRUE)` now flattens external acceleration tails to exact
  zeros using per-channel normalized acceleration amplitude before optional
  trimming. `Astop0 = 1e-3` corresponds to threshold `10` after PGA is
  normalized to `10000`. `trimZeros = TRUE` trims the union of component
  supports, not the intersection. The default path without `flatZeros` /
  `trimZeros` is unchanged.
* The constructor `auditSTFT()` blocks were simplified by removing one-use
  local metadata variables while preserving audit behavior and warnings.
* The final `TSL` packing code no longer creates one-use local `id.vars` and
  `measure.vars` variables. The actual `data.table::melt()` arguments remain
  named as `id.vars` and `measure.vars`.
* The finite-output guard before returning `TSL` is retained but compacted;
  this release does not change the constructor output or failure contract.

# gmsp 0.4.6

## New exported functions

* `TSL2AI()`, `TSL2CAV()`, `TSL2CAV5()`, and `TSL2ND()` compute
  time-indexed relative cumulative histories from canonical acceleration
  `TSL` rows. These helpers are intentionally separate objects, not one mixed
  intensity table: `TSL2AI()` returns `AIW`/`AIL`, `TSL2CAV()` returns
  `CAVW`/`CAVL`, `TSL2CAV5()` returns `CAV5W`/`CAV5L`, and `TSL2ND()`
  returns `NDW`/`NDL`.
* Wide output returns `<metadata>, t, <OCID columns>` with ratios
  `value(t) / max(value)` in the component columns. Long output returns
  `<metadata>, OCID, t, ratio`. `TSL2ND()` also includes `kh` in both output
  shapes. Absolute final values remain the responsibility of `TSL2IM()` /
  `getIntensity()`.
* `TSL2ND(..., kh = 0.05)` interprets `kh` as a fraction of each
  record/component PGA: `ac = kh * PGA`, `ky = ac / g`. This matches the
  historical `DN05` convention and avoids fixed critical accelerations that
  can produce zero Newmark displacement for many records.
* `TSL2PS(.x, xi = 0.05, Tn = NULL, output = "PSL",
  D50 = FALSE, D100 = FALSE, nTheta = 180L)` is the explicit
  response-spectrum helper for canonical `TSL` tables produced by
  `AT2TS()`, `VT2TS()`, and `DT2TS()`. It does not expose `BY`,
  `COL.s`, `COL.t`, or `COL.ID`; grouping metadata is derived from the
  `TSL` schema. Projects that currently call
  `TS2PS(TSL, COL.s = "s", COL.t = "t", COL.ID = "ID",
  Output = "PSL")` for canonical `TSL` input should migrate to
  `TSL2PS(TSL, output = "PSL")`.
* D50 and D100 are part of the canonical `TSL2PS()` contract. D50 migration:
  `TSL2PS(TSL, output = "PSL", D50 = TRUE, nTheta = 12L)` replaces
  `TS2PS(..., D50 = TRUE, n.theta = 12L)`. This is a breaking 0.4.6
  migration note; other projects should read this changelog before updating.
* Canonical table-shape helpers are now exported for downstream pipelines:
  `TSL2TSW()`, `TSW2TSL()`, `TSL2IM()`, `IML2IMW()`, `PSL2PSW()`, and
  `PSW2PSL()`. These replace local project helpers that manually cast
  `TSL`, `IML`, and `PSL` tables with repeated `dcast()` / `melt()` code.

## Table contract helpers

* `TSL2TSW(.x, by = "auto", ids = c("AT", "VT", "DT"))` converts canonical
  long time-series tables (`t`, `s`, `ID`, `OCID`) to wide `TSW` columns such
  as `AT.H1`, `VT.H1`, and `DT.H1`. `TSW2TSL()` reverses the projection and
  accepts either `t` or legacy constructor `ts` as the time column.
* `TSL2IM(.x, units.source, units.target = "mm", output = c("IML", "IMW"))`
  is the primary intensity API. `getIntensity()` remains as a compatibility
  wrapper and keeps the long `IML` default. Use `output = "IMW"` or
  `IML2IMW()` when a downstream table needs one row per metadata and `OCID`.
* `PSL2PSW()` and `PSW2PSL()` expose the canonical spectra long/wide
  conversion. After `PSW2PSL()`, `D50` and `D100` are ordinary `OCID` values,
  so scripts can filter `OCID %in% c("D50", "D100")` instead of parsing
  column-name suffixes.
* `TSL2PS()` now accepts vector `xi`. A scalar `xi` preserves the previous
  schema with no `xi` column. A vector `xi` adds `xi` as metadata:

  ```r
  TSL2PS(TSL, xi = c(0.02, 0.05, 0.10), output = "PSW",
         D50 = TRUE, D100 = TRUE)
  ```

  Downstream scripts that currently loop over damping ratios can call
  `TSL2PS()` once and group or filter by `xi`.

## D100 spectra extension

* `TSL2PS()` now adds D100 horizontal response spectra beside the existing D50
  path:

  ```r
  TSL2PS(TSL, output = "PSW", D50 = TRUE, D100 = TRUE, nTheta = 180L)
  ```

  `output = "PSL"` adds rows with `OCID = "D100"`. `output = "PSW"` adds
  `PSA.D100`, `PSV.D100`, and `SD.D100`. Scripts that already consume
  `PSA.D50`, `PSV.D50`, or `SD.D50` should explicitly include the matching
  D100 columns when they opt in.
* Downstream configs/scripts that currently enumerate D50, such as
  `spectra.D50`, `DIR_TARGET = "D50"`, or plot/export direction lists, should
  add `D100` explicitly only for runs that request `D100 = TRUE`.
* D100 uses the same rotation grid as D50. It is the maximum over rotated
  horizontal spectra for each period and spectral ID; the maximizing angle can
  vary by `Tn` and by `PSA` / `PSV` / `SD`. The implementation returns spectra
  only, not the D100 angle. When `D50 = TRUE` and `D100 = TRUE`, both derived
  components are computed from one rotated response matrix.
* The downstream contract stays canonical `TSL2PS()`. Do not migrate scripts to
  `TS2PS()`, `TS2PSA()`, `RotD50`, or `RotD100` names. The implementation plan
  is recorded in `dev/SoT/PLAN-TSL2PS-D100.md`.

## API cleanup

* `getND()` has been moved out of the exported package surface to
  `dev/legacy/get_ND.R`. Its public signature mixed raw heterogeneous units,
  target units, and Newmark episode controls in a way that needs a separate
  design before returning to the package.
* `mapOCIDtoDir()` has been moved out of the exported package surface to
  `dev/legacy/mapOCIDtoDir.R`. `mapComponents()` now owns component
  classification, and `extractRecord()` calls `mapComponents(...,
  rotate = FALSE)` before alignment. The new SoT compares classification
  against the old helper and verifies byte-identical raw CSV/JSON outputs for
  fixture extraction.
* `mapComponents()` now accepts `output = c("long", "wide")` and can consume
  either long `t, OCID, s` input or wide `t, <OCID1>, <OCID2>, <OCID3>`
  input. The default `"long"` shape remains the compatible output;
  `output = "wide"` returns `data.table(t, H1, H2, UP)`. Both outputs preserve
  `componentMap` and `rotate` attributes; rotated outputs also preserve
  `theta`.
* `filterIMF()` has been moved out of the exported package surface to
  `dev/legacy/filterIMF.R`. Users should compose filtering workflows directly
  from canonical `TSL`, `TS2IMF()`, and `TSL2PS()`.
* The 0.4.x spectra path is closed around canonical `TSL` consumption:
  spectra code should call `TSL2PS()`, not `TS2PS()` with `BY`, `COL.s`,
  `COL.t`, and `COL.ID`.
* `TS2PS()` has been removed from the exported package surface. It mixed
  generic long-table dispatch, grouping autodetection, column-name arguments,
  and D50 behavior. Use `TSL2PS()` with canonical `TSL` input.
* `TS2IMF()` is now a single-series worker over canonical columns `t` and `s`.
  It no longer exposes `BY`, `COL.s`, `COL.t`, or `Output`; use `output`.
  Grouped callers should use `data.table` grouping, for example
  `TSL[ID == "AT", TS2IMF(.SD, output = "TSL"), by = .(RecordID, OCID),
  .SDcols = c("t", "s")]`.
* `normalizeTS()` now consumes canonical `TSL` input directly:
  `normalizeTS(TSL, norm = "PGA")`. It no longer exposes `COL.s`, `COL.ID`,
  `COL.OCID`, or `BY`; grouping is derived from metadata columns plus `OCID`.
* `readTS()`, `readAT()`, `readVT()`, and `readDT()` now use `.x` for the
  selection table and `path` for the records root. This is a named-argument
  breaking change:

  ```r
  # before
  readTS(SEL = SEL, recordsDir = RecordsDir, kind = "VT")
  readAT(SEL, recordsDir = RecordsDir)

  # after
  readTS(.x = SEL, path = RecordsDir, kind = "VT")
  readAT(.x = SEL, path = RecordsDir)
  ```

  Positional calls with both arguments, such as `readAT(SEL, RecordsDir)`,
  remain valid. The same `.x` / `path` rule applies to `readVT()` and
  `readDT()`; do not update callers to keep `recordsDir =`.
* `buildMaster()` has been removed from the exported package surface. It
  joined project-local database index tables and encoded source-precedence
  decisions that belong in downstream project scripts, not in the signal
  processing library. Project pipelines should own their master-table builder
  locally and pass the resulting selection tables to `readTS()` / `readAT()`.
* `buildRawFileTable()` now uses `path.records` for the records root and
  `path.index` for the index root:

  ```r
  # before
  buildRawFileTable(recordsDir = RecordsDir, indexDir = IndexDir,
                    owners = Owner)

  # after
  buildRawFileTable(path.records = RecordsDir, path.index = IndexDir,
                    owners = Owner)
  ```

  Positional calls with both paths remain valid. Named callers must not keep
  `recordsDir =` or `indexDir =`.
* `buildRawRecordTable()` and `buildRawIntensityTable()` now use the same
  path argument contract as `buildRawFileTable()`:

  ```r
  # before
  buildRawRecordTable(recordsDir = RecordsDir, indexDir = IndexDir,
                      owners = Owner)
  buildRawIntensityTable(recordsDir = RecordsDir, indexDir = IndexDir,
                         owners = Owner)

  # after
  buildRawRecordTable(path.records = RecordsDir, path.index = IndexDir,
                      owners = Owner)
  buildRawIntensityTable(path.records = RecordsDir, path.index = IndexDir,
                         owners = Owner)
  ```

  Named callers must not keep `recordsDir =` or `indexDir =`.
* Single-path raw/archive helpers now use `path`:

  ```r
  archiveRawOwner(path = StationDir)
  getRawIntensities(path = StationDir)
  writeSelection(DT, name = "subset", path = SelectionDir)
  ```

  Named callers must not keep `stationDir =` or `selectionDir =`.
* `parseRecord()` and `extractRecord()` now use `.x` for the one-record
  metadata subset and `path` for the records root:

  ```r
  # before
  parseRecord(masterRows = Rows, recordsDir = RecordsDir)
  extractRecord(masterRows = Rows, recordsDir = RecordsDir)

  # after
  parseRecord(.x = Rows, path = RecordsDir)
  extractRecord(.x = Rows, path = RecordsDir)
  ```

  Positional calls with both arguments, such as `extractRecord(Rows,
  RecordsDir)`, remain valid. Named callers must not keep `masterRows =` or
  `recordsDir =`.
* `auditParsers()` now uses `.x` for the record metadata table, `owner` for one
  OwnerID, and `path` for the records root:

  ```r
  # before
  auditParsers(OwnerID = Owner, master = Master, recordsDir = RecordsDir)

  # after
  auditParsers(.x = Master, owner = Owner, path = RecordsDir)
  ```

  Named callers must not keep `OwnerID =`, `master =`, or `recordsDir =`.
  Positional calls must use the new order `auditParsers(Master, Owner,
  RecordsDir)`.
* `AT2TS()`, `VT2TS()`, and `DT2TS()` now expose `time` as an input-column
  selector and always return canonical `TSL` columns `t`, `s`, `ID`, and
  `OCID`. The old `COL.t` and `COL.s` arguments are removed. `time` is not an
  output-column name; it is only the name of the time column in the input table.
  Scripts should migrate as follows:

  ```r
  # before
  AT2TS(Wide, Units = "mm", COL.t = "time", COL.s = "signal",
        Output = "TSL")

  # after
  AT2TS(Wide, units.source = "mm", time = "time", output = "TSL")
  ```

  Downstream code must consume the returned `TSL` as `t` and `s`. For example,
  `.SDcols = c("time", "signal")` should become `.SDcols = c("t", "s")`.
  Calls that already used a column named `t` and did not pass `COL.t`/`COL.s`
  do not need a script change.
* The constructor family also normalizes the approved public argument names:
  `Units` -> `units.source`, `TargetUnits` -> `units.target`,
  `Output` -> `output`, `Verbose` -> `verbose`, `Audit` -> `audit`,
  `isRawData` -> `isRaw`, `Resample` -> `resample`,
  `FlatZeros` -> `flatZeros`, `TrimZeros` -> `trimZeros`,
  `Detrend` -> `detrend`, and `Regularize` -> `regularize`.
  `VT2TS()` and `DT2TS()` additionally use `Derivate` -> `derivate` and
  `LowPass` -> `lowPass`.
* `regularize()` is no longer public API. Time-grid regularization is handled
  inside `AT2TS()`, `VT2TS()`, `DT2TS()`, `TS2IMF()`, and `TSL2PS()` via the
  internal helper `.regularize()`. External scripts should pass `time = "..."`
  to the constructor when the input time column is not named `t`; they should
  not call `regularize()` directly.
* `setSTFT()` is no longer public API. STFT strategy selection is now the
  internal helper `.setSTFT()` used by `AT2TS()`, `VT2TS()`, `DT2TS()`, and
  `auditSTFT()`. External scripts should configure the public constructors
  instead of calling the STFT strategy helper directly.

# gmsp 0.4.5

## New exported functions

* `mapComponents(DT, rotate = TRUE)` maps one long `(t, OCID, s)` record to
  canonical processed directions. Provider channels remain in `OCID`, returned
  `DIR` values are `H1` / `H2` / `UP`, and `attr(out, "componentMap")` records
  the `OCID` to `DIR` mapping. With `rotate = TRUE`, horizontal samples are
  rotated to principal axes and `attr(out, "theta")` stores the rotation angle.

## Compatibility

* `mapOCIDtoDir()` remained exported and unchanged in 0.4.5. It was removed
  from the exported package surface in 0.4.6.
* `extractRecord()` and raw CSV/JSON writer contracts are unchanged; raw
  products continue to preserve provider `OCID` values.

# gmsp 0.4.4

## Breaking changes — units contract

* `AT2TS()`, `VT2TS()`, `DT2TS()`, `getIntensity()` now require **canonical
  base** values for `Units` and `TargetUnits`:

  - length basis: `"mm"`, `"cm"`, `"m"` — valid for AT, VT, DT.
  - acceleration aliases: `"g"`, `"gal"` — valid for `AT2TS()` only.
  - `VT2TS()` and `DT2TS()` reject `"g"`/`"gal"` (an acceleration unit
    passed to a velocity/displacement function is a category error).
  - `getIntensity()` accepts length-base only (`"mm"`/`"cm"`/`"m"`) for
    `Units` and `TargetUnits`; the same scale factor applies to AT, VT,
    and DT rows of a long TSL, so only a length base is well-defined.
  - Verbose forms like `"mm/s"`, `"mm/s2"`, `"m/s2"`, `"cm/s"` are
    rejected with a clear error pointing to the canonical base.

  KIND (acceleration / velocity / displacement) is determined by the
  function called, not by a suffix of the `Units` string. Provider
  string parsers `.parseUnits()` and `.parseKind()` continue to accept
  heterogeneous raw strings (e.g. `"cm/sec2"`, `"G"`, `"GALS"`) —
  those operate at the ingestion layer and pre-normalize to canonical
  before flowing into the public API.

  **Migration**: callers passing `Units = "mm/s"` switch to
  `Units = "mm"`; same for displacement. Callers passing `Units = "g"`
  to `VT2TS()`/`DT2TS()` must move to `AT2TS()`.

  Validated by a Ship-of-Theseus harness
  (`inst/dev/sot/compare_units_contract.R`): Stage A 42/42 (canonical
  inputs byte-identical to prior gmsp via `identical()`), Stage A-bis
  3/3 (current verbose ≡ canonical, documents migration), Stage C-bis
  9/9 (illegal inputs error correctly). Verify mode (post-swap-in,
  in-place edit vs captured baseline) 51/51 PASS.

## New exported functions

* `readTS(SEL, recordsDir, kind)` — canonical KIND-parameterised
  sidecar reader. The previous `readAT()` is now a one-line wrapper
  (`kind = "AT"`); the two new wrappers `readVT()` and `readDT()`
  cover velocity and displacement.
* `readVT()`, `readDT()` — thin wrappers around `readTS()`.

## Blasting / velocity-record support

* `readISEE()` — Micromate ISEE blasting-seismograph parser
  (velocity, mm/s). Auto-detects v10 / `.TXT` and v11 / `.CSV`
  firmware variants from the file content.
* `extractRecord()` — KIND is routed through the sidecar filename
  prefix (`AT` / `VT` / `DT`). The JSON peak field is named
  accordingly: `PGA` / `PGV` / `PGD`. An explicit `kind = "VT"`
  override is accepted for records whose `Units` cannot be parsed
  by `.parseKind()`.
* `buildRawRecordTable()` — scans `raw/(AT|VT|DT).*.json` (no
  KIND change to the canonical schema).

## Bug fixes

* `extractRecord()` — full numeric precision preserved in the
  sidecar JSON (no `digits` truncation).
* `extractRecord()` / `getRawIntensities()` — raw CSV columns now
  consistently remain provider `OCID` values while the JSON sidecar
  carries the explicit `DIR` (`H1`/`H2`/`UP`) to `OCID` mapping used by
  raw intensity indexing.
* `TS2PS()` — rejects user-supplied `Tn` grids containing `0`; the
  function already prepends the `Tn = 0` peak-value anchor internally.
* `TS2PS(COL.ID = ...)` — structured long input now treats source
  time-series `ID` and spectral `ID` as distinct contracts:
  `AT -> PSA`, `VT -> PSV`, and `DT -> SD`.
* `TS2PS(Output = "PSW")` — multi-OCID long input now produces wide
  OCID-labelled spectral columns such as `PSA.H1`, `PSV.H1`, and
  `SD.H1` instead of source-ID-labelled spectral columns.
  Grouped PSW assembly no longer uses `rbindlist(fill = TRUE)`.
* `TS2PS(D50 = TRUE)` — structured long input can now add `OCID = "D50"`
  as the fourth horizontal spectral component. `PSA.D50`, `PSV.D50`,
  and `SD.D50` are computed independently from rotated AT, VT, and DT
  respectively. `TS2PS(D50 = TRUE)` is the public D50 path.
* `TS2IMF()` — numeric `imf.remove` now treats positive indices as IMFs
  counted from the start and negative indices as IMFs counted from the
  end, then removes the union. For example, `c(1L, -1L, -2L)` removes
  `IMF1` plus the last two IMFs. This replaces the previous
  positive-include / negative-protect behavior, which made mixed
  selections such as `c(1L, -1L)` remove nothing. Zero, non-finite,
  and out-of-range numeric values are ignored.
* `TS2IMF()` grouped output no longer pads inconsistent per-group
  schemas with `NA`. Groups must produce the same output columns, or
  the bind fails with a schema diagnostic instead of using
  `rbindlist(fill = TRUE)`.
* `AT2TS()`, `VT2TS()`, and `DT2TS()` no longer use blind `na.omit()`
  for time-step extraction or TSL packing. Non-finite time and signal
  samples now fail with explicit diagnostics instead of being hidden
  by regularization or row dropping; finite regular and irregular
  inputs are unchanged.
* `getIntensity()` no longer uses `rbindlist(fill = TRUE)` for the
  final acceleration/velocity/displacement IM bind. Internal IM
  schemas must match before binding.
* `buildRawIntensityTable()` no longer uses `fill = TRUE` when binding
  new per-station intensity tables or when merging an incremental cache.
  Raw intensity tables are canonicalized and schema-checked before
  binding.
* `buildRawFileTable()` no longer uses `rbindlist(fill = TRUE)`.
  Provider records are canonicalized to the documented RawFileTable
  schema before binding; absent canonical provider fields are emitted
  as typed `NA` columns.
* `buildMaster()` was retired from the package API; project-specific owner
  joins now belong in downstream scripts.
* `wideTrunc()` was removed. It had no internal callers.
* Local-only style cleanup renamed STFT/audit temporaries in
  `AT2TS()`, `VT2TS()`, `DT2TS()`, `auditSTFT()`, and `.ffilter()`
  without changing public interfaces. The internal source file
  `R/build_FFT.R` is now `R/buildFFT.R`.
* `.setSTFT()` is tracked separately under Tier 2 SoT. The approved passes
  remove a dotted boolean local, canonicalize the candidate-logging guard, and
  inline a short-lived downsample scalar. A later narrow local cleanup removes
  one-use `k.grid`, inlines the one-use `NP.required` threshold, and resolves
  `NW.max` / `NWmax` without changing candidate/frequency locals. These
  changes do not alter the STFT strategy or public interfaces.

## Documentation

* `vignettes/database.Rmd` — documents the `readTS()` family and
  adds the ISEE blasting flow (`readVT()` + `VT2TS()`).
* `selectRecords()` `@seealso` updated to point to the `readTS()`
  family.

# gmsp 0.4.0

First CRAN submission. Consolidates the standalone `gmdb` package
(provider-format parsers and per-record indexing) into `gmsp` so the
full strong-motion record pipeline — from raw provider file to
processed AT / VT / DT time series — lives in one place.

## New exported functions (merged from `gmdb`)

* **Format parsers** (provider file → LONG `(t, OCID, s)`):
  `readAT2()` (PEER NGA-W2), `readV2()` / `readV2A()` (CESMD multi /
  single-channel + NWZ), `readAC()` (IGP / UCR), `readTR()` (Geological
  Survey of Canada), `readTwoCol()` (generic two-column ASCII).
* **Extraction pipeline**: `parseRecord()`, `mapOCIDtoDir()`,
  `alignComponents()`, `extractRecord()`. The orchestrator writes
  per-record `raw/AT.<RecordID>.csv` plus a sidecar JSON carrying
  `DIR` / `OCID` / `NP` / `PGA` / `dt` / `Fs` / `Units` metadata.
  `RecordID` is the first 16 hex chars of the WIDE-CSV md5 hash.
* **Indexing**: `buildRawFileTable()`, `buildRawRecordTable()`,
  `buildRawIntensityTable()`. Project-specific event / station joins
  and master-table assembly are intentionally outside the exported API.
* **Selection / IO**: `selectRecords()`, `writeSelection()`,
  `readAT()` (loads `raw/AT.<RecordID>.csv` back into the shape
  `AT2TS()` expects), `getRawIntensities()`.
* **Audit**: `auditSite()` (Vs30 sanity), `auditDistances()`
  (lat / lon / Repi / Rhyp), `auditParsers()` (dry-run
  `parseRecord()` per owner).
* **Maintenance**: `archiveRawOwner()` (compress and verify provider
  files after extraction).
* Historical note: `setSTFT()` was exported in this line of development, but
  current 0.4.6 API cleanup internalizes it as `.setSTFT()`.

## Breaking changes

* The retired `gmdb` package is no longer maintained. Code that
  previously called `gmdb::extractRecord(...)` should now call
  `gmsp::extractRecord(...)`.

* **Required paths.** `parseRecord()`, `extractRecord()`,
  `buildRawFileTable()`, `buildRawRecordTable()`,
  `buildRawIntensityTable()`, `auditParsers()`,
  `writeSelection()` and `readAT()` no longer carry hard-coded
  developer-specific defaults for their path arguments
  (`recordsDir` / `indexDir` / `selectionDir`). The path is now a
  required argument; the function fails fast with a clear "argument
  ... is missing" error if not supplied, rather than silently
  reading from a developer-specific location.

  Validated end-to-end by a Ship-of-Theseus harness:
  Stage A (explicit-path identity) 16/16 across 4 real provider
  fixtures (IGP, NGAW, NWZ, UCR); Stage A-bis (no-arg corner-case
  errors) 10/10. Pipeline regression 22/22 against the Phase 1
  baseline.

* **`readAT()` signature.** Now `readAT(SEL, recordsDir)`; previously
  read from the unexported package constant `RECORDS`, which has
  been removed.

* **Removed fossil parameters.** Two parameters that were exposed in
  the public signature but never reached the function body are
  dropped:
  - `AT2TS(..., Derivate = "time")` — the source marked it `(future
    use)`; no caller used a non-default value and the body never
    branched on it.
  - `TS2IMF(..., TrimZeros = FALSE)` — the worker's `if (TrimZeros)`
    block was an explicit no-op placeholder.

  Callers that passed either argument explicitly (none observed in
  the gmsp ecosystem) will now get `unused argument`. The default
  behaviour is unchanged.

* **License.** Changed from `file LICENSE` (all-rights-reserved) to
  `MIT + file LICENSE`. `LICENSE` is the CRAN-form two-line
  declaration; `LICENSE.md` ships the full MIT text.

## Documentation

* Six vignettes ship with the package, replacing the previous
  Jekyll-only `docs/` site:
  `gmsp-quickstart` (5-minute runnable hello world),
  `signal-processing`, `imfs`, `spectra`, `intensity-measures` (math
  references with LaTeX), and `database` (indexing-layer pipeline
  and file-layout contract).
* `_pkgdown.yml` reorganised: navbar lists all six vignettes;
  reference grouped by signal-processing core, indexing helpers,
  selection / IO, and audit.
* `README.md` covers both layers (signal-processing + optional
  indexing) and includes CRAN status / downloads badges, a citation
  bibentry, and links to all six vignettes.

## Internal

* `R/local.R` and `R/global.R` consolidated into `R/gmsp-package.R`
  with a single `utils::globalVariables()` block and a single
  `TARGET_UNITS` declaration.
* Dropped `gmsp::` self-prefix on the call to `getIntensity()` in
  `getRawIntensities.R`.
* `@param LowPass` added to `DT2TS()` and `VT2TS()` (was
  undocumented).
* `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes.

# gmsp 0.3.0

Final pre-merge release of the standalone signal-processing package.
Public API: `AT2TS()`, `VT2TS()`, `DT2TS()`, `TS2IMF()`,
`getIntensity()`, `getND()`, `regularize()`.
