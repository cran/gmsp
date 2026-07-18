# Selection scripts

This directory contains the active selection runner template shipped with
`gmsp`.

| File | Purpose |
|---|---|
| `buildMaster.R` | Local helper for reading or building `MasterIndex` tables. |
| `runBuildMaster.R` | Reindexing runner. It materializes owner-level and optional global `MasterIndex` files under `path.index`. |
| `runSelect.R` | Active record-selection runner. |
| `runSelect.json` | Canonical editable selection JSON template. |

The normal selection entry point is `runSelect.R`. Installed package users
should prefer the exported wrapper:

```r
gmsp::runSelect("gmsp/select.json", root = "/path/to/project")
```

## `runBuildMaster.R`

Run `runBuildMaster.R` only when the database index changes, the
`MasterIndex` schema changes, or product-availability fields need to be
materialized. It is not part of every selection run.

Materialized products:

```text
index/MasterIndex.<OwnerID>.csv
index/MasterIndex.csv
```

## `runSelect.R`

`runSelect.R` consumes a `MasterIndex`; it does not rebuild the master index
for a normal selection. The selection JSON declares `path.index`, `owners`,
and `processID`.

`processID = "raw"` is valid when the selection feeds a producer such as
`runProcess()`. Downstream stages should consume either the materialized
selection published by that process or a selection made directly on the
processed `ProcessID`.

Top-level numeric filters such as `PGA`, `EventMagnitude`, `Repi`, `Rhyp`,
`EventDepth`, `StationVs30`, `Year`, `D0575`, `D0595`, `D2080`, and `Dmax`
are applied before optional named `criteria`. This allows project-wide bounds,
for example a PGA floor/ceiling, to be stated once.

Do not use `requireIMF = true` with `processID = "raw"`. If a selection
requires IMF products, select a processed `ProcessID` that has already
published those products.

## Target-aware PGA selection

The selection JSON may declare `path.matchConfig`. When present, the selector
reads the target spectrum and `shape.recordFactor` bounds from the referenced
`runMatch` JSON and derives the source-record PGA window:

```text
PGA.source.min = PGA.target / recordFactor.max
PGA.source.max = PGA.target / recordFactor.min
```

If `path.matchConfig` is set, do not also declare a top-level `PGA` range in
`runSelect.json`; the runner fails intentionally to avoid duplicate selection
surfaces.
