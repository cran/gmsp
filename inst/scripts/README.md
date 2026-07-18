# scripts

Project-template scripts live here.

The canonical template surface is organized by stage. Each stage owns its
runner, JSON configs, and local helper module:

```text
selection/runSelect.R
selection/runStage0.R
process/runProcess.R
gmsp/runGMSP.R
trim/runTrim.R
imf/runIMF.R
psa/runPSA.R
plot/runPlot.R
export/runExport.R
match/runMatch.R
```

`plot/runPlot.R` is an optional raw QA/debug widget runner. It is not the SRK
publication renderer. SRK report plots should be rendered by the reporting
layer from materialized `gmsp` CSV products.

When working inside a downstream project, prefer the exported wrappers:

```r
gmsp::runSelect("gmsp/select.json", root = "/path/to/project")
gmsp::runStage0("gmsp/match.json", root = "/path/to/project")
gmsp::runMatch("gmsp/match.json", root = "/path/to/project")
gmsp::runExport("gmsp/export.json", root = "/path/to/project")
```

The script files can also be run from a package checkout for template
development, for example:

```sh
Rscript examples/scripts/selection/runSelect.R scripts/selection/runSelect.json
Rscript examples/scripts/gmsp/runGMSP.R scripts/gmsp/runGMSP.json
Rscript examples/scripts/trim/runTrim.R scripts/trim/runTrim.json
```

Inside these example runners, relative paths resolve under `examples/`:

```text
gmsp/records/... -> examples/gmsp/records/...
gmsp/selection/... -> examples/gmsp/selection/...
oq/data/... -> examples/oq/data/...
scripts/... -> examples/scripts/...
```

When passing a config path, pass the `scripts/...` path shown above, not
`examples/scripts/...`.

The raw strong-motion database remains external and should be referenced by the
project JSON. Template placeholders use project-relative paths such as:

```text
gmsp/index
gmsp/raw
gmsp/database
```

Selection boundary:

```text
runSelect(processID = "raw") -> runProcess
runProcess -> stage/<ProcessID>.selection/selection.csv
runExport/runMatch -> existing ProcessID selection or produced package
runPlot -> optional raw QA/debug plots from existing products
```

Raw selections are valid producer input. They are how a user chooses source
records before creating a new process set such as `F30`. Downstream stages
should consume the materialized `ProcessID` selection or a produced package,
not reinterpret raw records as processed products.

The intended V3 contract is one canonical editable JSON per pipeline, stored
inside the corresponding stage folder:

```text
selection/runSelect.json
process/runProcess.json
gmsp/runGMSP.json
trim/runTrim.json
imf/runIMF.json
psa/runPSA.json
match/runMatch.json
plot/runPlot.json
export/runExport.json
```

The canonical `plot/runPlot.json` exists only for explicit raw QA/debug runs.
Do not use it as a final client-delivery plot contract when SRK report products
are available.

New pipeline references should use `psa/runPSA.R` and `psa/runPSA.json`.

Pipeline output folders copy the JSON they used as a sidecar for traceability.
Do not create per-run JSON swarms in this directory.

The active editable selector config is `selection/runSelect.json`.

`runScale` is inactive for the current spectral-match workflow. The current
match stage lives under `match/`.

Current match note:

```text
The current method is V4: Stage 0 spectral selection (runStage0, driven
by the same site match JSON via stage0.N and path.selection), Stage A
modal shaping with the full retained modal set, and Stage B global
band-conditioned suite scaling (shape.bandWeight, shape.deficitWeight,
optional recordFactor backstop). The full workflow, JSON anatomy,
screening options, and acceptance diagnostics are documented in
vignettes/spectral-matching.Rmd.

The canonical JSON contract is examples/scripts/match/runMatch.json.
Do not assume older batch/stageB generated folders are active. See
dev/plan/match/active.md and dev/plan/match/CONTRACT-match.md.
```

Other folders:

```text
tools/        shared runner utilities
```

Database maintenance and ingest utilities are no longer kept in this package
template. They should live with the database tree owned by the project.

Inactive scripts are not part of the canonical template surface.
