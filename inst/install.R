## ── gmdb: developer workflow ─────────────────────────────────────────────────
## Run sections manually as needed.  Do NOT source() this file end-to-end.
## For CRAN release workflow see inst/cran-release.R.

## ── Install ───────────────────────────────────────────────────────────────────
## From CRAN (stable release — update only when a new version is accepted):
# install.packages("gmdb")

## From GitHub (latest commit on main — picks up every push immediately):
# remotes::install_github("averriK/gmdb")
# pak::pak("averriK/gmdb")   # alternative if pak is available

## ── Development cycle ─────────────────────────────────────────────────────────
## 1. Regenerate documentation (roxygen2 → NAMESPACE + man/)
devtools::document()

## 2. Local CRAN check
devtools::check(document = FALSE, cran = TRUE, force_suggests = TRUE)

## 3. Local install
devtools::install(dependencies = TRUE, build = TRUE, upgrade = TRUE)

## 4. Render pkgdown site locally (optional sanity check before pushing)
# pkgdown::build_site()
