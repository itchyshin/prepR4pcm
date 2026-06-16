## Submission

This is a new CRAN submission for `prepR4pcm`, an R package for
reconciling species names across datasets and phylogenetic trees to
prepare inputs for phylogenetic comparative methods (PCM, PGLS, PGLMM).

This 1.0.0 release incorporates the CRAN incoming pretest fixes made
after the 0.5.1 candidate:

* Replaced a stale `https://www.itis.gov/` vignette link with
  `https://itis.gov/`.
* Changed the `reconcile_report()` Rd figure width from a percentage
  value to a pixel value.
* Reduced the `reconcile_diff()` and `reconcile_augment()` examples to
  small toy data so they remain below CRAN incoming example-time
  thresholds.

## R CMD check results

Local `R CMD check --as-cran --run-donttest`, with CRAN incoming and
remote incoming checks enabled:

    0 errors | 0 warnings | 1 note

The single note is expected for a first submission:

* `New submission`

Additional local checks:

* `devtools::test()` — 0 failures, 27 warnings, 5 skips, 2740 passes.
* `urlchecker::url_check()` — all URLs correct.
* `pkgdown::check_pkgdown()` — no problems found.

## Test environments

* local macOS Tahoe 26.5 (aarch64), R 4.5.2 — clean apart from the
  expected new-submission note above.

## Suggests usage

All packages in `Suggests` are used conditionally:

* `taxadb` — guarded by `requireNamespace("taxadb", quietly = TRUE)` in
  `R/pr_authority.R` (only required when `authority` is supplied).
* `phytools` — guarded in `R/reconcile_augment.R`; a pure-`ape`
  fallback is provided when phytools is unavailable.
* `caper`, `MCMCglmm` — referenced only in downstream PCM workflow
  illustrations. Executable vignette chunks that use these packages are
  guarded by `eval = requireNamespace(..., quietly = TRUE)` so the
  vignette knits cleanly without those packages installed.
* `clootl`, `rtrees` — optional backends of `pr_get_tree()` (issue
  #42). Each is guarded by runtime availability checks and returns a
  targeted install message when unavailable.
* `piggyback` — used indirectly by the optional `rtrees`/`megatrees`
  backend to download reference-tree data in non-CRAN backend tests.
* `knitr`, `rmarkdown` — used by the vignette builder.
* `dplyr`, `pkgdown`, `testthat` — used only by tests, vignettes, and
  site building.

## Optional GitHub-only integrations

`datelife`, `U.PhyloMaker`, `V.PhyloMaker`, and `V.PhyloMaker2` are
optional runtime-only integrations. They are not declared in
DESCRIPTION because they are not currently available from CRAN or
Bioconductor. The relevant functions check availability at runtime
and emit targeted installation guidance rather than loading these
packages unconditionally.

## Downstream dependencies

This is a new package with no reverse dependencies on CRAN.

## Notes on the package

* Example datasets are subsets of published trait databases (AVONET,
  NestTrait, Delhey plumage) and phylogenies (Jetz, Clements), each
  cited in `R/data.R` with source and DOI.
* Long-running examples are wrapped in `\donttest{}` where applicable.
* The package uses `cli` for user-facing messages and `rlang` for
  error handling. Two error paths in `R/pr_authority.R` were migrated
  from `rlang::abort()` to `cli::cli_abort()` so that `{.pkg ...}` /
  `{.code ...}` markup renders correctly (was issue #4 in our GitHub
  tracker).
* `authority = "ott"` (Open Tree of Life) was removed from the list
  of supported taxonomic authorities because the default `taxadb`
  release does not ship a working OTT schema (was issue #5).
