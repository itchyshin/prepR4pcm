## Resubmission / new submission

This is a new CRAN submission for `prepR4pcm`, an R package for
reconciling species names across datasets and phylogenetic trees to
prepare inputs for phylogenetic comparative methods (PCM, PGLS, PGLMM).

## R CMD check results

Local `R CMD check --as-cran`:

    0 errors | 0 warnings | 0 notes

## Test environments

* local macOS Tahoe 15 (aarch64), R 4.5.2 — clean
* win-builder (devel and release) — _results pending_
* R-hub v2 (linux, macos, windows) — _results pending_

(Remote check results will be added before final submission.)

## DOI notes for reviewers

Several DOIs used in help pages, the bird-workflow vignette, and
README return HTTP 403 to anonymous `curl`/`urlchecker` requests.
These are Wiley, University of Chicago Press, and Oxford University
Press rate-limiting anonymous bots; the DOIs are real and resolve in
a browser. Specifically:

* `10.1111/ele.13898` — Tobias et al. 2022 (AVONET), Wiley (403 bot-blocked)
* `10.1086/703588` — Delhey et al. 2019, UChicago Press (403 bot-blocked)
* `10.1093/bioinformatics/bty633` — Paradis & Schliep 2019 (ape), OUP (403 bot-blocked)
* `10.1111/2041-210X.13440` — Norman et al. 2020 (taxadb), Wiley (403 bot-blocked)

The following DOIs return 200:

* `10.1038/s41597-023-02837-1` — Chia et al. 2023 (NestTrait v2)
* `10.1038/nature11631` — Jetz et al. 2012

## Suggests usage

All packages in `Suggests` are used conditionally:

* `taxadb` — guarded by `requireNamespace("taxadb", quietly = TRUE)` in
  `R/pr_authority.R` (only required when `authority` is supplied).
* `phytools` — guarded in `R/reconcile_augment.R`; a pure-`ape`
  fallback is provided when phytools is unavailable.
* `caper`, `MCMCglmm` — referenced only in `\donttest{}` / commented
  example code as downstream PCM workflow illustrations. The
  bird-workflow vignette also illustrates them, with each chunk guarded
  by `eval = requireNamespace(..., quietly = TRUE)` so the vignette
  knits cleanly without those packages installed.
* `knitr`, `rmarkdown` — used by the vignette builder.
* `dplyr`, `pkgdown`, `testthat` — used only by tests, vignettes, and
  site building.

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
