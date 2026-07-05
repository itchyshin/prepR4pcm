## Submission

This is a patch release for `prepR4pcm` 1.0.1, following the accepted
1.0.0 CRAN release.

This release clarifies and safeguards taxonomy-crosswalk workflows:

* Documented that crosswalk-derived rows supplied as `overrides` are applied
  before the exact -> normalised -> synonym -> fuzzy matching cascade.
* Added a warning when non-one-to-one crosswalk rows, such as taxonomic
  splits or lumps, are kept as automatic overrides.
* Updated the bird workflow vignette to recommend baseline matching first,
  followed by reviewed one-to-one crosswalk rows for unresolved names only.
* Fixed a pure-`ape` fallback bug in `reconcile_augment()` when adding tips
  to zero-length or split terminal branches.

## R CMD check results

Local source-tree CRAN-shaped check:

    0 errors | 0 warnings | 0 notes

The persistent source tarball
`/tmp/prepR4pcm-cran-submit-1.0.1-20260702/prepR4pcm_1.0.1.tar.gz`
also passed local tarball-level `R CMD check --as-cran --run-donttest`
with `_R_CHECK_FORCE_SUGGESTS_=false` because several optional backend
packages in `Suggests` are not installed on the local machine.

Additional local checks:

* `devtools::test()` — 0 failures.
* `urlchecker::url_check()` — all URLs correct.
* `pkgdown::check_pkgdown()` — no problems found.
* Source tarball scan confirmed local agent files such as `CLAUDE.md` are
  excluded by `.Rbuildignore`.

## Test environments

* local macOS Tahoe 26.5.1 (aarch64), R-devel 4.6.0 — tarball check passed
  with optional Suggests not forced.
* local macOS Tahoe 26.5.1 (aarch64), R 4.5.2 — source-tree CRAN-shaped
  check passed with 0 errors, 0 warnings, and 0 notes.

## Downstream dependencies

There are no strong reverse dependencies to check.
