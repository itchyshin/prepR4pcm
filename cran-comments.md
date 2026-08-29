## Submission

This is a documentation-only patch update from the CRAN release
`prepR4pcm` 1.0.2.

This update:

* corrects the documented synonym-stage `match_score` from `1` to `0.95`,
  matching the existing implementation; and
* adds a regression assertion for the documented score.

There are no changes to matching behaviour or the public API.

## Pre-release checks

Source checks run on 2026-08-29:

* full `testthat` suite — 0 failures; and
* `pkgdown::check_pkgdown()` — no problems found.

The exact frozen tarball and platform matrix will be checked again immediately
before submission, and this section will be replaced with those results.

## Downstream dependencies

CRAN reports no reverse dependencies for `prepR4pcm`.
