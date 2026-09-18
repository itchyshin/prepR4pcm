## Submission

This is a documentation-only patch update from the CRAN release
`prepR4pcm` 1.0.2.

This update:

* corrects the documented synonym-stage `match_score` from `1` to `0.95`,
  matching the existing implementation; and
* adds a regression assertion for the documented score.

There are no changes to matching behaviour or the public API.

## Pre-release checks

Frozen candidate: `prepR4pcm_1.0.3.tar.gz`, generated from source commit
`276eca71c94f0681f72a49df27cfe81f29e56ad7`.

* SHA-256: `eb41ec2f8aa2eda015f7330ff91beb307b5c73228c3a5a0643a06c2f8f851687`;
* `R CMD check --as-cran --run-donttest`: 0 errors, 0 warnings, 0 notes;
* full `testthat` suite: passed within the CRAN-shaped check;
* package vignettes rebuilt: passed; and
* tarball inventory: 250 entries; agent instructions and repository metadata
  excluded.

## Downstream dependencies

CRAN reports no reverse dependencies for `prepR4pcm`.
