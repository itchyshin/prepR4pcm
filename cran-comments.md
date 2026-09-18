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
`17067494a1f343fa4f15a3ca2ddd0386a8948fe7`.

* SHA-256: `b1cc640fe6947e1f4c053f5f38d19a3238157dd254c59912c640fed13d4475e5`;
* `R CMD check --as-cran --run-donttest`: 0 errors, 0 warnings, 0 notes;
* full `testthat` suite: passed within the CRAN-shaped check;
* package vignettes rebuilt: passed; and
* tarball inventory: 250 entries; agent instructions and repository metadata
  excluded.

## Downstream dependencies

CRAN reports no reverse dependencies for `prepR4pcm`.
