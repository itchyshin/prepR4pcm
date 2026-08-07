## Submission

This is a patch update from the CRAN release `prepR4pcm` 1.0.0. A 1.0.1
upload made in July 2026 was not published; version 1.0.2 supersedes that
upload.

This update:

* fixes `reconcile_augment(source = "rtrees")` so family-level grafts marked
  with `**` are counted as augmented rather than skipped;
* retains exact, genus-level, and family-level placement information while
  preserving the existing result columns;
* adds regression tests for exact, `*`, `**`, skipped, and multi-tree cases;
* adds a safer post-baseline taxonomy-crosswalk workflow and clearer
  split/lump safeguards; and
* updates installation guidance now that `rtrees` and `clootl` are available
  from CRAN.

## R CMD check results

The exact source tarball was checked with all declared Suggests available:

    0 errors | 0 warnings | 0 notes

Additional checks:

* full `testthat` suite — 0 failures;
* `urlchecker::url_check()` — all 45 URLs correct;
* `pkgdown::check_pkgdown()` — no problems found;
* clean temporary-library install and load — passed; and
* tarball inventory and forbidden-path scan — passed.

## Test environments

* local macOS Tahoe 26.6 (aarch64), R-devel 4.6.0 —
  `R CMD check --as-cran --run-donttest` with Suggested packages forced.

## Downstream dependencies

CRAN reports no reverse dependencies for `prepR4pcm`.
