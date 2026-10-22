# Gates: optional botanical TNRS review adapter

Scope: implementation, validation, and publishing branch `codex/latin-review-recovery` for Erick to install and test. External issue communication, PR/merge, CRAN work and automatic spelling acceptance are outside this scope.

- [x] G0: Exact implementation scope and checkout boundary are recorded; no unrelated checkout is changed.
  EVIDENCE: Manual review of this worktree's status confirmed only intended files changed. Preflight found seven other worktrees; the shared lease record could not be written because of filesystem permissions. This work stayed in the existing isolated worktree, and no other checkout was edited.

- [x] G1: Ordinary reconciliation has no hard TNRS dependency, and the adapter gives a clear optional-package error before a service call.
  CHECK: OPENBLAS_NUM_THREADS=1 Rscript -e 'devtools::test(filter = "reconcile_tnrs"); cat("TNRS_FOCUSED_PASSED\n")'
  EXPECT: TNRS_FOCUSED_PASSED
  CWD: /private/tmp/prepr4pcm-latin-review-recovery
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=9b3b586a2d69/27 entries; output=ℹ Testing prepR4pcm | ! Dropped 2 rows with unresolved species from data

- [x] G2: The adapter requires one supported source, requests all matches, preserves caller input, and handles partial or invalid service results safely.
  EVIDENCE: Focused test suite in G1 covers invalid sources, all-match parameters, duplicates, blank and missing names, service errors, partial results, and malformed responses.

- [x] G3: Suggestions match only a unique normalized accepted name in the actual target inventory; source, score, warnings and version evidence are retained.
  EVIDENCE: Focused test suite in G1 covers accepted names present in tips, absent and ambiguous target names, author citations, alternative rows, occupied tips, and missing metadata.

- [x] G4: TNRS suggestions remain review-only and cannot alter mapping or be applied via `include_flagged`.
  EVIDENCE: Focused test suite in G1 confirms unchanged mapping, ineligible unknown-identity candidates, and no application through `include_flagged`.

- [x] G5: Public WCVP examples record observed service behavior and exercise accepted botanical author strings.
  EVIDENCE: Manual review of `live-web-examples.json` and `tests/testthat/fixtures/tnrs-wcvp-public-examples.csv`. Nine public names were submitted through the browser UI with WCVP selected. The R client could not connect in this shell; no private names were submitted.

- [x] G6: Package regression tests pass and the new Latin matching vignette validates; service resolution is described as review evidence rather than taxonomic proof.
  CHECK: OPENBLAS_NUM_THREADS=1 R_LIBS=/private/tmp/prepr4pcm-tnrs-lib Rscript -e 'devtools::test(); cat("PREPR4PCM_FULL_TESTS_PASSED\n")'
  EXPECT: PREPR4PCM_FULL_TESTS_PASSED
  CWD: /private/tmp/prepr4pcm-latin-review-recovery
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=9b3b586a2d69/27 entries; output=v Merged 100 species (left join) | v Override applied: 'E f' -> 'G_h' (accept)

- [x] G7: Package check outcome is scoped, and the new Latin matching vignette runs.
  EVIDENCE: Manual review of `/private/tmp/prepr4pcm-tnrs-check-retry.log` shows `latin-aware-matching.Rmd` passed. Two errors came from existing OpenTree network calls in `pr_get_tree_status(check_network = TRUE)`, `comparing-tree-backends.Rmd`, and `meta-analysis-with-rotl.Rmd`; four existing notes are listed in `AFTER-TASK.md`.

Result: focused adapter tests pass (51 assertions); full suite passes with 3,247 passed, 0 failed, 24 warnings, and 47 skips. The warning and skip counts predate this adapter. The live TNRS browser lookup and its evidence limits are in `live-web-examples.json`; the package client could not reach the service from this shell, so direct live API behavior remains unverified. The frozen public response exercises author citation normalization and candidate alignment.

Package check: completed in 4m 12s. The package check found two network-dependent failures in existing `pr_get_tree_status(check_network = TRUE)` examples and the existing OpenTree-backed vignettes `comparing-tree-backends.Rmd` and `meta-analysis-with-rotl.Rmd`. The new `latin-aware-matching.Rmd` vignette passed. Check also reported four notes: hidden `.zenodo.json`, system clock verification unavailable, the existing license stub, and optional `rgnparser`/`caper` Rd cross-references. The prototype is being prepared on a test branch; no issue reply, PR, merge, or CRAN work has been done.
