# Issue 137: candidate validation

## 1. Goal

Repair the reported reconciliation stall after COL lookup and test the supplied workload before handover. CRAN remains ON HOLD until Erick's feedback has been reviewed.

## 2. Implemented

Replace repeated synonym scans with indexed matching and apply synonym/fuzzy candidates in batches. Fuzzy matching processes one query genus at a time instead of allocating the dense genus distance matrix. Large fuzzy stages show a progress bar and estimated time remaining; quiet suppresses progress. Existing match priorities, scores, tie order and audit fields are retained.

## 3a. Decisions and Rejected Alternatives

Retain the original genus distance rule and scoring rather than narrowing candidate genera for speed. Compare complete predecessor outputs, including notes, instead of relying on match counts. Use the real COL database for the complete workflow; simulated authority results only isolate matching performance.

Assumptions made without asking: the supplied files may be used privately on the authorised Totoro test host, but may not be redistributed publicly. No data files are included in the package or commit.

## 4. Files Touched

R/pr_synonyms.R, R/pr_cascade.R, R/pr_fuzzy.R, man/pr_fuzzy_match.Rd, NEWS.md, both issue-137 test files, predecessor fixtures, and this report. Private data, profiles and detailed delivery receipts remain in ignored task storage.

## 5. Checks Run

The complete supplied workflow, with authority="col" and fuzzy=TRUE, passed in 1076.869 seconds (17 minutes 57 seconds): 190263 data names and 117934 tree tips, Ubuntu 24.04.5, R 4.6.1, taxadb 0.3.0, DuckDB 1.5.6, COL snapshot 2026. Database threads and CPU affinity were capped at four. Both authority lookups returned real accepted-name records, with no failed-batch warnings. Complete input coverage and no repeated assignment on either side passed. Output contains 2897 normalised, 2281 synonym, 902 fuzzy and 4006 flagged pairs, plus 288025 unresolved rows. Flagged pairs require review.

The complete workload without authority lookup passed in 104.8206 seconds. All 300370 mapping rows are exactly identical to the frozen predecessor, including audit notes and attributes. The predecessor completed in 172.6478 seconds. These are observed shared-server timings, not a controlled performance benchmark.

Eight bounded comparisons using the actual saved COL results passed: five random subsets and three connected direct, reverse and shared-accepted-name cases. They covered 1485 candidate pairs, with complete predecessor equality.

Before the final note-format correction, the source suite passed 2964 assertions with zero failures, 24 ultrametric warnings and 43 skips in 645.5 seconds. After correction, the fuzzy regression file passed 17 assertions and independent review passed. The final installed R CMD check ran the complete applicable suite: 2681 passes, zero failures, 24 ultrametric warnings and 87 skips (77 On CRAN guards, six missing rgnparser, three missing taxadb, one existing empty test). Live COL was tested separately on Linux as described above.

Final-artifact R CMD check --ignore-vignettes --no-manual: zero errors, zero warnings, three notes (the existing licence stub, .zenodo.json and unavailable optional help cross-references). This explicitly excludes vignettes and is not a CRAN release check. A separate check that ran vignette code failed on live Open Tree API access in two vignettes; no clean online-vignette claim is made. The pkgdown checker reported no problems, with Pandoc deprecation warnings. git diff --check passed.

Candidate package SHA-256: 3525b9d958214d0461b6334b87fd89b4dd99afcd7dd6bed09e8c856ad8878bfc.
Input SHA-256: species.Rds 407b0c75ee1e0e37f62f05937a05bfafce8f735289a03a6a35cfb00fe76c959e; wcvp_tree.tre bd32213488711351cad96f3f199fba2c86ec11b9629295bc1d61fe0dd75fe098.

Reproduction with the privately supplied files:

```r
search <- readRDS("species.Rds")
phylo <- ape::read.tree("wcvp_tree.tre")
rec <- prepR4pcm::reconcile_tree(search, phylo,
  x_species = "search_name", authority = "col", fuzzy = TRUE)
```

## 6. Tests of the Tests

The predecessor failed bounded quadratic-scan and candidate-application regressions. The fuzzy allocation guard rejects Cartesian multi-query/multi-reference distance matrices and failed against the predecessor. Fifty random synonym cases and ten cascade cases compare complete predecessor outputs. The reviewer independently checked 3844 short-string pairs for prefilter completeness.

The full-data comparison caught 73 note-format differences that the initial small fixtures missed. Tibble evaluation had masked the raw score with the rounded output column before formatting notes. A two-name regression failed before correction and passed after renaming the raw variable; six threshold-boundary comparisons also passed. Match selection and numeric output scores were unchanged. The final complete no-authority mapping comparison passed exactly.

## 7a. Issue Ledger

This candidate addresses [issue 137](https://github.com/itchyshin/prepR4pcm/issues/137). Closure is gated on merged-main installed testing and CI readback. Delivery receipts are retained in ignored task storage and reported in the task's final response. CRAN preparation and submission remain on hold.

## 8. Consistency Audit

The shared cascade serves data, tree, multi-data and multi-tree entry points. Existing matching and resolve-mode tests cover neighbouring callers. Internal help documents quiet; NEWS describes bounded memory and retained matching rules. An exhaustive graph search found no other production score-rounding constructor of the same form; the other two hits are frozen predecessor fixtures. The graph was refreshed after the changes. The original dirty checkout is untouched.

Memory receipt: ask-brain used a keyword file fallback after the local brain MCP was unavailable and the CLI configuration write was denied. route.py returned no LOAD-FIRST manifest. Recall preserved the CRAN hold and the distinction between wrapper tests and complete-workflow tests. Golden Set: NOT RUN; no applicable case was retrieved by the lightweight recall pass. Direct reproduction and predecessor fixtures provide the task-specific evidence.

## 9. What Did Not Go Smoothly

The files required the snakagaw browser account. An old shared Totoro vctrs binary failed under R 4.6.1; dependencies were rebuilt in an isolated task library. The first full-data equality check caught the note-format defect described above. The closeout template generator selected the brain checkout for its inventory; that unrelated inventory was replaced with the actual task files.

## 10. Known Residuals

Fuzzy genus scanning still performs quadratic genus comparison work, although it no longer allocates the dense matrix. Scientific assignment quality is unchanged and is not established by performance tests. Existing packaging notes and live-vignette access errors remain separate from this fix. No CRAN readiness claim is made.

## 11. Team Learning

A wrapper/progress test does not validate the complete reconciliation pipeline. Keep real-data tests, predecessor parity and algorithmic regressions distinct. Compare audit text as well as numeric fields. Avoid reusing a tibble column name for a raw value needed by a later column expression. An allocation improvement alone does not establish a speedup.

## 12. Cross-Product Coverage

Covers synonym priority, greedy assignment, fuzzy ordering/scoring, quiet behaviour, progress/ETA, and the complete supplied reconcile_tree workflow with and without live COL. It does NOT cover a full real-data run on Windows or macOS, other live authority providers, missing optional backends, online vignette availability, or CRAN acceptance. Neighbouring entry points have unit-level coverage; their complete real-data workflows were not run here.
