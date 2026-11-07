## 1. Goal

Implement WCVP synonym review matching and optional TNRS integration, test the original data and tree, and prepare an unsent reply to Erick.

## 2. Implemented

`reconcile_wcvp()` resolves unmatched input names and target labels against one frozen names table. Terminal accepted ID and accepted rank must agree. Multiple possible identities, broken links, unplaced names and multiple target labels remain audit-only. Explicit Kew redirects for invalid, illegitimate and orthographic labels remain reviewable with their status shown. Neither the mapping nor apply behavior changes.

`reconcile_tree()` now accepts `wcvp`, `tnrs` and `tnrs_source`. Local enrichment and network requests are separately optional. `reconcile_tnrs()` can use the frozen reference to bridge accepted names to synonym-labelled tips, while recording service and local releases separately. Default behavior remains unchanged.

Current WCVP support is exposed in candidate columns. Repeating a local lookup supersedes unsupported pending candidates originating from the earlier WCVP lookup, retains history, and preserves explicit reviewer decisions.

## 3a. Decisions and Rejected Alternatives

Kept suggestions separate from mappings. Rejected automatic spelling or service-based acceptance, direct comparison of local/service IDs, collapsing varieties into species, silent network failure and changing existing COL assignments. The full-size TNRS integration test uses explicitly simulated responses; the live-client smoke is separate. No full public-service campaign was launched.

## 4. Files Touched

R/reconcile_wcvp.R, R/reconcile_tnrs.R, R/reconcile_tree.R; NAMESPACE; their three manual pages; two new test files and updated TNRS performance tests; the Latin matching vignette and pkgdown reference index; this development-log directory. The canonical checkout's unrelated edits were preserved. The pkgdown reference index now includes both botanical adapters. No pigauto source was changed.

## 5. Checks Run

Focused adapter and integration tests passed after regression tests first demonstrated missing behavior. The final development suite recorded 3,386 passes, zero failures/errors, 26 warnings and 42 skips. The installed archive check recorded 3,046 passes, zero failures, 24 warnings and 92 skips; optional/environment checks account for the different coverage. The archive SHA-256 is 8934b8583b0590af80e78ff1206ef4571afd6541a51cedea0d063874c717f425, and all 224 R, manual, test and namespace files match the working source byte-for-byte. The final local adapter produces 174 exact accepted-ID/rank input-tip pairs; 169 pairs were absent from the earlier 7,508-pair table, producing 7,677 total candidate rows. Eight local targets are already occupied. An independent Python implementation scanned all 1,448,984 raw Kew records and found no missing, extra, duplicated, status, ID or rank discrepancies. The injected distinct-taxon negative control was rejected. The final full-size simulated TNRS integration used all 185,229 unresolved input names and returned the same number of response rows, completing in 29.288 seconds with the ordinary cascade reused; all 190,263 inputs and 117,934 tips remain covered and mappings unchanged. The final actual one-call pipeline, including recomputing the ordinary cascade, completed in 266.035 seconds with the same mappings and candidate set; only its local comparison result reused the saved baseline. Full-workload evidence is recorded in the accompanying JSON receipts and gate ledger. The five-name live R-client request succeeded; a separate one-call integrated smoke queried four unresolved names and produced the expected Trema and Glandularia/Verbena candidates with unchanged mapping.

The live service reported WCVP 12, whereas the local Kew table is v16. The service and local ID namespaces and releases remain separate.

## 6. Tests of the Tests

Observed failures before adding the local adapter and optional arguments. Independent review reproduced a stale-snapshot candidate and arrival-order evidence problem; new tests then failed before the repair. Changed-reference, same-label/different-fingerprint, explicit rejection, rank mismatch, distinct accepted species, missing links, cycles, empty tables and occupied targets are covered. The new nomenclatural redirect test failed before support was added. An independent Python verifier checks raw Kew records and deliberately rejects an injected wrong pair.

## 7a. Issue Ledger

Addressed synonym-labelled targets and optional one-call TNRS use. The first real-data run found 12 explicit nomenclatural redirects excluded by a status filter; the corrected implementation retains these as review-only evidence. The independent source review found two provenance problems, both corrected and rechecked. Existing COL/WCVP mapping disagreements are not adjudicated by this feature.

## 8. Consistency Audit

Checked default API compatibility, manual versus integrated calls, namespace export, generated help, vignette usage, unchanged mapping/apply behavior, review state, source fingerprints and remote input hashes. A read-only source review rechecked the stale-reference and evidence-order repairs. Reconciliation of plan and actual: the actual work added explicit nomenclatural redirects and snapshot invalidation because real data and review demonstrated concrete gaps; both remain within review-only scope.

## 9. What Did Not Go Smoothly

The archived worktree was older than the published recovery branch; resumed from af36895. A documentation consistency test caught a package-qualified rWCVPdata example without a declared dependency; changed it to describe the supplied table, since runtime rWCVPdata is not required. A test-results CSV export failed on a list column after tests completed; subsequent evidence saves the test object and prints numeric counts directly. The first full-size simulated integration was stopped after local output exposed the overly narrow nomenclatural filter. The independent verifier then found 79 accepted hybrid-rank records misclassified as unresolved (73 nothosubsp. and six nothovar.); the accepted-rank list and tests were corrected. Full-size response processing also exposed repeated named-list scans and large audit-column mutations; numeric vector indexes and separate audit vectors replaced them. Two owned slow processes were stopped and the final source rerun in fresh output directories. The closeout helper resolved a relative output against the brain root; moved its newly created report into this worktree and replaced the unrelated generated file list. Its wrapper also checks unrelated brain-root ledgers, so this report is validated directly with the structural checker in the package worktree.

## 10. Known Residuals

This is a prototype on a development branch. No merge, CRAN preparation or public reply is part of this task. The tested development branch is prepared for collaborator installation; the reply remains private until approved. A full live TNRS call over all unresolved names has not been run. The whole-package check reports two vignette execution failures outside the changed matching code: unavailable fishtree and a meta-analysis example species/correlation-name assertion. Missing built vignette outputs and existing packaging notes are also reported. These are not represented as a clean package check.

## 11. Team Learning

Memory receipt: recalled MEMORY.md 623-626 and the existing WCVP audit before implementation; canonical contract, efficiency procedure, ultra-plan, unlazy, compute routing and verification instructions shaped scope and evidence. route.py found no LOAD-FIRST manifest. Existing graph cards identified the adapter and candidate paths. Prior audit and cached COL lookups were reused. No memory files were updated.

Golden Set: no new cross-project guard was introduced; relevant previous mistakes were checked through defect-first regression tests, exact-source hashes and separate live/simulated evidence. The general memory regression suite was not run. No memory files were updated.

## 12. Cross-Product Coverage

Covers: prepR4pcm local botanical review, optional TNRS integration, the original 190,263-name and 117,934-tip workload, focused regression tests and a small live R-client smoke.

This does NOT cover: Erick's later final inventory of roughly 300,000 names, biological validation of every taxon, automatic acceptance, repair of existing cross-backbone mappings, a full live TNRS service campaign, pigauto implementation, a clean CRAN check, CRAN submission or a posted reply.
