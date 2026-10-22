## 1. Goal

Add an opt-in botanical TNRS adapter for unresolved Latin names. Keep its output review-only and add evidence-backed examples before drafting any issue response.

## 2. Implemented

Added `reconcile_tnrs()` with explicit WCVP, WFO, or Cactaceae source selection. It queries all available matches without client-side score filtering, records raw results and metadata, and offers only exact normalized accepted-name matches to a unique target tip as pending review candidates. Suggestions do not alter the mapping and cannot be applied through `include_flagged`.

Added optional dependency metadata, generated help, vignette instructions, 51 focused assertions, a frozen public WCVP response fixture, and a local browser-result receipt. The public-name examples recovered both Trema accepted-name variants and the Bignonia ternata synonym to Adenocalymma ternatum; they did not support the proposed distinct-tip alternatives.

## 3a. Decisions and Rejected Alternatives

- Use the CRAN `TNRS` client as an optional, explicit adapter; ordinary reconciliation does not load it.
- Request all service matches with `accuracy = 0`, preserving alternatives and leaving review decisions to the user.
- Compare accepted names to actual target tips after an adapter-specific removal of botanical author citations. Keep the package's general name normalizer unchanged.
- Do not accept suggestions automatically, merge sources, or use name similarity alone to crosswalk distinct taxa.
- Assumptions made without asking: WCVP is the default source because the user selected an optional botanical adapter and WCVP was the live example source. A different default can be selected later without changing the explicit-source interface.

## 4. Files Touched

- `DESCRIPTION`
- `NAMESPACE`
- `R/reconcile_tnrs.R`
- `man/reconcile_tnrs.Rd`
- `tests/testthat/test-claim-suggests-remotes-consistency.R`
- `tests/testthat/test-reconcile_tnrs.R`
- `tests/testthat/fixtures/tnrs-wcvp-public-examples.csv`
- `vignettes/latin-aware-matching.Rmd`
- `docs/dev-log/latin-review-recovery/TNRS-IMPLEMENTATION/GOAL.md`
- `docs/dev-log/latin-review-recovery/TNRS-IMPLEMENTATION/GATES.md`
- `docs/dev-log/latin-review-recovery/TNRS-IMPLEMENTATION/live-examples.R`
- `docs/dev-log/latin-review-recovery/TNRS-IMPLEMENTATION/live-examples.json`
- `docs/dev-log/latin-review-recovery/TNRS-IMPLEMENTATION/live-web-examples.json`
- `docs/dev-log/latin-review-recovery/TNRS-IMPLEMENTATION/ISSUE-139-REPLY-DRAFT.md`
- `docs/dev-log/latin-review-recovery/TNRS-IMPLEMENTATION/AFTER-TASK.md`

## 5. Checks Run

- Focused TNRS tests: 51 assertions passed.
- Full test suite: 3,247 passed, 0 failed, 24 warnings, 47 skips. The warnings and skips are existing suite cases; the focused adapter tests had no warnings or skips.
- `git diff --check`: passed.
- Live R-client query: unavailable because the client could not establish its internet-access check in this shell. The script records this as unavailable rather than passing it.
- Browser WCVP lookup: completed for nine public names. The saved receipt notes the UI's 0.53 threshold, missing source-release metadata, and that it is not the raw R-client response.
- R CMD check completed in 4m 12s with 2 errors, 0 warnings, and 4 notes. Both errors came from existing OpenTree network calls in `pr_get_tree_status(check_network = TRUE)` and the `comparing-tree-backends.Rmd` and `meta-analysis-with-rotl.Rmd` vignettes. The new `latin-aware-matching.Rmd` vignette passed. Notes were the existing hidden `.zenodo.json`, unavailable system-clock verification, license stub, and optional `rgnparser`/`caper` Rd cross-references.
- The corrected unlazy ledger was approved and reverified. All 8 gates are met, including a fresh focused and full suite run. The initial checker attempt exposed invalid shell quoting in the ledger; that was fixed before its successful re-verification.
- The after-task structure validator passed. Its ledger recheck was skipped because the unlazy checker had already reverified the acceptance gates in this turn.

## 6. Tests of the Tests

The focused tests exercise invalid sources, missing optional package behavior, service errors, malformed and partial responses, absent and ambiguous target names, author citation normalization, occupied tips, duplicate inputs, raw-result preservation, and application with flagged rows. They use a frozen service-shaped response and mocked client boundary. No deliberate code mutation was run. The real browser fixture confirms the service-shaped author strings and example results, but does not validate the package client's live HTTP path.

## 7a. Issue Ledger

- Issue #139: local work was continued after the user directed us to address Erick's comment. A cautious reply draft is saved locally; it has not been posted.
- Adapter bug found and fixed: accepted botanical names with parenthetical author citations did not match canonical target tips under the general normalizer.
- Live R-client access limitation recorded; direct API behavior remains unverified in this environment.
- A public test branch is being prepared so Erick can install the prototype. No issue reply, PR, merge, or CRAN work has been done.

## 8. Consistency Audit

Reviewed the reconciliation object, candidate review, and application boundary. The adapter enriches candidates but leaves mapping rows intact; candidate identity remains unknown and ineligible; `include_flagged` does not apply TNRS candidates. Target names absent from the tree and normalized names shared by multiple tips remain audit-only. The optional package is referenced only when a query is needed.

## 9. What Did Not Go Smoothly

The R client could not connect from the command-line environment, although the public TNRS browser succeeded. The browser's threshold and response view differ from the adapter's requested all-matches API call, so the receipt is supporting evidence rather than a direct API integration test. The full package check also exercised unrelated existing OpenTree examples and vignettes that require network access and failed in this offline environment.

## 10. Known Residuals

- R CMD check is not clean in this offline environment because unrelated existing OpenTree examples and vignettes require network access.
- A live query through the R client remains unverified.
- The browser receipt does not expose the source release version or every raw response field.
- TNRS service resolution is evidence for review, not proof of taxonomic correctness.
- No comprehensive service load, provider comparison, CRAN readiness check, or release work was performed.
- The `graft` command is not available in this worktree environment, so the repository graph was not rebuilt.

## 11. Team Learning

Botanical services may append authority strings to accepted names; adapter comparisons should normalize that evidence locally without changing general-purpose name normalization. Keep service evidence separate from taxonomic identity and from automatic mapping. The repo's project dossier was consulted; the route helper reported that no LOAD-FIRST manifest exists. Preflight found seven other worktrees, and the shared lane lease could not write its coordination record because of filesystem permissions. Work remained in this isolated checkout. The selected-user boundary against automatic acceptance was preserved. The Golden Set was not run because this was a new provider adapter rather than a known regression class.

## 12. Cross-Product Coverage

- TNRS adapter sources: WCVP covered in browser examples and frozen response tests; WFO and Cactaceae are accepted by the interface but live behavior does NOT cover those sources.
- Target matching: exact unique target-tip lookup, absent target, ambiguous target, authority citations, occupied tips, and review-only application are covered. General non-botanical name providers and all taxonomic ranks do NOT cover in this test slice.
- Release surfaces: local unit tests and package documentation are covered. CRAN checks, publication, external issue communication, and other worktrees do NOT cover in this authorization.
