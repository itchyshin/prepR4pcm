# Verification contract

These are requirements for future verification code, not implemented validators. Read and approve the finished scripts before executing the ledger. Missing files, skipped mandatory cases or incomplete receipts fail closed.

## Test runner

`tools/verify-latin-review-tests.R` loads the exact candidate checkout and runs the named test files for recovery/apply, or the full package suite for regression. It reads SCENARIOS.json and verifies each applicable ID occurs in the executed test results, has at least one actual expectation, and has zero skips, failures or errors. IDs in source text alone are insufficient. The regression group includes every mandatory ID. Report all non-core skips and warnings explicitly; do not label them passed.

Capture source revision, working-diff hash, loaded package path, R/package versions, test names, expectation counts, skip/failure/error counts and elapsed time. Test both interactive review via controlled input and the public non-interactive candidate/summary paths. A non-interactive early return from reconcile_review cannot satisfy the interactive scenario.

Print the ledger's success-only token after these checks. Verify the verifier with three deliberately bad inputs: a missing required scenario, a skipped required scenario, and a failing expectation. Each must return nonzero without a success token. Record this evidence for G7. Also demonstrate that the pre-repair version fails the ordinary-review Trema fixture and that an accidental-apply mutation fails the application fixture.

## Replay validator

`tools/validate-latin-review-replay.R` validates receipts and recomputes aggregates from referenced raw outputs. It cannot accept manually entered totals as proof. Required receipt fields:

- `schema_version`, phase, generated-at timestamp, command, platform, code revision and source/diff hashes;
- query/reference/tree input hashes, row counts, unique-name counts, duplicate/missing counts, normalization and matching settings;
- authority provider, snapshot, lookup-response hashes, source statuses, accepted-ID chains and per-name identity classifications;
- exact baseline revision or `baseline_kind = surrogate`, with a stated limitation; candidate revision and shared-input comparability checks;
- stable original row/name keys and pair keys; per-stage flags for generated, blocked, retained, visible, reviewed and applied;
- complete old/new mappings and candidate tables, not only a changed-row extract;
- independent labelled positive/negative fixtures with label provenance, expected disposition and unresolved/unadjudicated labels kept explicit;
- runtime and peak-memory observations, unavailable values declared, and execution log locations.

For diagnosis, the validator reconciles every input row to a disposition and verifies pair-stage totals against raw files. Every reported disappearance must be accounted for as no candidate, filtered candidate, separate review candidate, earlier-stage resolution or other explicit reason. A surrogate baseline permits a demonstrated-mechanism conclusion only; it cannot verify the reported 3,944-to-1 comparison.

For replay, recompute old/new pair-key sets. Their union must equal the disjoint added, removed and shared sets; shared pairs divide into unchanged and reclassified. Compare row multiplicities separately so deduplication cannot hide losses. Compare automatic-output and review-only sets separately. All automatic-output differences require an explicit explanation and approved policy scope; review-only recovery cannot silently justify an automatic difference. Verify all labelled positive recoveries and zero automatically applied labelled conflicts. Unknown labels remain outside any correctness numerator and are reported in the denominator accounting.

For identity, trace each of the five original #139 conflicts and the Bignonia example to frozen source records. The Bignonia outcome may remain uncertain, but an absent audit or an unsupported confident taxonomic conclusion fails. Validate separate positive cross-genus-synonym and negative ambiguous/conflicting-record controls. A current candidate being flagged is not evidence of an automatic-application failure.

Malformed receipts, changed input hashes, absent raw outputs, duplicate keys without multiplicity accounting, incomplete ID chains or unexplained changed automatic results must return nonzero. A reviewer independently spot-checks the raw records and disputed labels. This validator does not establish taxonomic truth by checking that a JSON field says valid.

## Policy boundaries

No test redefines an unresolved candidate as an accepted taxon merely to recover a count. G2 requires public review visibility; G3 measures application separately. Any proposed change to bulk `include_flagged` semantics or automatic identity classification must be documented and approved before implementation. Do not add a new class of automatic match in this arc.
