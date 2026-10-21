## 1. Goal

Restore useful spelling suggestions in ordinary Latin-name review while preserving automatic identity safeguards. The active user goal approved Phase A execution after the initial planning packet. The later TNRS choice and paper review remain a follow-up design.

## 2. Implemented

Pending candidates now appear in ordinary flagged/fuzzy review, printed counts and the summary table. Plausible spellings to occupied targets are retained with a warning and cannot silently replace the existing assignment. An existing genus-distance filter bounds authority lookups without repeating full species scoring. Review-only pairs stay separate from mapping rows.

Legacy tables missing review fields are initialised before individual overrides, preserving untouched alternatives. New authority lookups record their source version. Evidence labels distinguish unknown, ambiguous, incomparable and conflicting identity. The TNRS design note includes the supplied plant-tool comparison and a separate acceptance checklist.

## 3a. Decisions and Rejected Alternatives

- Preserve automatic eligibility and default application. Explicit review acceptance remains a traceable manual override.
- Use the saved authority-enabled baseline as a limited surrogate: 4,006 flagged rows, compared with the reporter's 3,944. Exact reporter reproduction and the surrogate's exact producing source revision remain unproven.
- Keep raw workload results on Totoro. Automatic approval rejected a proposed full RDS download; in-place checks and aggregate receipts completed verification safely.
- Defer botanical TNRS implementation, automatic spelling-only acceptance, public posting and CRAN preparation.
- Assumptions made without asking: retain the existing one-to-one target rule, so an occupied target must be resolved before another review acceptance. A request for deliberate many-to-one merging would require a separate change.

## 4. Files Touched

- `R/pr_cascade.R`
- `R/pr_fuzzy.R`
- `R/pr_identity.R`
- `R/reconcile_candidates.R`
- `R/reconcile_review.R`
- `R/reconcile_summary.R`
- `R/reconciliation-class.R`
- `docs/dev-log/latin-review-recovery/AFTER-TASK.assessment.json`
- `docs/dev-log/latin-review-recovery/AFTER-TASK.md`
- `docs/dev-log/latin-review-recovery/CHECK-CONTRACT.md`
- `docs/dev-log/latin-review-recovery/DIAGNOSIS.assessment.json`
- `docs/dev-log/latin-review-recovery/DIAGNOSIS.md`
- `docs/dev-log/latin-review-recovery/GATES.md`
- `docs/dev-log/latin-review-recovery/LOOP/checkpoint.md`
- `docs/dev-log/latin-review-recovery/LOOP/ultra-plan.md`
- `docs/dev-log/latin-review-recovery/PLAN-ACTUAL.assessment.json`
- `docs/dev-log/latin-review-recovery/PLAN-ACTUAL.md`
- `docs/dev-log/latin-review-recovery/README.assessment.json`
- `docs/dev-log/latin-review-recovery/README.md`
- `docs/dev-log/latin-review-recovery/REVIEW.assessment.json`
- `docs/dev-log/latin-review-recovery/REVIEW.md`
- `docs/dev-log/latin-review-recovery/SCENARIOS.json`
- `docs/dev-log/latin-review-recovery/TNRS-DESIGN.assessment.json`
- `docs/dev-log/latin-review-recovery/TNRS-DESIGN.md`
- `docs/dev-log/latin-review-recovery/VERIFICATION-REMOTE.json`
- `docs/dev-log/latin-review-recovery/VERIFICATION.json`
- `docs/dev-log/latin-review-recovery/replay.R`
- `man/pr_fuzzy_match.Rd`
- `man/reconcile_review.Rd`
- `tests/testthat/test-issue-139-review-recovery.R`
- `tools/validate-latin-review-replay.R`
- `tools/verify-latin-review-tests.R`
- `vignettes/latin-aware-matching.Rmd`

Ignored runtime evidence lives under `.unlazy/latin-review-recovery/`. Frozen source/build logs live in `/private/tmp/prepr4pcm-latin-review-build/`. Raw replay objects and isolated libraries live under `/home/snakagaw/prepr137/review-recovery-20261021` on Totoro. These runtime paths are recorded rather than committed as data.

## 5. Checks Run

- `devtools::document()` completed; generated review and internal matcher documentation agree with source.
- Unlazy re-ran the exact local recovery, apply and regression oracles from the isolated checkout. Recovery/apply each executed the whole core file: 71 assertions, no failures, errors, warnings or skips. The regression suite passed 3,195 assertions with zero failures/errors, 47 skips and 24 warnings outside the core scenarios. Receipts share source digest `b3977d899f85059bc4c4545b92b3db245cacdbbf09562910b4f957fa4d8fcd9e`.
- The exact installed archive passed the nine-name smoke test in 0.974 seconds. The full Totoro reconciliation of 190,263 names and 117,934 tree tips passed in 281.63 seconds with four database threads and a six-minute cap.
- Mapping is identical to saved main b055018. All 6,391 prior candidate pairs remain; 1,117 pairs were added, all with unresolved inputs and occupied targets. Identity labels for additions are 760 same, 232 conflict and 125 unknown. Both Trema examples are visible in ordinary review. Only the named examples and fixtures have labelled expectations; the full added set is unadjudicated.
- In-place raw validators passed diagnosis, identity and replay accounting. They rehashed the source archive, result objects and original input/tree files. All five original distinct-ID pairs are present as conflicts and excluded from automatic mapping. Bignonia's cached COL accepted-ID chain is recorded without biological adjudication.
- Source archive SHA-256: `74ada79de8fb95b1abb96006ed7ce20317f776316c22a52abda41df81e114c84`. All 225 archive R/test/tool/manual/vignette files were compared byte-for-byte with the final checkout.
- Ordinary offline `R CMD check --no-manual --ignore-vignettes --no-tests` on that archive completed with zero errors, zero warnings and three notes. Tests were run separately. Notes concern the existing .zenodo.json, licence stub and optional-package cross-references. The broader vignette run failed on unavailable OpenTree API calls; pkgdown checking failed because Pandoc is unavailable. These check results support the stated development scope; release checks remain deferred.
- The after-task structure validator and closeout compiler passed.
- `git diff --check` passed. Targeted assignment/object-use lint was run and its findings retained; namespace visibility and cli-expression false positives plus existing style findings mean this is not a lint-clean claim. The genus helper preserves the original predicate and source style. Graft was refreshed deterministically after the changes.

## 6. Tests of the Tests

Initial visibility tests failed against the original review path. An occupied-target test demonstrated the missing-generation mechanism. Independent-review additions reproduced legacy-alternative loss and incomplete source metadata before their fixes, then passed.

The test-result verifier rejects missing, skipped and failing mandatory scenarios, with a positive control. A runtime fault injection promoted review candidates into flagged mappings and forced permissive application; actual tests failed LR-A01, LR-A02 and LR-A05. Unmodified controls passed before and after. This exercises the exported apply boundary and production application code; it is not a mutation of tracked source.

The remote replay validator rejects an altered result hash, a missing archive and a no-recovery comparison, after validating the real positive evidence. Negative-control artifacts stay in ignored runtime directories.

## 7a. Issue Ledger

- #139 follow-up: hidden Trema review access and occupied-target generation repaired locally.
- Legacy candidate review and source-provenance defects found by independent review: repaired with failing-then-passing tests.
- Vacuous replay and absent-control oracle gaps: repaired before the final source was frozen.
- Optional botanical TNRS adapter: design/checklist prepared, implementation deferred.
- Exact 3,944-row historical reconstruction, full taxonomic adjudication, release packaging notes, network vignettes and Pandoc-dependent site checking remain outside the proven outcome. No GitHub state was changed.

## 8. Consistency Audit

The neighbourhood sweep covered candidate storage, ordinary and explicit identity review, summaries, printing, override provenance, application defaults, existing include_flagged behavior, target uniqueness and old objects. Source discovery used the repo graph before precise reads; callers had an ambiguous fixture symbol and were resolved by path. Rank/provider/snapshot identity rules and valid free-target cross-genus matches remain covered by fixtures and regression tests.

The final full workload establishes mapping identity and complete candidate-difference accounting. The remote field `automatic_outputs_identical` denotes mapping identity. Full data/tree application was tested with fixtures, not rerun on the whole workload. Graft reported a tool-estimated 446,468 tokens saved across the recorded map/caller queries; that is an estimate against whole-file reading, not measured billing savings.

## 9. What Did Not Go Smoothly

The native managed worktree was outside writable roots, so execution used an isolated permitted Git worktree. The shared dirty checkout was preserved. Shell login behaviour changed directories during one helper call, and the closeout template listed the vault's files; the task inventory was rebuilt directly from this checkout.

A species-score prepass passed small tests but reached the full replay's 300-second cap. It was stopped and replaced with a genus pool; the final replay completed within its revised estimate. Reviewer and validator schema assumptions also needed correction: an authority-free prompt concern was disproved, old baseline candidates can be NULL, and raw authority accepted concepts use taxon_id. The final verifier checks the actual schema and rejects missing identity-state columns.

## 10. Known Residuals

The verified local repair remains unpublished. CRAN preparation remains on hold. The branch and isolated worktree are retained for review. General taxonomic correctness of the 1,117 additions is unknown. Even comparable provider IDs do not prove the biological identification of a record or agreement with another tree taxonomy. The 760 same-ID occupied pairs remain review-only because the target is already assigned.

The final replay takes longer than the saved 164.8-second main run because of the additional review-discovery pass; these timings are observations on cached runs rather than a controlled performance benchmark. Broader network, Pandoc and packaging limitations are stated above.

## 11. Team Learning

Memory receipt: brain retrieval preceded planning; D-16 informed the distinction between safety and changing authority providers. The repo route has no LOAD-FIRST manifest, which was recorded without inventing one. Memory registry guidance on reproduction and the CRAN hold shaped scope. Durable choices and evidence are recorded in this repo's linked notes; the external vault was not modified.

Golden Set: reviewed the case inventory, particularly branch drift, external-state CRAN claims, partial-arc coverage and completion overclaims. No heuristic transcript benchmark is claimed to have passed. The observed checks above are the technical evidence.

A missing suggestion can be stored but hidden, or excluded earlier because its target was consumed. Trace both states. Reusing a cheap existing filter can bound authority work without repeating an expensive scoring pass. A resolution rate from a paper is useful provider evidence but does not establish match accuracy.

## 12. Cross-Product Coverage

The completed scope does NOT cover the excluded surfaces below.

| Surface | Proven coverage | Does NOT cover |
|---|---|---|
| Review and reporting | Ordinary flagged/fuzzy review, explicit identity mode, summary table, print discovery, legacy missing fields | A redesign of every review interaction or bulk approval |
| Mapping and apply | Identical full-workload mapping; default/include_flagged fixture controls; explicit overrides and occupied-target checks | Full-workload data/tree application outputs or deliberate many-to-one merging |
| Identity | Frozen fixtures and cached COL 2026 replay; five conflicts, unknown/ambiguous/incomparable evidence and valid shared-ID cross-genus controls | Fresh live responses from all authorities or all biological identifications |
| Botanical resolver | Optional TNRS design and acceptance checklist, supplied paper assessed | TNRS integration, live submissions, local Taxamatch implementation or a default-provider switch |
| Delivery | Scoped local source, tests, docs and receipts | Push, PR, merge, public reply, release or CRAN preparation/submission |
