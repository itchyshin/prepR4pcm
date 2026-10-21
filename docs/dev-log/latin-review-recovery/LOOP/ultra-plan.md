# 🎯 GOAL

```text
Solo platform: Codex
Deliverable: a review-recovery repair verified against Erick's examples and a matched baseline.
HEADLINE: useful spelling suggestions remain visible when authority identity is unknown.
IN PARALLEL: source diagnosis and botanical TNRS evidence review; code edits remain sequential.
DEFER: automatic spelling acceptance without authority confirmation, TNRS integration, CRAN, publication.
DISCIPLINE: verify candidate visibility and application separately; small tests local, full replay Totoro.
Closure: approved implementation passes GATES.md, with exact-source receipts and unresolved limits disclosed.
```

Planning version v1, 21 October 2026. Planning is authorized; implementation is not yet authorized. Only this planning packet was written. No package edits, tests, fits, live name submissions or release preparation were performed.

## Recommendation and decision

Repair the separation between finding a useful suggestion, displaying it for review, and applying it to data or a tree. Missing authority identity must not make an otherwise plausible suggestion inaccessible. A confirmed conflict must remain clearly labelled and excluded from automatic application. Reuse the existing candidate machinery and `pr_suggest_gender()` before considering a different matching engine.

User decision: **restore review suggestions first**. Do not relax automatic application rules in this arc. If diagnosis demonstrates an incorrect authority classification that affects automatic acceptance, present the exact evidence and the smallest correction for a separate scope decision. Do not silently extend this repair into a taxonomic-policy change.

The earlier design note remains a historical record. This plan replaces its recommendation to wait: Erick has now supplied feedback, and Shinichi has explicitly reopened planning. CRAN preparation remains on hold.

## What the brain already knows

- The prior instruction was to wait for Erick's feedback before further Latin work or CRAN preparation. That feedback is now present; it is not release approval.
- The brain's `memory/DECISIONS.md`, D-16, separates reconciliation safety from changing the default online authority. It calls for evidence on network behavior, reproducibility and false positives before a default-provider change.
- The project dossier is a stub and `route.py prepR4pcm` reports no LOAD-FIRST manifest. No missing manifest was invented or repaired during planning.

## What Shinichi told us

- The current behavior is too strict; assess Erick's Taxamatch/TNRS suggestion.
- Restore useful review suggestions first, rather than permitting spelling-only automatic matches.
- Use ultra-plan and unlazy. This authorizes a plan and acceptance contract, not implementation or external posting.

## Evidence and prior-work receipt

| Surface checked | Concrete evidence | Consequence |
|---|---|---|
| Live feedback | [Erick's new #139 reply](https://github.com/itchyshin/prepR4pcm/issues/139#issuecomment-6322919006): 3,944 earlier flagged suggestions, one current suggestion, two missing Trema examples and a disputed Bignonia pairing | Treat counts and taxonomic judgments as reporter evidence pending reproduction. Do not require recovery of exactly 3,944 rows. |
| Live GitHub | `commits/main` returned `b05501806751518506638167a59218903669cfa6`; prior implementation PRs #140, #141 and #142 are represented in local all-ref history | Base any approved work on verified main; retain earlier implementation and tests. |
| Local state | `git status -sb`, `git log --all`, branches, worktrees, stashes and `branch_drift_check.sh`: dirty detached `54d332b`, 0 ahead and 24 behind origin/main | Do not edit/reset the shared checkout. Create a separate managed worktree after approval; record the exact source and path. |
| Lane ownership | `~/shinichi-brain/tools/lane_preflight.sh`: no recently active foreign lane, but silence is weak evidence; dirty paths and old worktrees still exist | Planning packet lives outside the repository. Recheck ownership and claim scoped paths before implementation. |
| Existing matcher | Graft reconnaissance followed by `git show origin/main:R/pr_fuzzy.R`, lines 157–171, 237–260, 277–364 | Current local matcher uses genus filtering and weighted edit similarity, not the full Taxamatch algorithm. With identity checking active, ordinary results require comparable same-ID evidence. Other plausible pairs can enter a separate candidate table. |
| Candidate plumbing | `R/pr_cascade.R:440–467`; `R/reconcile_review.R:74–121,124–135` at the verified main | Existing candidates are recorded and explicitly reviewable; ordinary flagged/fuzzy review uses mapping rows instead. Diagnose hidden candidates before adding another search mechanism. |
| Existing TNRS work | `feat/tnrs-audit-capture` at `fab21a7` is an ancestor of main; `R/pr_get_tree.R:982–1005` uses `rotl` | This is OpenTree TNRS, not the botanical service in Erick's screenshot. Reuse audit ideas, not an assumed botanical adapter. No R/Julia twin is involved. |
| Brain retrieval | Vault query `"prepR4pcm" "Latin"` found no results; all-project query `prepR4pcm TNRS` returned NEWS; deterministic searches of AGENT_LOG, DECISIONS, OPEN_QUESTIONS, research index, project dossier and recent journals found D-16 and older project records | Current repo and issue are the technical evidence. Do not infer absence of prior work from the empty vault query. |
| External prior art | [Rees 2014](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0107510), [TNRS engine](https://github.com/ojalaquellueva/TNRSbatch), [service instructions](https://tnrs.biendata.org/instructions/), [warnings](https://tnrs.biendata.org/warnings/), [API](https://tnrs.biendata.org/tnrsapi/) | Botanical TNRS uses Taxamatch and has an R client. It still exposes ambiguity and source-dependent results; it is a comparator, not adjudicated truth. |

Sweep verdict: **reuse the merged matcher and candidate records; diagnose the review-path gap; defer a new botanical adapter**. The immediate route is a bounded diagnostic followed by the smallest demonstrated repair. A corpus-wide literature review is unnecessary for this scope; primary-source lookup was sufficient and no NotebookLM campaign was run.

## What the team raised

- Repository scout, Luna medium: `identity_candidates` has existing meanings and consumers. Preserve conflict reasons and compatibility when adding a clearer review path. Neither the reported Trema cases nor the Bignonia case is covered by the existing synthetic fixtures.
- TNRS evidence scout, Luna medium: service agreement cannot define correctness. Curate and adjudicate expected outcomes independently; a large candidate count is not evidence of useful recovery.
- Planning owner: audit candidate generation, display, user decision and application as separate stages. An unresolved name may still have useful candidates. A target absent from the tree must remain absent unless the user deliberately chooses a different biological operation.

## Phase A: immediate review-recovery repair

### A1. Reproduce and explain

Identify the exact old version, current version, inputs, tree, normalization settings, authority snapshot and cached lookup results. Use the reporter's saved objects where available. If the old version cannot be identified, label the selected pre-guard version a surrogate baseline; do not claim exact reproduction of 3,944.

Trace `Trema tomentosa` / `Trema tomentosum`, `Trema orientalis` / `Trema orientale`, and `Bignonia ternata` / `Adenocalymma macrophyllum` through every stage. Record whether each candidate was generated, scored, blocked, stored, visible in each review mode, selected and applied. Compare counts by unique input name and candidate pair so one-to-many results cannot distort the comparison.

For Bignonia, inspect raw lookup rows, accepted-ID chains, taxonomic status, rank, provider and snapshot. Compare with the intended tree backbone. The reporter's concern does not itself settle identity. Low spelling similarity alone cannot reject a documented cross-genus synonym; an ID match alone must not hide ambiguous or unsupported source status. If the authority data remain unresolved, record that uncertainty and keep the review decision explicit.

Output: `diagnosis.md`, a pair-stage audit table, exact-source/input receipts and frozen minimal fixtures. Review these before changing code.

### A2. Restore the review workflow

Start with the smallest change supported by A1. If the candidates already exist, repair visibility and discovery. Change generation only where the trace proves a genuine loss.

- Keep plausible spelling and bounded ending variants reviewable without demanding a shared accepted ID. Use existing score rules and `pr_suggest_gender()` as the starting point; do not introduce a wider global search without evidence of a missed candidate.
- Present missing evidence, incomparable evidence, ambiguity and confirmed distinct-ID conflicts separately. Do not relabel all of them as a rejected match or a valid synonym.
- Expose pending spelling candidates through ordinary `reconcile_review(type = "flagged")` and its summary, with a direct non-interactive route via `reconcile_candidates()`. Preserve the existing `identity_candidates` mode and documented return structures.
- Preserve all relevant alternatives and original source names. A suggested target must not reserve a tree tip or prevent another valid match before the user decides.
- A review-only candidate cannot modify data/tree contents by default. Accepting one creates a deliberate, traceable override. Existing `include_flagged` behavior must not accidentally apply newly exposed candidates in bulk; explicitly test this boundary and document any intentional compatibility decision before implementation.

Expected touched areas: `R/pr_fuzzy.R` only if needed, cascade candidate plumbing, review/summary/report visibility, tests, the two scoped verification scripts in CHECK-CONTRACT.md, and narrowly related documentation. Map callers before edits. One implementation owner writes these coupled files sequentially; no concurrent edits to shared source.

### A3. Verify and reconcile

Write failing tests before the repair. Use frozen authority fixtures for deterministic tests, including successful and adversarial cases. Verify both the public review path and downstream apply behavior; internal candidate existence alone is insufficient.

Replay the matched workload on Totoro only after small fixtures pass and runtime is estimated. Record added, removed, unchanged and reclassified pairs with reasons. Separately count automatically applied matches and review-only suggestions. Independently review a fixed labelled positive/negative set and a sample of changed pairs. Report unadjudicated cases separately; do not call all restored suggestions correct.

Output: verified patch in the approved isolated lane, test and replay receipts, updated user explanation, after-task report and plan-versus-actual note. Publication and merging remain separate decisions.

## Acceptance contract

The adjacent `.unlazy/GATES.md` contains G0–G8. All execution gates begin pending. Its runnable commands refer to tests and validators to be authored after approval; they are not existing passing checks. CHECK-CONTRACT.md fixes their observable requirements now, and SCENARIOS.json names the mandatory cases. No success marker may be printed unless every required scenario ran without a skip and all assertions passed.

1. The trace explains the reported disappearance, including hidden-versus-missing candidates, with a reproducible baseline or an explicit baseline limitation.
2. Both Trema pairs appear in ordinary flagged review, its summary, and `reconcile_candidates()` when their names occur in the relevant input/reference lists, including missing-authority fixtures. Access only through the separate identity-review mode does not pass. This is candidate recovery, not a claim that every pairing is taxonomically valid.
3. The five #139 distinct-ID cases remain excluded from automatic application. Unknown, conflicting, ambiguous and incomparable states remain distinguishable.
4. Review-only candidates are excluded by default, alternatives survive, accept/reject decisions are recorded, and saved objects plus existing review modes remain compatible. New candidates do not get applied through an unintended `include_flagged` path.
5. Bignonia's raw evidence is traced without an invented resolution. Tests separately cover valid cross-genus synonyms and uncertain/conflicting source records; any automatic-rule correction requires a scope decision.
6. Changed-pair accounting is complete, and no unexplained automatic-output changes are accepted. A candidate-count increase alone cannot pass the gate.
7. Focused and relevant regression tests pass from the exact candidate version, with no skipped core scenarios. Negative controls demonstrate that the checks catch hidden Trema candidates and accidental application.
8. Documentation, recorded sources, verification and independent review support the final claim; every required gate passes or any remaining work is explicitly left incomplete.

## Phase B: separately approved TNRS comparison

After A, evaluate botanical TNRS on a small adjudicated plant set. Choose the source matching the intended taxonomy; for Erick's WCVP-based tree, verify the relevant WCVP snapshot rather than silently using the service's WFO default. Normalize both lists against that source, then map to actual available tree tips. A service returning an accepted name is not evidence that the tree contains it.

Retain query IDs, all alternatives, scores, warnings, raw results, settings and source/version metadata. Compare recovery, misleading suggestions, abstentions and review burden against independent labels. The documented API limit is 5,000 names per request; assess batching, retries, partial failures and repeatability before scaling. No private/full workload is sent during planning. Decide whether an optional adapter is justified from these results. No new default provider or engine replacement is included in Phase A.

## Roles, order and estimates

| Slice | Owner/model and dispatch | Estimated time | Dependencies and output |
|---|---|---|---|
| Planning reconnaissance | Two Luna medium scouts, native/explicit, already completed read-only | Completed | Repo map and primary-source findings above |
| A1 diagnosis and fixtures | Package engineer, Sol high, native/explicit; coupled pipeline reasoning warrants Sol | 45–75 min | Approval; exact baseline and `diagnosis.md` |
| A2 focused repair | Same implementation owner, Sol high | 45–90 min | A1 evidence and failing fixtures; source/test patch |
| A3 mechanical verification | Luna medium, native/explicit, same isolated source after edits stop | 20–40 min plus measured replay | A2; test logs, replay receipts |
| Review and plan reconciliation | Independent Sol high reviewer; Luna medium records plan-versus-actual | 20–30 min | A3; verdict, after-task report and unresolved limits |

Provisional total for Phase A: **2–4 hours**, one bounded arc. This is an engineering estimate, not a measured runtime. Previous posted cached-workload replay was 164.81 seconds; use it only to size a fresh pre-run. Initial full cached replay budget: 10 minutes on Totoro after a successful small probe; stop and re-estimate on overrun. Local fixtures use a light single process with `OPENBLAS_NUM_THREADS=1`; do not run the full workload on the Mac or GitHub Actions. The compute-routing skill governs the eventual launch. Live TNRS latency is unmeasured and excluded from the Phase A estimate.

Execution scheduling uses at most one active source writer. Start a fresh bounded execution chat if context or the efficiency check requires it. Planning used two explicit Luna medium scouts and one Sol high independent plan reviewer. That review tightened ordinary-review visibility, zero-skip checks and replay denominators; see REVIEW.md. Usage-limit lookup failed, so available account allowance is unknown; no model quota was assumed.

## Execution boundary and G0

PRE-AUTHORISED AFTER G0: a managed worktree based on verified main; scoped source/test/doc edits for Phase A; routine local commands; regression tests; frozen-data diagnosis and bounded Totoro verification after pre-run; local checkpoints. Record the exact worktree and bind all ledger checks to it before executing. Review new validator/test scripts before approving the exact check batch.

OPTIONAL REMOTE AUTHORITY: none for GitHub. Totoro permission is limited to the named verification work and existing authorized access. No external name-service submissions are included.

MUST STOP: push, PR creation, merge, public reply, release/CRAN preparation, destructive actions, new credentials, unknown ownership overlap, a changed automatic-matching policy, a new service integration, or evidence requiring materially broader scope.

G0 requires explicit implementation approval. The plan and ledger being present, or the ledger parsing successfully, does not satisfy G0. `--status` is parsing only; `--reverify` later supplies execution evidence. Next decision: approve Phase A, keeping Phase B and CRAN deferred.
