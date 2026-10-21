# 🎯 GOAL — #139 identity protection and Rose audit

Solo platform: Codex. Deliverable: a tested fix for unsupported automatic taxon joins in synonym and fuzzy matching, with a documented audit of neighbouring routes. Headline: spelling similarity and identical accepted-name text must not override clear evidence of distinct taxa. In parallel: bounded interface/docs work after the evidence contract is frozen. Defer: CRAN preparation/submission, taxonomy-provider downloads beyond existing support, a general taxonomic engine, and changes to intentional species/subspecies aggregation. Discipline: executable acceptance gates, independent review, actual supplied-data tests on Totoro, and fresh installed merged-main verification before issue closure.

Status: **APPROVED AND IN EXECUTION.** The user approved this scoped implementation and conservative handling of unknown evidence. Independent review found and the regression suite now covers missing-ID backbone rows, cached identity ordering, synonym fallback, pair-specific candidate rejection, and augmentation provenance. The exact v18 artifact passed local and Totoro package checks: both had zero errors and zero package-check warnings (3 local notes, 2 Totoro notes; vignettes excluded). The local check ran without optional `taxadb`; the Totoro check ran with it. The full saved-workload replay passed in 299.665 seconds under the user-approved 10-minute cap. The raw-ID audit found 2,594/2,594 automatic pairs share an accepted ID, zero unknown/conflicting automatic pairs, 4,481 blocked distinct-ID candidates, 4,771 reviewable unknown-identity candidates, zero missing historical pairs, and full x/y coverage. Exact artifact, replay, and pair-audit receipts are in `.unlazy/issue139-plan/evidence/v18/`. G0-G4 and G7 are met. G5 merged-main verification and G6 plan reconciliation/after-task report remain open. GitHub currently shows draft PR #140 open and unmerged and issue #139 open; no merge or issue closure has been attempted or authorized. CRAN stays ON HOLD. The separate Latin-aware follow-up remains deferred until #139 is finished.

## What the brain already knows

The Rose principle means fix the class and inspect its neighbours; it does not mean assume every neighbouring behaviour is defective. The reusable lesson in `memory/WHAT-WORKS.md:334–339` also requires naming a non-instance. `protocols/after-task.md:12–27` requires the sibling search and the user-facing check. Prior prepR4pcm work requires original-report reproductions and installed merged-main testing before closure. CRAN remains ON HOLD; this plan does not lift it.

Recall used the brain MCP, vault first: query `"Rose" "prepR4pcm" fuzzy matching identity`, followed by a direct read of `shinichi-brain/protocols/after-task`, the project dossier, and deterministic file searches. The broad query returned weak matches; direct protocol/source evidence governs. The optional reranker failed on its configuration-directory permission operation; no successful reranked search is claimed. `route.py prepR4pcm` found no LOAD-FIRST manifest.

## What Shinichi told us

- Diagnose before editing; use ultra-plan for substantial work.
- Apply the Rose principle to possible wider issues.
- **Approved policy:** automatic protection when authority evidence is available.
- Use ultra-plan plus unlazy, and test ourselves before handing work back.
- Keep CRAN preparation on hold. Use reporter handle `ejlundgren` in any separately authorized reply.

## Evidence and scope boundaries

Inspected main: `b365d0a558af3b71522c65b90d1c649a8eaa5d87`; live main was checked on 2026-10-20. Issue: <https://github.com/itchyshin/prepR4pcm/issues/139>.

| Observation | Evidence level | Consequence |
| --- | --- | --- |
| All five reported pairs score 0.9333 in isolated small-tree reproductions | Executed on current source; no authority in these small reproductions | They are flagged under defaults; the default apply safeguard works for these five |
| Saved #137 COL records give each of the ten names a separate accepted ID | Executed read-only against actual saved records | The concern is supported independently in COL; this does not verify a WCVP snapshot |
| Of 4,777 saved fuzzy-stage pairs, 3,372 have accepted status and distinct nonmissing IDs on both sides: 3,173 flagged, 199 ordinary fuzzy | Executed read-only audit of the previous full workload | Protection cannot depend on a score threshold alone; these are record-level conflicts, not a claim that every biological identity has been independently adjudicated |
| Anthurium curtispadix / A. curvispadix scores 0.9636 and default apply relabels the tree tip | Executed isolated reproduction; the pair is in the saved distinct-ID subset | A downstream effect exists beyond review suggestions |
| Synonym records sharing accepted-name text can yield a candidate despite distinct resolved IDs | Controlled source-level reproduction; real-data frequency unknown | Include synonym eligibility in the same identity invariant |
| `rank="species"` collapses two subspecies; `rank="subspecies"` retains them | Executed controlled reproduction | Intentional rank behaviour is a **non-instance**; preserve it and explain its consequences |
| First-hit authority selection loses ambiguity; provider errors become `not_found`; cache uses a `latest` label | Source inspection, adapter-specific consequences still to test | Preserve evidence quality and actual snapshot provenance before trusting IDs |

The previous #137 parity tests established retained matching behaviour and scalability, not correct biological identity. Do not reuse their passing status as proof of this fix.

Baseline evidence receipt (ignored, private run state): `.unlazy/issue139-plan/evidence/baseline-audit.json`, SHA-256 `188f79cc3244094bacd414b8ec7e1befe17eac6ffdcaca98af3f6e262f086cef`. It includes the SHA-256 fingerprints and exact paths of the three retained raw RDS sources, provider/snapshot, source commit, denominators and reported pairs. The saved reproduction script is `.unlazy/issue139-plan/evidence/baseline-audit.R`. From this owned worktree, the exact source-read-only command below reproduces the JSON as `baseline-audit-reproduced.json`; compare its bytes/hash with the receipt. The script uses only name normalisation from the installed predecessor; identity comparisons use the raw status/ID fields, not the proposed production guard. Estimated runtime under 30 seconds, hard cap 60 seconds, four cores and one BLAS thread. The preserved first-hit baseline is an audit of the predecessor's records, not proof that ambiguous hits are unique.

```sh
ssh -o ControlPath='/Users/z3437171/.ssh/cm-snakagaw@totoro.biology.ualberta.ca:22' -o BatchMode=yes totoro \
  'OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 timeout --kill-after=5s 60s taskset -c 0-3 Rscript --vanilla -' \
  < .unlazy/issue139-plan/evidence/baseline-audit.R \
  > .unlazy/issue139-plan/evidence/baseline-audit-reproduced.json
```

## Prior-work sweep receipt

- **Repo state:** `lane_preflight.sh`, `git status --short`, `git log --all`, branch/worktree/stash inventory, and read-only branch-drift check. The owned worktree was clean at b365d0a, zero drift from origin/main. No prior #139/WCVP guard was found in the bounded history/source search. The original dirty checkout at 54d332b is protected.
- **Existing implementation:** reuse authority caches, exact/normalised matching, synonym lookup, explicit overrides, review machinery, and #137 scalability regressions. New gap: comparable identity evidence is discarded before decisions in `R/pr_synonyms.R:41–123` and `R/pr_cascade.R:259–355`; fuzzy selection is top-only in `R/pr_fuzzy.R:110–141`.
- **Sisters:** no established R/Julia twin or sibling guard implementation was identified in the bounded registered-project sweep. No broad cross-repo implementation search or novelty claim is needed.
- **Brain and history:** MCP searches plus deterministic searches of AGENT_LOG, DECISIONS, OPEN_QUESTIONS, journal and the research index found the prior hold/test decisions, not an existing identity guard. Reuse the instructions and evidence rather than rebuilding the package architecture.
- **Verdict:** build the missing invariant at the shared evidence/selection boundary. Five public matchers share this boundary. Do not equate a source hit with a reproduced fault.

## What the team raised

**Mechanical scout — Luna medium:** five entry points share the cascade; review, apply, export, crosswalks, suggestions and augmentation must be inspected. Some old parity fixtures provide only accepted names and cannot be identity oracles.

**Domain/design reviewer — Sol high:** compare canonical accepted IDs only within one declared provider/snapshot. Apply eligibility before best-candidate selection and greedy assignment. One governing identity source must cover both synonym and fuzzy stages; a WCVP guard added after independent COL synonym assignment would be too late.

**Rose lens:** `include_flagged=TRUE` is a bulk action. Known conflicts cannot merely be ordinary flagged mappings, or the protection can be bypassed accidentally. Unknown evidence, deliberate manual decisions and intentional rank collapse need distinct, visible treatment.

**Root:** the automatic policy and the implementation scope below are approved for this issue. The wider audit is bounded by the identity-loss class, not an open-ended rewrite.

## Approved public and scientific contract

1. When an authority or supplied backbone provides clear, comparable evidence, protection is automatic. Two different accepted IDs in the **same provider and resolved snapshot** cannot become an automatic synonym or fuzzy pair, at any score and under either resolve mode.
2. A shared canonical accepted ID supports an identity-preserving match. IDs from different namespaces/versions are not directly comparable. Accepted-name text alone is not a substitute for identity. Each adapter must demonstrate accepted-record ID semantics with accepted and synonym controls; an unresolved synonym target, conflicting hits, arbitrary record ID or unverified GNverifier identifier is unknown. Do not label the first accepted hit as unambiguous merely because its status is accepted.
3. In protected synonym/fuzzy decisions, missing, ambiguous, incomparable or failed lookup evidence leaves a **reviewable unresolved candidate**. Failure is distinguishable from a genuine absent record. **Trade-off:** this deliberately reduces unattended fuzzy acceptance; unknown spellings can still be reviewed and manually accepted. It is stronger than merely blocking the 199 observed conflicts.
4. An optional `backbone` input supplies frozen local records and provider/version metadata. Proposed shape: a named object containing a record table (`input`, `accepted_name`, `accepted_id`, `status`), provider, version and the identity rank represented by those records. Optional record field `side` is `x`, `y` or `both` (default): it permits deliberately disambiguated, identical strings to carry different identities on the two sides. Multiple distinct identities for the same side/key, conflicting general and side-specific records, or contradictory normalised keys are ambiguous, not first-hit wins. Exact duplicate evidence can be deduplicated without erasing provenance. The declared record rank must match the requested comparison rank; do not mistake two subspecies IDs for two species IDs. Record a content fingerprint. **Precedence is explicit:** a supplied backbone replaces authority lookup for both synonym and fuzzy decisions, even when `authority="col"` was supplied explicitly. Emit a clear notice for an explicitly supplied conflicting authority, record the requested and effective sources, and make no COL lookup in that case. Reject a conflicting explicit `db_version` instead of silently mixing snapshots. Test both omitted default authority and explicit COL with a WCVP table. No new downloader, ancestor-resolution engine or exported taxonomy-management framework is in scope.
5. With neither authority nor backbone, retain the explicitly taxonomy-unchecked spelling mode and label it clearly in metadata/help. Do not imply that this mode establishes taxon identity. Do not introduce network lookups into exact-only/no-authority workflows.
6. Known conflicts and unknown protected candidates remain outside accepted mapping rows. Store their candidate evidence separately, with original names, score where relevant, provider, requested/resolved version, IDs and reason. This is an additive reconciliation-object field, preserved by construction and validation. Provide a candidate view in review/accessors, retain counts/reasons in reports, and export a separate candidate table with the mapping. Pair-specific acceptance/rejection must work from this table and preserve its original evidence. Preserve complete unresolved inventories and the existing one-to-one rule where applicable. Filtering must happen before top-candidate/greedy consumption so an ineligible first candidate cannot hide an eligible alternative.
7. `resolve="first"` and `include_flagged=TRUE` cannot convert protected conflicts/unknowns into automatic matches. A deliberate pair-specific override/review decision can accept them, retaining an explicit audit reason. Existing supplied crosswalks are deliberate external-evidence input and must be identified as such rather than silently reclassified as automatic validation.
8. The known-conflict invariant also applies to exact/normalised joins when deliberate, comparable evidence is bound to this reconciliation. Build a supplied-backbone index before Stage 1; test side-specific identical-string conflicts and normalised conflicts. Retain evidence fetched for the run and check any automatic pairs it actually covers. **Do not let incidental warm session-cache contents change early-stage outcomes.** Bind the source/snapshot explicitly and test warm/cold determinism. Do not add live lookups solely to exact-only, already-resolved workflows. Without identity evidence, exact/normalised results remain documented lexical/rank alignment, not verified biological identity. Identified ambiguity cannot be advertised as unique identity. Intentional rank aggregation remains intact: compare IDs at the requested rank, and never veto species aggregation merely because raw infraspecific IDs differ. A material change to that policy returns to Shinichi rather than arriving as a hidden guard side effect.
9. Advisory gender/fuzzy suggestions must remain advisory. Apply/export must not relabel or align blocked pairs by default. Augmentation may explicitly add an unresolved species as an exploratory new tip; it must preserve the guard reason and must not masquerade as identity confirmation.

## Route check and decisions

Destination is clear: no unsupported automatic synonym/fuzzy identity assignment where protection is active, with useful review and preserved supported matches. The default decision is resolved by Shinichi. The evidence object, conservative unknown handling and deliberate-override contract above make the route concrete and are covered by the recorded implementation approval. A rejected or materially changed choice requires a plan revision before work proceeds.

Optional grounded literature search: not required for this software contract; a NotebookLM literature campaign is available if scientific taxonomy-policy research is desired, but is not proposed here. Jev: **OFF proposed** — this task needs coupled design judgment, not hundreds of repeated decisions; no paid decision service or configuration change is needed.

## Slices, dependencies, ownership and model routing

All producer writes stay in the owned isolated worktree, on new branch `codex/issue-139-identity` after approval and a fresh ownership check. Private #137 inputs/results remain ignored. Root coordinates integration; concurrent writers must claim disjoint scopes. There are at most three active agents including root. Existing planning children are reused. After approval, at most four new children are allowed: at most three bounded Luna workers and one fresh Sol reviewer; fewer are preferred. No ultra reasoning mode or Astra escalation is proposed.

| Slice/member | Model, effort, mechanism | Time estimate | Output and ownership | Depends on |
| --- | --- | --- | --- | --- |
| RECON / scope inventory | Luna medium, native/explicit; planning pass completed | 20–40 min including retained sweep | Identity-risk matrix and test disposition for every listed route; run-state inventory | Current diagnosis |
| A / evidence contract | Root, difficult coupled judgment; parent model not exposed by runtime | 90–150 min | `R/pr_identity.R`, `R/pr_authority.R`, `tests/testthat/test-issue-139-evidence.R` | Approved contract, failing fixtures |
| B / matching invariant | Root, same coupled implementation owner | 90–150 min | `R/pr_synonyms.R`, `R/pr_fuzzy.R`, `R/pr_cascade.R`, matching/control tests and validation harness | A |
| C / interfaces and consumers | Luna medium, native/explicit; root reviews semantic changes | 90–150 min | Five entry points, reconciliation object/accessors, review/report/apply/export/augment/crosswalk changes only where required; surface tests | A and frozen public contract; integration after B |
| D / docs | Luna medium, native/explicit; no source-code ownership overlap | 45–60 min | Help, tutorial, NEWS and audit report; generated man files run only after source changes settle | Frozen contract; can overlap B/C |
| MECHANICAL-VERIFY | Luna medium, native/explicit | 45–75 min | Installed-artifact receipts, test counts, real-data audit and check logs in ignored run state | B–D |
| Independent correctness review | Fresh Sol high, native/explicit; canonical-ID and selection rules are load-bearing | 30–45 min | Adversarial source/test/claim review, no implementation ownership | Candidate checks |
| RECONCILE / closeout | Luna medium, native/explicit, root approves claims | 15–25 min | Plan-versus-actual report on six axes; after-task report and final evidence receipt | Verification/review |

End-to-end estimate: **6–10 hours**, including roughly 45–75 minutes of tests/checks; runner queues are additional. This is larger than the initial 4–8-hour fuzzy-focused estimate because the approved planning request adds the Rose audit and synonym/evidence/consumer coverage. No compute run over three hours is proposed.

SCOUT SUITABILITY: yes; the read-only sweep ran on Luna. Planning dispatch receipts are the native agent calls for issue139_sweep (Luna medium) and issue139_design (Sol high). The Sol child handled identity semantics and plan challenge, not mechanical command loops. The requested tiered-CLI enforcement is not claimed: native explicit model parameters were used, and this routing deviation is retained for reconciliation. Execution should use the available enforced launcher for suitable checks, or document its unavailability without claiming enforcement.

## Rose coverage matrix and stop rule

| Neighbour | Required outcome | Disposition |
| --- | --- | --- |
| Exact / normalised / rank / duplicate collapse | Veto bound comparable conflicts and expose ambiguity; retain lexical-only/no-evidence and intentional rank controls; warm/cold outcomes deterministic | Same-class protection where evidence exists; no silent rank redesign or new exact-only live lookup |
| Synonym direct/reverse/shared-accepted routes | Shared text cannot override distinct comparable IDs; an ineligible direct hit cannot consume an identity-compatible runner-up | Regression and fix in this arc |
| Fuzzy genus filter, scores, ordering, runner-up, greedy assignment | Identity evidence aligns by requested name despite cache order and duplicate normalized inputs; identity eligibility precedes selection | Regression and fix in this arc |
| Provider adapters / cache / versions / multiple hits / errors | Comparable canonical IDs, resolved snapshot, ambiguity and failure provenance survive | Fix evidence required by invariant; unsupported semantics become unknown |
| Five reconciliation entry points | Same guard/evidence contract, intentional multi-source multiplicity preserved | Integration test matrix |
| Review / override / supplied crosswalk | Pair-specific human acceptance/rejection remains usable and audited; rejecting a withheld alternative cannot remove another accepted pair | Regression and fix in this arc |
| Apply / export | Bulk flags/resolve mode cannot relabel a protected pair | Integration tests |
| Suggest / gender suggestions | Candidate-only, no silent accepted mapping changes | Preserve; test a non-instance |
| Augment | Blocked names remain unresolved/eligible for explicitly exploratory augmentation; candidate evidence and identity-source provenance survive every return route | Regression and fix in this arc |
| Tutorial / examples / metadata | Scores described as spelling similarity; taxonomy-unchecked mode, unknowns and overrides explained | Update |

Stop and re-scope only for a materially different scientific/input model, new provider download, changes to intentional rank semantics, another lane's files, security/credential changes, unexpected destructive action, or compute beyond this envelope. A same-class defect is not deferred merely because it was outside the initial five examples. Every inspected neighbour gets a finding or a reason it is a non-instance; every unresolved finding gets an owner and disposition.

## Executable acceptance contract — unlazy

Run state: `.unlazy/issue139-plan/GATES.md`. The original leaf ledgers referenced validation scripts that were never created; the integration ledger replaces those stale commands with real checks and explicit manual gates. `gate-check.mjs --status` only parses the ledger. Runnable checks are source-reviewed, approved and reverified; manual gates remain open until evidence is recorded.

The harness must measure outcomes and exit nonzero on any failed assertion; it may print the corresponding success token only afterward. Its own negative controls must prove that disabling the identity check, reintroducing first-hit ambiguity or selecting before eligibility fails. Do not use copied counts, file existence or a green status parser as proof of correct matching.

Acceptance includes:

- Original five pairs plus a real >0.95 pair **against pinned COL evidence**, shared-ID synonyms, same-text/different-ID synonym records, genuine not-found, failed lookup, missing IDs, ambiguity, cross-provider/version, resolve modes, deliberate overrides and eligible alternatives. Keep the original no-authority small reproductions as controls, not tests of an authority guard.
- A cross-product matrix covering all five entry points and the consumers above, with explicit unsupported cells rather than assumed coverage.
- Independent output identity oracle on **raw saved COL records**, without calling the production identity helper. Recompute before/after counts rather than require the historical 199 figure. Account for denominators by eligible, blocked, unknown and deliberately overridden outcomes and provide an exact changed-pair audit. Every removed/changed pair must have a reason; supported exact/normalised/identity matches must not disappear unexplained. Preserve coverage and appropriate assignment cardinality.
- Fresh source tarball, checksum, installed library-path check, installed issue tests, and full saved-data replay using that same artifact. Private data/caches are absent from the tarball and public commits.
- New tests have no unaccounted skips. Existing warnings/skips/packaging notes are compared with the recorded baseline, not hidden. Scoped `R CMD check --ignore-vignettes --no-manual` is not a CRAN release check. Existing live OpenTree vignette failures remain separately identified; no all-platform/full-online-check claim follows.
- Document → test → scoped check → pkgdown check, independent review, ledger re-verification, then fresh merged-main installed verification and settled CI before closure if landing proceeds.

## Compute and data plan

Local: small fixtures, at most four cores per lane; BLAS/OMP one. Totoro: actual private workload and installed checks through the existing ControlMaster socket only; task-specific library, CPU affinity four, BLAS/OMP one. No new authentication prompt, no GPU/DRAC need, no campaigns on Actions.

Smoke on a bounded real-data subset first. Historically, the initial full cached replay estimate was 2–3 minutes with a five-minute hard cap; three attempts reached that cap without a receipt. The v11 Totoro route profile measured 2.006 seconds for 1,000 stratified pairs and 9.522 seconds for 4,652. The exact-v12 route profile passed on Totoro in 8.472 seconds for 4,652 stratified pairs, withholding 3,410 identity candidates with no fuzzy or synonym matches; a separate exact-v12 full-name identity-preparation profile passed in 57.5 seconds for 190,263 x names and 117,934 y tips. These estimates led the user to approve a 10-minute hard cap. **Executed:** the exact-v16 full route and independent raw-ID oracle passed on Totoro in 266.742 seconds, well within the cap; see the final receipt, raw log, pair-disposition table, and new-automatic-pairs audit under `.unlazy/issue139-plan/evidence/v16/`. The historical pair audit found no missing pairs, and the automatic identity and coverage counts are recorded in the receipt. The prior v5 stage profile measured only backbone/lookups/normalization and did not qualify the route. Whole applicable suite/checks were estimated at 10–20 minutes each, separately capped at 30 minutes. Installed targeted verification passed on v11, v12, and v16; a live database refresh has not run. Record input fingerprints, snapshot, artifact hash, environment, elapsed times, all attempted runs and failures. The previous #137 96.7-second cached replay is a baseline observation, not a promised time.

## Approval envelope and landing gates

After approval: scoped edits in the isolated worktree, named tests/builds and reviewed harnesses above, routine local checkpoints/commits, bounded Totoro checks, and branch/PR preparation. Under the standing project instruction to land finished ordinary-repo work, guarded merge may proceed only after all required checks settle green and independent review passes. Issue #139 must remain open until fresh installed merged-main reproductions pass; do not use an auto-closing PR keyword to pre-empt that gate. Confirm actual main SHA and exact post-merge CI, not only PR CI.

Stop for approval of a material scope/contract change, destructive or security-sensitive action, or any CRAN/release action. Public issue comments and messages are excluded unless separately authorized; a prior instruction to post on #137 does not authorize a reply on #139.

Plan approval was received before implementation. Execution is active in the named isolated worktree. Approval does not extend to CRAN preparation/submission, public comments, issue closure, or exceeding the approved compute cap.

## Verification, reconciliation and evidence limits

Independent plan review must happen on the written plan and ledger, not only the initial design idea. Retain its findings and root responses. At implementation close, a fresh reviewer challenges the identity and failure semantics independently; the parent re-runs the oracles rather than accepting worker prose. Melissa reconciliation compares scope, verification, routing, safety gates, public claims and handoff state. Retain a standard after-task report with the memory receipt, Golden-Set result and explicit `does NOT cover` statement.

No WCVP-specific guarantee without a supplied and verified WCVP snapshot. No cross-platform real-data guarantee from Linux evidence. Identical names without disambiguating context cannot establish unique homonym identity. Manual acceptance remains a user decision. No guarantee of biological truth follows from provider agreement. No merged fix, new release or issue closure is claimed by this plan.

## Ready-to-resume state

Execution is active in `/Users/z3437171/Dropbox/Github Local/prepR4pcm/.worktrees/authority-progress-gender` on `codex/issue-139-identity`; the original dirty checkout remains untouched. Resume by checking this worktree, current main, and `.unlazy/issue139-plan/STATUS.md`. Do not execute an inherited `CHECK:` without reviewing it. The task is complete only when required gates have decisive evidence or a visible, approved abandonment; a partially completed implementation is not complete.
