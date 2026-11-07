🎯 GOAL
Solo platform: Codex
Deliverable: tested WCVP synonym review candidates, optional TNRS integration, full-workload evidence and an unsent reply.
HEADLINE: resolve both input names and tree labels against one frozen WCVP reference.
IN PARALLEL: read-only replay inventory; bounded tree integration; synonym adapter implementation.
DEFER: CRAN, merging, publishing comments and automatic acceptance.
DISCIPLINE: tests before implementation; local focused checks; full replay on Totoro; exact-source receipts.

User authorization: 7 November 2026, implement and test both changes before showing a revised reply. This supersedes the earlier implementation hold. Public posting remains held.

## Prior-work sweep

- Git: `git status --short`, `git worktree list --porcelain`, `git log --all --oneline -12`, `git branch -a`, `git stash list`, `git ls-remote origin` found the existing recovery branch at af36895 and main at b055018. The canonical checkout has unrelated edits. Restored the managed clean archived worktree and branched from af36895.
- Graph: `graft map` and `graft ask 'reconcile_tnrs' --source` identify the existing optional adapter and batched candidate handling. Reuse them.
- Brain: MEMORY.md lines 623-626 and existing WCVP audit scripts/receipts identify the exact-ID gap, rank constraints and previous post. The dossier is a stub; route.py reports no LOAD-FIRST manifest. No twin package exists; pigauto is a downstream consumer, not a matching implementation.
- External: issue #139 comments confirm installed rWCVPdata v16 and request eventual integration. Existing official Kew v16 data and its recorded hashes are reused, not redownloaded. GitHub API creator query found no pigauto issues by ejlundgren.
- Ownership: lane_preflight reports no recent foreign lane or lease; this is weak evidence. PLATFORM: Codex; ON BRANCH: codex/wcvp-synonym-tnrs; scope owned here only.
- Verdict: resume existing adapter; build only frozen-WCVP review matching and explicit integration. Preserve distinct species, ranks, ambiguity, occupied targets, mappings and service provenance.

## Design

`reconcile_wcvp(rec, wcvp)` accepts `list(names = raw_names_table, version = frozen_label)` and adds review candidates sharing an unambiguous terminal accepted ID and accepted rank. It does not collapse infraspecific taxa to species. Ambiguous names and broken/cyclic synonym chains remain audit-only. Duplicate target identities remain audit-only; occupied unique tips remain visibly flagged for review. No mappings change.

`reconcile_tree(..., wcvp = NULL, tnrs = FALSE, tnrs_source = 'wcvp')` preserves current defaults. Local WCVP enrichment is optional; service use is separately explicit. `reconcile_tnrs(..., wcvp = NULL)` may resolve returned accepted names against the frozen table and use the same target identity index, preserving separate service/local release metadata. Without a local table its existing literal-tip behavior remains.

## Work and validation

1. Write regression tests for synonym targets, ambiguity, rank, cycles, occupied tips and unchanged mappings; run against af36895 to establish failures.
2. Implement local resolver with vectorized indexing and batched additions. Run focused tests including existing performance regression.
3. Integrate optional arguments and document examples. Test defaults, service errors, opt-in and equivalence with manual chaining.
4. Smoke on real WCVP names, then replay all original data/tree with saved COL lookups and frozen WCVP v16 on Totoro. Compare original mappings byte-for-byte and independently inspect exact-ID recoveries. Exercise service integration with recorded responses; separately attempt a small live R-client request with a finite timeout. Do not describe recorded/local results as live TNRS.
5. Full package tests and package check; fresh review; fix defects; record counts, runtimes, hashes and limitations. Prepare an installable local prototype. Show reply with handle and conditional pigauto reminder. Do not post.

Estimated total: 45-90 minutes. Full replay estimate: 5-15 minutes on Totoro, four threads and OPENBLAS_NUM_THREADS=1. Stop and re-estimate if exceeded. SCOUT SUITABILITY: yes, read-only replay inventory. Jev: OFF, ordinary implementation.
