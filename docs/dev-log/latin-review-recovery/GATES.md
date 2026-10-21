# Gates: Latin review recovery, Phase A

Scope: approved active-goal execution in /private/tmp/prepr4pcm-latin-review-recovery, branch codex/latin-review-recovery, base b055018. Automatic spelling acceptance, TNRS implementation, CRAN and publication remain deferred.

Raw workload results are validated in place on Totoro; no full dataset-derived RDS downloads. Commands bind the reviewed validator, source archive, existing socket, isolated package library, input/tree hashes and recorded surrogate baseline. The saved baseline has 4006 flagged rows, not the reporter's 3944.

- [x] G0: Shinichi approves Phase A implementation and the exact isolated lane and verification scope are recorded
  EVIDENCE: Active user goal explicitly requests the review-recovery repair, base and verification scope; exact isolated checkout and scoped lease recorded in LOOP/checkpoint.md.

- [x] G1: Exact baseline or explicitly limited surrogate, inputs and authority snapshot are identified; pair-stage accounting explains the reported loss
  CHECK: ssh -S '/Users/z3437171/.ssh/cm-snakagaw@totoro.biology.ualberta.ca:22' -o ControlMaster=no -o ProxyCommand=false -o BatchMode=yes -o ConnectTimeout=10 totoro 'env OPENBLAS_NUM_THREADS=1 R_LIBS=/home/snakagaw/prepr137/review-recovery-20261021/lib-verified:/home/snakagaw/prepr137/Rlib:/home/snakagaw/prepr137/merged-Rlib REVIEW_SOURCE_SHA=74ada79de8fb95b1abb96006ed7ce20317f776316c22a52abda41df81e114c84 REVIEW_INPUT_PATH=/home/snakagaw/prepr137/data/species.Rds REVIEW_TREE_PATH=/home/snakagaw/prepr137/data/wcvp_tree.tre /usr/bin/Rscript /home/snakagaw/prepr137/review-recovery-20261021/validate-verified.R --phase diagnosis --raw /home/snakagaw/prepr137/review-recovery-20261021/raw-verified --receipt /home/snakagaw/prepr137/review-recovery-20261021/diagnosis-verified.json'
  EXPECT: DIAGNOSIS_ACCOUNTING_VERIFIED
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=2ea25e4f8a09/10 entries; output=DIAGNOSIS_ACCOUNTING_VERIFIED

- [x] G2: Both Trema variants and ordinary plausible spelling candidates appear in normal flagged review, its summary and reconcile_candidates with unknown identity, preserving alternatives
  CHECK: OPENBLAS_NUM_THREADS=1 /usr/local/bin/Rscript tools/verify-latin-review-tests.R --group recovery --manifest .unlazy/latin-review-recovery/SCENARIOS.json
  EXPECT: REVIEW_RECOVERY_TESTS_PASSED
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=2ea25e4f8a09/10 entries; output=-- Interactive identity review: 2 candidate pairs ------------------------------ | ! Dropped 5 rows with unresolved species from data

- [x] G3: Review-only candidates never apply by default or by an unintended include_flagged path; explicit overrides, duplicate targets and old objects retain defined behavior
  CHECK: OPENBLAS_NUM_THREADS=1 /usr/local/bin/Rscript tools/verify-latin-review-tests.R --group apply --manifest .unlazy/latin-review-recovery/SCENARIOS.json
  EXPECT: REVIEW_APPLY_COMPATIBILITY_PASSED
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=2ea25e4f8a09/10 entries; output=-- Interactive identity review: 2 candidate pairs ------------------------------ | ! Dropped 5 rows with unresolved species from data

- [x] G4: Five original distinct-ID cases stay excluded automatically; Bignonia evidence is traced; valid cross-genus synonym and ambiguous/incomparable-source controls remain distinct
  CHECK: ssh -S '/Users/z3437171/.ssh/cm-snakagaw@totoro.biology.ualberta.ca:22' -o ControlMaster=no -o ProxyCommand=false -o BatchMode=yes -o ConnectTimeout=10 totoro 'env OPENBLAS_NUM_THREADS=1 R_LIBS=/home/snakagaw/prepr137/review-recovery-20261021/lib-verified:/home/snakagaw/prepr137/Rlib:/home/snakagaw/prepr137/merged-Rlib REVIEW_SOURCE_SHA=74ada79de8fb95b1abb96006ed7ce20317f776316c22a52abda41df81e114c84 REVIEW_INPUT_PATH=/home/snakagaw/prepr137/data/species.Rds REVIEW_TREE_PATH=/home/snakagaw/prepr137/data/wcvp_tree.tre /usr/bin/Rscript /home/snakagaw/prepr137/review-recovery-20261021/validate-verified.R --phase identity --raw /home/snakagaw/prepr137/review-recovery-20261021/raw-verified --receipt /home/snakagaw/prepr137/review-recovery-20261021/identity-verified.json'
  EXPECT: IDENTITY_AUDIT_VERIFIED
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=2ea25e4f8a09/10 entries; output=IDENTITY_AUDIT_VERIFIED

- [x] G5: Exact-source replay accounts for all changed candidate pairs and distinguishes useful labelled recovery from count inflation; no unexplained automatic-output changes
  CHECK: ssh -S '/Users/z3437171/.ssh/cm-snakagaw@totoro.biology.ualberta.ca:22' -o ControlMaster=no -o ProxyCommand=false -o BatchMode=yes -o ConnectTimeout=10 totoro 'env OPENBLAS_NUM_THREADS=1 R_LIBS=/home/snakagaw/prepr137/review-recovery-20261021/lib-verified:/home/snakagaw/prepr137/Rlib:/home/snakagaw/prepr137/merged-Rlib REVIEW_SOURCE_SHA=74ada79de8fb95b1abb96006ed7ce20317f776316c22a52abda41df81e114c84 REVIEW_INPUT_PATH=/home/snakagaw/prepr137/data/species.Rds REVIEW_TREE_PATH=/home/snakagaw/prepr137/data/wcvp_tree.tre /usr/bin/Rscript /home/snakagaw/prepr137/review-recovery-20261021/validate-verified.R --phase replay --raw /home/snakagaw/prepr137/review-recovery-20261021/raw-verified --receipt /home/snakagaw/prepr137/review-recovery-20261021/replay-verified.json'
  EXPECT: REPLAY_ACCOUNTING_VERIFIED
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=2ea25e4f8a09/10 entries; output=REPLAY_ACCOUNTING_VERIFIED

- [x] G6: Focused and relevant package regression suite passes from the candidate source with no skipped core cases
  CHECK: OPENBLAS_NUM_THREADS=1 /usr/local/bin/Rscript tools/verify-latin-review-tests.R --group regression --manifest .unlazy/latin-review-recovery/SCENARIOS.json
  EXPECT: PACKAGE_REGRESSION_SUITE_PASSED
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/private/tmp/prepr4pcm-latin-review-recovery; path=2ea25e4f8a09/10 entries; output=v Merged 100 species (left join) | v Override applied: 'E f' -> 'G_h' (accept)

- [x] G7: Independent reviewer checks public review usability, evidence labels, compatibility and docs; negative controls prove hidden-candidate and accidental-apply failures are caught
  EVIDENCE: REVIEW.md records fresh 2 Luna + 1 Sol panel, two reproduced compatibility repairs and oracle fixes. Focused 71-assertion checks pass; hidden-candidate red logs retained; runtime accidental-apply injection fails LR-A01/LR-A02/LR-A05 with positive controls before and after. Final source core/regression receipts share the source hash; no automatic-mapping bypass found.

- [x] G8: Final source/hash receipts, fresh gate re-verification, after-task report and plan-versus-actual review support the exact completed scope; TNRS and CRAN remain deferred
  EVIDENCE: Final source/archive binding (225 exact files), shared source digest b3977d899f85059bc4c4545b92b3db245cacdbbf09562910b4f957fa4d8fcd9e, leaf re-verification and all in-place raw oracles passed. AFTER-TASK.md passed structure and closeout compiler; exact-draft assessments recorded. VERIFICATION.json, VERIFICATION-REMOTE.json, REVIEW.md and PLAN-ACTUAL.md state scope and limits. Source and metadata are ready for a local scoped commit; no publication.
