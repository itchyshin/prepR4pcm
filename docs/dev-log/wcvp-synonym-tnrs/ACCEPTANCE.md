# Gates: botanical review matching

OWNS: R/reconcile_wcvp.R, R/reconcile_tnrs.R, R/reconcile_tree.R, tests/testthat/test-reconcile_wcvp.R, tests/testthat/test-reconcile_tree-botanical.R, man/reconcile_wcvp.Rd, man/reconcile_tnrs.Rd, man/reconcile_tree.Rd, NAMESPACE, docs/dev-log/wcvp-synonym-tnrs/**

- [x] G1: local synonym matching preserves rank, ambiguity, mappings and candidate review boundaries
  CHECK: /usr/local/bin/Rscript docs/dev-log/wcvp-synonym-tnrs/verify-focused.R
  EXPECT: BOTANICAL_FOCUSED_PASS
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/Users/z3437171/.codex/worktrees/latin-review-recovery/prepR4pcm; path=d950fa44f05e/5 entries; output=i Auto-detected species column: species | i Auto-detected species column: species

- [x] G2: full original workload is covered with unchanged mapping and independently verified exact identities
  CHECK: python3 docs/dev-log/wcvp-synonym-tnrs/verify-evidence.py /Users/z3437171/.codex/visualizations/2026/11/07/prepr139-synonym-tnrs
  EXPECT: BOTANICAL_EVIDENCE_PASS
  EVIDENCE: exit=0; shell=/bin/sh; cwd=/Users/z3437171/.codex/worktrees/latin-review-recovery/prepR4pcm; path=d950fa44f05e/5 entries; output=BOTANICAL_EVIDENCE_PASS

- [x] G3: package tests and build/check run on final source with failures surfaced
  EVIDENCE: 3386 assertions passed, zero failures; installed tests3046passed; build passes; fullcheck1ERROR2WARNING2NOTE from unchanged vignette/packaging issues recorded in REPORT; pkgdown check PASS.

- [x] G4: fresh review, unsent evidence-bound reply and pigauto issue lookup
  EVIDENCE: Fresh source and documentation reviews completed and fixes rechecked; private draft retained outside branch; GitHub API creator=ejlundgren state=all returned zero pigauto issues. No post.
