# Goal: optional botanical TNRS review adapter

```text
Deliverable: a user-invoked adapter that enriches unresolved reconciliation names with botanical TNRS suggestions.
Outcome: only review candidates whose TNRS accepted name is present among the actual target tips are offered; no candidate is applied automatically.
Source: codex/latin-review-recovery at a7c6dd2, continuing the completed Phase A worktree.
Scope: one explicit TNRS source per call; retain raw results, source/service/client metadata, scores and warnings; no default provider changes.
Validation: frozen client-response tests plus a small, named public botanical sample against WCVP.
Stop: no issue reply, push, PR, merge, CRAN preparation/submission, full-workload query, or automatic acceptance policy change.
```

Shinichi selected an optional botanical TNRS adapter and authorized continuing local work before drafting an issue reply. The adapter is a new, isolated slice after Phase A. CRAN and external communication remain on hold.
