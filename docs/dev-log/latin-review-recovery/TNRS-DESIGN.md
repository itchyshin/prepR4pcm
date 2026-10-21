# Optional botanical TNRS adapter

Status: proposed follow-up after the review-recovery repair. Shinichi selected an optional botanical TNRS adapter. This note specifies its scope; it does not authorize implementation or a CRAN update.

## Design

Use the botanical Taxonomic Name Resolution Service to suggest resolutions for unresolved plant names. Keep the existing local workflow usable without the service or its optional R client. Request one explicit plant reference database per run, retain alternative results and warnings, and return suggestions for review. A result from the service is not itself permission to rename a record or assign a tree tip.

Keep two steps visible: resolving the submitted name against a taxonomic reference, and finding the corresponding name among the actual tree tips. A service result whose accepted name is absent from the tree is informative but cannot create a tip. A target already assigned to another input must be labelled for review. Identifiers from different providers cannot establish shared taxonomic identity unless their relationship is independently established.

Preserve the submitted name, returned name, accepted name, provider identifiers, reference database and version where supplied, client version, retrieval date, scores, warnings and raw results. If the service does not report a version, state that limitation. Cache results so a review can be reproduced without a fresh network call. Service errors must leave existing reconciliation results intact.

## Why the new paper helps

[Santovito et al. (2026), *Bridging biodiversity gaps: Assessing R tools for harmonising vascular plant records*](https://doi.org/10.1016/j.ecoinf.2025.103543), Ecological Informatics 93, 103543, compares seven R tools using 6,117 names from Italian vascular-plant occurrence records. TNRS with WCVP resolved 6,106 names, or 99.82% (Table 1). The authors explicitly state that they prioritised the number resolved and did not evaluate the quality of every match (Discussion).

This supports evaluating TNRS as a botanical option. It does not establish its error rate on Erick's data, justify automatic spelling-based acceptance, or show that an accepted name occurs in a given phylogeny. Different databases and matching thresholds also limit comparisons of matching algorithms alone. The paper's warning about merging distinct taxa through permissive fuzzy matching directly supports retaining separate review and application steps.

The paper also motivates a later comparison with a local plant resolver such as rWCVP. Adding several providers at once would make the first adapter harder to assess, so that comparison remains separate.

## Acceptance checklist for later implementation

- [ ] The user explicitly opts into a botanical service; ordinary reconciliation works without its client package or network access.
- [ ] One reference database is chosen explicitly and reported. Service-provided versions are preserved; missing versions are disclosed.
- [ ] Duplicate inputs, blank names, original row order, multiple alternatives, unmatched names and warnings survive round trips.
- [ ] Returned suggestions retain their scores and provenance and appear in ordinary review without becoming accepted mapping rows.
- [ ] Accepted names absent from the tree and occupied tree tips are distinguished from usable target tips.
- [ ] Unrelated provider identifiers are never compared as if they were the same authority namespace.
- [ ] Timeouts, unavailable service and partial responses preserve the original reconciliation and provide a clear retry path.
- [ ] Tests use frozen raw service responses. Live tests are optional and small; normal package checks do not depend on the service.
- [ ] A labelled evaluation reports useful suggestions, incorrect suggestions, unresolved names and actual tree-tip matches separately. Resolution rate is not labelled accuracy.
- [ ] Erick's Trema cases and the five distinct-taxon counterexamples are included. Bignonia's returned synonym chain is shown with its source and any disagreement, without asserting biological correctness from a low spelling score.
- [ ] No default automatic-acceptance rule changes. Any later proposal to change those rules requires a separate design and evaluation.

## Sources and boundaries

The supplied PDF was read directly, particularly Methods 2.2, Table 1 and the Discussion. The publisher page returned HTTP 403, so the attached article supplied the substantive evidence.

Primary implementation references for the later adapter: [official R client](https://github.com/EnquistLab/RTNRS), [service instructions](https://tnrs.biendata.org/instructions/), and [API documentation](https://tnrs.biendata.org/tnrsapi/). The botanical service is distinct from OpenTree's name-resolution function already used elsewhere in prepR4pcm.

Related: [review-recovery plan](LOOP/ultra-plan.md).
