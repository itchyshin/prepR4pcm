# Independent review record

The final milestone used two fresh Luna reviewers and one fresh Sol reviewer under distinct lenses: user guidance, evidence oracles and application safety. These were read-only native subagents. This record summarises their returned findings; it does not claim human taxonomic adjudication or release review.

| Finding | Evidence and response | Disposition |
|---|---|---|
| Broad authority lookup for occupied targets | LR-R05 failed when an unrelated exact-matched target entered the lookup inventory. The first species-score prepass fixed the inventory but the full replay hit its 300-second limit. The final code reuses the conservative genus-distance filter before authority lookup and calculates species scores once. | Fixed; focused test passes. |
| Suspected missing authority-free prompt | Added a direct prompt assertion. It passed before code changes; R's indexed assignment expands the NULL ID vector to NA values. The reviewer retracted the finding. | False positive, regression assertion retained. |
| Missing legacy review columns hide alternatives after rejection | LR-A04 reproduced a single prompt and NA statuses. Initialising the stored candidate table before filtering preserves the remaining alternative. | Fixed; independent Sol reviewer reran the focused file successfully. |
| Occupied-target identity source says not queried | LR-R05 reproduced candidate col@2026 evidence with incomplete result provenance. The review lookup now calls the existing source-version recorder. | Fixed; source version and nonempty coverage assertions pass. |
| Replay oracle accepts an empty added set | Validator now requires at least one recovered pair and the named unknown-identity Trema tomentosa pair. A no-recovery negative control aliases new and comparison results. | Fixed; remote negative control passed. |
| Full-data distinct-ID oracle checks absence only | Validator now requires all five original pairs to exist in the candidate table and be conflict-labelled, while absent from automatic mapping. It explicitly requires the identity-state column. | Fixed; final raw validation passed. |

The documentation reviewer found no guidance blocker. The safety reviewer found no automatic-mapping bypass and closed both reproduced compatibility findings after repair. The evidence reviewer confirmed the substantive oracle fixes and identified the missing-column edge case, which the parent closed with an explicit schema assertion before freezing the final archive.

The final genus helper extracts the existing genus-distance predicate without changing it: length filter, conservative `agrep`, then full-string edit distance at most two. The parent checked the extraction against the prior loop. Package regression tests and the final whole-workload replay provide the remaining behavioural checks.

Scope limits: no independent biological classification of all changed pairs, no live botanical TNRS submissions, no release readiness assessment. Missing Pandoc and unavailable OpenTree endpoints limit broader documentation checks.
