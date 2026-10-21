# Why the Trema suggestions disappeared

The reported loss has two mechanisms. `Trema orientalis` already had a stored suggestion to `Trema_orientale`, but ordinary flagged review inspected mapping rows rather than the separate candidate table. `Trema tomentosa` had no such suggestion in the full saved workload: `Trema_tomentosum` was already assigned by normalisation to `Trema tomentosum var. tomentosum` before fuzzy matching ran.

The repair exposes pending candidates in ordinary flagged and fuzzy review, printed counts and the summary's `review_candidates` table. It also retains plausible spelling suggestions to occupied targets, after finding a spelling pool using the existing genus-distance filter before requesting authority evidence. Such a target is labelled occupied and cannot be accepted through the review prompt until its existing assignment is resolved. Displaying a candidate does not assign it.

## Baseline and limits

The workload contains 190,263 input names and 117,934 tree tips. The replay uses the saved COL 2026 authority responses. Raw inputs, authority responses and saved result objects stay on Totoro under `/home/snakagaw/prepr137`.

| Saved comparison | Flagged mapping rows | Stored candidate pairs |
|---|---:|---:|
| Authority-enabled `results/col-cached/result.rds` | 4,006 | 0 |
| Verified main `b055018` result | 96 | 6,391 |

The first row is an explicitly limited surrogate. It is not the reporter's exact 3,944-row result, and its exact producing source revision has not been recovered. The difference in flagged rows is not by itself a count of lost useful suggestions. The raw pair-stage audit and final candidate-difference receipt distinguish stored candidates from accepted mapping rows. Tests establish the intended review and application behaviour; they do not establish taxonomic correctness for every suggested pair.

Final replay results and hashes are recorded in `VERIFICATION.json` and the after-task report. The two Trema suggestions are labelled examples of recovered review access. The rest of the changed pairs remain unadjudicated unless explicitly identified otherwise.

## Bignonia and identity checks

The cached COL 2026 response gives both `Bignonia ternata` and `Adenocalymma macrophyllum` the accepted concept `COL:64QQC`, named `Adenocalymma ternatum`. This explains why a low spelling score can coexist with an authority-based cross-genus suggestion. It does not settle agreement with the intended WCVP tree taxonomy or prove the biological identification of a record.

The original five distinct-ID examples are still excluded from automatic mapping. Synthetic tests separately exercise comparable shared IDs, conflicting IDs, missing identity, ambiguous identity and incomparable snapshots. Provider, snapshot and rank must remain comparable before IDs can establish shared identity. This repair restores review access. Automatic eligibility rules are preserved.

## Follow-up

Shinichi selected an optional botanical TNRS adapter. [TNRS-DESIGN.md](TNRS-DESIGN.md) records the design, the supplied comparison paper and acceptance checklist. Adapter implementation, automatic spelling-only acceptance, public posting and CRAN preparation remain outside this repair.
