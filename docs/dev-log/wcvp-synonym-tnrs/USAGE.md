# Botanical review prototype

This development branch adds offline WCVP synonym candidates and an optional TNRS step inside `reconcile_tree()`. Both routes add pending suggestions. They do not change mappings or automatically accept names.

## Offline synonym review

Use your local WCVP names table and record its release explicitly. With rWCVPdata loaded, `wcvp_names` is the names table. Confirm the installed WCVP release and use its actual frozen label for your copy.

```r
library(prepR4pcm)
library(rWCVPdata)

wcvp <- list(names = wcvp_names, version = "16")

rec <- reconcile_tree(
  search, phylo, x_species = "search_name",
  authority = "col", fuzzy = TRUE,
  wcvp = wcvp
)

reconcile_candidates(rec)
rec$wcvp$input_audit
```

For an existing reconciliation, use `reconcile_wcvp(rec, wcvp)` instead. This avoids repeating ordinary matching. Input and target names must resolve to the same unambiguous accepted taxon ID and rank in the supplied table. Multiple tips for one identity remain audit-only. An occupied tip can appear as a candidate, but review must resolve its existing assignment.

The lookup preserves infraspecific ranks. A variety and its parent species are not exact identity matches. Unplaced names, missing records, homonyms and broken synonym chains remain unresolved. No fuzzy spelling search is performed by the local WCVP adapter.

Invalid, illegitimate and orthographic labels can still point explicitly to an accepted Kew taxon. Those links are retained for review, and the input and target nomenclatural statuses are shown in the candidate evidence.

`rec$wcvp` records the reference version, table fingerprint and both name inventories. Candidate columns beginning with `wcvp_` report support from the latest local lookup, even when the pair was first proposed by another source. Primary provider/ID fields retain the first candidate evidence; inspect the WCVP columns and audit for the local identity. Repeating the lookup with a changed reference supersedes unsupported pending candidates that originated from WCVP. Explicit reviewer decisions and earlier audit records are retained.

## Optional TNRS review

Install the optional TNRS package, then explicitly enable the service:

```r
rec <- reconcile_tree(
  search, phylo, x_species = "search_name",
  authority = "col", fuzzy = TRUE,
  wcvp = wcvp,
  tnrs = TRUE, tnrs_source = "wcvp"
)
reconcile_candidates(rec)
rec$tnrs$target_audit
```

The default `tnrs = FALSE` makes no TNRS request. `tnrs = TRUE` requires internet access and queries unmatched input names. Service errors are reported; they do not silently become successful no-match results. The existing manual call remains available: `reconcile_tnrs(rec, source = "wcvp", wcvp = wcvp)`.

With a local reference, returned accepted names are resolved against that table before comparison with target identities. Service IDs and local WCVP IDs are never compared directly. Service and local release labels are recorded separately. The live service reported WCVP 12 during the 7 November 2026 smoke test; that is different from the local v16 reference used for this workload. Without `wcvp`, the existing exact normalized accepted-name-to-target comparison is retained.

Inspect candidates with `reconcile_review()`. These suggestions do not enter the mapping, including when `reconcile_apply(include_flagged = TRUE)` is used. The prototype does not replace previously assigned mappings or adjudicate COL/WCVP disagreements.
