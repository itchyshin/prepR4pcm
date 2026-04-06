# prepR4pcm 0.3.0

## New features

* `reconcile_report()` generates a self-contained HTML report documenting
  all name-matching decisions — suitable for sharing or archiving.
* `reconcile_merge()` joins two reconciled datasets into a single
  analysis-ready data frame using the mapping table from `reconcile_data()`.
* `reconcile_augment()` grafts unresolved species onto a tree using
  genus-level placement (sister to congener or MRCA of congeners).
* `reconcile_splits_lumps()` detects taxonomic splits and lumps from
  synonym resolution results.
* Fuzzy matching via `fuzzy = TRUE` catches likely typos using
  component-based Levenshtein similarity.
* `resolve = "flag"` marks low-confidence matches for manual review.
* `reconcile_plot()` visualises match composition as a bar chart or pie
  chart using base R graphics.
* `reconcile_suggest()` shows the closest fuzzy candidates for each
  unresolved species — useful for finding near-misses.
* `reconcile_diff()` compares two reconciliation objects and reports
  gained/lost matches, type changes, and target changes.
* `reconcile_override_batch()` applies multiple overrides at once from a
  data frame or CSV file.
* `reconcile_review()` provides an interactive console interface for
  accepting or rejecting flagged and fuzzy matches one at a time.
* `print.reconciliation()` now shows a coverage bar:
  `[████████████████████░░░░░░░░░░] 71% (657/919)`.
* Stage-level progress messages for large datasets (> 500 species) in
  the matching cascade.

## Data

* Example datasets expanded from ~200 to ~920 species (Corvoidea +
  allied passerine families) for realistic demonstrations.
* All `\dontrun{}` examples replaced with runnable examples using
  bundled data.

## Performance

* Fuzzy matching (`pr_fuzzy_match()`) now uses genus pre-filtering: only
  species whose genus is within 2 edits are compared. This reduces
  computation from O(n×m) to roughly O(n×k) where k is the number of
  congeners+near-genera, giving ~100× speedup on large datasets
  (e.g., 1360×6504 from >10 min to ~3 sec).
* `reconcile_suggest()` uses the same genus pre-filter and vectorised
  `adist()`, making it usable for 1000+ unresolved species.

## Bug fixes

* **Fixed crosswalk overrides having no effect.** The cascade's override
  pre-stage required exact string matches between override names and
  input names. When override names used spaces but tree tips used
  underscores (or vice versa), no overrides were applied. Overrides
  now normalise both sides before comparison.
* Fixed `reconcile_plot()` error when passing `main` argument: the
  internal `pr_plot_bar()` hardcoded `main` and also passed `...`,
  causing a duplicate argument error.
* Fixed `.Rbuildignore` patterns that excluded `.rda` data files on
  case-insensitive filesystems.

# prepR4pcm 0.2.0

* Core reconciliation engine: exact → normalised → synonym → fuzzy cascade.
* `reconcile_tree()`, `reconcile_data()`, `reconcile_trees()`.
* `reconcile_apply()`, `reconcile_export()`, `reconcile_override()`.
* `reconcile_to_trees()`, `reconcile_multi()`, `reconcile_crosswalk()`.
* `reconcile_summary()`, `reconcile_mapping()`.
* Bundled datasets: avonet_subset, nesttrait_subset, delhey_subset,
  crosswalk_birdlife_birdtree, tree_jetz, tree_clements25.
* Bird workflow vignette.
