# prepR4pcm (development version)

## Documentation

* Rewrote every exported function's help page for an ecologist /
  evolutionary biologist audience (the primary users running PCM,
  PGLS, and PGLMM analyses). The previous pages read like API
  reference; the new pages explain the reconciliation workflow,
  taxadb authority choices, fuzzy matching semantics, and tree
  augmentation trade-offs in terms the audience actually uses.
* `?prepR4pcm` is now a proper package landing page with a canonical
  workflow code block, a "Key concepts" section (reconciliation
  object, four-stage cascade, provenance, splits/lumps, augmentation),
  and function-family pointers.
* Each taxadb `authority` option in `reconcile_data()` and
  `reconcile_tree()` is now glossed in one line (`"col"` = Catalogue
  of Life, `"gbif"` = GBIF Backbone, etc.) to help users choose
  without consulting the taxadb manual.
* `reconcile_augment()` gained a "When to use this" section with
  explicit cautions about reporting augmented tips, running
  sensitivity analyses, and preferring PhyloMaker / TACT for
  publication-grade augmentation.
* `reconcile_suggest()` now explains Levenshtein similarity and the
  60/40 genus/epithet weighting in plain language.
* `reconcile_mapping()` documents every column of the returned
  tibble, including when `name_resolved` is `NA` and the full
  `match_type` vocabulary.
* New help page for the `reconciliation` S3 class documenting its
  four list components (`mapping`, `meta`, `counts`, `overrides`)
  and S3 methods. This also clears previous R CMD check
  "Missing link(s)" warnings from cross-references.
* Reorganised the pkgdown reference index into seven task-oriented
  groups (Match species names / Inspect and audit / Corrections and
  crosswalks / Apply, merge, export / Augment phylogenies /
  Name utilities / Bundled example data), each with a short
  descriptive line.

## Tests

* Added a combinatorial test layer that stresses parameter
  combinations rather than single cases. Three new test files
  (`test-authority-mocked.R`, `test-workflows.R`,
  `test-robustness.R`) and parametric grid extensions to nine
  existing files take the suite from ~311 expectations to
  **1,868 expectations** across 252 `test_that()` blocks (0 failures).
* Every historical bug that has shipped — #495 cartesian merge
  explosion, the `drop_unresolved` no-op, the diacritics regex
  failure, factor coercion, silent multi-phylo handling — lived in
  parameter combinations that single-axis tests never exercised.
  The new layer tests combinations and asserts invariants
  (row counts, NA counts, tree tip counts, set membership, S3 class,
  idempotence).
* `test-authority-mocked.R` stubs `pr_lookup_authority()` via
  `local_mocked_bindings()` so the synonym-resolution branch is
  exercised without hitting taxadb or the network, covering
  accepted→synonym, synonym→accepted, neither-found, and
  network-error scenarios for `col` / `itis` / `gbif` / `ncbi`.
* `test-workflows.R` chains functions end-to-end the way real users
  do, including the #495 asymmetric pattern (750 shared /
  96 only_x / 10,400 only_y).
* `test-robustness.R` covers adversarial inputs: empty data,
  all-`NA` species columns, factor columns, Unicode (diacritics
  and Japanese kana), minimal 1-row/1-tip cases, large-input
  smoke tests, invalid types, and malformed arguments.

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
