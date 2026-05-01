# prepR4pcm 0.3.1.9000 (development version)

## New features

* `reconcile_augment()` gains `source = c("internal", "rtrees")`
  (default `"internal"`, the existing genus-level grafting behaviour)
  and `taxon` arguments. With `source = "rtrees"`, grafting is
  delegated to `rtrees::get_tree(tree_by_user = TRUE)`, which uses
  your tree as the backbone and lets `rtrees`' taxon-specific
  reference tree place each missing species via genus / family
  information. Helpful when the genus is absent from your tree but
  present in `rtrees`' reference -- the internal mode would skip
  these. Returns the same result shape (with `meta$source` recording
  which backend was used). Refs #42 (Ayumi Mizuno).

* `pr_get_tree()` gains a fourth backend, `source = "fishtree"`,
  exposing the time-calibrated fish-only phylogeny of Rabosky et al.
  (2018, *Nature* 559:392) via the CRAN package \pkg{fishtree}.
  Returns a chronogram by default; pass `type = "phylogram"` for the
  uncalibrated version. Captures `fishtree`'s own warning text into
  `backend_meta$warnings` so the matched/unmatched report is honest.

* `pr_get_tree()` connects a reconciled species list to an external
  phylogenetic resource and returns a pruned candidate tree plus a
  matching report (matched / unmatched / source / backend metadata).
  Four backends ship:
  - `"rotl"` -- Open Tree of Life synthesis tree (universal coverage,
    via the CRAN package \pkg{rotl}).
  - `"rtrees"` -- taxon-specific mega-trees (bird, mammal, fish,
    amphibian, reptile, plant, shark/ray, bee, butterfly), via the
    GitHub package \pkg{rtrees} (`pak::pak("daijiang/rtrees")`).
  - `"clootl"` -- bird-only phylogenies in current Clements
    taxonomy, via the GitHub package \pkg{clootl}
    (`pak::pak("eliotmiller/clootl")`).
  - `"fishtree"` -- fish-only time-calibrated phylogeny, via the
    CRAN package \pkg{fishtree}.

  Accepts a reconciliation object, a character vector, or a data
  frame as input. Each backend is loaded only on demand --
  asking for a backend you don't have installed produces a helpful
  migration error with the install command. Refs #42 (Ayumi
  Mizuno).

## What's NOT in this round (deferred work)

For honesty / handoff so future contributors don't lose track:

* **#10** -- `reconcile_multi()` may undercount dataset-specific
  matches when names differ only by formatting (Round 4 candidate;
  needs root-cause debug).
* **#12** -- `reconcile_summary()` prints even when assigned to a
  variable (Round 4 candidate; small UX fix, needs `quiet` /
  `print_report` argument).
* **#14** -- Ayumi's documentation-feedback summary (Round 3).
* **#15-#21, #26** -- Per-function documentation feedback wave from
  pooherna and Sergio (Round 3; will be batched into a single
  doc-quality pass).
* **#16** -- Suggested Delhey citation correction. Requires
  verification before changing; current citation may already be
  correct.
* The remaining 22 of 37 Tier-4 defence-in-depth claim-parity tests
  outlined in the round-2 plan; they're documented but not yet
  written.
* **Round 5 work** (separate tracking issue): a unified `n_tree`
  parameter on `pr_get_tree()` so each backend can return a posterior
  sample (multiPhylo) when one is available; a new `pr_date_tree()`
  function wrapping `datelife::datelife_use()` for time-calibrating
  user topologies; a `datelife` retrieval backend on `pr_get_tree()`;
  per-tree provenance metadata; and a cross-package vignette
  documenting the prepR4pcm -> pigauto pipeline for posterior-tree
  PCMs.

## New vignette and example data

* New vignette **"Assembling mammal trait databases for phylogenetic
  comparative models"** (`db-assembly-workflow_mammals`), contributed
  by Santiago Ortega. Walks through combining three mammal trait
  sources (Amniote, PanTHERIA, TetrapodTraits), reconciling the
  unique species names against a phylogenetic tree, applying
  manual corrections, and collapsing the matched records into a
  model-ready species-level data frame aligned with a pruned tree.
  Closes #11.

* Four new bundled example datasets used by the new vignette:
  `mammal_amniote_example` (Myhrvold et al. 2015),
  `mammal_pantheria_example` (Jones et al. 2009),
  `mammal_tetrapodtraits_example` (Moura et al. 2024), and
  `mammal_tree_example` (a 5,987-tip mammal phylogeny, source to
  be confirmed with the contributor).

* `dplyr`, `readr`, and `stringr` moved from `Suggests` to `Imports`,
  reflecting that they are used routinely in the package's vignettes
  and R code rather than being optional.

## Breaking changes

* `pr_valid_authorities()` no longer lists `iucn`, `tpl`, `fb`, `slb`,
  or `wd`. Empirical testing against `taxadb` v22.12 (the database the
  package depends on) showed that `iucn` errors with a schema mismatch
  and the other four are not `taxadb` providers at all. Anyone passing
  one of these values was getting a hard error from inside `taxadb`;
  the call sites now produce a targeted migration message pointing
  users at the working authorities. (Identified in follow-up to #5,
  Ayumi Mizuno.)

  If you were passing one of the removed authorities, switch to one of
  `"col"`, `"itis"`, `"gbif"`, `"ncbi"`, or `"ott"`, or pass
  `authority = NULL` to skip synonym resolution.

## New features

* `authority = "ott"` (Open Tree of Life) is supported again, after
  Round 1 dropped it on incomplete diagnosis. The original failure
  was at `taxadb::td_create()` with the default schema set
  `c("dwc", "common")` --- `taxadb` v22.12 does not ship a `common`
  schema for OTT. We now restrict the schema to `"dwc"` (the only
  schema the cascade actually consumes), unblocking OTT and
  potentially other providers that lack a `common` schema. Re-closes
  #5 properly.

* `authority = "itis_test"` exposes `taxadb`'s small bundled testing
  dataset. Useful for examples and unit tests without a network
  round-trip.

## Bug fixes

* `pr_lookup_authority()` and `pr_ensure_db()` no longer print raw
  `{.pkg ...}` / `{.code ...}` cli template strings in their error
  messages. The functions now route errors through `cli::cli_abort()`,
  which interprets the markup. Closes #4 (Eduardo Santos).

* The matching cascade no longer prints `Stage 3/4: Synonym
  resolution (...)` when `authority = NULL`, or
  `Stage 4/4: Fuzzy matching (...)` when `fuzzy = FALSE`. Stage
  numbering is computed from the active stages only, so a call with
  `authority = NULL, fuzzy = FALSE` reports `Stage 1/2: Exact ...`
  and `Stage 2/2: Normalised ...`. Previously, the fixed `Stage X/4`
  labels suggested matches were being made at synonym/fuzzy stages
  even when they were skipped. Closes #13 (Ayumi Mizuno).

* `reconcile_tree()` and `reconcile_data()` previously dropped manual
  overrides silently when the override `name_x` was not in the data
  or `name_y` was not in the target. The reconciliation object now
  carries an `unused_overrides` slot listing every rejected override
  with a `reason` (`name_x_not_in_data`, `name_y_not_in_target`, or
  `already_matched`), and the functions emit a `cli_alert_warning()`
  pointing the user at it. `reconcile_summary()` includes a count
  and a per-row listing in the verbose section. Closes #8a
  (Ayumi Mizuno).

## New features

* `reconcile_crosswalk()` now accepts `.csv`, `.tsv`, or `.txt`
  (tab-delimited) file paths in addition to data frames. The format
  is inferred from the file extension. Closes #8b (Ayumi Mizuno).

## Documentation

* Installation instructions are standardised on `pak::pak(...)` across
  the README and the *Getting started* vignette. Closes #6
  (Ayumi Mizuno).

* The `@param authority` block in `reconcile_tree()` /
  `reconcile_data()` now reflects which authorities are actually
  supported. `tpl`, `slb`, `wd`, `iucn`, `fb` are flagged as
  experimental ("coverage and current availability vary"); `ott` is
  documented as not supported in the current default `taxadb` release.

* The `bird-workflow` vignette now guards its `caper` and `MCMCglmm`
  chunks with `eval = requireNamespace(..., quietly = TRUE)`, so the
  vignette knits cleanly for users (and CRAN check environments)
  without those Suggests packages installed.

* Added a hex sticker logo (`man/figures/logo.svg` / `logo.png`) to
  the README and the pkgdown site.

* pkgdown site rebuilt to fix stale search-index links that pointed
  to 404 pages for `reconcile_override_batch` and `reconcile_suggest`.
  Closes #7 (Ayumi Mizuno).

## Breaking changes

* The first argument of 13 exported functions has been renamed from
  `x` to `reconciliation`: `reconcile_apply()`, `reconcile_augment()`,
  `reconcile_export()`, `reconcile_mapping()`, `reconcile_merge()`,
  `reconcile_override()`, `reconcile_override_batch()`,
  `reconcile_plot()`, `reconcile_report()`, `reconcile_review()`,
  `reconcile_splits_lumps()`, `reconcile_suggest()`, and
  `reconcile_summary()`. This fixes #3 (Santiago Ortega), where
  `reconcile_apply(result = res_tree, ...)` raised an "unused
  argument" error because the parameter was named `x`. Positional
  calls (`reconcile_apply(res_tree, ...)`) continue to work
  unchanged; only code that passed the reconciliation as `x = ...`
  needs updating to `reconciliation = ...`. `reconcile_diff(x, y)`
  is intentionally unchanged — both arguments are reconciliation
  objects in a symmetric comparison, so neither is the
  "reconciliation".

## Documentation

* New vignette subsections on **multi-row species** and
  **asymmetric datasets** in the bird workflow vignette, addressing
  #1 (Ayumi Mizuno). Shows how to aggregate to species level before
  merging, how to join the mapping back to a full multi-row dataset,
  and when to pick `how = "inner"` vs `how = "left"` for focal
  study × reference database merges.
* `reconcile_merge()` help page now carries the same guidance in a
  `@details` block, covering pairwise row expansion warnings and
  the four join types.
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
