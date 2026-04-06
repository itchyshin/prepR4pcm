
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prepR4pcm

<!-- badges: start -->

[![R-CMD-check](https://github.com/itchyshin/prepR4pcm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/itchyshin/prepR4pcm/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**prepR4pcm** reconciles species names across datasets and phylogenetic
trees for comparative biology workflows. Taxonomic mismatches —
formatting differences, synonymy, and typos — are a persistent source of
silent data loss in phylogenetic comparative methods (PCM). This package
detects and resolves those mismatches through a multi-stage matching
cascade, producing documented, reproducible alignments between data and
trees.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("itchyshin/prepR4pcm")
```

## Quick example

``` r
library(prepR4pcm)
library(ape)

# Reconcile a dataset against a phylogenetic tree
rec <- reconcile_tree(
  x         = avonet_subset,
  tree      = tree_jetz,
  x_species = "Species1",
  fuzzy     = TRUE,
  resolve   = "flag"
)
#> ℹ Reconciling 919 data names vs 657 tree tips
#> ℹ Matching 919 x 657 names through 4 stages...
#> ℹ Stage 1/4: Exact matching...
#> ℹ Stage 2/4: Normalised matching (0 matched so far)...
#> ℹ Stage 3/4: Synonym resolution (657 matched so far)...
#> ℹ Stage 4/4: Fuzzy matching (657 matched so far)...
#> ✔ Matched 657/919 data names to tree tips
rec
#> 
#> ── Reconciliation: data vs tree ────────────────────────────────────────────────
#>   Source x: avonet_subset
#>   Source y: phylo (657 tips)
#>   Authority: col
#>   Timestamp: 2026-04-06 15:46:03
#> ℹ Match coverage: [█████████████████████░░░░░░░░░] 71% (657/919)
#> 
#> ── Match summary ──
#> 
#> • Exact: 0 ( 0.0%)
#> • Normalized: 657 (71.5%)
#> • Synonym: 0 ( 0.0%)
#> • Fuzzy: 0 ( 0.0%)
#> • Manual: 0 ( 0.0%)
#> ! Unresolved (x only):262 (28.5%)
#> ! Unresolved (y only):0
#> ! Flagged for review: 0
#> ℹ Use `reconcile_summary()` for details, `reconcile_mapping()` for the full table.

# Apply the reconciliation: aligned data + pruned tree
aligned <- reconcile_apply(rec, data = avonet_subset, tree = tree_jetz,
                           species_col = "Species1", drop_unresolved = TRUE)
#> ! Dropped 262 rows with unresolved species from data
#> ℹ Tree has 657 tips after alignment
nrow(aligned$data)
#> [1] 657
ape::Ntip(aligned$tree)
#> [1] 657
```

## What it does

**Core matching** — A four-stage cascade resolves names progressively:

1.  **Exact** — identical strings
2.  **Normalised** — case, whitespace, and underscore differences
3.  **Synonym** — taxonomic synonym lookup via local databases
    (optional)
4.  **Fuzzy** — Levenshtein-based similarity with genus pre-filtering
    (~100x faster than naive pairwise comparison)

**Diagnostics** — every decision is recorded in a mapping table.
Functions like `reconcile_summary()`, `reconcile_plot()`,
`reconcile_report()`, and `reconcile_suggest()` make it easy to audit
matches, visualise coverage, and find near-misses for unresolved
species.

**Multi-tree support** — `reconcile_to_trees()` matches a dataset
against several trees at once, and `reconcile_diff()` compares results
across trees or parameter settings.

**Crosswalks and overrides** — `reconcile_crosswalk()` converts
published taxonomy crosswalks (e.g., BirdLife–BirdTree) into override
tables. `reconcile_override()` and `reconcile_override_batch()` let you
inject manual corrections at any stage.

**Advanced** — `reconcile_augment()` grafts unresolved species onto a
tree using genus-level placement. `reconcile_splits_lumps()` detects
taxonomic splits and lumps from synonym resolution results.

## Typical workflow

    Load data + tree
           |
      reconcile_tree()
           |
      Review: reconcile_summary(), reconcile_plot(), reconcile_report()
           |
      Fix: reconcile_override(), reconcile_suggest()
           |
      reconcile_apply()
           |
      Aligned data + pruned tree --> PGLS / PGLMM

## Vignettes

- [Getting
  Started](https://itchyshin.github.io/prepR4pcm/docs/articles/getting-started.html)
  ([source](vignettes/getting-started.Rmd)) — core concepts and a
  minimal worked example
- [Bird Trait
  Workflow](https://itchyshin.github.io/prepR4pcm/docs/articles/bird-workflow.html)
  ([source](vignettes/bird-workflow.Rmd)) — a realistic multi-dataset,
  multi-tree analysis pipeline

## Citation

If you use prepR4pcm in your research, please cite:

``` r
citation("prepR4pcm")
```

## License

MIT
