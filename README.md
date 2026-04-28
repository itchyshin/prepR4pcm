
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prepR4pcm <img src="man/figures/logo.png" align="right" height="139" alt="prepR4pcm logo" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Species names in your data rarely match the tip labels in your tree.
Formatting differences (`Homo_sapiens` vs `Homo sapiens`), taxonomic
synonyms, and simple typos silently drop species from phylogenetic
comparative analyses. **prepR4pcm** detects and resolves these
mismatches through a multi-stage matching cascade (exact, normalised,
synonym, fuzzy), documents every decision, and produces aligned
data–tree pairs ready for PGLS, phylogenetic mixed models, or any other
PCM.

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
#>   Timestamp: 2026-04-24 15:30:00
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

## Features

- **Four-stage matching cascade**: exact match, normalised match (case,
  whitespace, underscores), synonym resolution via
  [taxadb](https://docs.ropensci.org/taxadb/) (Norman et al. 2020), and
  fuzzy matching with genus pre-filtering for typos.
- **Full provenance**: every name-matching decision is recorded.
  `reconcile_summary()`, `reconcile_plot()`, `reconcile_report()`, and
  `reconcile_suggest()` help you audit matches and find near-misses.
- **Multi-tree support**: `reconcile_to_trees()` matches a dataset
  against several trees at once; `reconcile_diff()` compares results.
- **Crosswalks and overrides**: `reconcile_crosswalk()` converts
  published taxonomy crosswalks (e.g., BirdLife–BirdTree) into override
  tables.
- **Tree augmentation**: `reconcile_augment()` grafts unresolved species
  onto a tree using genus-level placement (always run sensitivity
  analyses with and without augmented tips).

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

If you use prepR4pcm in your research, please cite the package:

``` r
citation("prepR4pcm")
```

## Key dependencies

- [ape](https://cran.r-project.org/package=ape) — phylogenetic tree
  handling (Paradis & Schliep 2019, *Bioinformatics* 35:526–528)
- [taxadb](https://docs.ropensci.org/taxadb/) — local taxonomic synonym
  resolution (Norman et al. 2020, *Methods in Ecology and Evolution*
  11:1153–1159)

## Bundled data sources

The package includes subset datasets for examples and testing. Full
credit to the original data providers:

- **AVONET**: Tobias et al. (2022) *Ecology Letters* 25:581–597.
  [doi:10.1111/ele.13898](https://doi.org/10.1111/ele.13898)
- **NestTrait v2**: Chia et al. (2023) *Scientific Data* 10:923.
  [doi:10.1038/s41597-023-02837-1](https://doi.org/10.1038/s41597-023-02837-1)
- **Plumage lightness**: Delhey et al. (2019) *American Naturalist*
  194:13–27. [doi:10.1086/703588](https://doi.org/10.1086/703588)
- **Jetz phylogeny**: Jetz et al. (2012) *Nature* 491:444–448.
  [doi:10.1038/nature11631](https://doi.org/10.1038/nature11631)
- **Clements checklist**: Clements et al. (2025) eBird/Clements
  Checklist of Birds of the World.
- **BirdLife-BirdTree crosswalk**: Tobias et al. (2022).

## License

MIT
