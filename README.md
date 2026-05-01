
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prepR4pcm <img src="man/figures/logo.png" align="right" height="139" alt="prepR4pcm logo" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Species names in your data rarely match the tip labels in your
phylogenetic tree exactly. Such mismatches prevent you from combining
species traits (data tables) with their evolutionary relationships (the
phylogenetic tree) — which is what phylogenetic comparative methods
(PCMs, e.g. studies of trait evolution, niche conservatism, or
correlated trait change) need. Three kinds of mismatch silently drop
species from these analyses:

- **Formatting differences**, e.g. `Homo_sapiens` vs `Homo sapiens`,
  trailing whitespace, capitalisation
- **Taxonomic synonyms / different ranks**, e.g. `Homo sapiens` vs
  `Homo sapiens sapiens`, or a recent name vs the historical synonym
  used in the tree
- **Simple typos**, e.g. `Homo sapiens` vs `Hamo sapiens`

**prepR4pcm** detects and resolves all three through a multi-stage
matching cascade (exact → normalised → synonym → fuzzy), documents every
decision so the choices are auditable, and produces aligned data–tree
pairs ready for phylogenetic generalised least squares (PGLS),
phylogenetic mixed models (PGLMMs), or any other PCM.

Below you’ll find: how to install, a quick example, the typical
workflow, vignettes covering realistic pipelines, citation information,
and a list of the bundled example datasets.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("itchyshin/prepR4pcm")
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

## Quick example

This minimal example reconciles a small bird trait dataset
(`avonet_subset`, a sample of AVONET) against a small bird phylogeny
(`tree_jetz`, a sample of the Jetz et al. 2012 phylogeny) and produces a
model-ready aligned data frame plus a pruned tree.

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
#>   Timestamp: 2026-05-01 14:18:01
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

What just happened: `reconcile_tree()` matched every species name in
`avonet_subset$Species1` against the tip labels of `tree_jetz`, trying
exact matches first and falling back through normalised, synonym, and
fuzzy matches as needed. The printed `rec` object shows the count in
each match category. `reconcile_apply()` then takes that reconciliation
and produces (a) a data frame with rows restricted to species that
resolved to a tree tip, and (b) the tree pruned to those tips. The
`nrow()` and `ape::Ntip()` calls confirm the two sides agree on species
count — the precondition for any downstream PGLS or PGLMM call.

## Vignettes

- [Getting
  Started](https://itchyshin.github.io/prepR4pcm/articles/getting-started.html)
  ([source](vignettes/getting-started.Rmd)) — core concepts and a
  minimal worked example
- [Bird Trait
  Workflow](https://itchyshin.github.io/prepR4pcm/articles/bird-workflow.html)
  ([source](vignettes/bird-workflow.Rmd)) — a realistic multi-dataset,
  multi-tree analysis pipeline ending in PGLS and phylogenetic GLMM fits
- [Mammal Database-Assembly
  Workflow](https://itchyshin.github.io/prepR4pcm/articles/db-assembly-workflow_mammals.html)
  ([source](vignettes/db-assembly-workflow_mammals.Rmd)) — assembling a
  trait database from three sources (Amniote, PanTHERIA,
  TetrapodTraits), reconciling species names against a mammal phylogeny,
  and producing a model-ready species-level data frame

## Citation

If you use prepR4pcm in your research, please cite the package and the
original publication for any bundled example dataset you used (see
*Bundled data sources* below).

For the package itself:

> Nakagawa S, Ortega S, Mizuno A, Santos E, Lagisz M, Celeste J, Poo
> Hernandez S (2026). *prepR4pcm: Prepare Data and Trees for
> Phylogenetic Comparative Methods.* R package version 0.4.0.
> <https://github.com/itchyshin/prepR4pcm>

BibTeX:

``` bibtex
@Manual{,
  title  = {prepR4pcm: Prepare Data and Trees for Phylogenetic Comparative Methods},
  author = {Shinichi Nakagawa and Santiago Ortega and Ayumi Mizuno and
            Eduardo S.A. Santos and Malgorzata Lagisz and Jimuel Jr Celeste and
            Sergio {Poo Hernandez}},
  year   = {2026},
  note   = {R package version 0.4.0},
  url    = {https://github.com/itchyshin/prepR4pcm},
}
```

Or run in R to get the same entry programmatically:

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

The package ships small sample datasets — each is a *subset* (a few
hundred rows or tips) of a larger published dataset, used only for the
package’s examples, vignettes, and tests. They are not full versions: if
you want to *do science* with these data, download the full original
dataset from the source listed below. If you use any of these examples
in published work, please cite the original provider.

**Bird data (used by the bird-workflow vignette):**

- **AVONET** (`avonet_subset`): Tobias et al. (2022) *Ecology Letters*
  25:581–597. [doi:10.1111/ele.13898](https://doi.org/10.1111/ele.13898)
- **NestTrait v2** (`nesttrait_subset`): Chia et al. (2023) *Scientific
  Data* 10:923.
  [doi:10.1038/s41597-023-02837-1](https://doi.org/10.1038/s41597-023-02837-1)
- **Plumage lightness** (`delhey_subset`): Delhey et al. (2019) *Ecology
  Letters* 22:726–736.
  [doi:10.1111/ele.13233](https://doi.org/10.1111/ele.13233)
- **Jetz phylogeny** (`tree_jetz`): Jetz et al. (2012) *Nature*
  491:444–448.
  [doi:10.1038/nature11631](https://doi.org/10.1038/nature11631)
- **Clements checklist** (`tree_clements25`): Clements et al. (2025)
  eBird/Clements Checklist of Birds of the World, v2025
  (<https://www.birds.cornell.edu/clementschecklist/>).
- **BirdLife-BirdTree crosswalk** (`crosswalk_birdlife_birdtree`):
  distributed with AVONET (Tobias et al. 2022,
  [doi:10.1111/ele.13898](https://doi.org/10.1111/ele.13898)); maps
  BirdLife taxonomy to the BirdTree (Jetz et al. 2012,
  [doi:10.1038/nature11631](https://doi.org/10.1038/nature11631))
  taxonomy.

**Mammal data (used by the mammal database-assembly vignette):**

- **Amniote life-history** (`mammal_amniote_example`): Myhrvold et
  al. (2015) *Ecology* 96:3109.
  [doi:10.1890/15-0846R.1](https://doi.org/10.1890/15-0846R.1)
- **PanTHERIA** (`mammal_pantheria_example`): Jones et al. (2009)
  *Ecology* 90:2648.
  [doi:10.1890/08-1494.1](https://doi.org/10.1890/08-1494.1)
- **TetrapodTraits** (`mammal_tetrapodtraits_example`): Moura et al.
  2024) *PLOS Biology* 22:e3002658.
        [doi:10.1371/journal.pbio.3002658](https://doi.org/10.1371/journal.pbio.3002658)
- **Mammal phylogeny** (`mammal_tree_example`): provenance pending
  confirmation; see [issue
  \#11](https://github.com/itchyshin/prepR4pcm/issues/11). Bundled as an
  example object only — if you need a published mammal phylogeny for
  analysis, see Upham et al. 2019, Faurby & Svenning 2015, or
  Bininda-Emonds et al. 2007 (full citations in `?mammal_tree_example`).

## License

MIT
