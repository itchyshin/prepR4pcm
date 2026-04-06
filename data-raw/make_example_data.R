# data-raw/make_example_data.R
# Generates bundled example datasets for prepR4pcm from full data files.
#
# Run with: source("data-raw/make_example_data.R")
# Requires: ape, here, usethis
#
# Data sources and citations:
#   AVONET:    Tobias et al. (2022) Ecology Letters 25:581-597
#   NestTrait: Chia et al. (2023) Scientific Data 10:444
#   Delhey:    Delhey et al. (2019) American Naturalist 194:13-27
#   Crosswalk: Tobias et al. (2022) — BirdLife-BirdTree taxonomy crosswalk
#   Jetz tree: Jetz et al. (2012) Nature 491:444-448
#   Clements:  Clements et al. (2025) eBird/Clements Checklist v2025

library(ape)

root <- here::here()

# --- 1. Load the BirdLife-BirdTree crosswalk (full, small enough) ---------

crosswalk_raw <- read.csv(
  file.path(root, "data-raw/sources/AVONET/data/PhylogeneticData/BirdLife-BirdTree crosswalk.csv"),
  stringsAsFactors = FALSE
)

# Keep only rows with actual match types
crosswalk_birdlife_birdtree <- crosswalk_raw[
  crosswalk_raw$Match.type %in% c("1BL to 1BT", "Many BL to 1BT",
                                    "1BL to many BT", "Extinct",
                                    "Newly described species", "Invalid taxon"),
]
crosswalk_birdlife_birdtree <- crosswalk_birdlife_birdtree[
  !is.na(crosswalk_birdlife_birdtree$Species1) &
    nchar(crosswalk_birdlife_birdtree$Species1) > 0,
]

# --- 2. Load AVONET (BirdLife taxonomy) ------------------------------------

avonet_full <- read.csv(
  file.path(root, "data-raw/sources/AVONET/data/TraitData/AVONET1_BirdLife.csv"),
  stringsAsFactors = FALSE
)

# --- 3. Load NestTrait v2 --------------------------------------------------

nesttrait_full <- read.csv(
  file.path(root, "data-raw/sources/nest/NestTrait_v2.csv"),
  stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
)

# --- 4. Load Delhey plumage -------------------------------------------------

delhey_full <- read.csv(
  file.path(root, "data-raw/sources/Delhey2019_plumage_lightness.csv"),
  stringsAsFactors = FALSE
)

# --- 5. Load phylogenetic trees ---------------------------------------------

tree_jetz_full <- read.tree(
  file.path(root, "data-raw/sources/phylo_tree/birdtree_Stage2_Hackett_MCC_no_neg.tre")
)

tree_clements25_full <- read.nexus(
  file.path(root, "data-raw/sources/phylo_tree/clements_2025.nex")
)

# --- 6. Select species from a coherent clade --------------------------------

# Strategy: take ALL species from a phylogenetically coherent set of
# passerine families (Corvoidea and allies — the Australasian corvoid
# radiation). This is realistic: real comparative studies focus on clades,
# not random samples from across the tree.
#
# Selected families (12, all within the broader Corvoidea/basal oscine
# radiation):
#   Corvidae (crows, jays), Monarchidae (monarch flycatchers),
#   Campephagidae (cuckooshrikes), Laniidae (shrikes),
#   Sturnidae (starlings), Oriolidae (orioles),
#   Artamidae (woodswallows), Dicruridae (drongos),
#   Meliphagidae (honeyeaters), Maluridae (fairy-wrens),
#   Acanthizidae (thornbills), Pachycephalidae (whistlers)
#
# This gives ~920 species with:
#   - 71% in Jetz tree (260 unresolved → good for augmentation demos)
#   - 93% in Clements tree (shows tree difference)
#   - 71% in Delhey (plumage lightness data available)
#   - 100% in NestTrait (nest data available)

target_families <- c(
  "Corvidae", "Monarchidae", "Campephagidae", "Laniidae",
  "Sturnidae", "Oriolidae", "Artamidae", "Dicruridae",
  "Meliphagidae", "Maluridae", "Acanthizidae", "Pachycephalidae"
)

pass <- avonet_full[avonet_full$Order1 == "Passeriformes", ]
target_spp <- unique(pass$Species1[pass$Family1 %in% target_families])

cat(sprintf("Selected %d species from %d families (Corvoidea + allies)\n",
            length(target_spp), length(target_families)))
cat(sprintf("  Families: %s\n", paste(target_families, collapse = ", ")))

# --- 7. Subset AVONET -------------------------------------------------------

avonet_subset <- avonet_full[avonet_full$Species1 %in% target_spp,
                              c("Species1", "Family1", "Order1",
                                "Beak.Length_Culmen", "Beak.Length_Nares",
                                "Beak.Width", "Beak.Depth",
                                "Tarsus.Length", "Wing.Length", "Mass",
                                "Habitat", "Habitat.Density",
                                "Migration", "Trophic.Level",
                                "Trophic.Niche", "Primary.Lifestyle")]
rownames(avonet_subset) <- NULL

cat(sprintf("avonet_subset: %d species, %d columns\n",
            nrow(avonet_subset), ncol(avonet_subset)))

# --- 8. Subset NestTrait ----------------------------------------------------

nesttrait_subset <- nesttrait_full[nesttrait_full$Scientific_name %in% target_spp,
                                    c("Scientific_name", "Order", "Family",
                                      "Common_name",
                                      "NestSite_ground", "NestSite_tree",
                                      "NestSite_nontree", "NestSite_cliff_bank",
                                      "NestStr_scrape", "NestStr_platform",
                                      "NestStr_cup", "NestStr_dome",
                                      "NestStr_primary_cavity",
                                      "NestStr_second_cavity")]
rownames(nesttrait_subset) <- NULL

cat(sprintf("nesttrait_subset: %d species, %d columns\n",
            nrow(nesttrait_subset), ncol(nesttrait_subset)))

# --- 9. Subset Delhey -------------------------------------------------------

delhey_target <- gsub(" ", "_", target_spp)
delhey_subset <- delhey_full[delhey_full$TipLabel %in% delhey_target,
                              c("TipLabel", "family",
                                "annual_mean_temperature",
                                "annual_precipitation",
                                "lightness_male", "lightness_female")]
rownames(delhey_subset) <- NULL

cat(sprintf("delhey_subset: %d species, %d columns\n",
            nrow(delhey_subset), ncol(delhey_subset)))

# --- 10. Prune trees --------------------------------------------------------

# For Jetz tree: keep target species that have tips
jetz_target_tips <- gsub(" ", "_", target_spp)
jetz_keep <- intersect(tree_jetz_full$tip.label, jetz_target_tips)
tree_jetz <- drop.tip(tree_jetz_full,
                       setdiff(tree_jetz_full$tip.label, jetz_keep))

cat(sprintf("tree_jetz: %d tips\n", Ntip(tree_jetz)))

# For Clements tree: keep target species that have tips
clements_target_tips <- gsub(" ", "_", target_spp)
clements_keep <- intersect(tree_clements25_full$tip.label, clements_target_tips)
tree_clements25 <- drop.tip(tree_clements25_full,
                              setdiff(tree_clements25_full$tip.label,
                                      clements_keep))

cat(sprintf("tree_clements25: %d tips\n", Ntip(tree_clements25)))

# Report data-vs-tree gaps (important for augmentation demos)
n_data <- nrow(avonet_subset)
n_jetz <- Ntip(tree_jetz)
n_clem <- Ntip(tree_clements25)
cat(sprintf("\nData-tree gaps:\n"))
cat(sprintf("  AVONET (%d) vs Jetz (%d):     %d in data but not tree\n",
            n_data, n_jetz, n_data - n_jetz))
cat(sprintf("  AVONET (%d) vs Clements (%d): %d in data but not tree\n",
            n_data, n_clem, n_data - n_clem))

# --- 11. Save .rda files ----------------------------------------------------

usethis::use_data(avonet_subset, overwrite = TRUE)
usethis::use_data(nesttrait_subset, overwrite = TRUE)
usethis::use_data(delhey_subset, overwrite = TRUE)
usethis::use_data(crosswalk_birdlife_birdtree, overwrite = TRUE)
usethis::use_data(tree_jetz, overwrite = TRUE)
usethis::use_data(tree_clements25, overwrite = TRUE)

cat("\nAll example datasets saved to data/\n")

# --- Summary -----------------------------------------------------------------

cat("\n=== Summary ===\n")
cat(sprintf("  avonet_subset:                %d rows x %d cols\n",
            nrow(avonet_subset), ncol(avonet_subset)))
cat(sprintf("  nesttrait_subset:             %d rows x %d cols\n",
            nrow(nesttrait_subset), ncol(nesttrait_subset)))
cat(sprintf("  delhey_subset:                %d rows x %d cols\n",
            nrow(delhey_subset), ncol(delhey_subset)))
cat(sprintf("  crosswalk_birdlife_birdtree:  %d rows x %d cols\n",
            nrow(crosswalk_birdlife_birdtree), ncol(crosswalk_birdlife_birdtree)))
cat(sprintf("  tree_jetz:                    %d tips\n", Ntip(tree_jetz)))
cat(sprintf("  tree_clements25:              %d tips\n", Ntip(tree_clements25)))
