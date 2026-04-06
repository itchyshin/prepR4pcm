# data-raw/make_example_data.R
# Generates bundled example datasets for prepR4pcm from full data files.
#
# Run with: source("data-raw/make_example_data.R")
# Requires: ape, readxl (for BIRDBASE if needed)
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
  file.path(root, "data/AVONET/data/PhylogeneticData/BirdLife-BirdTree crosswalk.csv"),
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
  file.path(root, "data/AVONET/data/TraitData/AVONET1_BirdLife.csv"),
  stringsAsFactors = FALSE
)

# --- 3. Load NestTrait v2 --------------------------------------------------

nesttrait_full <- read.csv(
  file.path(root, "data/nest/NestTrait_v2.csv"),
  stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
)

# --- 4. Load Delhey plumage -------------------------------------------------

delhey_full <- read.csv(
  file.path(root, "data/Delhey2019_plumage_lightness.csv"),
  stringsAsFactors = FALSE
)

# --- 5. Load phylogenetic trees ---------------------------------------------

tree_jetz_full <- read.tree(
  file.path(root, "data/phylo_tree/birdtree_Stage2_Hackett_MCC_no_neg.tre")
)

tree_clements25_full <- read.nexus(
  file.path(root, "data/phylo_tree/clements_2025.nex")
)

# --- 6. Select 200 representative species -----------------------------------

# Strategy: pick species that appear in AVONET AND have entries in the
# crosswalk with varied match types. Include some that are in Delhey and
# NestTrait too.

# Species in AVONET
avonet_spp <- unique(avonet_full$Species1)

# Species in NestTrait
nesttrait_spp <- unique(nesttrait_full$Scientific_name)

# Species in Delhey (uses underscores in TipLabel)
delhey_spp <- gsub("_", " ", delhey_full$TipLabel)

# Species in Jetz tree tips (uses underscores)
jetz_tips <- gsub("_", " ", tree_jetz_full$tip.label)

# Species in Clements tree tips
clements_tips <- gsub("_", " ", tree_clements25_full$tip.label)

# Crosswalk species
xw_1to1 <- crosswalk_birdlife_birdtree$Species1[
  crosswalk_birdlife_birdtree$Match.type == "1BL to 1BT"
]
xw_many_to_1 <- crosswalk_birdlife_birdtree$Species1[
  crosswalk_birdlife_birdtree$Match.type == "Many BL to 1BT"
]
xw_1_to_many <- crosswalk_birdlife_birdtree$Species1[
  crosswalk_birdlife_birdtree$Match.type == "1BL to many BT"
]

# Find species present in ALL major sources
in_all <- intersect(intersect(avonet_spp, nesttrait_spp),
                    intersect(jetz_tips, delhey_spp))

# Pick 120 from in_all (good coverage), plus some edge cases
set.seed(42)
core_spp <- sample(in_all, min(120, length(in_all)))

# Add 30 species that are in AVONET + Jetz but NOT in Delhey (broader taxa)
avonet_jetz_only <- setdiff(intersect(avonet_spp, jetz_tips), delhey_spp)
extra_broad <- sample(avonet_jetz_only, min(30, length(avonet_jetz_only)))

# Add 20 species with many-to-1 crosswalk match type
m2o_candidates <- intersect(xw_many_to_1, avonet_spp)
extra_m2o <- sample(m2o_candidates, min(20, length(m2o_candidates)))

# Add 15 species with 1-to-many crosswalk match type
o2m_candidates <- intersect(xw_1_to_many, avonet_spp)
extra_o2m <- sample(o2m_candidates, min(15, length(o2m_candidates)))

# Add 15 species that are in AVONET but NOT in Jetz tree (unresolved cases)
avonet_not_jetz <- setdiff(avonet_spp, jetz_tips)
extra_unresolved <- sample(avonet_not_jetz, min(15, length(avonet_not_jetz)))

target_spp <- unique(c(core_spp, extra_broad, extra_m2o, extra_o2m,
                        extra_unresolved))

cat(sprintf("Selected %d species for example datasets\n", length(target_spp)))

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

# Delhey uses underscores and a row-number first column
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

# For Jetz tree: match with underscored names
jetz_target_tips <- gsub(" ", "_", target_spp)
jetz_keep <- intersect(tree_jetz_full$tip.label, jetz_target_tips)
tree_jetz_200 <- drop.tip(tree_jetz_full,
                           setdiff(tree_jetz_full$tip.label, jetz_keep))

cat(sprintf("tree_jetz_200: %d tips\n", Ntip(tree_jetz_200)))

# For Clements tree: match with underscored names
clements_target_tips <- gsub(" ", "_", target_spp)
clements_keep <- intersect(tree_clements25_full$tip.label, clements_target_tips)
tree_clements25_200 <- drop.tip(tree_clements25_full,
                                 setdiff(tree_clements25_full$tip.label,
                                         clements_keep))

cat(sprintf("tree_clements25_200: %d tips\n", Ntip(tree_clements25_200)))

# --- 11. Save .rda files ----------------------------------------------------

data_dir <- file.path(root, "data")

usethis::use_data(avonet_subset, overwrite = TRUE)
usethis::use_data(nesttrait_subset, overwrite = TRUE)
usethis::use_data(delhey_subset, overwrite = TRUE)
usethis::use_data(crosswalk_birdlife_birdtree, overwrite = TRUE)
usethis::use_data(tree_jetz_200, overwrite = TRUE)
usethis::use_data(tree_clements25_200, overwrite = TRUE)

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
cat(sprintf("  tree_jetz_200:                %d tips\n", Ntip(tree_jetz_200)))
cat(sprintf("  tree_clements25_200:          %d tips\n", Ntip(tree_clements25_200)))
