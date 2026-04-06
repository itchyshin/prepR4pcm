# =============================================================================
# 01_harmonise_species_names.R
#
# Stage 1: Harmonise species names across datasets using BLrange_name
# (from range_trait_10597spp) as the canonical reference taxonomy.
#
# Inputs:
#   - data-raw/sources/range_trait_10597spp-001.csv  (reference taxonomy)
#   - data-raw/sources/nest/NestTrait_v2.csv         (nest traits)
#   - data-raw/sources/Delhey2019_plumage_lightness.csv  (plumage lightness)
#
# Outputs (in scripts/output/):
#   - harmonised_data.csv        merged dataset with all traits
#   - ref_species.csv            species-level reference table
#   - mapping_nesttrait.csv      reconciliation mapping for NestTrait
#   - mapping_delhey.csv         reconciliation mapping for Delhey
#   - report_nesttrait.html      HTML report for NestTrait reconciliation
#   - report_delhey.html         HTML report for Delhey reconciliation
# =============================================================================

library(prepR4pcm)

# -- Paths --------------------------------------------------------------------

base_dir  <- here::here("data-raw", "sources")
out_dir   <- here::here("scripts", "output")

path_range_trait <- file.path(base_dir, "range_trait_10597spp-001.csv")
path_nesttrait   <- file.path(base_dir, "nest", "NestTrait_v2.csv")
path_delhey      <- file.path(base_dir, "Delhey2019_plumage_lightness.csv")
path_crosswalk   <- file.path(base_dir, "AVONET", "data",
                               "PhylogeneticData",
                               "BirdLife-BirdTree crosswalk.csv")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =============================================================================
# Step 1: Load datasets
# =============================================================================

cat("Loading datasets...\n")

# --- Range-trait reference ---
# This file has many rows per species (spatial aggregations).
# Collapse to one row per species, keeping species-level trait columns.
range_raw <- read.csv(path_range_trait, stringsAsFactors = FALSE)

# Species-level columns (identical across aggregation rows for a given species)
species_cols <- c(
  "BLrange_name", "Avonet_name", "phylo_name",
  "Family1", "Order1", "Avibase.ID1",
  "Total.individuals",
  "Beak.Length_Culmen", "Beak.Length_Nares", "Beak.Width", "Beak.Depth",
  "Tarsus.Length", "Wing.Length", "Kipps.Distance", "Secondary1",
  "Hand.Wing.Index", "Tail.Length", "Mass",
  "Habitat", "Habitat.Density", "Avonet_Migration",
  "Trophic.Level", "Trophic.Niche", "Primary.Lifestyle"
)

ref <- range_raw[!duplicated(range_raw$BLrange_name), species_cols]
rownames(ref) <- NULL

cat(sprintf("  Reference: %d unique species (from %d rows)\n",
            nrow(ref), nrow(range_raw)))

# Free memory
rm(range_raw)

# --- NestTrait v2 ---
nesttrait <- read.csv(path_nesttrait, stringsAsFactors = FALSE)
cat(sprintf("  NestTrait: %d species\n", nrow(nesttrait)))

# --- Delhey 2019 plumage lightness ---
delhey <- read.csv(path_delhey, stringsAsFactors = FALSE)
cat(sprintf("  Delhey:    %d species\n", nrow(delhey)))

# =============================================================================
# Step 2: Reconcile NestTrait -> reference
# =============================================================================

cat("\n--- Reconciling NestTrait -> reference (BLrange_name) ---\n")

rec_nest <- reconcile_data(
  x         = nesttrait,
  y         = ref,
  x_species = "Scientific_name",
  y_species = "BLrange_name",
  authority = NULL,       # skip synonym lookup (same BirdLife taxonomy)
  fuzzy     = TRUE,       # catch typos
  fuzzy_threshold = 0.9,
  resolve   = "flag"      # flag low-confidence matches for review
)
print(rec_nest)

# Match type breakdown
map_nest <- reconcile_mapping(rec_nest)
cat("\nNestTrait match types:\n")
print(table(map_nest$match_type))

# Show suggestions for unresolved species (top 20)
cat("\nTop fuzzy suggestions for unresolved NestTrait species:\n")
sugg_nest <- reconcile_suggest(rec_nest, n = 2, threshold = 0.85)
if (nrow(sugg_nest) > 0) print(head(sugg_nest, 20))

# Save full suggestions for later review
if (nrow(sugg_nest) > 0) {
  write.csv(as.data.frame(sugg_nest),
            file.path(out_dir, "suggestions_nesttrait.csv"),
            row.names = FALSE)
}

# =============================================================================
# Step 3: Reconcile Delhey -> reference
# =============================================================================

cat("\n--- Preparing BirdTree->BirdLife crosswalk for Delhey ---\n")

# Delhey uses BirdTree/Jetz tip labels. The crosswalk maps BirdLife (Species1)
# to BirdTree (Species3). We reverse it: Species3 -> Species1, so Delhey's
# BirdTree names get mapped to BirdLife names matching our reference.
crosswalk_raw <- read.csv(path_crosswalk, stringsAsFactors = FALSE)
overrides_delhey <- reconcile_crosswalk(
  crosswalk_raw,
  from_col       = "Species3",   # BirdTree (matches Delhey TipLabel)
  to_col         = "Species1",   # BirdLife (matches reference BLrange_name)
  match_type_col = "Match.type"
)
cat(sprintf("  Crosswalk overrides for Delhey: %d\n", nrow(overrides_delhey)))

cat("\n--- Reconciling Delhey -> reference (BLrange_name) ---\n")

rec_delhey <- reconcile_data(
  x         = delhey,
  y         = ref,
  x_species = "TipLabel",
  y_species = "BLrange_name",
  authority = NULL,
  overrides = overrides_delhey,
  fuzzy     = TRUE,
  fuzzy_threshold = 0.9,
  resolve   = "flag"
)
print(rec_delhey)

map_delhey <- reconcile_mapping(rec_delhey)
cat("\nDelhey match types:\n")
print(table(map_delhey$match_type))

cat("\nTop fuzzy suggestions for unresolved Delhey species:\n")
sugg_delhey <- reconcile_suggest(rec_delhey, n = 2, threshold = 0.85)
if (nrow(sugg_delhey) > 0) print(head(sugg_delhey, 20))

# =============================================================================
# Step 4: Build harmonised dataset
# =============================================================================

cat("\n--- Building harmonised dataset ---\n")

# Extract matched mappings: name_x = source name, name_y = BLrange_name
# For each source dataset, add a species_resolved column using the mapping.

add_resolved_name <- function(data, species_col, mapping) {
  # mapping has name_x (source name) and name_y (BLrange_name)
  matched <- mapping[mapping$in_x & !is.na(mapping$name_y), ]
  lookup <- stats::setNames(matched$name_y, matched$name_x)

  # Use original names as-is for lookup (the mapping stores original names)
  src_names <- as.character(data[[species_col]])
  data$species_resolved <- unname(lookup[src_names])

  # Also add match_type for provenance
  type_lookup <- stats::setNames(matched$match_type, matched$name_x)
  data$match_type <- unname(type_lookup[src_names])

  data
}

# Add resolved names to NestTrait
nesttrait_res <- add_resolved_name(nesttrait, "Scientific_name", map_nest)

# Add resolved names to Delhey
delhey_res <- add_resolved_name(delhey, "TipLabel", map_delhey)

# Check coverage
cat(sprintf("  NestTrait: %d / %d matched to reference (%.1f%%)\n",
            sum(!is.na(nesttrait_res$species_resolved)), nrow(nesttrait_res),
            100 * mean(!is.na(nesttrait_res$species_resolved))))
cat(sprintf("  Delhey:    %d / %d matched to reference (%.1f%%)\n",
            sum(!is.na(delhey_res$species_resolved)), nrow(delhey_res),
            100 * mean(!is.na(delhey_res$species_resolved))))

# --- Merge: ref LEFT JOIN nesttrait LEFT JOIN delhey ---
# This keeps all reference species and adds trait columns where available.

# Prepare NestTrait columns for merge (prefix nest_ to avoid collisions)
nest_cols_to_merge <- setdiff(names(nesttrait_res),
                              c("species_resolved", "match_type"))
nest_merge <- nesttrait_res[!is.na(nesttrait_res$species_resolved),
                            c("species_resolved", nest_cols_to_merge)]
# Remove duplicates (keep first occurrence per resolved species)
nest_merge <- nest_merge[!duplicated(nest_merge$species_resolved), ]
names(nest_merge) <- c("species_resolved",
                        paste0("nest_", nest_cols_to_merge))
# Preserve original species name under a clear column
names(nest_merge)[names(nest_merge) == "nest_Scientific_name"] <- "NestTrait_original_name"

# Prepare Delhey columns for merge
delhey_cols_to_merge <- setdiff(names(delhey_res),
                                c("species_resolved", "match_type"))
delhey_merge <- delhey_res[!is.na(delhey_res$species_resolved),
                           c("species_resolved", delhey_cols_to_merge)]
delhey_merge <- delhey_merge[!duplicated(delhey_merge$species_resolved), ]
names(delhey_merge) <- c("species_resolved",
                          paste0("delhey_", delhey_cols_to_merge))
names(delhey_merge)[names(delhey_merge) == "delhey_TipLabel"] <- "Delhey_original_name"

# Merge
harmonised <- merge(ref, nest_merge, by.x = "BLrange_name",
                    by.y = "species_resolved", all.x = TRUE)
harmonised <- merge(harmonised, delhey_merge, by.x = "BLrange_name",
                    by.y = "species_resolved", all.x = TRUE)

# Rename for clarity
names(harmonised)[names(harmonised) == "BLrange_name"] <- "species_resolved"

cat(sprintf("\nHarmonised dataset: %d species, %d columns\n",
            nrow(harmonised), ncol(harmonised)))
cat(sprintf("  With NestTrait data: %d species\n",
            sum(!is.na(harmonised$NestTrait_original_name))))
cat(sprintf("  With Delhey data:    %d species\n",
            sum(!is.na(harmonised$Delhey_original_name))))

# =============================================================================
# Step 5: Export
# =============================================================================

cat("\n--- Exporting results ---\n")

# Harmonised data
write.csv(harmonised,
          file.path(out_dir, "harmonised_data.csv"),
          row.names = FALSE)
cat(sprintf("  Written: %s\n", file.path(out_dir, "harmonised_data.csv")))

# Reference species list
write.csv(ref,
          file.path(out_dir, "ref_species.csv"),
          row.names = FALSE)
cat(sprintf("  Written: %s\n", file.path(out_dir, "ref_species.csv")))

# Mapping tables
write.csv(as.data.frame(map_nest),
          file.path(out_dir, "mapping_nesttrait.csv"),
          row.names = FALSE)
cat(sprintf("  Written: %s\n", file.path(out_dir, "mapping_nesttrait.csv")))

write.csv(as.data.frame(map_delhey),
          file.path(out_dir, "mapping_delhey.csv"),
          row.names = FALSE)
cat(sprintf("  Written: %s\n", file.path(out_dir, "mapping_delhey.csv")))

# HTML reports
reconcile_report(rec_nest,
                 file = file.path(out_dir, "report_nesttrait.html"),
                 title = "NestTrait vs Reference Reconciliation",
                 open = FALSE)
cat(sprintf("  Written: %s\n", file.path(out_dir, "report_nesttrait.html")))

reconcile_report(rec_delhey,
                 file = file.path(out_dir, "report_delhey.html"),
                 title = "Delhey vs Reference Reconciliation",
                 open = FALSE)
cat(sprintf("  Written: %s\n", file.path(out_dir, "report_delhey.html")))

# =============================================================================
# Step 6: Visualise match results
# =============================================================================

cat("\n--- Match composition plots ---\n")

par(mfrow = c(1, 2))
reconcile_plot(rec_nest, main = "NestTrait vs Reference")
reconcile_plot(rec_delhey, main = "Delhey vs Reference")
par(mfrow = c(1, 1))

cat("\nDone. Harmonised dataset written to: scripts/output/harmonised_data.csv\n")
cat("Proceed to 02_match_to_phylogenies.R to match against trees.\n")
