# =============================================================================
# 02_match_to_phylogenies.R
#
# Stage 2: Match the harmonised dataset against 4 phylogenetic trees
# and export aligned data + pruned tree pairs for each.
#
# Prerequisite: Run 01_harmonise_species_names.R first.
#
# Inputs:
#   - scripts/output/harmonised_data.csv  (from Script 1)
#   - data-raw/sources/phylo_tree/clements_2023.nex
#   - data-raw/sources/phylo_tree/clements_2024.nex
#   - data-raw/sources/phylo_tree/clements_2025.nex
#   - data-raw/sources/phylo_tree/birdtree_Stage2_Hackett_MCC_no_neg.tre
#   - data-raw/sources/AVONET/.../BirdLife-BirdTree crosswalk.csv
#
# Outputs (in scripts/output/):
#   Per tree: aligned data CSV, pruned tree (Nexus), mapping CSV, HTML report
#   Summary comparison table printed to console
# =============================================================================

library(prepR4pcm)
library(ape)

# -- Paths --------------------------------------------------------------------

base_dir  <- here::here("data-raw", "sources")
out_dir   <- here::here("scripts", "output")
tree_dir  <- file.path(base_dir, "phylo_tree")

path_harmonised <- file.path(out_dir, "harmonised_data.csv")
path_crosswalk  <- file.path(base_dir, "AVONET", "data",
                              "PhylogeneticData",
                              "BirdLife-BirdTree crosswalk.csv")

if (!file.exists(path_harmonised)) {
  stop("Harmonised data not found. Run 01_harmonise_species_names.R first.")
}

# =============================================================================
# Step 1: Load harmonised data
# =============================================================================

cat("Loading harmonised data...\n")
harmonised <- read.csv(path_harmonised, stringsAsFactors = FALSE)
cat(sprintf("  %d species, %d columns\n", nrow(harmonised), ncol(harmonised)))

# =============================================================================
# Step 2: Load phylogenetic trees
# =============================================================================

cat("\nLoading phylogenetic trees...\n")

tree_clements23 <- read.nexus(file.path(tree_dir, "clements_2023.nex"))
tree_clements24 <- read.nexus(file.path(tree_dir, "clements_2024.nex"))
tree_clements25 <- read.nexus(file.path(tree_dir, "clements_2025.nex"))
tree_jetz       <- read.tree(file.path(tree_dir,
                                        "birdtree_Stage2_Hackett_MCC_no_neg.tre"))

cat(sprintf("  Clements 2023: %d tips\n", Ntip(tree_clements23)))
cat(sprintf("  Clements 2024: %d tips\n", Ntip(tree_clements24)))
cat(sprintf("  Clements 2025: %d tips\n", Ntip(tree_clements25)))
cat(sprintf("  Jetz 2012:     %d tips\n", Ntip(tree_jetz)))

# =============================================================================
# Step 3: Load and prepare crosswalk for Jetz tree
# =============================================================================

cat("\nPreparing BirdLife-BirdTree crosswalk for Jetz tree...\n")

crosswalk_raw <- read.csv(path_crosswalk, stringsAsFactors = FALSE)
cat(sprintf("  Crosswalk entries: %d\n", nrow(crosswalk_raw)))
cat("  Match types:\n")
print(table(crosswalk_raw$Match.type))

overrides_jetz_all <- reconcile_crosswalk(
  crosswalk_raw,
  from_col       = "Species1",
  to_col         = "Species3",
  match_type_col = "Match.type"
)

# Filter: drop overrides whose BirdTree target (name_y) would normalise-match
# a species already in the data. Otherwise the override "steals" the tree tip
# from the natural match, displacing it as unresolved.
data_names_norm <- gsub("_", " ", trimws(tolower(harmonised$species_resolved)))
override_y_norm <- gsub("_", " ", trimws(tolower(overrides_jetz_all$name_y)))
keep <- !(override_y_norm %in% data_names_norm) |
         (gsub("_", " ", trimws(tolower(overrides_jetz_all$name_x))) == override_y_norm)
overrides_jetz <- overrides_jetz_all[keep, ]

cat(sprintf("  Overrides generated: %d (of %d; %d redundant filtered)\n",
            nrow(overrides_jetz), nrow(overrides_jetz_all),
            nrow(overrides_jetz_all) - nrow(overrides_jetz)))

# =============================================================================
# Step 4: Reconcile against Clements trees (no crosswalk needed)
# =============================================================================

cat("\n--- Reconciling against Clements trees ---\n")

rec_clements <- reconcile_to_trees(
  x         = harmonised,
  trees     = list(
    clements_2023 = tree_clements23,
    clements_2024 = tree_clements24,
    clements_2025 = tree_clements25
  ),
  x_species = "species_resolved",
  authority = NULL,
  fuzzy     = TRUE,
  fuzzy_threshold = 0.9,
  resolve   = "flag"
)

# =============================================================================
# Step 5: Reconcile against Jetz tree (with crosswalk overrides)
# =============================================================================

cat("\n--- Reconciling against Jetz 2012 tree ---\n")

# With crosswalk
rec_jetz <- reconcile_tree(
  x         = harmonised,
  tree      = tree_jetz,
  x_species = "species_resolved",
  authority = NULL,
  overrides = overrides_jetz,
  fuzzy     = TRUE,
  fuzzy_threshold = 0.9,
  resolve   = "flag"
)

# Also run without crosswalk for comparison
rec_jetz_nocw <- reconcile_tree(
  x         = harmonised,
  tree      = tree_jetz,
  x_species = "species_resolved",
  authority = NULL,
  fuzzy     = TRUE,
  fuzzy_threshold = 0.9,
  resolve   = "flag",
  quiet     = TRUE
)

# Compare with vs without crosswalk
cat("\n--- Effect of crosswalk on Jetz matching ---\n")
diff_jetz <- reconcile_diff(rec_jetz_nocw, rec_jetz)

# Combine all results
all_results <- c(rec_clements, list(jetz_2012 = rec_jetz))

# =============================================================================
# Step 6: Summary comparison across trees
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY: Match results across phylogenies\n")
cat(strrep("=", 70), "\n\n")

summary_table <- data.frame(
  tree = names(all_results),
  total_tips = sapply(all_results, function(r) r$counts$n_y),
  matched    = sapply(all_results, function(r) {
    sum(r$mapping$in_x & r$mapping$in_y, na.rm = TRUE)
  }),
  exact      = sapply(all_results, function(r) r$counts$n_exact),
  normalized = sapply(all_results, function(r) r$counts$n_normalized),
  fuzzy      = sapply(all_results, function(r) r$counts$n_fuzzy),
  flagged    = sapply(all_results, function(r) {
    sum(r$mapping$match_type == "flagged", na.rm = TRUE)
  }),
  manual     = sapply(all_results, function(r) r$counts$n_manual),
  unresolved_data = sapply(all_results, function(r) r$counts$n_unresolved_x),
  unresolved_tree = sapply(all_results, function(r) r$counts$n_unresolved_y),
  stringsAsFactors = FALSE
)
rownames(summary_table) <- NULL
print(summary_table)

cat(sprintf("\nJetz WITHOUT crosswalk: %d matched\n",
            sum(rec_jetz_nocw$mapping$in_x & rec_jetz_nocw$mapping$in_y,
                na.rm = TRUE)))
cat(sprintf("Jetz WITH crosswalk:    %d matched (+%d)\n",
            sum(rec_jetz$mapping$in_x & rec_jetz$mapping$in_y, na.rm = TRUE),
            sum(rec_jetz$mapping$in_x & rec_jetz$mapping$in_y, na.rm = TRUE) -
              sum(rec_jetz_nocw$mapping$in_x & rec_jetz_nocw$mapping$in_y,
                  na.rm = TRUE)))

# =============================================================================
# Step 7: Generate aligned outputs for each tree
# =============================================================================

cat("\n--- Generating aligned data + tree pairs ---\n")

trees_list <- list(
  clements_2023 = tree_clements23,
  clements_2024 = tree_clements24,
  clements_2025 = tree_clements25,
  jetz_2012     = tree_jetz
)

for (tree_name in names(all_results)) {
  cat(sprintf("\n  [%s]\n", tree_name))

  rec   <- all_results[[tree_name]]
  tree  <- trees_list[[tree_name]]

  # Aligned output (drop unresolved)
  aligned <- reconcile_apply(
    rec,
    data = harmonised,
    tree = tree,
    species_col = "species_resolved",
    drop_unresolved = TRUE
  )

  cat(sprintf("    Aligned: %d species in data, %d tips in tree\n",
              nrow(aligned$data), Ntip(aligned$tree)))

  # Export aligned data
  write.csv(aligned$data,
            file.path(out_dir, paste0(tree_name, "_aligned_data.csv")),
            row.names = FALSE)

  # Export pruned tree
  write.nexus(aligned$tree,
              file = file.path(out_dir, paste0(tree_name, "_aligned_tree.nex")))

  # Export mapping
  write.csv(as.data.frame(reconcile_mapping(rec)),
            file.path(out_dir, paste0(tree_name, "_mapping.csv")),
            row.names = FALSE)

  # HTML report
  reconcile_report(
    rec,
    file  = file.path(out_dir, paste0(tree_name, "_report.html")),
    title = paste("Reconciliation:", tree_name),
    open  = FALSE
  )

  cat(sprintf("    Exported: aligned data, tree, mapping, report\n"))
}

# =============================================================================
# Step 8: Show unresolved species for manual inspection
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("UNRESOLVED SPECIES (in data but not matched to any tree)\n")
cat(strrep("=", 70), "\n\n")

# Species unresolved in ALL trees
unresolved_all <- Reduce(intersect, lapply(all_results, function(r) {
  m <- r$mapping
  m$name_x[m$match_type == "unresolved" & m$in_x & !is.na(m$name_x)]
}))

cat(sprintf("Species unresolved in ALL %d trees: %d\n",
            length(all_results), length(unresolved_all)))
if (length(unresolved_all) > 0 && length(unresolved_all) <= 50) {
  cat(paste("  ", sort(unresolved_all), collapse = "\n"), "\n")
} else if (length(unresolved_all) > 50) {
  cat(paste("  ", sort(unresolved_all)[1:50], collapse = "\n"), "\n")
  cat(sprintf("  ... and %d more\n", length(unresolved_all) - 50))
}

# Suggest fixes for unresolved species (using Jetz as example)
cat("\nFuzzy suggestions for unresolved species (Jetz tree):\n")
sugg <- reconcile_suggest(rec_jetz, n = 2, threshold = 0.85)
if (nrow(sugg) > 0) print(head(sugg, 30))

# =============================================================================
# Step 9: Export summary table
# =============================================================================

write.csv(summary_table,
          file.path(out_dir, "tree_match_summary.csv"),
          row.names = FALSE)
cat(sprintf("\nSummary table written to: %s\n",
            file.path(out_dir, "tree_match_summary.csv")))

# =============================================================================
# Done
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("DONE. All outputs in: scripts/output/\n")
cat(strrep("=", 70), "\n\n")

cat("Files per tree:\n")
cat("  <tree>_aligned_data.csv  - dataset aligned to tree tip labels\n")
cat("  <tree>_aligned_tree.nex  - pruned tree (matched species only)\n")
cat("  <tree>_mapping.csv       - full reconciliation mapping\n")
cat("  <tree>_report.html       - HTML report of matching decisions\n")
cat("\nAlso: tree_match_summary.csv  - comparison across all trees\n")
cat("\nNext steps:\n")
cat("  1. Review mapping CSVs for flagged/unresolved species\n")
cat("  2. Open HTML reports for detailed match documentation\n")
cat("  3. Use aligned data + tree pairs for PGLS / PGLMM analyses\n")
