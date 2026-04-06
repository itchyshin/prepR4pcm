# =============================================================================
# prepR4pcm Demo & Test Script
# Run this step by step in RStudio to verify everything works.
# =============================================================================

# --- Step 0: Install --------------------------------------------------------
# If you haven't installed yet:
# devtools::install()
# Or just load in-place:
devtools::load_all()

# --- Step 1: Check bundled datasets -----------------------------------------
cat("=== Bundled datasets ===\n")

data(avonet_subset)
cat(sprintf("avonet_subset:    %d rows, %d cols\n", nrow(avonet_subset), ncol(avonet_subset)))
head(avonet_subset[, 1:5])

data(nesttrait_subset)
cat(sprintf("\nnesttrait_subset: %d rows, %d cols\n", nrow(nesttrait_subset), ncol(nesttrait_subset)))
head(nesttrait_subset[, 1:5])

data(delhey_subset)
cat(sprintf("\ndelhey_subset:    %d rows, %d cols\n", nrow(delhey_subset), ncol(delhey_subset)))
head(delhey_subset)  # Note: TipLabel uses underscores

data(crosswalk_birdlife_birdtree)
cat(sprintf("\ncrosswalk:        %d rows\n", nrow(crosswalk_birdlife_birdtree)))
table(crosswalk_birdlife_birdtree$Match.type)

data(tree_jetz)
data(tree_clements25)
cat(sprintf("\ntree_jetz:       %d tips\n", ape::Ntip(tree_jetz)))
cat(sprintf("tree_clements25: %d tips\n", ape::Ntip(tree_clements25)))


# --- Step 2: Basic reconcile_tree() ----------------------------------------
cat("\n=== reconcile_tree: AVONET vs Jetz tree ===\n")

rec1 <- reconcile_tree(
  x = avonet_subset,
  tree = tree_jetz,
  x_species = "Species1",
  authority = NULL     # skip synonym lookup for speed
)
print(rec1)


# --- Step 3: Inspect the mapping -------------------------------------------
cat("\n=== Mapping table ===\n")
mapping <- reconcile_mapping(rec1)

# Show match type breakdown
table(mapping$match_type)

# Show some normalised matches
norm_matches <- mapping[mapping$match_type == "normalized", ]
if (nrow(norm_matches) > 0) {
  cat("\nNormalised matches (formatting differences):\n")
  print(norm_matches[1:min(5, nrow(norm_matches)), c("name_x", "name_y", "notes")])
}

# Show some unresolved
unresolved <- mapping[mapping$match_type == "unresolved" & mapping$in_x, ]
cat(sprintf("\nUnresolved (in data, not in tree): %d species\n", nrow(unresolved)))
if (nrow(unresolved) > 0) {
  head(unresolved$name_x, 10)
}


# --- Step 4: Detailed summary report ---------------------------------------
cat("\n=== Detailed summary ===\n")
reconcile_summary(rec1, detail = "mismatches_only")


# --- Step 5: reconcile_data (two datasets) ----------------------------------
cat("\n=== reconcile_data: AVONET vs NestTrait ===\n")

rec2 <- reconcile_data(
  x = avonet_subset,
  y = nesttrait_subset,
  x_species = "Species1",
  y_species = "Scientific_name",
  authority = NULL
)
print(rec2)


# --- Step 6: reconcile_data with Delhey (underscored names) -----------------
cat("\n=== reconcile_data: AVONET vs Delhey (underscores) ===\n")

rec3 <- reconcile_data(
  x = avonet_subset,
  y = delhey_subset,
  x_species = "Species1",
  y_species = "TipLabel",
  authority = NULL
)
print(rec3)
# Delhey uses underscores → should see many "normalized" matches


# --- Step 7: Use the crosswalk as overrides ---------------------------------
cat("\n=== Crosswalk → overrides ===\n")

overrides <- reconcile_crosswalk(
  crosswalk_birdlife_birdtree,
  from_col = "Species1",
  to_col = "Species3",
  match_type_col = "Match.type"
)
cat(sprintf("Generated %d overrides\n", nrow(overrides)))
head(overrides)


# --- Step 8: reconcile_tree with crosswalk overrides ------------------------
cat("\n=== reconcile_tree with crosswalk ===\n")

rec4 <- reconcile_tree(
  x = avonet_subset,
  tree = tree_jetz,
  x_species = "Species1",
  authority = NULL,
  overrides = overrides
)
print(rec4)

# Compare: more matches with overrides?
cat(sprintf("\nWithout crosswalk: %d matched\n",
            sum(rec1$mapping$in_x & rec1$mapping$in_y, na.rm = TRUE)))
cat(sprintf("With crosswalk:    %d matched\n",
            sum(rec4$mapping$in_x & rec4$mapping$in_y, na.rm = TRUE)))


# --- Step 9: reconcile_to_trees (1 dataset → 2 trees) ----------------------
cat("\n=== reconcile_to_trees: AVONET vs Jetz + Clements ===\n")

results <- reconcile_to_trees(
  x = avonet_subset,
  trees = list(
    jetz     = tree_jetz,
    clements = tree_clements25
  ),
  x_species = "Species1",
  authority = NULL,
  overrides = overrides
)

# Compare match rates across trees
sapply(results, function(r) {
  c(matched       = sum(r$mapping$in_x & r$mapping$in_y, na.rm = TRUE),
    unresolved_x  = r$counts$n_unresolved_x,
    unresolved_y  = r$counts$n_unresolved_y)
})


# --- Step 10: Apply and produce aligned objects -----------------------------
cat("\n=== reconcile_apply: aligned data + tree ===\n")

aligned <- reconcile_apply(
  results$jetz,
  data = avonet_subset,
  tree = tree_jetz,
  species_col = "Species1",
  drop_unresolved = TRUE
)

cat(sprintf("Aligned data: %d rows\n", nrow(aligned$data)))
cat(sprintf("Aligned tree: %d tips\n", ape::Ntip(aligned$tree)))

# Verify names match
data_spp <- aligned$data$Species1
tree_spp <- gsub("_", " ", aligned$tree$tip.label)
cat(sprintf("Names in common: %d\n", length(intersect(data_spp, tree_spp))))


# --- Step 11: Export to files -----------------------------------------------
cat("\n=== reconcile_export ===\n")

out_dir <- file.path(tempdir(), "prepR4pcm_demo")
paths <- reconcile_export(
  results$jetz,
  data = avonet_subset,
  tree = tree_jetz,
  species_col = "Species1",
  dir = out_dir,
  prefix = "avonet_jetz"
)

cat("\nFiles written:\n")
cat(sprintf("  Data:    %s (%s)\n", paths$data,
            format(file.size(paths$data), big.mark = ",")))
cat(sprintf("  Tree:    %s (%s)\n", paths$tree,
            format(file.size(paths$tree), big.mark = ",")))
cat(sprintf("  Mapping: %s (%s)\n", paths$mapping,
            format(file.size(paths$mapping), big.mark = ",")))


# --- Step 12: Manual override demo -----------------------------------------
cat("\n=== Manual override ===\n")

# Pick an unresolved species and override it
unresolved_x <- rec1$mapping[rec1$mapping$match_type == "unresolved" &
                               rec1$mapping$in_x, ]
if (nrow(unresolved_x) > 0) {
  sp_to_fix <- unresolved_x$name_x[1]
  cat(sprintf("Rejecting unresolved species: '%s'\n", sp_to_fix))

  rec1_fixed <- reconcile_override(
    rec1,
    name_x = sp_to_fix,
    action = "reject",
    note = "Not in target phylogeny; exclude from analysis"
  )
  cat(sprintf("Overrides recorded: %d\n", nrow(rec1_fixed$overrides)))
}


# --- Step 13: Fuzzy matching (Phase 2) ----------------------------------------
cat("\n=== Fuzzy matching ===\n")

# Create a dataset with deliberate typos
typo_data <- data.frame(
  species = c("Accipiter nisus", "Accipiter gentilis",
              "Falco peregrinns",   # typo: peregrinns → peregrinus
              "Turdus merula"),
  trait = rnorm(4),
  stringsAsFactors = FALSE
)
ref_data <- data.frame(
  species = c("Accipiter nisus", "Accipiter gentilis",
              "Falco peregrinus", "Turdus merula"),
  value = rnorm(4),
  stringsAsFactors = FALSE
)

rec_fuzzy <- reconcile_data(
  typo_data, ref_data,
  x_species = "species", y_species = "species",
  authority = NULL,
  fuzzy = TRUE,
  fuzzy_threshold = 0.85,
  resolve = "flag",   # flag low-confidence matches
  quiet = FALSE
)
print(rec_fuzzy)

# Check for flagged matches
mapping_fuzzy <- reconcile_mapping(rec_fuzzy)
flagged <- mapping_fuzzy[mapping_fuzzy$match_type == "flagged", ]
cat(sprintf("Flagged for review: %d\n", nrow(flagged)))
if (nrow(flagged) > 0) {
  print(flagged[, c("name_x", "name_y", "match_score", "notes")])
}

# Same with resolve = "first" — accepts all matches
rec_first <- reconcile_data(
  typo_data, ref_data,
  x_species = "species", y_species = "species",
  authority = NULL,
  fuzzy = TRUE,
  fuzzy_threshold = 0.85,
  resolve = "first",
  quiet = TRUE
)
cat(sprintf("\nWith resolve='first': %d fuzzy matches accepted\n",
            rec_first$counts$n_fuzzy))


# --- Step 14: Split/lump detection (Phase 2) ----------------------------------
cat("\n=== Split/lump detection ===\n")
cat("(Splits/lumps are detected from synonym resolution results.)\n")
cat("(Requires authority-based matching to generate synonym rows.)\n")

# Demo with a synthetic reconciliation showing a lump
sl <- reconcile_splits_lumps(rec1, quiet = FALSE)
cat(sprintf("Splits detected: %d\n", nrow(sl$splits)))
cat(sprintf("Lumps detected:  %d\n", nrow(sl$lumps)))


# --- Step 15: Tree augmentation (Phase 2) -----------------------------------
cat("\n=== Tree augmentation ===\n")

# With ~920 data species and ~660 tree tips, ~260 are unresolved.
# reconcile_augment() grafts missing species onto the tree using congeners.

aug <- reconcile_augment(rec1, tree_jetz, seed = 42)

cat(sprintf("Original tree: %d tips\n", ape::Ntip(tree_jetz)))
cat(sprintf("Augmented tree: %d tips\n", ape::Ntip(aug$tree)))
cat(sprintf("Added: %d | Skipped (no congener): %d\n",
            nrow(aug$augmented), nrow(aug$skipped)))

# What was added and where?
cat("\nFirst 10 augmented species:\n")
print(aug$augmented[1:min(10, nrow(aug$augmented)),
                    c("species", "genus", "placed_near", "branch_length", "n_congeners")])

cat("\nFirst 5 skipped species:\n")
print(aug$skipped[1:min(5, nrow(aug$skipped)), ])

# Alternative: MRCA placement with zero-length branches
aug_near <- reconcile_augment(rec1, tree_jetz,
                               where = "near",
                               branch_length = "zero",
                               seed = 42, quiet = TRUE)
cat(sprintf("\nMRCA placement: %d added, %d skipped\n",
            nrow(aug_near$augmented), nrow(aug_near$skipped)))

# Use the augmented tree for downstream analysis
aligned_aug <- reconcile_apply(
  rec1,
  data = avonet_subset,
  tree = aug$tree,
  species_col = "Species1",
  drop_unresolved = TRUE
)
cat(sprintf("\nAligned with augmented tree: %d rows, %d tips\n",
            nrow(aligned_aug$data), ape::Ntip(aligned_aug$tree)))


# --- Step 16: HTML report (Phase 3) -------------------------------------------
cat("\n=== HTML reconciliation report ===\n")

report_file <- tempfile(fileext = ".html")
reconcile_report(rec1, file = report_file, open = FALSE)

cat(sprintf("Report file exists: %s\n", file.exists(report_file)))
cat(sprintf("Report size: %s bytes\n", file.size(report_file)))

# Quick check that it contains expected content
report_content <- paste(readLines(report_file), collapse = "\n")
cat(sprintf("Contains 'Match summary': %s\n",
            grepl("Match summary", report_content, fixed = TRUE)))
cat(sprintf("Contains 'Normalized matches': %s\n",
            grepl("Normalized matches", report_content, fixed = TRUE)))

# Custom title
reconcile_report(rec1, file = report_file,
                 title = "AVONET vs Jetz Tree Reconciliation",
                 open = FALSE)
cat("Custom-titled report written successfully.\n")
unlink(report_file)


# --- Step 17: Merge two datasets (Phase 3) ------------------------------------
cat("\n=== reconcile_merge: merge AVONET + NestTrait ===\n")

merged <- reconcile_merge(
  rec2,
  data_x = avonet_subset,
  data_y = nesttrait_subset,
  species_col_x = "Species1",
  species_col_y = "Scientific_name"
)

cat(sprintf("Merged data: %d rows, %d cols\n", nrow(merged), ncol(merged)))
cat(sprintf("First column: %s\n", names(merged)[1]))
cat("\nFirst 5 rows (selected columns):\n")
print(head(merged[, c("species_resolved",
                       grep("Family|Common_name|Mass", names(merged), value = TRUE)[1:3])], 5))

# Try with Delhey (uses underscores → normalised matches)
merged_delhey <- reconcile_merge(
  rec3,
  data_x = avonet_subset,
  data_y = delhey_subset,
  species_col_x = "Species1",
  species_col_y = "TipLabel"
)
cat(sprintf("\nMerged AVONET + Delhey: %d rows, %d cols\n",
            nrow(merged_delhey), ncol(merged_delhey)))


# --- Step 18: Plot reconciliation results (Phase 4) --------------------------
cat("\n=== reconcile_plot ===\n")

# Bar chart (default)
reconcile_plot(rec1)
cat("Bar chart plotted.\n")

# Pie chart
reconcile_plot(rec1, type = "pie")
cat("Pie chart plotted.\n")


# --- Step 19: Suggest matches for unresolved species (Phase 4) ---------------
cat("\n=== reconcile_suggest ===\n")

suggestions <- reconcile_suggest(rec1, n = 3)
cat(sprintf("Suggestions generated: %d rows\n", nrow(suggestions)))
if (nrow(suggestions) > 0) {
  cat("\nTop suggestions:\n")
  print(head(suggestions, 10))
}


# --- Step 20: Compare two reconciliations (Phase 4) ---------------------------
cat("\n=== reconcile_diff ===\n")

# Compare: without crosswalk vs with crosswalk
d <- reconcile_diff(rec1, rec4)
cat(sprintf("Gained: %d | Lost: %d | Type changed: %d | Target changed: %d\n",
            nrow(d$gained), nrow(d$lost),
            nrow(d$type_changed), nrow(d$target_changed)))
if (nrow(d$gained) > 0) {
  cat("\nFirst 5 gained matches:\n")
  print(head(d$gained, 5))
}


# --- Step 21: Batch override (Phase 4) ----------------------------------------
cat("\n=== reconcile_override_batch ===\n")

# Take first 3 unresolved species and map them to arbitrary tree tips
unres <- reconcile_mapping(rec1)
unres <- unres[unres$match_type == "unresolved" & unres$in_x, ]
if (nrow(unres) >= 3) {
  batch <- data.frame(
    name_x = unres$name_x[1:3],
    name_y = tree_jetz$tip.label[1:3],
    action = "accept",
    note = "Batch override demo",
    stringsAsFactors = FALSE
  )
  rec1_batch <- reconcile_override_batch(rec1, batch, quiet = TRUE)
  cat(sprintf("After batch override: %d manual matches\n",
              rec1_batch$counts$n_manual))
}


# --- Step 22: Run the test suite ----------------------------------------------
cat("\n=== Running test suite ===\n")
devtools::test()


# --- Done! ------------------------------------------------------------------
cat("\n=== All demo steps completed successfully ===\n")
cat("Next steps:\n")
cat("  - Try with your own data: reconcile_tree(my_data, my_tree)\n")
cat("  - Use authority = 'col' for synonym resolution (requires taxadb)\n")
cat("  - Use fuzzy = TRUE to catch typos in species names\n")
cat("  - Use reconcile_augment() to add missing species to your tree\n")
cat("  - Use reconcile_splits_lumps() to detect taxonomic splits/lumps\n")
cat("  - Use reconcile_report() to generate an HTML report\n")
cat("  - Use reconcile_merge() to join reconciled datasets\n")
cat("  - Use reconcile_plot() to visualise match composition\n")
cat("  - Use reconcile_suggest() to see candidates for unresolved species\n")
cat("  - Use reconcile_diff() to compare reconciliation runs\n")
cat("  - Use reconcile_review() for interactive match auditing\n")
cat("  - Read vignette('bird-workflow', package = 'prepR4pcm')\n")
