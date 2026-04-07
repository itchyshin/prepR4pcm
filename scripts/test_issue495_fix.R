# =============================================================================
# test_issue495_fix.R
#
# Manual verification of the reconcile_merge() cartesian explosion fix (#495).
# Reproduces Ayumi's scenario: small data_x, large data_y, few shared species.
# Run interactively after reinstalling prepR4pcm from the dev branch.
#
# Install first:
#   pak::local_install()
# =============================================================================

library(prepR4pcm)

cat("prepR4pcm version:", as.character(packageVersion("prepR4pcm")), "\n\n")

# =============================================================================
# Test 1: Reproduce the original bug (asymmetric left join)
# =============================================================================

cat("== Test 1: Asymmetric left join (Ayumi's scenario) ==\n\n")

# Simulate: 846 species in x, ~11,000 in y, ~750 shared
shared  <- paste("Genus", sprintf("sp%04d", 1:750))
only_x  <- paste("Xonly", sprintf("sp%04d", 1:96))
only_y  <- paste("Yonly", sprintf("sp%04d", 1:10400))

df_x <- data.frame(
  species = c(shared, only_x),
  family  = sample(c("Estrildidae", "Muscicapidae", "Trochilidae"),
                   846, replace = TRUE),
  stringsAsFactors = FALSE
)

df_y <- data.frame(
  species = c(shared, only_y),
  nest_type = sample(c("cup", "cavity", "dome"), 11150, replace = TRUE),
  stringsAsFactors = FALSE
)

cat(sprintf("  data_x: %d rows\n", nrow(df_x)))
cat(sprintf("  data_y: %d rows\n", nrow(df_y)))

rec <- reconcile_data(
  df_x, df_y,
  x_species = "species",
  y_species = "species",
  authority = NULL,
  quiet = TRUE
)

cat(sprintf("  Matched: %d\n", sum(rec$mapping$match_type != "unresolved")))
cat(sprintf("  Unresolved in x: %d\n",
            sum(rec$mapping$match_type == "unresolved" & rec$mapping$in_x)))
cat(sprintf("  Unresolved in y: %d\n\n",
            sum(rec$mapping$match_type == "unresolved" & !rec$mapping$in_x)))

# This used to crash with:
#   Error in vecseq(f__, len__, limit) :
#     Join results in X rows; more than Y = nrow(x)+nrow(i).
cat("Running reconcile_merge(how = 'left') ... ")
t0 <- Sys.time()

merged_left <- reconcile_merge(
  rec, df_x, df_y,
  species_col_x = "species",
  species_col_y = "species",
  how = "left",
  drop_unresolved = TRUE
)

elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 2)
cat(sprintf("done in %s sec\n", elapsed))
cat(sprintf("  Result: %d rows (expected: %d)\n", nrow(merged_left), nrow(df_x)))
cat(sprintf("  Matched rows: %d\n", sum(!is.na(merged_left$species_resolved))))
cat(sprintf("  Unmatched x rows (NA): %d\n\n", sum(is.na(merged_left$species_resolved))))

stopifnot(nrow(merged_left) == nrow(df_x))
cat("  PASS: left join returned correct row count\n\n")

# =============================================================================
# Test 2: Inner join (should keep only shared species)
# =============================================================================

cat("== Test 2: Inner join ==\n\n")

merged_inner <- reconcile_merge(
  rec, df_x, df_y,
  species_col_x = "species",
  species_col_y = "species",
  how = "inner"
)

cat(sprintf("  Result: %d rows (expected: 750)\n", nrow(merged_inner)))
stopifnot(nrow(merged_inner) == 750)
stopifnot(!any(is.na(merged_inner$species_resolved)))
cat("  PASS: inner join correct\n\n")

# =============================================================================
# Test 3: Full join (should keep everything, no explosion)
# =============================================================================

cat("== Test 3: Full join ==\n\n")

merged_full <- reconcile_merge(
  rec, df_x, df_y,
  species_col_x = "species",
  species_col_y = "species",
  how = "full"
)

expected_full <- 750 + 96 + 10400  # shared + x-only + y-only
cat(sprintf("  Result: %d rows (expected: %d)\n", nrow(merged_full), expected_full))
stopifnot(nrow(merged_full) == expected_full)
cat("  PASS: full join correct, no explosion\n\n")

# =============================================================================
# Test 4: Multi-row data (duplicate species warning)
# =============================================================================

cat("== Test 4: Multi-row data (male/female per species) ==\n\n")

df_multi <- data.frame(
  species = rep(c("Genus sp0001", "Genus sp0002", "Genus sp0003"), each = 2),
  sex     = rep(c("M", "F"), 3),
  mass    = c(10, 8, 20, 18, 5, 4),
  stringsAsFactors = FALSE
)

df_ref <- data.frame(
  species   = c("Genus sp0001", "Genus sp0002", "Genus sp0003"),
  nest_type = c("cup", "cavity", "dome"),
  stringsAsFactors = FALSE
)

cat(sprintf("  data_x: %d rows (%d species, 2 rows each)\n",
            nrow(df_multi), length(unique(df_multi$species))))
cat(sprintf("  data_y: %d rows\n", nrow(df_ref)))

rec_multi <- reconcile_data(
  df_multi, df_ref,
  x_species = "species",
  y_species = "species",
  authority = NULL,
  quiet = TRUE
)

# This should warn about duplicates
merged_multi <- reconcile_merge(
  rec_multi, df_multi, df_ref,
  species_col_x = "species",
  species_col_y = "species",
  how = "inner"
)

cat(sprintf("  Result: %d rows (expected: 6 = 3 species x 2 rows)\n",
            nrow(merged_multi)))
stopifnot(nrow(merged_multi) == 6)
cat("  PASS: duplicate species handled correctly with warning\n\n")

# =============================================================================
# Test 5: Use bundled data (AVONET + NestTrait)
# =============================================================================

cat("== Test 5: Bundled data — AVONET vs NestTrait ==\n\n")

data(avonet_subset)
data(nesttrait_subset)

cat(sprintf("  AVONET:    %d rows\n", nrow(avonet_subset)))
cat(sprintf("  NestTrait: %d rows\n", nrow(nesttrait_subset)))

rec_real <- reconcile_data(
  avonet_subset, nesttrait_subset,
  x_species = "Species1",
  y_species = "Scientific_name",
  authority = NULL,
  quiet = TRUE
)

n_matched <- sum(rec_real$mapping$in_x & rec_real$mapping$in_y &
                   rec_real$mapping$match_type != "unresolved")
cat(sprintf("  Matched: %d\n\n", n_matched))

merged_real <- reconcile_merge(
  rec_real, avonet_subset, nesttrait_subset,
  species_col_x = "Species1",
  species_col_y = "Scientific_name",
  how = "left"
)

cat(sprintf("  Left join: %d rows (expected: %d)\n",
            nrow(merged_real), nrow(avonet_subset)))
stopifnot(nrow(merged_real) == nrow(avonet_subset))
cat("  PASS: bundled data left join correct\n\n")

# =============================================================================
# Summary
# =============================================================================

cat("==============================================\n")
cat("All 5 tests passed. The fix is working.\n")
cat("==============================================\n")
