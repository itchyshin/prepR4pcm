test_that("reconcile_merge() produces an inner join by default", {
  data(avonet_subset, package = "prepR4pcm")
  data(nesttrait_subset, package = "prepR4pcm")

  rec <- reconcile_data(avonet_subset, nesttrait_subset,
                        x_species = "Species1",
                        y_species = "Scientific_name",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, avonet_subset, nesttrait_subset,
                            species_col_x = "Species1",
                            species_col_y = "Scientific_name")

  expect_true(is.data.frame(merged))
  expect_true("species_resolved" %in% names(merged))

  # Inner join: only matched species
  n_matched <- sum(rec$mapping$in_x & rec$mapping$in_y &
                     rec$mapping$match_type != "unresolved", na.rm = TRUE)
  expect_equal(nrow(merged), n_matched)

  # No NA in species_resolved
  expect_false(any(is.na(merged$species_resolved)))
})

test_that("reconcile_merge() handles left join", {
  df_x <- data.frame(species = c("A b", "C d", "E f"), val = 1:3,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), score = c(10, 20),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "left")

  # Left join keeps all x rows (including unresolved if drop_unresolved = FALSE)
  # With drop_unresolved = TRUE (default), "E f" has NA join key and is excluded
  # in the inner-join-like behaviour, but left join keeps it
  expect_true(is.data.frame(merged))
  expect_true("species_resolved" %in% names(merged))
})

test_that("reconcile_merge() disambiguates common columns", {
  df_x <- data.frame(species = c("A b", "C d"), value = 1:2,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), value = 3:4,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species")

  expect_true("value_x" %in% names(merged))
  expect_true("value_y" %in% names(merged))
})

test_that("reconcile_merge() uses custom suffixes", {
  df_x <- data.frame(species = c("A b"), value = 1,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b"), value = 2,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            suffix = c(".data1", ".data2"))

  expect_true("value.data1" %in% names(merged))
  expect_true("value.data2" %in% names(merged))
})

test_that("reconcile_merge() rejects non-reconciliation input", {
  expect_error(
    reconcile_merge(list(), data.frame(), data.frame()),
    "reconciliation"
  )
})

test_that("reconcile_merge() rejects non-data.frame inputs", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")

  rec <- reconcile_tree(avonet_subset, tree_jetz,
                        x_species = "Species1", authority = NULL,
                        quiet = TRUE)

  expect_error(reconcile_merge(rec, "not_a_df", data.frame()),
               "data_x.*must be a data frame")
})

test_that("reconcile_merge() species_resolved is first column", {
  df_x <- data.frame(species = c("A b", "C d"), x_val = 1:2,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), y_val = 3:4,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species")

  expect_equal(names(merged)[1], "species_resolved")
})


# --- Regression tests for NA cartesian explosion (issue #495) ---

test_that("reconcile_merge() left join does not explode with asymmetric data", {

  # Simulate Ayumi's scenario: small data_x, large data_y, few shared species
  shared  <- paste("Genus", letters[1:5])
  only_x  <- paste("Xonly", letters[1:5])
  only_y  <- paste("Yonly", letters[1:95])

  df_x <- data.frame(
    species = c(shared, only_x),
    val_x   = seq_len(10),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c(shared, only_y),
    val_y   = seq_len(100),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Left join: must return exactly nrow(df_x) rows, NOT a cartesian product
  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "left")

  expect_equal(nrow(merged), nrow(df_x))

  # 5 matched rows have non-NA species_resolved
  expect_equal(sum(!is.na(merged$species_resolved)), 5)

  # 5 unmatched x rows have NA species_resolved
  expect_equal(sum(is.na(merged$species_resolved)), 5)
})

test_that("reconcile_merge() full join does not explode with asymmetric data", {
  shared <- paste("Genus", letters[1:2])
  only_x <- paste("Xonly", letters[1:3])
  only_y <- paste("Yonly", letters[1:4])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(5),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, only_y), score = seq_len(6),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "full")

  # Full join: 2 matched + 3 x-only + 4 y-only = 9 rows
  expect_equal(nrow(merged), 9)

  # 2 matched rows have non-NA species_resolved
  expect_equal(sum(!is.na(merged$species_resolved)), 2)
})

test_that("reconcile_merge() inner join drops all unmatched", {
  shared <- paste("Genus", letters[1:3])
  only_x <- paste("Xonly", letters[1:7])
  only_y <- paste("Yonly", letters[1:10])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(10),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, only_y), score = seq_len(13),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "inner")

  expect_equal(nrow(merged), 3)
  expect_false(any(is.na(merged$species_resolved)))
})

test_that("reconcile_merge() warns about duplicate species", {
  # data_x has 2 rows per species (e.g., male + female)
  df_x <- data.frame(
    species = c("A b", "A b", "C d", "C d"),
    sex     = c("M", "F", "M", "F"),
    mass    = c(10, 8, 20, 18),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("A b", "C d"),
    score   = c(5, 9),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Should warn about duplicates and produce 4 rows (2 per species × 1 y row)
  expect_message(
    merged <- reconcile_merge(rec, df_x, df_y,
                              species_col_x = "species",
                              species_col_y = "species",
                              how = "inner"),
    "Duplicate species"
  )
  expect_equal(nrow(merged), 4)
})


# --- Tests for drop_unresolved parameter ---

test_that("drop_unresolved = TRUE removes NA species_resolved rows from left join", {
  shared <- paste("Genus", letters[1:3])
  only_x <- paste("Xonly", letters[1:4])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(7),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, "Yonly a"), score = seq_len(4),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Default (drop_unresolved = FALSE): keeps all x rows
  merged_keep <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "left",
                                 drop_unresolved = FALSE)
  expect_equal(nrow(merged_keep), 7)
  expect_equal(sum(is.na(merged_keep$species_resolved)), 4)

  # drop_unresolved = TRUE: removes unmatched x rows
  merged_drop <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "left",
                                 drop_unresolved = TRUE)
  expect_equal(nrow(merged_drop), 3)
  expect_false(any(is.na(merged_drop$species_resolved)))
})

test_that("drop_unresolved = TRUE removes NA species_resolved rows from full join", {
  shared <- paste("Genus", letters[1:2])
  only_x <- paste("Xonly", letters[1:3])
  only_y <- paste("Yonly", letters[1:4])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(5),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, only_y), score = seq_len(6),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Default: full join keeps everything
  merged_keep <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "full",
                                 drop_unresolved = FALSE)
  expect_equal(nrow(merged_keep), 9)  # 2 matched + 3 x-only + 4 y-only

  # drop_unresolved = TRUE: only matched rows survive
  merged_drop <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "full",
                                 drop_unresolved = TRUE)
  expect_equal(nrow(merged_drop), 2)
  expect_false(any(is.na(merged_drop$species_resolved)))
})

test_that("drop_unresolved has no effect on inner join", {
  df_x <- data.frame(species = c("A b", "C d", "E f"), val = 1:3,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), score = 1:2,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged_t <- reconcile_merge(rec, df_x, df_y,
                              species_col_x = "species",
                              species_col_y = "species",
                              how = "inner",
                              drop_unresolved = TRUE)
  merged_f <- reconcile_merge(rec, df_x, df_y,
                              species_col_x = "species",
                              species_col_y = "species",
                              how = "inner",
                              drop_unresolved = FALSE)
  expect_equal(nrow(merged_t), nrow(merged_f))
  expect_equal(nrow(merged_t), 2)
})
