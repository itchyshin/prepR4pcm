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
