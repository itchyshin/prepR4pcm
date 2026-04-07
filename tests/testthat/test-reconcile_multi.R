test_that("reconcile_multi works with two datasets", {
  df1 <- data.frame(species = c("A b", "C d"), val = 1:2,
                    stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("A b", "E f"), score = 1:2,
                    stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,(C_d:1,E_f:1):1);")

  result <- reconcile_multi(
    list(d1 = df1, d2 = df2), tree,
    species_cols = "species",
    authority = NULL, quiet = TRUE
  )

  expect_s3_class(result, "reconciliation")
  mapping <- reconcile_mapping(result)
  # All 3 unique species should appear in the mapping
  matched <- mapping[mapping$in_x & mapping$in_y, ]
  expect_equal(nrow(matched), 3)
})

test_that("reconcile_multi works with a single dataset", {
  df <- data.frame(species = c("A b"), val = 1,
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")

  result <- reconcile_multi(
    list(only = df), tree,
    species_cols = "species",
    authority = NULL, quiet = TRUE
  )

  expect_s3_class(result, "reconciliation")
})

test_that("reconcile_multi errors on empty datasets list", {
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_multi(list(), tree, authority = NULL, quiet = TRUE),
    "non-empty"
  )
})

test_that("reconcile_multi errors on 0-row dataset", {
  df <- data.frame(species = character(0), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_multi(list(empty = df), tree,
                    species_cols = "species",
                    authority = NULL, quiet = TRUE),
    "0 rows"
  )
})

test_that("reconcile_multi errors on mismatched species_cols length", {
  df1 <- data.frame(species = c("A b"), val = 1,
                    stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("C d"), val = 2,
                    stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_multi(list(d1 = df1, d2 = df2), tree,
                    species_cols = c("species", "species", "extra"),
                    authority = NULL, quiet = TRUE),
    "species_cols"
  )
})
