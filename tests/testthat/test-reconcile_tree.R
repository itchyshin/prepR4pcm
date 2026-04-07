test_that("reconcile_tree works with phylo object", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes", "Gorilla gorilla"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  expect_s3_class(result, "reconciliation")
  expect_equal(result$meta$type, "data_tree")
  # All should match via normalisation (underscores vs spaces)
  n_matched <- sum(result$mapping$in_x & result$mapping$in_y, na.rm = TRUE)
  expect_equal(n_matched, 3L)
})

test_that("reconcile_tree auto-detects species column", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, authority = NULL, quiet = TRUE)
  expect_s3_class(result, "reconciliation")
})

test_that("reconcile_tree handles underscored tips", {
  df <- data.frame(species = c("Homo sapiens"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  mapping <- reconcile_mapping(result)
  matched <- mapping[mapping$in_x & mapping$in_y, ]
  expect_equal(nrow(matched), 1L)
  expect_equal(matched$name_x, "Homo sapiens")
})

test_that("reconcile_tree records tree source in metadata", {
  df <- data.frame(species = "Homo sapiens")
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, authority = NULL, quiet = TRUE)
  expect_true(grepl("phylo", result$meta$y_source))
})

test_that("reconcile_tree errors on invalid input", {
  expect_error(
    reconcile_tree("not a df", ape::rtree(5), authority = NULL),
    "must be a data frame"
  )
})

test_that("reconcile_tree errors on 0-row data frame", {
  df_empty <- data.frame(species = character(0), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_tree(df_empty, tree, x_species = "species",
                   authority = NULL, quiet = TRUE),
    "0 rows"
  )
})

test_that("reconcile_tree errors on all-NA species column", {
  df_na <- data.frame(species = c(NA_character_, NA_character_))
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_tree(df_na, tree, x_species = "species",
                   authority = NULL, quiet = TRUE),
    "All species names.*NA"
  )
})

test_that("reconcile_tree errors on tree with duplicate tip labels", {
  df <- data.frame(species = "A b", stringsAsFactors = FALSE)
  # Construct a tree with duplicate tips manually
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  tree$tip.label <- c("A_b", "A_b")  # force duplicate
  expect_error(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE),
    "duplicate tip"
  )
})
