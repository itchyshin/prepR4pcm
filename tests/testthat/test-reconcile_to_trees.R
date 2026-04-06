test_that("reconcile_to_trees returns named list of reconciliations", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes", "Gorilla gorilla"))
  tree1 <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")
  tree2 <- ape::read.tree(text = "(Homo_sapiens:1,Pongo_pygmaeus:1);")

  results <- reconcile_to_trees(
    df,
    trees = list(tree_a = tree1, tree_b = tree2),
    x_species = "species",
    authority = NULL,
    quiet = TRUE
  )

  expect_type(results, "list")
  expect_equal(names(results), c("tree_a", "tree_b"))
  expect_s3_class(results$tree_a, "reconciliation")
  expect_s3_class(results$tree_b, "reconciliation")

  # tree_a should match all 3 species
  expect_equal(
    sum(results$tree_a$mapping$in_x & results$tree_a$mapping$in_y, na.rm = TRUE),
    3L
  )

  # tree_b should match only Homo sapiens
  expect_equal(
    sum(results$tree_b$mapping$in_x & results$tree_b$mapping$in_y, na.rm = TRUE),
    1L
  )
})

test_that("reconcile_to_trees assigns default names", {
  df <- data.frame(species = "Homo sapiens")
  tree1 <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  results <- reconcile_to_trees(
    df,
    trees = list(tree1),
    x_species = "species",
    authority = NULL,
    quiet = TRUE
  )

  expect_equal(names(results), "tree_1")
})

test_that("reconcile_to_trees errors on invalid input", {
  expect_error(
    reconcile_to_trees("not a df", list(ape::rtree(3)),
                        authority = NULL),
    "must be a data frame"
  )

  df <- data.frame(species = "A sp")
  expect_error(
    reconcile_to_trees(df, list(), authority = NULL),
    "non-empty"
  )
})
