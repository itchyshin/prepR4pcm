test_that("reconcile_suggest returns tibble with correct columns", {
  df <- data.frame(species = c("Homo sapiens", "Parus mejor", "Unknown species"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Parus_major:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 3, quiet = TRUE)
  expect_s3_class(suggestions, "tbl_df")
  expect_true(all(c("unresolved", "suggestion", "score") %in% names(suggestions)))
})

test_that("reconcile_suggest returns at most n suggestions per unresolved", {
  df <- data.frame(species = c("Homo sapiens", "Parus mejor", "Zappa confluentus"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,(Parus_major:0.5,Parus_minor:0.5):0.5):1,Gorilla_gorilla:2);"
  )

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 2, threshold = 0.3, quiet = TRUE)

  # Check no unresolved name has more than n suggestions
  counts <- table(suggestions$unresolved)
  expect_true(all(counts <= 2))
})

test_that("reconcile_suggest scores are between 0 and 1", {
  df <- data.frame(species = c("Homo sapiens", "Parus mejor"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Parus_major:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 3, threshold = 0.3, quiet = TRUE)

  if (nrow(suggestions) > 0) {
    expect_true(all(suggestions$score >= 0 & suggestions$score <= 1))
  }
})

test_that("reconcile_suggest works with bundled data", {
  data(avonet_subset)
  data(tree_jetz)
  result <- reconcile_tree(avonet_subset, tree_jetz,
                            x_species = "Species1", authority = NULL,
                            quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 2, quiet = TRUE)
  expect_s3_class(suggestions, "tbl_df")
  expect_true(all(c("unresolved", "suggestion", "score") %in% names(suggestions)))
})

test_that("reconcile_suggest returns empty tibble when no unresolved species", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 3, quiet = TRUE)
  expect_s3_class(suggestions, "tbl_df")
  expect_equal(nrow(suggestions), 0)
  expect_true(all(c("unresolved", "suggestion", "score") %in% names(suggestions)))
})

test_that("reconcile_suggest respects threshold", {
  df <- data.frame(species = c("Abc xyz"))
  tree <- ape::read.tree(text = "(Mno_pqr:1,Stu_vwx:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # Very high threshold should exclude all

  suggestions <- reconcile_suggest(result, n = 3, threshold = 0.99, quiet = TRUE)
  expect_equal(nrow(suggestions), 0)
})

test_that("reconcile_suggest errors on non-reconciliation input", {
  expect_error(
    reconcile_suggest(list(a = 1)),
    "reconciliation"
  )
})
