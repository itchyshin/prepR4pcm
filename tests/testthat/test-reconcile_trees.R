test_that("reconcile_trees finds overlapping tips", {
  tree1 <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
  tree2 <- ape::read.tree(text = "((B:1,C:1):1,D:2);")

  result <- reconcile_trees(tree1, tree2, authority = NULL, quiet = TRUE)

  expect_s3_class(result, "reconciliation")
  expect_equal(result$meta$type, "tree_tree")

  matched <- result$mapping[result$mapping$in_x & result$mapping$in_y, ]
  expect_equal(nrow(matched), 2L)  # B and C
  expect_true(all(c("B", "C") %in% matched$name_x))
})

test_that("reconcile_trees handles no overlap", {
  tree1 <- ape::read.tree(text = "(A:1,B:1);")
  tree2 <- ape::read.tree(text = "(C:1,D:1);")

  result <- reconcile_trees(tree1, tree2, authority = NULL, quiet = TRUE)

  matched <- result$mapping[result$mapping$in_x & result$mapping$in_y, ]
  expect_equal(nrow(matched), 0L)
  expect_equal(result$counts$n_unresolved_x, 2L)
  expect_equal(result$counts$n_unresolved_y, 2L)
})

test_that("reconcile_trees matches normalised tips", {
  tree1 <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  tree2 <- ape::read.tree(text = "(Homo_sapiens:1,Gorilla_gorilla:1);")

  result <- reconcile_trees(tree1, tree2, authority = NULL, quiet = TRUE)

  matched <- result$mapping[result$mapping$in_x & result$mapping$in_y, ]
  expect_equal(nrow(matched), 1L)
  expect_equal(matched$name_x, "Homo_sapiens")
})
