test_that("reconcile_apply drops unresolved species", {
  df <- data.frame(
    species = c("Homo sapiens", "Missing species"),
    mass = c(70, 100)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, data = df, tree = tree,
                              species_col = "species",
                              drop_unresolved = TRUE)

  expect_equal(nrow(aligned$data), 1L)
  expect_equal(aligned$data$species, "Homo sapiens")
  expect_equal(ape::Ntip(aligned$tree), 1L)
})

test_that("reconcile_apply keeps all when drop_unresolved = FALSE", {
  df <- data.frame(
    species = c("Homo sapiens", "Missing species"),
    mass = c(70, 100)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, data = df, tree = tree,
                              species_col = "species",
                              drop_unresolved = FALSE)

  expect_equal(nrow(aligned$data), 2L)
  expect_equal(ape::Ntip(aligned$tree), 2L)
})

test_that("reconcile_apply works with data only", {
  df <- data.frame(species = c("Homo sapiens"), mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, data = df, species_col = "species",
                              drop_unresolved = TRUE)

  expect_equal(nrow(aligned$data), 1L)
  expect_null(aligned$tree)
})

test_that("reconcile_apply works with tree only", {
  df <- data.frame(species = c("Homo sapiens"), mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, tree = tree, drop_unresolved = TRUE)

  expect_null(aligned$data)
  expect_s3_class(aligned$tree, "phylo")
})
