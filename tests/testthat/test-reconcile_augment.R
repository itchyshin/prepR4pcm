# Helper: create a test tree and reconciliation
make_test_setup <- function() {
  tree <- ape::rtree(5, tip.label = c(
    "Parus_major", "Parus_caeruleus", "Corvus_corax",
    "Corvus_corone", "Falco_peregrinus"
  ))

  df <- data.frame(
    species = c("Parus major", "Parus caeruleus", "Corvus corax",
                "Corvus corone", "Falco peregrinus",
                "Parus ater",           # congener exists (Parus)
                "Corvus monedula",       # congener exists (Corvus)
                "Aquila chrysaetos"),    # no congener (Aquila)
    trait = rnorm(8),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  list(tree = tree, df = df, rec = rec)
}


test_that("reconcile_augment adds species with congeners", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  # Should have added Parus ater and Corvus monedula
  expect_true(nrow(aug$augmented) >= 2)
  expect_true("Parus ater" %in% aug$augmented$species)
  expect_true("Corvus monedula" %in% aug$augmented$species)

  # Tree should have more tips
  expect_gt(ape::Ntip(aug$tree), ape::Ntip(setup$tree))
})


test_that("reconcile_augment skips species with no congener", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  # Aquila chrysaetos has no congener
  expect_true("Aquila chrysaetos" %in% aug$skipped$species)
  expect_true(grepl("No congener", aug$skipped$reason[
    aug$skipped$species == "Aquila chrysaetos"
  ]))
})


test_that("reconcile_augment preserves original tree", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  expect_equal(ape::Ntip(aug$original), ape::Ntip(setup$tree))
  expect_equal(aug$original$tip.label, setup$tree$tip.label)
})


test_that("branch_length = 'zero' produces zero-length branches", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "zero", seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length == 0))
  }
})


test_that("branch_length = 'congener_median' produces positive lengths", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "congener_median",
                            seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length > 0))
  }
})


test_that("branch_length = 'half_terminal' produces positive lengths", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "half_terminal",
                            seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length > 0))
  }
})


test_that("where = 'near' works with multiple congeners", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            where = "near", seed = 42, quiet = TRUE)

  expect_true(nrow(aug$augmented) >= 2)
  # Parus ater placed near MRCA of Parus congeners
  parus_row <- aug$augmented[aug$augmented$species == "Parus ater", ]
  expect_true(grepl("MRCA", parus_row$placed_near))
})


test_that("seed produces reproducible results", {
  setup <- make_test_setup()
  aug1 <- reconcile_augment(setup$rec, setup$tree, seed = 123, quiet = TRUE)
  aug2 <- reconcile_augment(setup$rec, setup$tree, seed = 123, quiet = TRUE)

  expect_equal(aug1$augmented$placed_near, aug2$augmented$placed_near)
  expect_equal(ape::Ntip(aug1$tree), ape::Ntip(aug2$tree))
})


test_that("reconcile_augment handles no unresolved species", {
  tree <- ape::rtree(2, tip.label = c("Homo_sapiens", "Pan_troglodytes"))
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                   stringsAsFactors = FALSE)
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  aug <- reconcile_augment(rec, tree, quiet = TRUE)
  expect_equal(nrow(aug$augmented), 0)
  expect_equal(nrow(aug$skipped), 0)
  expect_equal(ape::Ntip(aug$tree), ape::Ntip(tree))
})


test_that("reconcile_augment validates input", {
  expect_error(reconcile_augment("not a reconciliation", ape::rtree(3)),
               "reconciliation")
})


test_that("meta contains correct counts", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  expect_equal(aug$meta$n_augmented, nrow(aug$augmented))
  expect_equal(aug$meta$n_skipped, nrow(aug$skipped))
  expect_equal(aug$meta$original_n_tips, ape::Ntip(setup$tree))
  expect_equal(aug$meta$augmented_n_tips, ape::Ntip(aug$tree))
})
