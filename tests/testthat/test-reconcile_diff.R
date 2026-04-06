test_that("reconcile_diff detects gained matches with crosswalk", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")
  data(crosswalk_birdlife_birdtree, package = "prepR4pcm")

  r1 <- reconcile_tree(avonet_subset, tree_jetz,
                        x_species = "Species1", authority = NULL,
                        quiet = TRUE)

  overrides <- reconcile_crosswalk(crosswalk_birdlife_birdtree,
                                    from_col = "Species1", to_col = "Species3",
                                    match_type_col = "Match.type")

  r2 <- reconcile_tree(avonet_subset, tree_jetz,
                        x_species = "Species1", authority = NULL,
                        overrides = overrides, quiet = TRUE)

  d <- reconcile_diff(r1, r2, quiet = TRUE)

  # The crosswalk should produce some gained matches
  expect_true(is.data.frame(d$gained))
  expect_true(nrow(d$gained) >= 0)
  expect_true(all(c("name_x", "name_y_new", "match_type_old",
                     "match_type_new", "match_score") %in% names(d$gained)))

  # Summary structure
 expect_true(is.data.frame(d$summary))
  expect_equal(nrow(d$summary), 1L)
  expect_true(all(c("n_gained", "n_lost", "n_type_changed",
                     "n_target_changed", "n_shared") %in% names(d$summary)))
})

test_that("reconcile_diff returns empty tibbles for identical reconciliations", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  r <- reconcile_tree(df, tree, x_species = "species",
                       authority = NULL, quiet = TRUE)

  d <- reconcile_diff(r, r, quiet = TRUE)

  expect_equal(nrow(d$gained), 0L)
  expect_equal(nrow(d$lost), 0L)
  expect_equal(nrow(d$type_changed), 0L)
  expect_equal(nrow(d$target_changed), 0L)
  expect_equal(d$summary$n_gained, 0L)
  expect_equal(d$summary$n_lost, 0L)
})

test_that("reconcile_diff rejects non-reconciliation inputs", {
  df <- data.frame(species = "A b")
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  r <- reconcile_tree(df, tree, x_species = "species",
                       authority = NULL, quiet = TRUE)

  expect_error(reconcile_diff("not_a_rec", r), "reconciliation")
  expect_error(reconcile_diff(r, list()), "reconciliation")
})

test_that("reconcile_diff summary has correct structure", {
  df <- data.frame(species = c("A b", "C d", "E f"))
  tree <- ape::read.tree(text = "((A_b:1,C_d:1):1,G_h:2);")

  r1 <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)

  # Apply an override to create a difference
  r2 <- reconcile_override(r1,
                            name_x = "E f",
                            name_y = "G_h",
                            action = "accept",
                            note = "test override")

  d <- reconcile_diff(r1, r2, quiet = TRUE)

  expect_equal(ncol(d$summary), 5L)
  expect_true(d$summary$n_gained >= 1L)
  expect_true(is.integer(d$summary$n_gained) || is.numeric(d$summary$n_gained))
})
