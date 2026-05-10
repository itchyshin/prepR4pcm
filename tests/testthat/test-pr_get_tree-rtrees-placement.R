# Round 16 regression tests for rtrees placement-status table (#74).
#
# rtrees::get_tree() may graft species at higher taxonomic ranks when
# they aren't in the mega-tree exactly: genus-level (tip suffix `*`)
# or family-level (`**`). Some species fall back to "skipped" (rtrees
# tells the user inline but doesn't put them in the tree). Round 16
# surfaces this distinction so users can choose to exclude grafted
# tips from their analysis.
#
# Contract: result$backend_meta$placement is a tibble (one row per
# unique input species) with columns:
#   input_name, tree_name, placement_status
# where placement_status is one of
#   exact, genus_added, family_added, skipped, unmatched.


test_that("rtrees backend: placement table exists with the documented columns", {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  species <- c("Corvus corax", "Pica pica", "Turdus merula")
  r <- pr_get_tree(species, source = "rtrees", taxon = "bird",
                   tnrs = "never")
  expect_true(is.data.frame(r$backend_meta$placement),
              info = "placement should be a data frame")
  expect_setequal(
    names(r$backend_meta$placement),
    c("input_name", "tree_name", "placement_status")
  )
})


test_that("rtrees backend: every unique input has exactly one placement row", {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  species <- c("Corvus corax", "Pica pica", "Turdus merula", "Corvus corax")  # 1 dup
  r <- pr_get_tree(species, source = "rtrees", taxon = "bird",
                   tnrs = "never")
  placement <- r$backend_meta$placement
  expect_equal(nrow(placement), length(unique(species)),
               info = "one row per unique input species")
  expect_setequal(placement$input_name, unique(species))
})


test_that("rtrees backend: placement_status uses the documented enum", {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  species <- c("Corvus corax", "Pica pica", "Turdus merula")
  r <- pr_get_tree(species, source = "rtrees", taxon = "bird",
                   tnrs = "never")
  valid <- c("exact", "genus_added", "family_added", "skipped", "unmatched")
  expect_true(all(r$backend_meta$placement$placement_status %in% valid),
              info = "every status must be in the documented enum")
})


test_that("rtrees backend: exact-match species are flagged as 'exact', not 'genus_added'", {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  # All three species are real and should be in the bird mega-tree exactly.
  species <- c("Corvus corax", "Pica pica", "Turdus merula")
  r <- pr_get_tree(species, source = "rtrees", taxon = "bird",
                   tnrs = "never")
  status <- r$backend_meta$placement$placement_status
  # At least the three real species should resolve exactly.
  expect_true(all(status[r$backend_meta$placement$input_name %in% species] == "exact"),
              info = "real species in the mega-tree should be flagged 'exact'")
})


test_that("rtrees backend: a made-up species in a real genus is flagged 'genus_added'", {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  # Corvus is a real bird genus; 'Corvus madeupensis' isn't a real
  # species but rtrees will graft it at the genus level. Include
  # additional real species so the resulting tree has >= 2 tips
  # (rtrees errors on 1-tip results).
  species <- c("Corvus corax", "Pica pica", "Corvus madeupensis")
  r <- pr_get_tree(species, source = "rtrees", taxon = "bird",
                   tnrs = "never")
  placement <- r$backend_meta$placement
  row <- placement[placement$input_name == "Corvus madeupensis", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$placement_status, "genus_added",
               info = "made-up species in a real genus should be 'genus_added'")
})


test_that("rtrees backend: a species in no recognised family is flagged 'skipped'", {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  # "Madeupgenus" doesn't match any real family -> rtrees skips it.
  # Include >= 2 real species so the tree has >= 2 tips.
  species <- c("Corvus corax", "Pica pica", "Madeupgenus madeupspecies")
  r <- pr_get_tree(species, source = "rtrees", taxon = "bird",
                   tnrs = "never")
  placement <- r$backend_meta$placement
  row <- placement[placement$input_name == "Madeupgenus madeupspecies", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$placement_status, "skipped",
               info = "a name in no recognised family is 'skipped' by rtrees")
})


test_that("rtrees backend: skipped species appear in result$unmatched, not result$matched", {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  species <- c("Corvus corax", "Pica pica", "Madeupgenus madeupspecies")
  r <- pr_get_tree(species, source = "rtrees", taxon = "bird",
                   tnrs = "never")
  expect_true("Madeupgenus madeupspecies" %in% r$unmatched,
              info = "skipped species should be in unmatched")
  expect_false("Madeupgenus madeupspecies" %in% r$matched,
               info = "skipped species must NOT be in matched")
})
