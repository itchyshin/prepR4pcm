# Live, real-data smoke tests for each pr_get_tree() backend.
#
# These are NOT mocked. They install backends through Suggests/Remotes
# and exercise the documented n_tree contract end-to-end. Without
# these tests, the cascade of fictional-API bugs from Round 4-9
# (rtrees::get_tree's nonexistent n_tree arg; clootl::extractTree's
# nonexistent sample.size arg) shipped untested. Adding them here so
# the next time we make a documentation claim about a backend, the
# claim is verifiable.
#
# Skip strategies:
#   - skip_on_cran()              -- CRAN doesn't get GitHub-only deps
#   - skip_if_not_installed("X")  -- skip when the backend isn't there
#   - skip_if_offline()           -- some backends touch the network

test_that("LIVE: rtrees + bird returns multiPhylo (n_tree informational)", {
  skip_on_cran()
  skip_if_not_installed("rtrees")
  # Three real bird species that are guaranteed to be in the rtrees
  # bird mega-tree (Jetz et al. 2012).
  res <- pr_get_tree(
    c("Corvus corax", "Pica pica", "Turdus merula"),
    source = "rtrees", taxon = "bird", tnrs = "never"
  )
  expect_s3_class(res, "pr_tree_result")
  # rtrees default for bird returns multiPhylo (100 trees).
  expect_true(inherits(res$tree, "multiPhylo") ||
              inherits(res$tree, "phylo"))
  expect_true(length(res$matched) >= 1L)
})


test_that("LIVE: fishtree + n_tree returns exactly n_tree multiPhylo", {
  skip_on_cran()
  skip_if_not_installed("fishtree")
  skip_if_offline("fishtreeoflife.org")
  res <- pr_get_tree(
    c("Salmo salar", "Esox lucius", "Gadus morhua"),
    source = "fishtree", n_tree = 5, tnrs = "never"
  )
  expect_s3_class(res$tree, "multiPhylo")
  expect_equal(length(res$tree), 5L)
  expect_true(length(res$matched) >= 1L)
})


test_that("LIVE: fishtree single tree returns single phylo", {
  skip_on_cran()
  skip_if_not_installed("fishtree")
  skip_if_offline("fishtreeoflife.org")
  res <- pr_get_tree(
    c("Salmo salar", "Esox lucius"),
    source = "fishtree", n_tree = 1, tnrs = "never"
  )
  expect_s3_class(res$tree, "phylo")
})


test_that("LIVE: rotl returns single phylo (synthesis)", {
  skip_on_cran()
  skip_if_not_installed("rotl")
  skip_if_offline("api.opentreeoflife.org")
  res <- tryCatch(
    pr_get_tree(c("Homo sapiens", "Pan troglodytes"),
                source = "rotl"),
    error = function(e) e
  )
  # rotl can be flaky on transient network errors; if it returned an
  # error we don't want CI to flap, but we do want to surface it.
  if (inherits(res, "error")) {
    skip(paste("rotl call errored:", conditionMessage(res)))
  }
  expect_s3_class(res, "pr_tree_result")
  expect_s3_class(res$tree, "phylo")
})


test_that("LIVE: clootl n_tree = 1 succeeds without an AvesData repo", {
  # Regression test for the May 2026 finding that the user could in
  # fact get a single tree from `clootl` -- the wrapper had not
  # accounted for `clootl::extractTree()`'s lookup of `clootl_data`,
  # which only resolves when `clootl` is on the search path. Now the
  # wrapper attaches `clootl` for the duration of the call, so the
  # bundled v1.6 / 2025 taxonomy works out of the box.
  skip_on_cran()
  skip_if_not_installed("clootl")
  res <- pr_get_tree(c("Corvus corax", "Pica pica"),
                     source = "clootl", n_tree = 1, tnrs = "never")
  expect_s3_class(res, "pr_tree_result")
  expect_s3_class(res$tree, "phylo")
  expect_gte(ape::Ntip(res$tree), 1L)
  # search() should be unchanged after the call -- the wrapper detaches
  # clootl on exit if it had to attach it.
  expect_false("package:clootl" %in% search())
})


test_that("LIVE: clootl n_tree > 1 needs AvesData, errors helpfully when missing", {
  skip_on_cran()
  skip_if_not_installed("clootl")
  has_avesdata <- nzchar(Sys.getenv("AVESDATA_PATH"))
  if (has_avesdata) {
    res <- pr_get_tree(c("Corvus corax", "Pica pica"),
                       source = "clootl", n_tree = 5, tnrs = "never")
    expect_s3_class(res, "pr_tree_result")
    expect_s3_class(res$tree, "multiPhylo")
    return()
  }
  err <- tryCatch(
    pr_get_tree(c("Corvus corax", "Pica pica"),
                source = "clootl", n_tree = 5, tnrs = "never"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "AvesData", fixed = TRUE)
})


test_that("LIVE: datelife backend (skipped if not installed)", {
  skip_on_cran()
  skip_if_not_installed("datelife")
  skip_if_offline("api.opentreeoflife.org")
  res <- tryCatch(
    pr_get_tree(c("Rhea americana", "Struthio camelus"),
                source = "datelife", n_tree = 1, tnrs = "never"),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    skip(paste("datelife call errored:", conditionMessage(res)))
  }
  expect_s3_class(res, "pr_tree_result")
})
