# pr_get_tree() — pluggable tree retrieval (issue #42).
#
# We don't want the test suite to depend on three external backends
# (rotl on CRAN, clootl + rtrees on GitHub) being installed AND the
# Open Tree of Life servers being reachable. So we mock each backend's
# entry-point function. The tests verify:
#
#   1. Input dispatch: character vector, data frame, reconciliation
#      object, NULL/empty -- each handled correctly.
#   2. Backend dispatch: source = "rotl" / "rtrees" / "clootl" calls
#      the right underlying function.
#   3. Helpful errors: missing backend package; rtrees without taxon;
#      empty species list.
#   4. Result shape: matched + unmatched + tree + source +
#      backend_meta + class("pr_tree_result").
#   5. Print method: works without error and emits expected fields.

mini_phylo <- function(tip_labels) {
  ape::read.tree(
    text = paste0(
      "(",
      paste(tip_labels, collapse = ","),
      ");"
    )
  )
}


# 1. Input dispatch -----------------------------------------------------

test_that("character-vector input is deduplicated and NA-cleaned", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species, unmatched = character(),
           backend_meta = list(n_queried = length(species)))
    },
    .package = "prepR4pcm"
  )

  res <- pr_get_tree(c("Homo sapiens", NA, "Homo sapiens", "Pan troglodytes"),
                    source = "rotl")
  expect_s3_class(res, "pr_tree_result")
  expect_equal(sort(res$matched), c("Homo sapiens", "Pan troglodytes"))
  expect_equal(res$source, "rotl")
})


test_that("data-frame input pulls the species column (autodetected)", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species, unmatched = character(),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )

  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  res <- pr_get_tree(df, source = "rotl")
  expect_equal(sort(res$matched), c("Homo sapiens", "Pan troglodytes"))
})


test_that("data-frame input respects species_col", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species, unmatched = character(),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )

  df <- data.frame(scientific = c("Homo sapiens", "Pan troglodytes"))
  res <- pr_get_tree(df, source = "rotl", species_col = "scientific")
  expect_length(res$matched, 2)
})


test_that("reconciliation input pulls name_y and ignores species_col", {
  rec <- reconcile_data(
    data.frame(species = c("Homo sapiens", "Pan troglodytes")),
    data.frame(species = c("Homo sapiens", "Pan troglodytes")),
    x_species = "species", y_species = "species",
    authority = NULL, quiet = TRUE
  )

  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species, unmatched = character(),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )

  expect_message(
    res <- pr_get_tree(rec, source = "rotl",
                      species_col = "ignored_with_a_warning"),
    "ignored"
  )
  expect_equal(sort(res$matched), c("Homo sapiens", "Pan troglodytes"))
})


test_that("empty species list errors with a helpful message", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) stop("should not run"),
    .package = "prepR4pcm"
  )

  expect_error(pr_get_tree(character(0), source = "rotl"),
               "No species names")
  expect_error(pr_get_tree(NA_character_, source = "rotl"),
               "No species names")
})


test_that("invalid input class errors with a helpful message", {
  expect_error(
    pr_get_tree(123L, source = "rotl"),
    "must be"
  )
})


# 2. Backend dispatch ---------------------------------------------------

test_that("source = 'rotl' calls the rotl backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      called <<- TRUE
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("foo", source = "rotl")
  expect_true(called)
})

test_that("source = 'clootl' calls the clootl backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_clootl = function(species, ...) {
      called <<- TRUE
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("foo", source = "clootl")
  expect_true(called)
})

test_that("source = 'rtrees' calls the rtrees backend with taxon", {
  seen_taxon <- NA
  testthat::local_mocked_bindings(
    .pr_get_tree_rtrees = function(species, taxon = NULL, ...) {
      seen_taxon <<- taxon
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list(taxon = taxon))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("Salmo salar", source = "rtrees", taxon = "fish")
  expect_equal(seen_taxon, "fish")
  expect_equal(res$backend_meta$taxon, "fish")
})


# 3. Helpful errors -----------------------------------------------------

test_that("missing rotl package gives a helpful migration error", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "rotl")) FALSE else TRUE
    },
    .package = "base"
  )

  err <- tryCatch(.pr_get_tree_rotl("foo"), error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("rotl", msg, fixed = TRUE))
  expect_true(grepl("install.packages", msg, fixed = TRUE))
})


test_that("missing rtrees package mentions the GitHub URL", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "rtrees")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_rtrees("foo", taxon = "bird"),
                  error = function(e) e)
  msg <- conditionMessage(err)
  expect_true(grepl("rtrees", msg, fixed = TRUE))
  expect_true(grepl("daijiang", msg, fixed = TRUE))
})


test_that("missing clootl package mentions the GitHub URL", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "clootl")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_clootl("foo"), error = function(e) e)
  msg <- conditionMessage(err)
  expect_true(grepl("clootl", msg, fixed = TRUE))
  expect_true(grepl("eliotmiller", msg, fixed = TRUE))
})


test_that("rtrees without taxon errors helpfully", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) TRUE,
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_rtrees("foo", taxon = NULL),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_true(grepl("taxon", conditionMessage(err)))
})


# 4. Result shape -------------------------------------------------------

test_that("result has the documented class and components", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species, unmatched = character(),
           backend_meta = list(n_queried = length(species)))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a b", "c d"), source = "rotl")
  expect_s3_class(res, "pr_tree_result")
  expect_named(res, c("tree", "matched", "unmatched", "source", "backend_meta"))
  expect_s3_class(res$tree, "phylo")
})


# 5. Print method -------------------------------------------------------

test_that("print.pr_tree_result runs without error", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species[1], unmatched = species[-1],
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a b", "c d", "e f"), source = "rotl")
  expect_no_error(capture.output(print(res)))
})
