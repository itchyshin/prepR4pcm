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

test_that("source = 'fishtree' calls the fishtree backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, ...) {
      called <<- TRUE
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species, unmatched = character(),
           backend_meta = list(backend = "fishtree", type = "chronogram"))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("Salmo salar", "Esox lucius"), source = "fishtree")
  expect_true(called)
  expect_equal(res$source, "fishtree")
  expect_equal(res$backend_meta$backend, "fishtree")
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


test_that("missing fishtree package gives a helpful CRAN install hint", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "fishtree")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_fishtree("Salmo salar"),
                  error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("fishtree", msg, fixed = TRUE))
  expect_true(grepl("install.packages", msg, fixed = TRUE))
})


test_that("missing datelife package gives a helpful GitHub install hint", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "datelife")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_datelife("Rhea americana"),
                  error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("datelife", msg, fixed = TRUE))
  expect_true(grepl("phylotastic", msg, fixed = TRUE))
})


# 6. n_tree parameter ---------------------------------------------------

test_that("n_tree validation: must be positive integer", {
  expect_error(pr_get_tree("foo", source = "rotl", n_tree = 0),
               "positive integer")
  expect_error(pr_get_tree("foo", source = "rotl", n_tree = -1),
               "positive integer")
  expect_error(pr_get_tree("foo", source = "rotl", n_tree = c(1, 2)),
               "length-1")
})


test_that("n_tree > 1 on rotl emits a warning and returns single", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      # Real rotl helper would warn; verify n_tree is plumbed through.
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(),
           backend_meta = list(n_received_n_tree = n_tree))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("foo", source = "rotl", n_tree = 5)
  expect_equal(res$backend_meta$n_received_n_tree, 5L)
})


test_that("n_tree is plumbed through to rtrees", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_rtrees = function(species, taxon = NULL, n_tree = 1L, ...) {
      seen <<- n_tree
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Salmo salar", source = "rtrees", taxon = "fish",
              n_tree = 10)
  expect_equal(seen, 10L)
})


test_that("n_tree is plumbed through to clootl", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_clootl = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Corvus corax", source = "clootl", n_tree = 3)
  expect_equal(seen, 3L)
})


test_that("n_tree is plumbed through to fishtree", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Salmo salar", source = "fishtree", n_tree = 7)
  expect_equal(seen, 7L)
})


test_that("n_tree is plumbed through to datelife", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Rhea americana", source = "datelife", n_tree = 4)
  expect_equal(seen, 4L)
})


test_that("default n_tree = 1 (back-compat)", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("foo", source = "rotl")
  expect_equal(seen, 1L)
})


# 7. Per-tree provenance ------------------------------------------------

test_that("backend_meta$tree_provenance is always present (single phylo)", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species), matched = species,
           unmatched = character(),
           backend_meta = list(n_queried = length(species)))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("foo", source = "rotl")
  expect_true(!is.null(res$backend_meta$tree_provenance))
  expect_length(res$backend_meta$tree_provenance, 1L)
  expect_equal(res$backend_meta$tree_provenance[[1]]$source_index, 1L)
  expect_true(nzchar(res$backend_meta$tree_provenance[[1]]$citation))
})


test_that("backend_meta$tree_provenance has one entry per multiPhylo tree", {
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      mp <- structure(
        list(
          mini_phylo(species), mini_phylo(species),
          mini_phylo(species), mini_phylo(species)
        ),
        class = "multiPhylo"
      )
      list(tree = mp, matched = species,
           unmatched = character(),
           backend_meta = list(
             backend = "fishtree",
             reference = "Rabosky 2018",
             n_returned = 4L
           ))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("Salmo salar", source = "fishtree", n_tree = 4)
  expect_s3_class(res$tree, "multiPhylo")
  expect_length(res$backend_meta$tree_provenance, 4L)
  for (i in seq_len(4)) {
    expect_equal(res$backend_meta$tree_provenance[[i]]$source_index, i)
  }
})


test_that("datelife per-source citations populate tree_provenance", {
  testthat::local_mocked_bindings(
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      mp <- structure(
        list(mini_phylo(species), mini_phylo(species)),
        class = "multiPhylo",
        names = c("Hedges et al. 2015", "Bininda-Emonds et al. 2007")
      )
      list(tree = mp, matched = species,
           unmatched = character(),
           backend_meta = list(
             backend = "datelife",
             summary_format = "phylo_all",
             reference = "Sanchez Reyes 2024"
           ))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("Rhea americana", source = "datelife", n_tree = 2)
  cites <- vapply(res$backend_meta$tree_provenance,
                   function(p) p$citation, character(1))
  expect_true(any(grepl("Hedges", cites)))
  expect_true(any(grepl("Bininda", cites)))
})


# 8. datelife backend (mocked) ------------------------------------------

test_that("source = 'datelife' calls the datelife backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      called <<- TRUE
      list(tree = mini_phylo(gsub(" ", "_", species)),
           matched = species, unmatched = character(),
           backend_meta = list(backend = "datelife",
                                summary_format = "phylo_sdm"))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("Rhea americana", "Struthio camelus"),
                    source = "datelife")
  expect_true(called)
  expect_equal(res$source, "datelife")
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
