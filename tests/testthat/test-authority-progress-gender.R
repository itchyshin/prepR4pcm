test_that("large authority lookups report bounded batches and reuse cache", {
  calls <- list()
  testthat::local_mocked_bindings(
    .pr_cache = new.env(parent = emptyenv()),
    .pr_lookup_gnverifier = function(names, db_version = NULL) {
      calls[[length(calls) + 1L]] <<- names
      tibble::tibble(input = names, accepted_name = names,
                     status = "accepted", taxon_id = names,
                     authority = "gnverifier")
    }, .package = "prepR4pcm")
  key <- "lookup_gnverifier_progress_test"
  x <- paste("Plant", seq_len(2501))
  messages <- character()
  out <- withCallingHandlers(
    pr_lookup_authority(x, "gnverifier", "progress_test"),
    message = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    })
  expect_equal(lengths(calls), c(1000L, 1000L, 501L))
  expect_setequal(out$input, x)
  expect_match(paste(messages, collapse = " "), "2501")
  expect_message(pr_lookup_authority(x, "gnverifier", "progress_test", quiet = TRUE), NA)
  expect_length(calls, 3L)
  expect_message(pr_lookup_authority(c(x, "Plant new"), "gnverifier",
                                   "progress_test", quiet = TRUE), NA)
  expect_identical(calls[[4L]], "Plant new")
})

test_that("synonym quiet mode reaches both authority lookups", {
  seen <- logical()
  testthat::local_mocked_bindings(
    pr_lookup_authority = function(names, authority, db_version, quiet = FALSE) {
      seen <<- c(seen, quiet)
      tibble::tibble(input = names, accepted_name = NA_character_)
    }, .package = "prepR4pcm")
  expect_message(pr_resolve_synonyms("Plant alba", "Plant albus", quiet = TRUE), NA)
  expect_identical(seen, c(TRUE, TRUE))
})

test_that("gender suggestions retain reference spellings and ambiguous candidates", {
  x <- c("Plant_albus", "Other rubrum", "Plant viridis", "Plant alba",
         "Plant smithii", NA_character_, "Plant")
  y <- c("Plant alba L.", "Plant album", "Other rubra", "Plant viride",
         "Different albus", "Plant smithia")
  out <- pr_suggest_gender(x, y)
  expect_setequal(out$suggestion[out$input == "Plant_albus"], y[1:2])
  expect_true(all(out$ambiguous[out$input == "Plant_albus"]))
  expect_true(all(out$requires_review))
  expect_identical(out$suggestion[out$input == "Other rubrum"], "Other rubra")
  expect_identical(out$suggestion[out$input == "Plant viridis"], "Plant viride")
  expect_false(any(out$input %in% c("Plant alba", "Plant smithii", "Plant")))
  expect_identical(as.character(pr_normalize_names("Plant albus")), "Plant albus")
})

test_that("gender suggestions handle empty, invalid and infraspecific names", {
  expect_equal(nrow(pr_suggest_gender(character(), character())), 0)
  expect_error(pr_suggest_gender(1:3, "Plant alba"), "character")
  expect_error(pr_suggest_gender("Plant albus", list("Plant alba")), "character")
  expect_equal(nrow(pr_suggest_gender("Plant albus minor", "Plant alba major",
                                     rank = "subspecies")), 0)
  expect_equal(nrow(pr_suggest_gender("Plant albus minor", "Plant alba minor",
                                     rank = "subspecies")), 1)
  expect_equal(nrow(pr_suggest_gender("Plant albus", "Other alba")), 0)
})

test_that("completed authority batches survive an interrupted lookup", {
  key <- "lookup_gnverifier_interrupted_test"
  calls <- 0L
  testthat::local_mocked_bindings(
    .pr_cache = new.env(parent = emptyenv()),
    .pr_lookup_gnverifier = function(names, db_version = NULL) {
      calls <<- calls + 1L
      if (calls == 2L) stop("interrupted")
      tibble::tibble(input = names, accepted_name = names, status = "accepted",
                     taxon_id = names, authority = "gnverifier")
    }, .package = "prepR4pcm")
  expect_error(pr_lookup_authority(paste("Plant", 1:1001), "gnverifier",
                                  "interrupted_test", quiet = TRUE), "interrupted")
  expect_equal(nrow(get(key, envir = getFromNamespace(".pr_cache", "prepR4pcm"))), 1000)
})

test_that("taxadb lookups also batch and forward the database version", {
  key <- "lookup_col_progress_taxadb_test"
  sizes <- integer()
  testthat::local_mocked_bindings(
    .pr_cache = new.env(parent = emptyenv()),
    pr_ensure_db = function(...) NULL,
    .pr_lookup_taxadb = function(names, authority, db_version = NULL) {
      expect_identical(db_version, "progress_taxadb_test")
      sizes <<- c(sizes, length(names))
      tibble::tibble(input = names, accepted_name = names, status = "accepted",
                     taxon_id = names, authority = authority)
    }, .package = "prepR4pcm")
  expect_message(pr_lookup_authority(paste("Plant", 1:2001), "col",
                                    "progress_taxadb_test", quiet = TRUE), NA)
  expect_identical(sizes, c(1000L, 1000L, 1L))
})
