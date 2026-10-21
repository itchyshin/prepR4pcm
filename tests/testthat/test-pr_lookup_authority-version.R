# Regression tests for pinned snapshot identity: when db_version is NULL,
# resolve taxadb's current snapshot once and pass that exact version to the
# lookup so every cached row and identity comparison has stable provenance.

test_that("filter_name receives the resolved latest version when db_version = NULL", {
  skip_if_not_installed("taxadb")

  captured <- NULL
  fake_filter_name <- function(...) {
    captured <<- list(...)
    tibble::tibble(
      scientificName       = "Salmo salar",
      taxonomicStatus      = "accepted",
      taxonID              = "COL:1",
      acceptedNameUsageID  = "COL:1"
    )
  }

  testthat::local_mocked_bindings(
    filter_name = fake_filter_name,
    .package    = "taxadb"
  )
  testthat::local_mocked_bindings(
    latest_version = function(...) "2025",
    .package       = "taxadb"
  )
  testthat::local_mocked_bindings(
    pr_ensure_db = function(authority, db_version = NULL) invisible(authority),
    .package     = "prepR4pcm"
  )
  testthat::local_mocked_bindings(
    .pr_cache = new.env(parent = emptyenv()),
    .package  = "prepR4pcm"
  )

  res <- pr_lookup_authority(
    c("Salmo salar"),
    authority  = "col",
    db_version = NULL
  )

  expect_s3_class(res, "data.frame")
  expect_identical(captured$version, "2025")
})

test_that("filter_name receives the explicit version when db_version is set", {
  skip_if_not_installed("taxadb")

  captured <- NULL
  fake_filter_name <- function(...) {
    captured <<- list(...)
    tibble::tibble(
      scientificName       = "Salmo salar",
      taxonomicStatus      = "accepted",
      taxonID              = "COL:1",
      acceptedNameUsageID  = "COL:1"
    )
  }

  testthat::local_mocked_bindings(
    filter_name = fake_filter_name,
    .package    = "taxadb"
  )
  testthat::local_mocked_bindings(
    pr_ensure_db = function(authority, db_version = NULL) invisible(authority),
    .package     = "prepR4pcm"
  )

  pr_lookup_authority(
    c("Salmo salar"),
    authority  = "col",
    db_version = "22.12"
  )

  expect_equal(captured$version, "22.12")
})

test_that("scientificName-only taxadb output does not crash subscripting (#83)", {
  skip_if_not_installed("taxadb")

  # Real taxadb 0.2.1 returns filter_name output WITHOUT an `input`
  # column. The previous code did
  #   all_hits$scientificName == name | all_hits$input == name
  # which evaluates the RHS to `NULL == name` -> logical(0). Tibble
  # then complained: "Unknown or uninitialised column: `input`".
  fake_filter_name <- function(...) {
    tibble::tibble(
      scientificName       = c("Salmo salar", "Esox lucius"),
      taxonomicStatus      = c("accepted", "accepted"),
      taxonID              = c("COL:1", "COL:2"),
      acceptedNameUsageID  = c("COL:1", "COL:2")
    )
  }

  testthat::local_mocked_bindings(
    filter_name = fake_filter_name,
    .package    = "taxadb"
  )
  testthat::local_mocked_bindings(
    pr_ensure_db = function(authority, db_version = NULL) invisible(authority),
    .package     = "prepR4pcm"
  )

  expect_warning(
    res <- pr_lookup_authority(
      c("Salmo salar", "Esox lucius"),
      authority = "col"
    ),
    regexp = NA  # no warning expected
  )
  expect_equal(nrow(res), 2)
  expect_equal(res$status, c("accepted", "accepted"))
})
