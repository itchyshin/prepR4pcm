test_that("an installed but unloadable dbplyr gives recovery steps (#123)", {
  broken_load <- function(package) {
    stop("object 'filter_out' is not exported by 'namespace:dplyr'")
  }
  expect_error(.pr_check_dbplyr(load = broken_load), "could not load dbplyr")
  expect_error(.pr_check_dbplyr(load = broken_load), "filter_out")
  expect_error(.pr_check_dbplyr(load = broken_load), "install.packages")
  expect_error(.pr_check_dbplyr(load = broken_load), "Restart R")
})

test_that("a missing dbplyr gives installation steps", {
  broken_load <- function(package) stop("there is no package called 'dbplyr'")
  expect_error(.pr_check_dbplyr(load = broken_load), "there is no package called")
  expect_error(.pr_check_dbplyr(load = broken_load), "install.packages")
})

test_that("a loadable dbplyr passes the check", {
  seen <- NULL
  expect_null(.pr_check_dbplyr(load = function(package) {
    seen <<- package
    new.env()
  }))
  expect_identical(seen, "dbplyr")
})

test_that("dbplyr is checked before taxadb can prompt to install it", {
  checked <- FALSE
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) TRUE,
    .package = "base"
  )
  testthat::local_mocked_bindings(
    .pr_check_dbplyr = function() {
      checked <<- TRUE
      stop("dependency check reached")
    },
    .package = "prepR4pcm"
  )
  expect_error(pr_ensure_db("col"), "dependency check reached")
  expect_true(checked)
})
