test_that("reconcile_data works with exact matches", {
  df1 <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  df2 <- data.frame(species = c("Homo sapiens", "Gorilla gorilla"))

  result <- reconcile_data(df1, df2, authority = NULL, quiet = TRUE)

  expect_s3_class(result, "reconciliation")
  expect_equal(result$meta$type, "data_data")
  expect_equal(result$counts$n_exact, 1L)
})

test_that("reconcile_data detects species column automatically", {
  df1 <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  df2 <- data.frame(
    species = c("Homo sapiens", "Gorilla gorilla"),
    mass = c(70, 150)
  )

  result <- reconcile_data(df1, df2, authority = NULL, quiet = TRUE)
  expect_s3_class(result, "reconciliation")
})

test_that("reconcile_data handles normalised matches", {
  df1 <- data.frame(species = c("Homo_sapiens", "Pan_troglodytes"))
  df2 <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))

  result <- reconcile_data(df1, df2, authority = NULL, quiet = TRUE)

  expect_equal(result$counts$n_normalized, 2L)
  expect_equal(result$counts$n_exact, 0L)
})

test_that("reconcile_data errors on invalid input", {
  expect_error(
    reconcile_data("not a df", data.frame(species = "A"), authority = NULL),
    "must be a data frame"
  )
})

test_that("reconcile_data errors on 0-row data frame", {
  df_empty <- data.frame(species = character(0), stringsAsFactors = FALSE)
  df_ok <- data.frame(species = "A b", stringsAsFactors = FALSE)
  expect_error(
    reconcile_data(df_empty, df_ok,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "0 rows"
  )
  expect_error(
    reconcile_data(df_ok, df_empty,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "0 rows"
  )
})

test_that("reconcile_data errors on all-NA species column", {
  df_na <- data.frame(species = c(NA, NA), stringsAsFactors = FALSE)
  df_ok <- data.frame(species = "A b", stringsAsFactors = FALSE)
  expect_error(
    reconcile_data(df_na, df_ok,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "All species names.*NA"
  )
})

test_that("reconcile_data handles factor species columns", {
  df1 <- data.frame(species = factor(c("A b", "C d")))
  df2 <- data.frame(species = factor(c("A b", "E f")))
  expect_message(
    result <- reconcile_data(df1, df2,
                             x_species = "species", y_species = "species",
                             authority = NULL, quiet = FALSE),
    "factor"
  )
  expect_s3_class(result, "reconciliation")
})
