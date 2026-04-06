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
