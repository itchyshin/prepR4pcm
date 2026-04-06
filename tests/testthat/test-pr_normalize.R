test_that("underscores are replaced with spaces", {
  expect_equal(
    as.character(pr_normalize_names("Homo_sapiens")),
    "Homo sapiens"
  )
})

test_that("case is standardised", {
  expect_equal(
    as.character(pr_normalize_names("homo sapiens")),
    "Homo sapiens"
  )
  expect_equal(
    as.character(pr_normalize_names("HOMO SAPIENS")),
    "Homo sapiens"
  )
})

test_that("whitespace is normalised", {
  expect_equal(
    as.character(pr_normalize_names("Homo  sapiens")),
    "Homo sapiens"
  )
  expect_equal(
    as.character(pr_normalize_names("  Homo sapiens  ")),
    "Homo sapiens"
  )
})

test_that("OTT suffixes are stripped", {
  expect_equal(
    as.character(pr_normalize_names("Homo_sapiens_ott770315")),
    "Homo sapiens"
  )
})

test_that("infraspecific names are stripped by default", {
  expect_equal(
    as.character(pr_normalize_names("Parus major major")),
    "Parus major"
  )
  expect_equal(
    as.character(pr_normalize_names("Quercus robur subsp. sessiliflora")),
    "Quercus robur"
  )
})

test_that("infraspecific names are kept with rank = 'subspecies'", {
  result <- as.character(
    pr_normalize_names("Parus major major", rank = "subspecies")
  )
  expect_equal(result, "Parus major major")
})

test_that("NA values are preserved", {
  result <- pr_normalize_names(c("Homo sapiens", NA, "Pan troglodytes"))
  expect_true(is.na(result[2]))
  expect_equal(as.character(result[1]), "Homo sapiens")
  expect_equal(as.character(result[3]), "Pan troglodytes")
})

test_that("normalisation log is attached as attribute", {
  result <- pr_normalize_names(c("Homo_sapiens", "Pan troglodytes"))
  log <- attr(result, "normalisation_log")
  expect_s3_class(log, "tbl_df")
  expect_true(log$changed[1])   # Homo_sapiens changed
  expect_false(log$changed[2])  # Pan troglodytes unchanged
})

test_that("authority strings are stripped", {
  expect_equal(
    as.character(pr_normalize_names("Homo sapiens Linnaeus, 1758")),
    "Homo sapiens"
  )
})
