test_that("reconcile_override_batch applies overrides correctly", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp", "Mystery sp"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = c("Unknown sp", "Mystery sp"),
    name_y = c("Pan_troglodytes", "Gorilla_gorilla"),
    action = c("accept", "accept"),
    note   = c("Batch test 1", "Batch test 2"),
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)

  mapping <- reconcile_mapping(result2)
  manual_rows <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual_rows), 2L)
  expect_true("Unknown sp" %in% manual_rows$name_x)
  expect_true("Mystery sp" %in% manual_rows$name_x)
})

test_that("reconcile_override_batch accepts CSV file path", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  batch <- data.frame(
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    action = "accept",
    note   = "From CSV",
    stringsAsFactors = FALSE
  )
  utils::write.csv(batch, tmp, row.names = FALSE)

  result2 <- reconcile_override_batch(result, tmp, quiet = TRUE)
  mapping <- reconcile_mapping(result2)
  manual_rows <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual_rows), 1L)
  expect_equal(manual_rows$name_x, "Unknown sp")
})

test_that("reconcile_override_batch defaults action to accept when column missing", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # No action column
  batch <- data.frame(
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)
  mapping <- reconcile_mapping(result2)
  manual_rows <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual_rows), 1L)
})

test_that("reconcile_override_batch rejects invalid input", {
  df <- data.frame(species = "Homo sapiens")
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # Not a data frame or path
  expect_error(
    reconcile_override_batch(result, 42),
    "data frame"
  )

  # Missing name_x column
  expect_error(
    reconcile_override_batch(result, data.frame(name_y = "Foo bar")),
    "name_x"
  )

  # Non-existent file
  expect_error(
    reconcile_override_batch(result, "/nonexistent/file.csv"),
    "not found"
  )

  # Invalid action values
  expect_error(
    reconcile_override_batch(
      result,
      data.frame(name_x = "Homo sapiens", action = "invalid",
                 stringsAsFactors = FALSE)
    ),
    "Invalid action"
  )
})

test_that("reconcile_override_batch records overrides in log", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    action = "accept",
    note   = "Test note",
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)
  expect_equal(nrow(result2$overrides), 1L)
  expect_equal(result2$overrides$action, "accept")
  expect_equal(result2$overrides$user_note, "Test note")
})

test_that("reconcile_override_batch handles reject action", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = "Pan troglodytes",
    action = "reject",
    note   = "Reject test",
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)
  mapping <- reconcile_mapping(result2)
  pan_row <- mapping[mapping$name_x == "Pan troglodytes" &
                       !is.na(mapping$name_x), ]
  expect_equal(pan_row$match_type, "unresolved")
})
