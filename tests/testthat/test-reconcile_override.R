test_that("reconcile_override accept action adds manual match", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  result2 <- reconcile_override(
    result,
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    action = "accept",
    note = "Known to be Pan"
  )

  mapping <- reconcile_mapping(result2)
  manual <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual), 1L)
  expect_equal(manual$name_x, "Unknown sp")
  expect_equal(manual$name_y, "Pan_troglodytes")
})

test_that("reconcile_override reject action marks as unresolved", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # Reject the Pan match
  result2 <- reconcile_override(
    result,
    name_x = "Pan troglodytes",
    action = "reject",
    note = "Not confident in this match"
  )

  mapping <- reconcile_mapping(result2)
  pan_row <- mapping[mapping$name_x == "Pan troglodytes" & !is.na(mapping$name_x), ]
  expect_equal(pan_row$match_type, "unresolved")
})

test_that("reconcile_override errors without name_y for accept", {
  df <- data.frame(species = "Homo sapiens")
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  expect_error(
    reconcile_override(result, name_x = "Homo sapiens", name_y = NULL,
                        action = "accept"),
    "Must provide"
  )
})

test_that("reconcile_override records in overrides table", {
  df <- data.frame(species = c("A sp", "B sp"))
  tree <- ape::read.tree(text = "(A_sp:1,C_sp:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  result2 <- reconcile_override(
    result,
    name_x = "B sp",
    name_y = "C_sp",
    action = "replace",
    note = "Lab convention"
  )

  expect_equal(nrow(result2$overrides), 1L)
  expect_equal(result2$overrides$action, "replace")
})
