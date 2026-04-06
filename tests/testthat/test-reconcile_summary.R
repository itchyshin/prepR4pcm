test_that("reconcile_summary prints without error", {
  df <- data.frame(species = c("Homo sapiens", "Missing sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  expect_output(reconcile_summary(result), "Reconciliation Report")
})

test_that("reconcile_summary returns data.frame format", {
  df <- data.frame(species = c("Homo sapiens", "Pan_troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  summ <- reconcile_summary(result, format = "data.frame")
  expect_s3_class(summ, "reconciliation_summary")
  expect_true("tables" %in% names(summ))
  expect_true("by_type" %in% names(summ$tables))
})

test_that("reconcile_summary detail levels work", {
  df <- data.frame(species = c("Homo sapiens", "Pan_troglodytes", "Missing sp"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  expect_output(reconcile_summary(result, detail = "brief"), "Match Summary")
  expect_output(reconcile_summary(result, detail = "full"), "Reconciliation Report")
  expect_output(reconcile_summary(result, detail = "mismatches_only"),
                "Reconciliation Report")
})

test_that("reconcile_summary writes to file", {
  df <- data.frame(species = c("Homo sapiens"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  reconcile_summary(result, file = tmp)
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("Reconciliation Report", content)))
})
