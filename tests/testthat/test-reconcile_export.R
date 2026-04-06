test_that("reconcile_export writes data, tree, and mapping files", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  out_dir <- tempfile("export_test")
  on.exit(unlink(out_dir, recursive = TRUE))

  paths <- reconcile_export(result, data = df, tree = tree,
                             species_col = "species",
                             dir = out_dir, prefix = "test")

  expect_true(file.exists(paths$data))
  expect_true(file.exists(paths$tree))
  expect_true(file.exists(paths$mapping))

  # Check data is readable
  exported_data <- read.csv(paths$data)
  expect_true("species" %in% names(exported_data))

  # Check tree is readable
  exported_tree <- ape::read.nexus(paths$tree)
  expect_s3_class(exported_tree, "phylo")

  # Check mapping is readable
  exported_mapping <- read.csv(paths$mapping)
  expect_true("match_type" %in% names(exported_mapping))
})

test_that("reconcile_export handles newick format", {
  df <- data.frame(species = "Homo sapiens", mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  out_dir <- tempfile("newick_test")
  on.exit(unlink(out_dir, recursive = TRUE))

  paths <- reconcile_export(result, data = df, tree = tree,
                             species_col = "species",
                             dir = out_dir, tree_format = "newick")

  expect_true(grepl("\\.nwk$", paths$tree))
  expect_true(file.exists(paths$tree))
})

test_that("reconcile_export works with data only", {
  df <- data.frame(species = "Homo sapiens", mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  out_dir <- tempfile("data_only_test")
  on.exit(unlink(out_dir, recursive = TRUE))

  paths <- reconcile_export(result, data = df, species_col = "species",
                             dir = out_dir)

  expect_true(file.exists(paths$data))
  expect_null(paths$tree)
  expect_true(file.exists(paths$mapping))
})
