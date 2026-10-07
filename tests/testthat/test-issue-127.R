test_that("reconcile_override validates source and target names", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")
  rec <- reconcile_tree(avonet_subset, tree_jetz, authority = NULL,
                        quiet = TRUE)

  expect_error(
    reconcile_override(rec, name_x = "Not a source species",
                       name_y = tree_jetz$tip.label[[1]], action = "replace"),
    "name_x.*not found|source.*not found"
  )
  expect_error(
    reconcile_override(rec, name_x = rec$mapping$name_x[rec$mapping$in_x][[1]],
                       name_y = "Not_a_tree_tip", action = "replace"),
    "name_y.*not found|target.*not found"
  )
  expect_error(
    reconcile_override(rec, name_x = NA_character_, action = "reject"),
    "name_x"
  )
  expect_error(
    reconcile_override(rec, name_x = c("A sp", "B sp"), action = "reject"),
    "single|one"
  )
})

test_that("reconcile_override rejects many-to-one target assignments", {
  df <- data.frame(species = c("Homo sapiens", "Unknown species"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)

  expect_error(
    reconcile_override(rec, name_x = "Unknown species",
                       name_y = "Homo sapiens", action = "accept"),
    "already.*matched|already assigned|conflict"
  )
  expect_error(
    reconcile_override(rec, name_x = "Unknown species", name_y = NA_character_,
                       action = "replace"),
    "name_y"
  )
})

test_that("replace and reject preserve source and target inventories", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)

  replaced <- reconcile_override(rec, name_x = "Homo sapiens",
                                 name_y = "Gorilla_gorilla",
                                 action = "replace")
  expect_setequal(replaced$mapping$name_x[replaced$mapping$in_x], df$species)
  expect_setequal(replaced$mapping$name_y[replaced$mapping$in_y],
                  tree$tip.label)
  expect_true(any(replaced$mapping$name_y == "Homo_sapiens" &
                    !replaced$mapping$in_x & replaced$mapping$in_y))

  rejected <- reconcile_override(rec, name_x = "Homo sapiens", action = "reject")
  expect_setequal(rejected$mapping$name_x[rejected$mapping$in_x], df$species)
  expect_setequal(rejected$mapping$name_y[rejected$mapping$in_y],
                  tree$tip.label)
  expect_true(any(rejected$mapping$name_y == "Homo_sapiens" &
                    !rejected$mapping$in_x & rejected$mapping$in_y))
})

test_that("reconcile_override accepts normalized source and target spellings", {
  df <- data.frame(species = c("Homo sapiens", "Unknown species"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)

  out <- reconcile_override(rec, name_x = "Unknown_species",
                            name_y = "Pan troglodytes", action = "accept")
  manual <- out$mapping[out$mapping$match_type == "manual", ]
  expect_equal(manual$name_x, "Unknown species")
  expect_equal(manual$name_y, "Pan_troglodytes")
})


test_that("reject accepts a missing target without adding an all-NA row", {
  df <- data.frame(species=c("Homo sapiens", "Missing species"))
  tree <- ape::read.tree(text="(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df,tree,x_species="species",authority=NULL,quiet=TRUE)
  rejected <- reconcile_override(rec,"Homo sapiens",name_y=NA,action="reject")
  expect_false(any(is.na(rejected$mapping$name_x) &
                     is.na(rejected$mapping$name_y)))
  expect_false(anyNA(rejected$mapping$in_x))
  expect_false(anyNA(rejected$mapping$in_y))
})
