# Issue #125: chained grafts must not create nodes beyond the present.
check_issue_125_augmentation <- function() {
  data("avonet_subset", package = "prepR4pcm", envir = environment())
  data("tree_jetz", package = "prepR4pcm", envir = environment())
  rec <- reconcile_tree(avonet_subset, tree_jetz, x_species = "Species1",
                        authority = NULL, quiet = TRUE)
  original_distances <- ape::cophenetic.phylo(tree_jetz)
  original_depths <- ape::node.depth.edgelength(tree_jetz)[
    seq_along(tree_jetz$tip.label)
  ]

  for (where in c("genus", "near")) {
    for (method in c("congener_median", "half_terminal", "zero")) {
      info <- paste(where, method)
      aug <- reconcile_augment(rec, tree_jetz, where = where,
                                branch_length = method, seed = 1, quiet = TRUE)
      expect_equal(nrow(aug$augmented), 136L, info = info)
      expect_equal(ape::Ntip(aug$tree), ape::Ntip(tree_jetz) + 136L,
                   info = info)
      expect_true(all(is.finite(aug$tree$edge.length)), info = info)
      expect_true(all(aug$tree$edge.length >= 0), info = info)
      expect_equal(
        ape::cophenetic.phylo(aug$tree)[tree_jetz$tip.label,
                                       tree_jetz$tip.label],
        original_distances, tolerance = 1e-7, info = info
      )
      expect_equal(
        ape::node.depth.edgelength(aug$tree)[
          match(tree_jetz$tip.label, aug$tree$tip.label)
        ],
        original_depths, tolerance = 1e-7, info = info
      )
      new_tips <- match(gsub(" ", "_", aug$augmented$species),
                        aug$tree$tip.label)
      expect_equal(
        aug$augmented$branch_length,
        aug$tree$edge.length[match(new_tips, aug$tree$edge[, 2])],
        info = info
      )
      if (method == "zero") {
        expect_true(all(aug$tree$edge.length[
          match(new_tips, aug$tree$edge[, 2])
        ] == 0), info = info)
      } else {
        expect_true(ape::is.ultrametric(aug$tree), info = info)
      }
    }
  }

  # Congeners on both sides of the root exercise the root-MRCA placement.
  root_tree <- ape::read.tree(text =
    "((Aus_one:2,Bus_one:2):3,Aus_two:5);")
  root_rec <- reconcile_tree(data.frame(species = "Aus three"), root_tree,
                            x_species = "species", authority = NULL,
                            quiet = TRUE)
  for (method in c("congener_median", "half_terminal", "zero")) {
    aug <- reconcile_augment(root_rec, root_tree, where = "near",
                              branch_length = method, seed = 1, quiet = TRUE)
    expect_true(all(aug$tree$edge.length >= 0), info = method)
    expect_equal(ape::Ntip(aug$tree), 4L, info = method)
    expect_equal(
      ape::cophenetic.phylo(aug$tree)[root_tree$tip.label, root_tree$tip.label],
      ape::cophenetic.phylo(root_tree), info = method
    )
    if (method != "zero") {
      expect_true(ape::is.ultrametric(aug$tree), info = method)
    }
  }
}

test_that("issue #125: bundled augmentation keeps valid branch lengths", {
  check_issue_125_augmentation()
})

test_that("issue #125: ape fallback keeps valid branch lengths", {
  # Exercise the dependency-free binding path even if phytools is installed.
  binder <- prepR4pcm:::pr_bind_tip
  environment(binder) <- new.env(parent = environment(binder))
  environment(binder)$requireNamespace <- function(...) FALSE
  local_mocked_bindings(pr_bind_tip = binder, .package = "prepR4pcm")
  check_issue_125_augmentation()
})
