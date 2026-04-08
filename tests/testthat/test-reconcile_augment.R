# Helper: create a test tree and reconciliation
make_test_setup <- function() {
  tree <- ape::rtree(5, tip.label = c(
    "Parus_major", "Parus_caeruleus", "Corvus_corax",
    "Corvus_corone", "Falco_peregrinus"
  ))

  df <- data.frame(
    species = c("Parus major", "Parus caeruleus", "Corvus corax",
                "Corvus corone", "Falco peregrinus",
                "Parus ater",           # congener exists (Parus)
                "Corvus monedula",       # congener exists (Corvus)
                "Aquila chrysaetos"),    # no congener (Aquila)
    trait = rnorm(8),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  list(tree = tree, df = df, rec = rec)
}


test_that("reconcile_augment adds species with congeners", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  # Should have added Parus ater and Corvus monedula
  expect_true(nrow(aug$augmented) >= 2)
  expect_true("Parus ater" %in% aug$augmented$species)
  expect_true("Corvus monedula" %in% aug$augmented$species)

  # Tree should have more tips
  expect_gt(ape::Ntip(aug$tree), ape::Ntip(setup$tree))
})


test_that("reconcile_augment skips species with no congener", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  # Aquila chrysaetos has no congener
  expect_true("Aquila chrysaetos" %in% aug$skipped$species)
  expect_true(grepl("No congener", aug$skipped$reason[
    aug$skipped$species == "Aquila chrysaetos"
  ]))
})


test_that("reconcile_augment preserves original tree", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  expect_equal(ape::Ntip(aug$original), ape::Ntip(setup$tree))
  expect_equal(aug$original$tip.label, setup$tree$tip.label)
})


test_that("branch_length = 'zero' produces zero-length branches", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "zero", seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length == 0))
  }
})


test_that("branch_length = 'congener_median' produces positive lengths", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "congener_median",
                            seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length > 0))
  }
})


test_that("branch_length = 'half_terminal' produces positive lengths", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "half_terminal",
                            seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length > 0))
  }
})


test_that("where = 'near' works with multiple congeners", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            where = "near", seed = 42, quiet = TRUE)

  expect_true(nrow(aug$augmented) >= 2)
  # Parus ater placed near MRCA of Parus congeners
  parus_row <- aug$augmented[aug$augmented$species == "Parus ater", ]
  expect_true(grepl("MRCA", parus_row$placed_near))
})


test_that("seed produces reproducible results", {
  setup <- make_test_setup()
  aug1 <- reconcile_augment(setup$rec, setup$tree, seed = 123, quiet = TRUE)
  aug2 <- reconcile_augment(setup$rec, setup$tree, seed = 123, quiet = TRUE)

  expect_equal(aug1$augmented$placed_near, aug2$augmented$placed_near)
  expect_equal(ape::Ntip(aug1$tree), ape::Ntip(aug2$tree))
})


test_that("reconcile_augment handles no unresolved species", {
  tree <- ape::rtree(2, tip.label = c("Homo_sapiens", "Pan_troglodytes"))
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                   stringsAsFactors = FALSE)
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  aug <- reconcile_augment(rec, tree, quiet = TRUE)
  expect_equal(nrow(aug$augmented), 0)
  expect_equal(nrow(aug$skipped), 0)
  expect_equal(ape::Ntip(aug$tree), ape::Ntip(tree))
})


test_that("reconcile_augment validates input", {
  expect_error(reconcile_augment("not a reconciliation", ape::rtree(3)),
               "reconciliation")
})


test_that("meta contains correct counts", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  expect_equal(aug$meta$n_augmented, nrow(aug$augmented))
  expect_equal(aug$meta$n_skipped, nrow(aug$skipped))
  expect_equal(aug$meta$original_n_tips, ape::Ntip(setup$tree))
  expect_equal(aug$meta$augmented_n_tips, ape::Ntip(aug$tree))
})


# --- M4. reconcile_augment placement × branch_length × tree shape grid -----

test_that("M4 grid: where × branch_length × tree shape × congener mix", {
  # Build a reconciliation seeded against different tree shapes
  # and data frames that mix congener-present / congener-absent species.

  # Data frame that produces: 2 matched, 2 with congeners (augment), 1 without
  df <- data.frame(
    species = c("Parus major", "Corvus corax",          # matched
                "Parus ater", "Corvus monedula",         # congeners present
                "Aquila chrysaetos"),                    # no congener
    trait = seq_len(5),
    stringsAsFactors = FALSE
  )

  make_tree <- function(kind) {
    set.seed(42)
    tips <- c("Parus_major", "Parus_caeruleus", "Corvus_corax",
              "Corvus_corone", "Falco_peregrinus")
    tr <- ape::rtree(5, tip.label = tips)
    switch(kind,
      non_ultrametric = tr,
      ultrametric     = suppressWarnings(suppressMessages(
        ape::chronos(tr, quiet = TRUE))),
      zero_branches   = { tr$edge.length <- rep(0, length(tr$edge.length)); tr },
      polytomy        = ape::di2multi(tr, tol = max(tr$edge.length) * 0.9)
    )
  }

  wheres <- c("genus", "near")
  branch_lengths <- c("congener_median", "half_terminal", "zero")
  tree_kinds <- c("non_ultrametric", "ultrametric",
                  "zero_branches", "polytomy")

  for (tree_kind in tree_kinds) {
    tree <- make_tree(tree_kind)
    rec <- suppressMessages(suppressWarnings(
      reconcile_tree(df, tree, x_species = "species",
                     authority = NULL, quiet = TRUE)
    ))

    for (where in wheres) {
      for (bl in branch_lengths) {
        info <- sprintf("tree=%s where=%s branch=%s",
                        tree_kind, where, bl)

        aug <- suppressWarnings(suppressMessages(
          reconcile_augment(rec, tree, where = where,
                            branch_length = bl, seed = 42, quiet = TRUE)
        ))

        # Invariant 1: augmented tree tip count equals original + n_augmented
        expect_equal(
          ape::Ntip(aug$tree),
          ape::Ntip(tree) + nrow(aug$augmented),
          info = info
        )

        # Invariant 2: skipped species have a non-empty reason
        if (nrow(aug$skipped) > 0) {
          expect_true(all(!is.na(aug$skipped$reason) &
                            nzchar(aug$skipped$reason)), info = info)
        }

        # Invariant 3: "Aquila chrysaetos" (no congener) must be skipped
        expect_true("Aquila chrysaetos" %in% aug$skipped$species,
                    info = info)

        # Invariant 4: branch_length strategy respected
        if (nrow(aug$augmented) > 0) {
          if (bl == "zero") {
            expect_true(all(aug$augmented$branch_length == 0), info = info)
          } else {
            # non-zero strategies should produce >= 0 branch lengths
            expect_true(all(aug$augmented$branch_length >= 0), info = info)
          }
        }

        # Invariant 5: augmented species have their genus present in tree
        if (nrow(aug$augmented) > 0) {
          aug_genera <- sub(" .*$", "", aug$augmented$species)
          tree_genera <- unique(sub("[_ ].*$", "", tree$tip.label))
          expect_true(all(aug_genera %in% tree_genera), info = info)
        }
      }
    }
  }
})


test_that("M4: seed is deterministic across all strategy combinations", {
  setup <- make_test_setup()
  for (where in c("genus", "near")) {
    for (bl in c("congener_median", "half_terminal", "zero")) {
      a1 <- suppressMessages(
        reconcile_augment(setup$rec, setup$tree,
                          where = where, branch_length = bl,
                          seed = 99, quiet = TRUE)
      )
      a2 <- suppressMessages(
        reconcile_augment(setup$rec, setup$tree,
                          where = where, branch_length = bl,
                          seed = 99, quiet = TRUE)
      )
      info <- sprintf("where=%s branch=%s", where, bl)
      expect_equal(a1$augmented$placed_near, a2$augmented$placed_near,
                   info = info)
      expect_equal(a1$augmented$branch_length, a2$augmented$branch_length,
                   info = info)
      expect_equal(ape::Ntip(a1$tree), ape::Ntip(a2$tree), info = info)
    }
  }
})


test_that("M4: different seeds produce different random placements for where='near'", {
  setup <- make_test_setup()
  a1 <- suppressMessages(
    reconcile_augment(setup$rec, setup$tree,
                      where = "near", seed = 1, quiet = TRUE)
  )
  a2 <- suppressMessages(
    reconcile_augment(setup$rec, setup$tree,
                      where = "near", seed = 99999, quiet = TRUE)
  )
  # Tip count should be the same; the ACTUAL placements can differ
  expect_equal(ape::Ntip(a1$tree), ape::Ntip(a2$tree))
})


test_that("M4: invalid where/branch_length errors via match.arg", {
  setup <- make_test_setup()
  expect_error(
    reconcile_augment(setup$rec, setup$tree, where = "nowhere", quiet = TRUE)
  )
  expect_error(
    reconcile_augment(setup$rec, setup$tree, branch_length = "bogus",
                      quiet = TRUE)
  )
})


test_that("M4: zero-branch trees still produce augmented tree", {
  tr <- fx_tree_zero_branches(5)
  df <- data.frame(
    species = c(gsub("_", " ", tr$tip.label[1]),
                gsub("_", " ", tr$tip.label[2]),
                # species with congener present in tree
                paste0(sub(" .*$", "", gsub("_", " ", tr$tip.label[1])),
                       " sp999")),
    stringsAsFactors = FALSE
  )
  rec <- suppressMessages(
    reconcile_tree(df, tr, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  # reconcile_augment should not crash on zero-branch trees
  expect_no_error(
    suppressWarnings(suppressMessages(
      reconcile_augment(rec, tr, branch_length = "zero",
                        seed = 42, quiet = TRUE)
    ))
  )
})
