# Coverage round: exercises previously untested branches in the lowest-cov
# modules (reconcile_review, pr_date_tree, pr_splits_lumps, reconcile_augment
# internals, pr_authority/taxadb, reconcile_summary console paths, pr_phylo_cor,
# reconcile_diff messaging). All network/package backends are mocked.

mini_phylo <- function(tip_labels) {
  ape::read.tree(text = paste0("(", paste(tip_labels, collapse = ","), ");"))
}

make_demo_meta <- function() {
  list(
    call = NULL, type = "data_tree", timestamp = Sys.time(),
    authority = "col", db_version = "2025",
    fuzzy = TRUE, fuzzy_threshold = 0.85, fuzzy_method = "jw",
    resolve = "flag", prepR4pcm_version = packageVersion("prepR4pcm"),
    x_source = "df", y_source = "tree", rank = "species"
  )
}

# --- reconcile_review (interactive paths) ------------------------------------

test_that("reconcile_review interactive flagged: accept, reject, skip, quit", {
  mapping <- tibble::tibble(
    name_x        = c("Homo sapiens", "Pan troglodytes", "Gorilla bogus"),
    name_y        = c("Homo sapiens", "Pan troglodytes", NA_character_),
    name_resolved = NA_character_,
    match_type    = c("exact", "flagged", "unresolved"),
    match_score   = c(1, 0.88, NA_real_),
    match_source  = c("exact_string", "fuzzy_flagged", NA_character_),
    in_x          = TRUE,
    in_y          = c(TRUE, TRUE, FALSE),
    notes         = c("", "low score", "unresolved")
  )
  meta <- make_demo_meta()
  rec <- new_reconciliation(mapping, meta)
  responses <- c("a", "r", "s", "q")
  n <- 0L
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline = function(prompt) {
      n <<- n + 1L
      responses[min(n, length(responses))]
    },
    .package = "base"
  )
  out <- suppressMessages(reconcile_review(rec, type = "flagged", quiet = TRUE))
  expect_s3_class(out, "reconciliation")
  manual <- out$mapping[out$mapping$name_x == "Pan troglodytes", , drop = FALSE]
  expect_equal(manual$match_type, "manual")
})


test_that("reconcile_review interactive all_unresolved shows hint and cannot accept empty target", {
  mapping <- tibble::tibble(
    name_x        = "Missing species",
    name_y        = NA_character_,
    name_resolved = NA_character_,
    match_type    = "unresolved",
    match_score   = NA_real_,
    match_source  = NA_character_,
    in_x          = TRUE,
    in_y          = FALSE,
    notes         = "no match"
  )
  rec <- new_reconciliation(mapping, make_demo_meta())
  n <- 0L
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline = function(prompt) {
      n <<- n + 1L
      if (n == 1L) "a" else "q"
    },
    .package = "base"
  )
  out <- suppressMessages(
    reconcile_review(rec, type = "all_unresolved", suggest = TRUE, quiet = TRUE)
  )
  expect_s3_class(out, "reconciliation")
})


test_that("reconcile_review identity_candidates accept and reject", {
  candidates <- tibble::tibble(
    name_x = c("Alpha a", "Beta b"),
    name_y = c("Alpha x", "Beta y"),
    identity_state = "withheld", eligible = FALSE,
    reason = c("id mismatch", "rank mismatch"),
    x_id = c("X1", "X2"), y_id = c("Y1", "Y2"),
    provider = "col", version = "2025",
    stage = "fuzzy", review_status = c("pending", "pending"),
    review_note = NA_character_
  )
  mapping <- tibble::tibble(
    name_x = candidates$name_x, name_y = NA_character_,
    name_resolved = NA_character_, match_type = "unresolved",
    match_score = NA_real_, match_source = NA_character_,
    in_x = TRUE, in_y = FALSE, notes = ""
  )
  rec <- new_reconciliation(mapping, make_demo_meta(), candidates = candidates)
  n <- 0L
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline = function(prompt) {
      n <<- n + 1L
      c("a", "r")[n]
    },
    .package = "base"
  )
  out <- suppressMessages(
    reconcile_review(rec, type = "identity_candidates", quiet = FALSE)
  )
  expect_s3_class(out, "reconciliation")
  expect_equal(nrow(out$overrides), 2L)
})


test_that("reconcile_review identity_candidates with no pending returns unchanged", {
  candidates <- tibble::tibble(
    name_x = "A", name_y = "B", identity_state = "withheld", eligible = FALSE,
    reason = "test", x_id = "1", y_id = "2", provider = "col", version = "2025",
    stage = "fuzzy", review_status = "done", review_note = NA_character_
  )
  rec <- new_reconciliation(
    tibble::tibble(
      name_x = "A", name_y = NA_character_, name_resolved = NA_character_,
      match_type = "unresolved", match_score = NA_real_, match_source = NA_character_,
      in_x = TRUE, in_y = FALSE, notes = ""
    ),
    make_demo_meta(),
    candidates = candidates
  )
  testthat::local_mocked_bindings(interactive = function() TRUE, .package = "base")
  out <- suppressMessages(reconcile_review(rec, type = "identity_candidates"))
  expect_identical(out$candidates, rec$candidates)
})


# --- pr_date_tree / datelife helper ------------------------------------------

test_that(".pr_date_tree_datelife caps multiPhylo and tracks matched tips", {
  tr <- mini_phylo(c("tip_keep", "tip_drop"))
  dated_one <- mini_phylo("tip_keep")
  dated_two <- mini_phylo(c("tip_keep", "extra"))
  mp <- structure(list(dated_one, dated_two, dated_two), class = "multiPhylo")
  attr(mp[[1]], "datelife_calibrations") <- list(source = "mock")

  req_ns <- getFromNamespace("requireNamespace", "base")
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "datelife")) return(TRUE)
      req_ns(package, ..., quietly = quietly)
    },
    getExportedValue = function(pkg, name) {
      if (identical(pkg, "datelife") && identical(name, "datelife_use")) {
        function(...) mp
      } else {
        base::getExportedValue(pkg, name)
      }
    },
    .package = "base"
  )
  testthat::local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "datelife")) {
        structure(c(1L, 0L, 0L), class = c("package_version", "numeric_version"))
      } else {
        utils::packageVersion(pkg, ...)
      }
    },
    .package = "utils"
  )

  res <- prepR4pcm:::.pr_date_tree_datelife(tr, n_dated = 2L, dating_method = "bladj")
  expect_s3_class(res$tree, "multiPhylo")
  expect_equal(length(res$tree), 2L)
  expect_equal(res$matched, "tip_keep")
  expect_equal(res$unmatched, "tip_drop")
  expect_equal(res$backend_meta$n_returned, 2L)
  expect_equal(res$backend_meta$dating_method, "bladj")
})


test_that("pr_date_tree check_ultrametric warns on bad trees when mocked", {
  tr <- mini_phylo(c("a", "b"))
  bad_tr <- tr
  bad_tr$edge.length <- c(1, 3)
  testthat::local_mocked_bindings(
    .pr_date_tree_datelife = function(...) {
      list(
        tree = bad_tr,
        matched = tr$tip.label,
        unmatched = character(),
        backend_meta = list(backend = "datelife", n_returned = 1L)
      )
    },
    .package = "prepR4pcm"
  )
  expect_warning(pr_date_tree(tr, check_ultrametric = TRUE), "ultrametric")
  expect_no_warning(pr_date_tree(tr, check_ultrametric = FALSE))
})


# --- pr_splits_lumps / reconcile_splits_lumps --------------------------------

test_that("pr_detect_splits_lumps classifies split_lump when both sides multiply", {
  mapping <- tibble::tibble(
    name_x        = c("Old A", "Old B"),
    name_y        = c("New A", "New B"),
    name_resolved = c("Accepted", "Accepted"),
    match_type    = "synonym",
    match_score   = 1,
    match_source  = "col",
    in_x          = TRUE,
    in_y          = TRUE,
    notes         = NA_character_
  )
  sl <- prepR4pcm:::pr_detect_splits_lumps(mapping)
  expect_equal(sl$splits$type, "split_lump")
  expect_equal(sl$lumps$type, "split_lump")
  expect_equal(sl$splits$n_x, 2L)
  expect_equal(sl$splits$n_y, 2L)
})


test_that("reconcile_splits_lumps prints split and lump summaries when quiet = FALSE", {
  mapping <- tibble::tibble(
    name_x        = c("Acanthiza pusilla", "Acanthiza pusilla",
                    "Parus caeruleus", "Cyanistes caeruleus"),
    name_y        = c("Acanthiza pusilla", "Acanthiza apicalis",
                    "Cyanistes caeruleus", "Cyanistes caeruleus"),
    name_resolved = rep(c("Acanthiza pusilla", "Cyanistes caeruleus"), each = 2),
    match_type    = "synonym",
    match_score   = 1,
    match_source  = "col",
    in_x          = TRUE,
    in_y          = TRUE,
    notes         = NA_character_
  )
  demo <- new_reconciliation(mapping, make_demo_meta())
  expect_output(
    suppressMessages(reconcile_splits_lumps(demo, quiet = FALSE)),
    "split"
  )
})


# --- reconcile_augment internals ---------------------------------------------

test_that("pr_extract_genus and pr_calc_augment_bl handle edge trees", {
  expect_equal(prepR4pcm:::pr_extract_genus(c("Parus major", "", "Solo")),
               c("Parus", NA_character_, "Solo"))
  tr <- mini_phylo(c("Parus_major", "Parus_minor"))
  tr$edge.length <- NULL
  expect_equal(prepR4pcm:::pr_calc_augment_bl(tr, "Parus_major", "congener_median"), 0)
  expect_equal(prepR4pcm:::pr_calc_augment_bl(tr, "Parus_major", "half_terminal"), 0)
  expect_equal(prepR4pcm:::pr_calc_augment_bl(tr, "Parus_major", "zero"), 0)
})


test_that("pr_bind_species near placement uses MRCA of congeners", {
  tr <- ape::read.tree(text = "((Parus_major:1,Parus_minor:1):1,Corvus_corax:2);")
  bound <- prepR4pcm:::pr_bind_species(tr, "Parus_sp", c("Parus_major", "Parus_minor"),
                                       where = "near", bl = 0.1)
  expect_true(grepl("MRCA", bound$placed_near))
  expect_equal(length(bound$tree$tip.label), 4L)
})


test_that("pr_bind_tip uses pure-ape fallback when phytools is unavailable", {
  tr <- mini_phylo(c("a", "b"))
  tr$edge.length <- c(1, 1)
  req_ns <- getFromNamespace("requireNamespace", "base")
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "phytools")) return(FALSE)
      req_ns(package, ..., quietly = quietly)
    },
    .package = "base"
  )
  out <- prepR4pcm:::pr_bind_tip(tr, "new_tip", where = 1L, position = 0.25,
                                 edge.length = 0.05)
  expect_s3_class(out, "phylo")
  expect_true("new_tip" %in% out$tip.label)
})


test_that("pr_bind_species monotypic genus uses genus strategy for where = near", {
  tr <- mini_phylo(c("Parus_major"))
  res <- prepR4pcm:::pr_bind_species(tr, "Parus_sp", "Parus_major", where = "near", bl = 0)
  expect_true(res$placed_near %in% tr$tip.label)
})


# --- pr_phylo_cor ------------------------------------------------------------

test_that("pr_phylo_cor returns correlation matrix for phylo and pr_tree_result", {
  tr <- ape::rcoal(6)
  cor1 <- pr_phylo_cor(tr)
  expect_equal(dim(cor1), c(6, 6))
  expect_true(all(diag(cor1) == 1))
  wrapped <- structure(list(tree = tr), class = "pr_tree_result")
  expect_equal(pr_phylo_cor(wrapped), cor1)
})


test_that("pr_phylo_cor recurses over multiPhylo", {
  tr <- ape::rcoal(4)
  mp <- structure(list(tr, tr), class = "multiPhylo")
  mats <- pr_phylo_cor(mp)
  expect_length(mats, 2L)
  expect_equal(dim(mats[[1]]), c(4, 4))
})


test_that("pr_phylo_cor errors on bad input and missing branch lengths", {
  expect_error(pr_phylo_cor("not a tree"), "phylo")
  tr <- mini_phylo(c("a", "b"))
  tr$edge.length <- NULL
  expect_error(pr_phylo_cor(tr), "branch lengths")
})


# --- reconcile_summary console report ----------------------------------------

test_that("reconcile_summary full report lists all sections and truncates long tables", {
  n <- 25L
  mapping <- tibble::tibble(
    name_x        = paste("Species", seq_len(n)),
    name_y        = paste("Tree", seq_len(n)),
    name_resolved = NA_character_,
    match_type    = rep(c("normalized", "synonym", "fuzzy", "manual", "flagged", "unresolved"),
                        length.out = n),
    match_score   = seq_len(n) / n,
    match_source  = "test",
    in_x          = TRUE,
    in_y          = rep(c(TRUE, FALSE), length.out = n),
    notes         = paste("note", seq_len(n))
  )
  meta <- make_demo_meta()
  identity_source <- list(
    provider = "col", version = "2025", mode = "local", rank = "species",
    coverage = c("normalize", "synonym"), backbone_fingerprint = "fp123",
    unchecked = FALSE
  )
  unused <- tibble::tibble(
    name_x = "unused x", name_y = "unused y", reason = "not in data"
  )
  candidates <- tibble::tibble(
    name_x = "Cx", name_y = "Cy", identity_state = "withheld", eligible = FALSE,
    reason = "id", x_id = "1", y_id = "2", provider = "col", version = "2025",
    stage = "fuzzy", review_status = "pending", review_note = NA_character_
  )
  rec <- new_reconciliation(
    mapping, meta,
    unused_overrides = unused,
    candidates = candidates,
    identity_source = identity_source
  )
  summ <- reconcile_summary(rec, detail = "full", format = "console")
  expect_true(grepl("Covered stages", summ$formatted_text, fixed = TRUE))
  expect_true(grepl("Backbone fingerprint", summ$formatted_text, fixed = TRUE))
  expect_true(grepl("Unused Overrides", summ$formatted_text, fixed = TRUE))
  expect_true(grepl("Identity Candidates", summ$formatted_text, fixed = TRUE))
  expect_true(grepl("and 5 more", summ$formatted_text, fixed = TRUE))
})


test_that("print.reconciliation_summary falls back for data.frame-only objects", {
  summ <- reconcile_summary(
    fx_rec_unresolved(), format = "data.frame", detail = "brief"
  )
  expect_output(print(summ), "Reconciliation summary object")
})


# --- reconcile_diff messaging -----------------------------------------------

test_that("reconcile_diff quiet = FALSE prints gained and lost sections", {
  df <- data.frame(species = c("A b", "C d", "E f"), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "((A_b:1,C_d:1):1,E_f:2);")
  r1 <- reconcile_tree(df, tree, x_species = "species", authority = NULL, quiet = TRUE)
  r2 <- reconcile_override(r1, name_x = "E f", name_y = "E_f",
                           action = "accept", note = "test")
  r1$unused_overrides <- tibble::tibble(
    name_x = "ghost", name_y = "ghost_y", reason = "missing"
  )
  expect_output(
    suppressMessages(reconcile_diff(r1, r2, quiet = FALSE)),
    "Gained"
  )
})


# --- pr_authority / taxadb branches (mocked) -----------------------------------

test_that(".pr_taxadb_status_class and .pr_taxadb_accepted_target", {
  expect_equal(
    prepR4pcm:::.pr_taxadb_status_class(c("accepted", "Synonym", NA)),
    c("accepted", "synonym", "unknown")
  )
  hit <- tibble::tibble(
    taxonomicStatus = "accepted",
    taxonID = "ID1",
    scientificName = "Homo sapiens"
  )
  out <- prepR4pcm:::.pr_taxadb_accepted_target(hit, "ID1")
  expect_equal(out$accepted_name, "Homo sapiens")
  expect_null(prepR4pcm:::.pr_taxadb_accepted_target(hit, "OTHER"))
})


test_that(".pr_lookup_taxadb handles accepted, synonym, and lookup failure", {
  skip_if_not_installed("taxadb")
  testthat::local_mocked_bindings(
    filter_name = function(names, provider, version = NULL) {
      tibble::tibble(
        scientificName      = names,
        taxonomicStatus     = "accepted",
        taxonID             = paste0("ID:", names),
        acceptedNameUsageID = paste0("ID:", names)
      )
    },
    filter_id = function(id, provider, version = NULL) {
      tibble::tibble(
        taxonomicStatus = "accepted",
        taxonID = id,
        scientificName = sub("^ID:", "", id)
      )
    },
    .package = "taxadb"
  )
  acc <- prepR4pcm:::.pr_lookup_taxadb("Homo sapiens", "col", NULL)
  expect_equal(acc$status, "accepted")
  expect_equal(acc$accepted_name, "Homo sapiens")

  testthat::local_mocked_bindings(
    filter_name = function(names, provider, version = NULL) {
      tibble::tibble(
        scientificName = names,
        taxonomicStatus = "synonym",
        taxonID = "SYN:1",
        acceptedNameUsageID = "ID:Homo sapiens"
      )
    },
    filter_id = function(id, provider, version = NULL) {
      tibble::tibble(
        taxonomicStatus = "accepted",
        taxonID = "ID:Homo sapiens",
        scientificName = "Homo sapiens"
      )
    },
    .package = "taxadb"
  )
  syn <- prepR4pcm:::.pr_lookup_taxadb("Homo archaicus", "col", "2025")
  expect_equal(syn$status, "synonym")
  expect_equal(syn$accepted_name, "Homo sapiens")

  testthat::local_mocked_bindings(
    filter_name = function(...) stop("network down"),
    .package = "taxadb"
  )
  err <- prepR4pcm:::.pr_lookup_taxadb("X y", "col", NULL)
  expect_equal(err$status, "lookup_error")
})


test_that("pr_lookup_authority returns empty tibble for all-NA names", {
  testthat::local_mocked_bindings(
    pr_ensure_db = function(authority, db_version = NULL) invisible(authority),
    .pr_lookup_taxadb = function(...) stop("should not be called"),
    .package = "prepR4pcm"
  )
  empty <- prepR4pcm:::pr_lookup_authority(NA_character_, authority = "col", quiet = TRUE)
  expect_equal(nrow(empty), 0L)
})


test_that("pr_ensure_db surfaces taxadb install errors via mock", {
  req_ns <- getFromNamespace("requireNamespace", "base")
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "taxadb")) return(FALSE)
      req_ns(package, ..., quietly = quietly)
    },
    .package = "base"
  )
  expect_error(prepR4pcm:::pr_ensure_db("col"), "taxadb")
})


test_that("reconcile_review interactive fuzzy walks flagged and fuzzy rows", {
  mapping <- tibble::tibble(
    name_x        = c("Species one", "Species two"),
    name_y        = c("Species_one", "Species_two"),
    name_resolved = NA_character_,
    match_type    = c("fuzzy", "flagged"),
    match_score   = c(0.91, 0.87),
    match_source  = c("fuzzy", "fuzzy_flagged"),
    in_x          = TRUE,
    in_y          = TRUE,
    notes         = c("", "")
  )
  rec <- new_reconciliation(mapping, make_demo_meta())
  n <- 0L
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline = function(prompt) {
      n <<- n + 1L
      "s"
    },
    .package = "base"
  )
  out <- suppressMessages(reconcile_review(rec, type = "fuzzy", quiet = TRUE))
  expect_equal(n, 2L)
  expect_identical(out$mapping$match_type, rec$mapping$match_type)
})


test_that("reconcile_suggest quiet = FALSE prints a table header", {
  df <- data.frame(species = c("Homo sapiens", "Pan trogloytes"), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species", authority = NULL, quiet = TRUE)
  expect_output(
    suppressMessages(reconcile_suggest(rec, n = 2, threshold = 0.5, quiet = FALSE)),
    "Found suggestions"
  )
})


test_that("pr_cite_tree emits bibtex with auto-generated header", {
  tr <- ape::rcoal(3, tip.label = c("a", "b", "c"))
  res <- structure(
    list(
      tree = tr,
      matched = tr$tip.label,
      unmatched = character(),
      source = "rotl",
      backend_meta = list(backend = "rotl", n_returned = 1L)
    ),
    class = "pr_tree_result"
  )
  bib <- pr_cite_tree(res, format = "bibtex")
  expect_match(bib, "@")
  expect_match(bib, "Auto-generated")
})


test_that("pr_get_tree_status reports installed backends", {
  st <- pr_get_tree_status()
  expect_s3_class(st, "data.frame")
  expect_true("rotl" %in% st$source)
  expect_type(st$installed, "logical")
})


test_that(".pr_rtrees tip parser flags family- and genus-level grafts", {
  parsed <- prepR4pcm:::.pr_parse_rtrees_tip_labels(c(
    "Species_alpha", "Genus_beta*", "Family_gamma**"
  ))
  expect_equal(
    parsed$placement_status,
    c("exact", "genus_added", "family_added")
  )
})


test_that(".pr_resolved_taxadb_version caches resolved snapshot", {
  skip_if_not_installed("taxadb")
  testthat::local_mocked_bindings(
    latest_version = function() "2099-test",
    .package = "taxadb"
  )
  testthat::local_mocked_bindings(
    .pr_cache = new.env(parent = emptyenv()),
    .package = "prepR4pcm"
  )
  v1 <- prepR4pcm:::.pr_resolved_taxadb_version("col", NULL)
  v2 <- prepR4pcm:::.pr_resolved_taxadb_version("col", NULL)
  expect_equal(v1, "2099-test")
  expect_identical(v1, v2)
})


test_that(".pr_lookup_taxadb returns ambiguous when canonical IDs disagree", {
  skip_if_not_installed("taxadb")
  testthat::local_mocked_bindings(
    filter_name = function(names, provider, version = NULL) {
      tibble::tibble(
        scientificName = c(names, names),
        taxonomicStatus = c("accepted", "synonym"),
        taxonID = c("ID1", "SYN1"),
        acceptedNameUsageID = c("ID1", "ID2")
      )
    },
    .package = "taxadb"
  )
  amb <- prepR4pcm:::.pr_lookup_taxadb("Messy name", "col", NULL)
  expect_equal(amb$status, "ambiguous")
})

