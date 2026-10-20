issue139_backbone <- function() {
  list(
    records = tibble::tibble(
      input = c("Coelogyne candoonensis", "Coelogyne cantonensis",
                "Plant olda", "Plant alba", "Plant albus", "Other species"),
      accepted_name = c("Coelogyne candoonensis", "Coelogyne cantonensis",
                        "Plant alba", "Plant alba", "Plant alba", "Other species"),
      accepted_id = c("wcvp:1", "wcvp:2", "wcvp:3", "wcvp:3", "wcvp:3", "wcvp:5"),
      status = c("accepted", "accepted", "synonym", "accepted", "accepted", "accepted"),
      side = "both"
    ),
    provider = "WCVP",
    version = "2025",
    rank = "species"
  )
}

test_that("#139 backbone lookup scales to realistic authority tables", {
  skip_on_cran()

  n <- 10000L
  names <- sprintf("Genus species%05d", seq_len(n))
  records <- data.frame(
    input = names,
    accepted_name = names,
    accepted_id = paste0("COL:", seq_len(n)),
    status = "accepted",
    side = "both",
    stringsAsFactors = FALSE
  )
  backbone <- prepR4pcm:::.pr_validate_identity_backbone(
    list(records = records, provider = "COL", version = "2025", rank = "species"),
    rank = "species"
  )

  elapsed <- system.time({
    found <- prepR4pcm:::.pr_identity_lookup(
      names, backbone, side = "x", rank = "species"
    )
  })[["elapsed"]]
  found_again <- prepR4pcm:::.pr_identity_lookup(
    names, backbone, side = "x", rank = "species"
  )

  expect_equal(nrow(found), n)
  expect_identical(found$accepted_id, paste0("COL:", seq_len(n)))
  expect_identical(found_again, found)
  expect_true(exists("x", envir = backbone$index, inherits = FALSE))
  expect_lt(elapsed, 5)
})

test_that("#139 backbone validation retains frozen provider and identity rank", {
  backbone <- issue139_backbone()
  validated <- prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species")

  expect_identical(validated$provider, "WCVP")
  expect_identical(validated$version, "2025")
  expect_identical(validated$rank, "species")
  expect_true(nzchar(validated$fingerprint))
  expect_equal(nrow(validated$records), nrow(backbone$records))
})

test_that("#139 backbone lookup treats duplicate identities as unique and conflicting IDs as ambiguous", {
  backbone <- issue139_backbone()
  backbone$records <- rbind(
    backbone$records,
    backbone$records[backbone$records$input == "Plant alba", ]
  )
  backbone$records <- rbind(
    backbone$records,
    tibble::tibble(input = "Plant alba", accepted_name = "Plant alba",
                   accepted_id = "wcvp:other", status = "accepted", side = "both")
  )
  validated <- prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species")
  found <- prepR4pcm:::.pr_identity_lookup(
    c("Plant alba", "Plant olda"), validated, side = "x", rank = "species"
  )

  expect_identical(found$identity_status[found$input == "Plant alba"], "ambiguous")
  expect_identical(found$identity_status[found$input == "Plant olda"], "known")
  expect_identical(found$accepted_id[found$input == "Plant olda"], "wcvp:3")
})

test_that("#139 a synonym's accepted target must itself be an accepted COL record", {
  target <- tibble::tibble(
    scientificName = "Gymnosporia gracilis",
    taxonomicStatus = "synonym",
    taxonID = "COL:6KW98",
    acceptedNameUsageID = "COL:6KW8W"
  )

  expect_null(prepR4pcm:::.pr_taxadb_accepted_target(target, "COL:6KW98"))
  expect_identical(
    prepR4pcm:::.pr_taxadb_accepted_target(
      transform(target, taxonomicStatus = "accepted", taxonID = "COL:6KW8W"),
      "COL:6KW8W"
    ),
    list(accepted_name = "Gymnosporia gracilis", accepted_id = "COL:6KW8W")
  )
})

test_that("#139 taxadb identity accepts only explicit accepted or synonym statuses", {
  expect_identical(
    prepR4pcm:::.pr_taxadb_status_class(
      c("accepted", "synonym", "misapplied", "ambiguous synonym",
        "provisionally accepted", NA_character_)
    ),
    c("accepted", "synonym", "unknown", "unknown", "unknown", "unknown")
  )
})

test_that("#139 supplied backbones keep unsupported statuses unknown", {
  backbone <- list(
    records = tibble::tibble(
      input = c("Plant misapplied", "Plant ambiguous", "Plant provisional"),
      accepted_name = c("Plant accepted", "Plant accepted", "Plant accepted"),
      accepted_id = c("W:1", "W:1", "W:1"),
      status = c("misapplied", "ambiguous synonym", "provisionally accepted")
    ),
    provider = "WCVP", version = "2025", rank = "species"
  )
  found <- prepR4pcm:::.pr_identity_lookup(
    backbone$records$input,
    prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species"),
    side = "x", rank = "species"
  )

  expect_identical(found$identity_status, rep("unknown", 3))
  expect_true(all(is.na(found$accepted_id)))

  backbone$records <- rbind(
    backbone$records,
    tibble::tibble(input = "Plant mixed", accepted_name = "Plant mixed",
                   accepted_id = "W:2", status = "accepted"),
    tibble::tibble(input = "Plant mixed", accepted_name = "Plant accepted",
                   accepted_id = "W:1", status = "misapplied")
  )
  mixed <- prepR4pcm:::.pr_identity_lookup(
    "Plant mixed",
    prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species"),
    side = "x", rank = "species"
  )
  expect_identical(mixed$identity_status, "unknown")
  expect_true(is.na(mixed$accepted_id))

  backbone$records <- rbind(
    backbone$records,
    tibble::tibble(input = "Plant incomplete", accepted_name = "Plant incomplete",
                   accepted_id = "W:4", status = "accepted"),
    tibble::tibble(input = "Plant incomplete", accepted_name = "Plant incomplete",
                   accepted_id = NA_character_, status = "accepted")
  )
  incomplete <- prepR4pcm:::.pr_identity_lookup(
    "Plant incomplete",
    prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species"),
    side = "x", rank = "species"
  )
  expect_identical(incomplete$identity_status, "unknown")
  expect_true(is.na(incomplete$accepted_id))
})

test_that("#139 distinct IDs block only the conflicting candidate and keep an eligible runner-up", {
  backbone <- issue139_backbone()
  backbone$records <- backbone$records[backbone$records$input != "Plant albus", ]
  backbone$records <- rbind(
    backbone$records,
    tibble::tibble(input = "Plant albaa", accepted_name = "Plant albaa",
                   accepted_id = "wcvp:4", status = "accepted", side = "both")
  )
  backbone$records <- rbind(
    backbone$records,
    tibble::tibble(input = "Plant albus", accepted_name = "Plant albus",
                   accepted_id = "wcvp:4", status = "accepted", side = "y")
  )
  validated <- prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species")
  x <- prepR4pcm:::.pr_identity_lookup(
    c("Plant albaa"), validated, side = "x", rank = "species"
  )
  y <- prepR4pcm:::.pr_identity_lookup(
    c("Plant alba", "Plant albus"), validated, side = "y", rank = "species"
  )
  candidates <- tibble::tibble(
    name_x = c("Plant albaa", "Plant albaa"),
    name_y = c("Plant alba", "Plant albus"),
    score = c(0.99, 0.96)
  )
  checked <- prepR4pcm:::.pr_check_identity_candidates(candidates, x, y)

  expect_identical(checked$identity_state, c("conflict", "same"))
  expect_identical(checked$eligible, c(FALSE, TRUE))
  expect_identical(checked$name_y[checked$eligible], "Plant albus")
})

test_that("#139 synonym direct matches do not consume an identity-compatible runner-up", {
  lookup <- function(input, accepted_name, id) {
    tibble::tibble(
      input = input, accepted_name = accepted_name,
      accepted_id = id, identity_status = "known",
      provider = "WCVP", version = "2025", rank = "species"
    )
  }
  x <- lookup("Plant olda", "Plant alba", "W:1")
  y <- rbind(
    lookup("Plant alba", "Plant alba", "W:2"),
    lookup("Plant newa", "Plant alba", "W:1")
  )

  found <- pr_resolve_synonyms(
    "Plant olda", c("Plant alba", "Plant newa"), quiet = TRUE,
    lookup_x = x, lookup_y = y
  )

  expect_identical(found$name_x, "Plant olda")
  expect_identical(found$name_y, "Plant newa")

  unknown <- lookup("Plant alba", NA_character_, NA_character_)
  unknown$identity_status <- "unknown"
  y_unknown <- rbind(
    unknown,
    lookup("Plant newa", "Plant alba", "W:1")
  )
  found_unknown <- pr_resolve_synonyms(
    "Plant olda", c("Plant alba", "Plant newa"), quiet = TRUE,
    lookup_x = x, lookup_y = y_unknown
  )
  expect_identical(found_unknown$name_y, "Plant newa")
  expect_identical(attr(found_unknown, "identity_candidates")$name_y, "Plant alba")
})

test_that("#139 withheld direct synonym evidence survives a valid fallback", {
  backbone <- list(
    records = tibble::tibble(
      input = c("Plant olda", "Plant alba", "Plant newa"),
      accepted_name = c("Plant alba", "Plant alba", "Plant alba"),
      accepted_id = c("W:1", "W:2", "W:1"),
      status = c("synonym", "accepted", "synonym"),
      side = c("x", "y", "y")
    ),
    provider = "WCVP", version = "2025", rank = "species"
  )
  out <- reconcile_data(
    data.frame(species = "Plant olda"),
    data.frame(species = c("Plant alba", "Plant newa")),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, fuzzy = FALSE, quiet = TRUE
  )

  matched <- out$mapping[!is.na(out$mapping$match_source) &
                           out$mapping$match_source == "WCVP_synonym", , drop = FALSE]
  expect_identical(matched$name_y, "Plant newa")
  expect_true(any(out$candidates$name_x == "Plant olda" &
                    out$candidates$name_y == "Plant alba" &
                    out$candidates$reason == "distinct_accepted_ids"))
})

test_that("#139 reverse synonym fallback cannot join distinct accepted IDs", {
  backbone <- list(
    records = tibble::tibble(
      input = c("Plant alba", "Plant olda"),
      accepted_name = c("Plant alba", "Plant alba"),
      accepted_id = c("W:1", "W:2"),
      status = c("accepted", "synonym"),
      side = c("x", "y")
    ),
    provider = "WCVP", version = "2025", rank = "species"
  )
  out <- reconcile_data(
    data.frame(species = "Plant alba"),
    data.frame(species = "Plant olda"),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, fuzzy = FALSE, quiet = TRUE
  )

  expect_false(any(out$mapping$match_source == "WCVP_synonym", na.rm = TRUE))
  expect_true(any(out$candidates$name_x == "Plant alba" &
                    out$candidates$name_y == "Plant olda" &
                    out$candidates$reason == "distinct_accepted_ids"))
})

test_that("#139 authority identity evidence follows requested order and duplicates", {
  cached_order <- tibble::tibble(
    input = c("Plant albus", "Plant alba"),
    accepted_name = c("Plant albus", "Plant alba"),
    status = "accepted", taxon_id = c("W:2", "W:1"), authority = "col"
  )
  attr(cached_order, "resolved_version") <- "2025"
  testthat::local_mocked_bindings(
    pr_lookup_authority = function(names, authority, db_version, quiet) cached_order,
    .package = "prepR4pcm"
  )
  requested <- c("Plant alba", "Plant albus", "Plant alba", "Plant absent")
  identity <- prepR4pcm:::.pr_authority_identity_lookup(
    requested, "col", "2025", "species"
  )

  expect_identical(identity$input, requested)
  expect_identical(identity$accepted_id, c("W:1", "W:2", "W:1", NA_character_))
  expect_identical(identity$identity_status, c("known", "known", "known", "unknown"))
  y <- tibble::tibble(
    input = "Plant albaa", accepted_name = "Plant albaa",
    status = "accepted", taxon_id = "W:2", authority = "col"
  )
  attr(y, "resolved_version") <- "2025"
  testthat::local_mocked_bindings(
    pr_lookup_authority = function(names, authority, db_version, quiet) {
      if (identical(names, "Plant albaa")) y else cached_order
    },
    .package = "prepR4pcm"
  )
  identity_x <- prepR4pcm:::.pr_authority_identity_lookup(
    "Plant alba", "col", "2025", "species"
  )
  identity_y <- prepR4pcm:::.pr_authority_identity_lookup(
    "Plant albaa", "col", "2025", "species"
  )
  fuzzy <- pr_fuzzy_match(
    "Plant alba", "Plant albaa", identity_x = identity_x,
    identity_y = identity_y, quiet = TRUE
  )
  expect_equal(nrow(fuzzy), 0L)
})

test_that("#139 a deliberate pair-specific override retains the conflicting evidence", {
  backbone <- issue139_backbone()
  out <- reconcile_data(
    data.frame(species = "Coelogyne candoonensis"),
    data.frame(species = "Coelogyne cantonensis"),
    x_species = "species", y_species = "species", authority = NULL,
    fuzzy = TRUE, backbone = backbone, quiet = TRUE
  )
  reviewed <- reconcile_override(
    out, "Coelogyne candoonensis", "Coelogyne cantonensis",
    note = "Reviewed against an external specimen record"
  )
  manual <- reviewed$mapping[reviewed$mapping$match_type == "manual", , drop = FALSE]

  expect_equal(nrow(manual), 1L)
  expect_identical(reviewed$candidates$reason, "distinct_accepted_ids")
  expect_identical(reviewed$candidates$review_status, "accepted_manual")
  expect_match(reviewed$candidates$review_note, "external specimen record")
})

test_that("#139 rejecting a withheld pair leaves a different accepted pair intact", {
  backbone <- list(
    records = tibble::tibble(
      input = c("Plant olda", "Plant alba", "Plant newa"),
      accepted_name = c("Plant alba", "Plant alba", "Plant alba"),
      accepted_id = c("W:1", "W:2", "W:1"),
      status = c("synonym", "accepted", "synonym"),
      side = c("x", "y", "y")
    ),
    provider = "WCVP", version = "2025", rank = "species"
  )
  out <- reconcile_data(
    data.frame(species = "Plant olda"),
    data.frame(species = c("Plant alba", "Plant newa")),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, fuzzy = FALSE, quiet = TRUE
  )
  synonym_rows <- !is.na(out$mapping$match_source) &
    out$mapping$match_source == "WCVP_synonym"
  expect_identical(out$mapping$name_y[synonym_rows], "Plant newa")
  before <- out$mapping
  reviewed <- reconcile_override(
    out, "Plant olda", "Plant alba", action = "reject", note = "distinct IDs"
  )

  expect_identical(reviewed$mapping, before)
  expect_identical(reviewed$candidates$review_status, "rejected_manual")
})

test_that("#139 tree augmentation retains withheld identity provenance", {
  out <- fx_rec_tree()
  out$candidates <- tibble::tibble(
    name_x = "Plant blocked", name_y = "Plant target",
    reason = "distinct_accepted_ids", eligible = FALSE
  )
  out$meta$identity_source <- list(provider = "WCVP", version = "2025")
  augmented <- reconcile_augment(out, fx_tree_with_congeners(), quiet = TRUE,
                                 check_ultrametric = FALSE)

  expect_identical(augmented$candidates, out$candidates)
  expect_identical(augmented$meta$identity_source, out$meta$identity_source)
})

test_that("#139 backbone rejects contradictory duplicate records and rank mismatch", {
  backbone <- issue139_backbone()
  backbone$records <- rbind(
    backbone$records,
    tibble::tibble(input = "Coelogyne candoonensis",
                   accepted_name = "Coelogyne candoonensis",
                   accepted_id = "wcvp:other", status = "accepted", side = "both")
  )
  validated <- prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species")
  found <- prepR4pcm:::.pr_identity_lookup("Coelogyne candoonensis", validated,
                                          side = "x", rank = "species")
  expect_identical(found$identity_status, "ambiguous")

  backbone <- issue139_backbone()
  backbone$rank <- "subspecies"
  expect_error(
    prepR4pcm:::.pr_validate_identity_backbone(backbone, rank = "species"),
    "rank"
  )
})

test_that("#139 backbone stage filters known conflicts and records candidate evidence", {
  backbone <- issue139_backbone()
  x <- data.frame(species = "Coelogyne candoonensis")
  y <- data.frame(species = "Coelogyne cantonensis")
  out <- reconcile_data(
    x, y, x_species = "species", y_species = "species", authority = NULL,
    fuzzy = TRUE, resolve = "first", backbone = backbone, quiet = TRUE
  )

  expect_equal(out$counts$n_fuzzy, 0L)
  expect_equal(out$counts$n_unresolved_x, 1L)
  expect_equal(nrow(out$candidates), 1L)
  expect_identical(out$candidates$reason, "distinct_accepted_ids")
  expect_identical(out$candidates$provider, "WCVP")
  expect_identical(reconcile_candidates(out), out$candidates)
  expect_match(paste(capture.output(print(reconcile_summary(out))), collapse = "\n"),
               "Identity candidates withheld: 1")
  report <- reconcile_report(out, file = tempfile(fileext = ".html"))
  report_html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(report_html, "Identity candidates withheld")
  expect_match(report_html, "distinct_accepted_ids")
  unlink(report)
  export_dir <- tempfile("issue139-export-")
  files <- reconcile_export(out, dir = export_dir, prefix = "identity")
  expect_true(file.exists(files$candidates))
  expect_identical(read.csv(files$candidates)$reason, "distinct_accepted_ids")
  unlink(export_dir, recursive = TRUE)
})

test_that("#139 supplied backbone takes precedence over requested authority", {
  backbone <- issue139_backbone()
  calls <- 0L
  testthat::local_mocked_bindings(
    pr_lookup_authority = function(...) {
      calls <<- calls + 1L
      stop("authority lookup must not run when backbone is supplied")
    },
    .package = "prepR4pcm"
  )
  x <- data.frame(species = "Plant olda")
  y <- data.frame(species = "Plant alba")

  out <- reconcile_data(x, y, x_species = "species", y_species = "species",
                        authority = "col", backbone = backbone, quiet = TRUE)

  expect_equal(calls, 0L)
  expect_true(out$counts$n_synonym == 1L)
  expect_identical(out$meta$identity_source$provider, "WCVP")
  expect_identical(out$meta$identity_source$version, "2025")
})

test_that("#139 exact and normalized joins are checked when backbone evidence is bound", {
  backbone <- list(
    records = tibble::tibble(
      input = c("Plant alba", "Plant alba", "Plant_betaa", "Plant betaa"),
      accepted_name = c("Plant alba", "Plant alba", "Plant betaa", "Plant betaa"),
      accepted_id = c("W:1", "W:2", "W:3", "W:4"),
      status = "accepted", side = c("x", "y", "x", "y")
    ), provider = "WCVP", version = "2025", rank = "species"
  )
  exact <- reconcile_data(
    data.frame(species = "Plant alba"), data.frame(species = "Plant alba"),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, quiet = TRUE
  )
  normalized <- reconcile_data(
    data.frame(species = "Plant_betaa"), data.frame(species = "Plant betaa"),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, quiet = TRUE
  )

  expect_equal(exact$counts$n_exact, 0L)
  expect_equal(normalized$counts$n_normalized, 0L)
  expect_equal(nrow(exact$candidates), 1L)
  expect_equal(nrow(normalized$candidates), 1L)
  expect_true(all(c(exact$candidates$reason, normalized$candidates$reason) ==
                     "distinct_accepted_ids"))
})

test_that("#139 identical accepted-name text cannot join distinct IDs, while a shared ID can", {
  backbone <- list(
    records = tibble::tibble(
      input = c("Plant syna", "Plant synb", "Plant sync", "Plant alba"),
      accepted_name = rep("Plant alba", 4),
      accepted_id = c("W:1", "W:2", "W:3", "W:3"),
      status = c("synonym", "synonym", "synonym", "accepted"),
      side = "both"
    ), provider = "WCVP", version = "2025", rank = "species"
  )
  conflicting <- reconcile_data(
    data.frame(species = "Plant syna"), data.frame(species = "Plant synb"),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, quiet = TRUE
  )
  supported <- reconcile_data(
    data.frame(species = "Plant sync"), data.frame(species = "Plant alba"),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, quiet = TRUE
  )

  expect_equal(conflicting$counts$n_synonym, 0L)
  expect_identical(conflicting$candidates$reason, "distinct_accepted_ids")
  expect_equal(supported$counts$n_synonym, 1L)
  expect_equal(nrow(supported$candidates), 0L)
})

test_that("#139 all five reconciliation entry points apply the same guard", {
  backbone <- issue139_backbone()
  tree_x <- ape::read.tree(text = "(Coelogyne_candoonensis:1,Other_species:1);")
  tree_y <- ape::read.tree(text = "(Coelogyne_cantonensis:1,Other_species:1);")
  data_x <- data.frame(species = c("Coelogyne candoonensis", "Other species"))
  data_y <- data.frame(species = c("Coelogyne cantonensis", "Other species"))
  check <- function(out) {
    expect_true(any(out$candidates$reason == "distinct_accepted_ids"))
    incorrect <- out$mapping$name_x == "Coelogyne candoonensis" &
      out$mapping$name_y == "Coelogyne cantonensis" &
      out$mapping$match_type != "unresolved"
    expect_false(any(incorrect, na.rm = TRUE))
  }

  check(reconcile_data(data_x, data_y, x_species = "species", y_species = "species",
                       authority = NULL, fuzzy = TRUE, backbone = backbone, quiet = TRUE))
  check(reconcile_tree(data_x, tree_y, x_species = "species", authority = NULL,
                       fuzzy = TRUE, backbone = backbone, quiet = TRUE))
  check(reconcile_trees(tree_x, tree_y, authority = NULL, fuzzy = TRUE,
                        backbone = backbone, quiet = TRUE))
  check(reconcile_multi(list(a = data_x), tree_y, species_cols = "species",
                        authority = NULL, fuzzy = TRUE, backbone = backbone,
                        quiet = TRUE))
  multi_tree <- reconcile_to_trees(data_x, list(target = tree_y),
                                  x_species = "species", authority = NULL,
                                  fuzzy = TRUE, backbone = backbone, quiet = TRUE)
  check(multi_tree$target)
})

test_that("#139 authority evidence blocks distinct-ID fuzzy pairs under both resolve modes", {
  mock <- function(names, authority = "col", db_version = NULL, quiet = FALSE) {
    ids <- c("Coelogyne candoonensis" = "COL:1",
             "Coelogyne cantonensis" = "COL:2")
    tibble::tibble(
      input = names, accepted_name = names, status = "accepted",
      taxon_id = unname(ids[names]), authority = authority
    )
  }
  testthat::local_mocked_bindings(pr_lookup_authority = mock,
                                  .package = "prepR4pcm")
  x <- data.frame(species = "Coelogyne candoonensis")
  y <- data.frame(species = "Coelogyne cantonensis")

  for (mode in c("flag", "first")) {
    out <- reconcile_data(x, y, x_species = "species", y_species = "species",
                          authority = "col", fuzzy = TRUE, resolve = mode,
                          quiet = TRUE)
    expect_equal(out$counts$n_fuzzy, 0L)
    expect_equal(out$counts$n_unresolved_x, 1L)
    expect_true(any(out$candidates$reason == "distinct_accepted_ids"))
  }
})

test_that("#139 authority eligibility is applied before selecting the best fuzzy candidate", {
  ids <- c("Plant albaa" = "COL:4", "Plant alba" = "COL:3",
           "Plant albas" = "COL:4")
  mock <- function(names, authority = "col", db_version = NULL, quiet = FALSE) {
    tibble::tibble(input = names, accepted_name = names, status = "accepted",
                   taxon_id = unname(ids[names]), authority = authority)
  }
  testthat::local_mocked_bindings(pr_lookup_authority = mock,
                                  .package = "prepR4pcm")
  out <- reconcile_data(
    data.frame(species = "Plant albaa"),
    data.frame(species = c("Plant alba", "Plant albas")),
    x_species = "species", y_species = "species", authority = "col",
    fuzzy = TRUE, fuzzy_threshold = 0.8, resolve = "first", quiet = TRUE
  )

  matched <- out$mapping[out$mapping$in_x & out$mapping$in_y, , drop = FALSE]
  expect_equal(nrow(matched), 1L)
  expect_identical(matched$name_y, "Plant albas")
  expect_true(any(out$candidates$reason == "distinct_accepted_ids"))
})

test_that("#139 taxadb adapter preserves ambiguity and distinguishes lookup errors", {
  skip_if_not_installed("taxadb")
  testthat::local_mocked_bindings(
    filter_name = function(...) tibble::tibble(
      scientificName = c("Aus alba", "Aus alba"),
      taxonomicStatus = c("accepted", "accepted"),
      taxonID = c("COL:1", "COL:2"),
      acceptedNameUsageID = c("COL:1", "COL:2")
    ),
    .package = "taxadb"
  )
  ambiguous <- prepR4pcm:::.pr_lookup_taxadb("Aus alba", "col", "2025")
  expect_identical(ambiguous$status, "ambiguous")
  expect_true(is.na(ambiguous$taxon_id))

  testthat::local_mocked_bindings(
    filter_name = function(...) stop("database unavailable"),
    .package = "taxadb"
  )
  failed <- prepR4pcm:::.pr_lookup_taxadb("Aus alba", "col", "2025")
  expect_identical(failed$status, "lookup_error")
})

test_that("#139 taxadb adapter does not treat unsupported statuses as synonyms", {
  skip_if_not_installed("taxadb")
  testthat::local_mocked_bindings(
    filter_name = function(names, ...) tibble::tibble(
      scientificName = c("Aus alba", "Aus alba", "Bus alba"),
      taxonomicStatus = c("misapplied", "accepted", "misapplied"),
      taxonID = c("COL:misapplied", "COL:accepted", "COL:other"),
      acceptedNameUsageID = c("COL:accepted", "COL:accepted", "COL:target")
    ),
    filter_id = function(...) stop("unsupported statuses must not be followed"),
    .package = "taxadb"
  )

  out <- prepR4pcm:::.pr_lookup_taxadb(c("Aus alba", "Bus alba"), "col", "2025")

  expect_identical(out$status, c("unknown", "unknown"))
  expect_true(all(is.na(out$accepted_name)))
  expect_true(all(is.na(out$taxon_id)))

  testthat::local_mocked_bindings(
    filter_name = function(...) tibble::tibble(
      scientificName = c("Aus alba", "Aus alba"),
      taxonomicStatus = c("accepted", "synonym"),
      taxonID = c("COL:accepted", NA_character_),
      acceptedNameUsageID = c("COL:accepted", NA_character_)
    ),
    filter_id = function(...) stop("a missing synonym ID must not be followed"),
    .package = "taxadb"
  )
  mixed <- prepR4pcm:::.pr_lookup_taxadb("Aus alba", "col", "2025")
  expect_identical(mixed$status, "unknown")
  expect_true(is.na(mixed$taxon_id))
})

test_that("#139 shared accepted-name synonyms pair by identity before consuming names", {
  backbone <- list(
    records = tibble::tibble(
      input = c("Plant olda", "Plant oldb", "Plant newa", "Plant newb"),
      accepted_name = rep("Plant alba", 4),
      accepted_id = c("W:1", "W:2", "W:2", "W:1"),
      status = "synonym",
      side = c("x", "x", "y", "y")
    ),
    provider = "WCVP", version = "2025", rank = "species"
  )
  out <- reconcile_data(
    data.frame(species = c("Plant olda", "Plant oldb")),
    data.frame(species = c("Plant newa", "Plant newb")),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, quiet = TRUE
  )
  matched <- out$mapping[out$mapping$match_type != "unresolved", , drop = FALSE]

  expect_equal(nrow(matched), 2L)
  expect_setequal(
    paste(matched$name_x, matched$name_y),
    c("Plant olda Plant newb", "Plant oldb Plant newa")
  )
  expect_equal(nrow(out$candidates), 0L)
})
