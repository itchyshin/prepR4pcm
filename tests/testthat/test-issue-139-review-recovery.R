review_recovery_fixture <- function(x = c("Trema tomentosa", "Trema orientalis"),
                                    y = c("Trema tomentosum", "Trema orientale")) {
  backbone <- list(
    records = tibble::tibble(input = c(x, y), accepted_name = NA_character_,
      accepted_id = NA_character_, status = "unknown", side = "both"),
    provider = "fixture", version = "1", rank = "species"
  )
  reconcile_data(data.frame(species = x), data.frame(species = y),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = backbone, fuzzy = TRUE, quiet = TRUE)
}

review_recovery_interact <- function(rec, answers = "s", type = "flagged") {
  review <- reconcile_review
  io <- new.env(parent = environment(review))
  io$interactive <- function() TRUE
  at <- 0L
  io$readline <- function(...) {
    at <<- at + 1L
    if (at > length(answers)) "q" else answers[[at]]
  }
  environment(review) <- io
  output <- capture.output(result <- review(rec, type = type, quiet = TRUE))
  list(result = result, output = paste(output, collapse = "\n"), prompts = at)
}

test_that("LR-R01 Trema tomentosa is visible in ordinary review with unknown identity", {
  rec <- review_recovery_fixture()
  expect_true(any(reconcile_candidates(rec)$name_x == "Trema tomentosa"))
  reviewed <- review_recovery_interact(rec, c("s", "s"))
  expect_match(reviewed$output, "Trema tomentosa")
  expect_match(reviewed$output, "Trema tomentosum")
  expect_identical(reviewed$result$mapping, rec$mapping)
  summary <- reconcile_summary(rec, format = "data.frame")
  expect_true("review_candidates" %in% names(summary$tables))
  expect_true(any(summary$tables$review_candidates$name_x == "Trema tomentosa"))
})

test_that("LR-R02 Trema orientalis is visible in ordinary review with unknown identity", {
  rec <- review_recovery_fixture()
  reviewed <- review_recovery_interact(rec, c("s", "s"))
  expect_match(reviewed$output, "Trema orientalis")
  expect_match(reviewed$output, "Trema orientale")
  expect_match(paste(capture.output(reconcile_summary(rec)), collapse = "\n"),
               "review.*reconcile_review|reconcile_review.*review")
})

test_that("LR-R03 ordinary spelling candidates remain visible without an ending label", {
  rec <- review_recovery_fixture("Plant albba", "Plant alba")
  reviewed <- review_recovery_interact(rec)
  expect_match(reviewed$output, "Plant albba")
  expect_match(reviewed$output, "identity_not_established")
  expect_false(grepl("Latin-aware", reviewed$output, fixed = TRUE))
})

test_that("LR-R04 alternatives and identity evidence remain distinct", {
  rec <- review_recovery_fixture("Trema tomentosa",
                                 c("Trema tomentosum", "Trema tomentosus"))
  candidates <- reconcile_candidates(rec)
  expect_setequal(candidates$name_y, c("Trema tomentosum", "Trema tomentosus"))
  expect_true(all(candidates$identity_state == "unknown"))
  expect_match(review_recovery_interact(rec, c("s", "s"))$output, "Trema tomentosus")
  ix <- tibble::tibble(input = "Plant albus", identity_status = "ambiguous",
    accepted_id = NA_character_, provider = "COL", version = "2026", rank = "species")
  iy <- tibble::tibble(input = "Plant alba", identity_status = "known",
    accepted_id = "COL:1", provider = "COL", version = "2026", rank = "species")
  ambiguous <- attr(pr_fuzzy_match("Plant albus", "Plant alba",
    identity_x = ix, identity_y = iy, quiet = TRUE), "identity_candidates")
  expect_match(ambiguous$evidence_detail, "ambiguous")
  ix$identity_status <- "known"
  ix$accepted_id <- "COL:1"
  ix$version <- "2025"
  incomparable <- attr(pr_fuzzy_match("Plant albus", "Plant alba",
    identity_x = ix, identity_y = iy, quiet = TRUE), "identity_candidates")
  expect_match(incomparable$evidence_detail, "incomparable")
})

test_that("LR-R05 pending alternatives never consume reference targets", {
  rec <- review_recovery_fixture(c("Plant albba", "Plant albaa"), "Plant alba")
  expect_equal(nrow(rec$candidates), 2L)
  expect_false(any(rec$mapping$in_x & rec$mapping$in_y))
  skipped <- review_recovery_interact(rec, c("s", "s"))$result
  expect_identical(skipped$mapping, rec$mapping)
  absent <- review_recovery_fixture("Trema tomentosa", "Other species")
  expect_equal(nrow(absent$candidates), 0L)
  expect_false(any(absent$mapping$in_x & absent$mapping$in_y))
  occupied <- reconcile_data(
    data.frame(species = c("Trema tomentosa", "Trema tomentosum var. tomentosum")),
    data.frame(species = "Trema tomentosum"), x_species = "species",
    y_species = "species", authority = NULL, fuzzy = TRUE, quiet = TRUE)
  expect_true(any(reconcile_candidates(occupied)$name_x == "Trema tomentosa"))
  reviewed <- review_recovery_interact(occupied, "a")
  expect_match(reviewed$output, "Trema tomentosa -> Trema tomentosum")
  expect_match(reviewed$output, "already assigned")
  expect_identical(reviewed$result$mapping, occupied$mapping)
  expect_equal(nrow(reviewed$result$overrides), 0L)
  looked_up <- character()
  local_mocked_bindings(.pr_authority_identity_lookup = function(names, authority,
      db_version, rank, quiet) {
    looked_up <<- c(looked_up, names)
    tibble::tibble(input = names, accepted_id = NA_character_,
      accepted_name = NA_character_, identity_status = "unknown",
      provider = "col", version = "2026", rank = rank)
  }, .package = "prepR4pcm")
  occupied_authority <- pr_run_cascade(c("Trema tomentosa", "Trema tomentosum", "Unrelated arborea"),
    c("Trema tomentosum", "Unrelated arborea"), authority = "col",
    fuzzy = TRUE, quiet = TRUE)
  expect_setequal(looked_up, c("Trema tomentosa", "Trema tomentosum"))
  expect_identical(attr(occupied_authority, "identity_source")$version, "2026")
  expect_true(length(attr(occupied_authority, "identity_source")$coverage) > 0L)
})

test_that("LR-A01 pending review does not change default data or tree application", {
  rec <- review_recovery_fixture()
  data <- data.frame(species = c("Trema tomentosa", "Trema orientalis"))
  tree <- ape::read.tree(text = "(Trema_tomentosum:1,Trema_orientale:1);")
  out <- reconcile_apply(rec, data = data, tree = tree, species_col = "species")
  expect_identical(out$data, data)
  expect_identical(out$tree$tip.label, tree$tip.label)
  expect_false(any(rec$mapping$in_x & rec$mapping$in_y))
})

test_that("LR-A02 include_flagged cannot apply candidate-table suggestions", {
  rec <- review_recovery_fixture()
  data <- data.frame(species = c("Trema tomentosa", "Trema orientalis"))
  out <- reconcile_apply(rec, data = data, species_col = "species",
    include_flagged = TRUE, drop_unresolved = TRUE)
  expect_equal(nrow(out$data), 0L)
  legacy <- rec
  legacy$mapping <- tibble::tibble(name_x = "Trema tomentosa",
    name_y = "Trema tomentosum", name_resolved = NA_character_,
    match_type = "flagged", match_score = .93, match_source = "fuzzy",
    in_x = TRUE, in_y = TRUE, notes = "legacy flagged mapping")
  expect_warning(applied <- reconcile_apply(legacy, data = data,
    species_col = "species", include_flagged = TRUE, drop_unresolved = TRUE),
    "Including flagged")
  expect_identical(applied$data$species, "Trema tomentosa")
})

test_that("LR-A03 decisions are pair-specific and competing alternatives stay consistent", {
  rec <- review_recovery_fixture("Trema tomentosa",
                                 c("Trema tomentosum", "Trema tomentosus"))
  rejected <- review_recovery_interact(rec, c("r", "s"))$result
  expect_equal(sum(rejected$candidates$review_status == "rejected_manual"), 1L)
  expect_equal(sum(rejected$candidates$review_status == "pending"), 1L)
  expect_identical(rejected$mapping, rec$mapping)
  accepted <- review_recovery_interact(rec, c("a", "a"))
  expect_equal(accepted$prompts, 1L)
  expect_equal(sum(accepted$result$mapping$match_type == "manual"), 1L)
  expect_equal(sum(accepted$result$candidates$review_status == "accepted_manual"), 1L)
  expect_true(nzchar(accepted$result$overrides$user_note[[1L]]))
  expect_equal(nrow(reconcile_summary(accepted$result,
    format = "data.frame")$tables$review_candidates), 0L)
})

test_that("LR-A04 old objects and all review modes remain usable", {
  rec <- review_recovery_fixture()
  legacy <- rec
  legacy$candidates <- NULL
  expect_identical(review_recovery_interact(legacy)$result$mapping, rec$mapping)
  old_candidates <- review_recovery_fixture("Trema tomentosa",
    c("Trema tomentosum", "Trema tomentosus"))
  old_candidates$candidates$review_status <- NULL
  old_candidates$candidates$review_note <- NULL
  rejected <- review_recovery_interact(old_candidates, c("r", "s"))
  expect_equal(rejected$prompts, 2L)
  expect_equal(sum(rejected$result$candidates$review_status == "pending"), 1L)
  expect_equal(sum(rejected$result$candidates$review_status == "rejected_manual"), 1L)
  for (mode in c("flagged", "fuzzy", "identity_candidates")) {
    reviewed <- review_recovery_interact(rec, c("s", "s"), type = mode)
    expect_match(reviewed$output, "Trema orientalis")
    expect_identical(reviewed$result$mapping, rec$mapping)
  }
  quit <- review_recovery_interact(rec, "q")
  expect_equal(quit$prompts, 1L)
  expect_identical(quit$result$candidates, rec$candidates)
})

test_that("LR-A05 five original distinct-ID pairs cannot be applied automatically", {
  x <- c("Coelogyne candoonensis", "Coussarea petiolaris", "Croton heterocarpus",
         "Croton leiophyllus", "Croton stenopetalus")
  y <- c("Coelogyne cantonensis", "Coussarea impetiolaris", "Croton heterochrous",
         "Croton leucophyllus", "Croton stenosepalus")
  bb <- list(records = tibble::tibble(input = c(x, y), accepted_name = c(x, y),
    accepted_id = paste0("fixture:", seq_len(10L)), status = "accepted", side = "both"),
    provider = "fixture", version = "1", rank = "species")
  rec <- reconcile_data(data.frame(species = x), data.frame(species = y),
    x_species = "species", y_species = "species", authority = NULL,
    backbone = bb, fuzzy = TRUE, quiet = TRUE)
  expect_false(any(rec$mapping$in_x & rec$mapping$in_y))
  for (i in seq_along(x)) {
    rows <- rec$candidates$name_x == x[[i]] & rec$candidates$name_y == y[[i]]
    expect_true(any(rows))
    expect_true(all(rec$candidates$reason[rows] == "distinct_accepted_ids"))
  }
  applied <- reconcile_apply(rec, data = data.frame(species = x),
    species_col = "species", include_flagged = TRUE, drop_unresolved = TRUE)
  expect_equal(nrow(applied$data), 0L)
})

test_that("LR-A06 cross-genus accepted identities and uncertain evidence stay separate", {
  ix <- tibble::tibble(input = "Oldgenus albus", identity_status = "known",
    accepted_id = "fixture:1", provider = "fixture", version = "1", rank = "species")
  iy <- ix
  iy$input <- "Differentgenus alba"
  known <- pr_fuzzy_match(ix$input, iy$input, identity_x = ix, identity_y = iy, quiet = TRUE)
  expect_equal(nrow(known), 1L)
  ix$identity_status <- "unknown"
  ix$accepted_id <- NA_character_
  unknown <- pr_fuzzy_match(ix$input, iy$input, identity_x = ix, identity_y = iy, quiet = TRUE)
  expect_equal(nrow(unknown), 0L)
  expect_true(all(known$name_y == "Differentgenus alba"))
})
