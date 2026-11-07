wcvp_fixture <- function() {
  list(version = "16", names = data.frame(
    plant_name_id = as.character(1:13),
    taxon_name = c("Glandularia verecunda", "Verbena verecunda", "Oldgenus verecunda",
                   "Adelia panamensis", "Adelia triloba", "Plant species",
                   "Plant species var. minor", "Plant oldminor", "Ambigua plant",
                   "Ambigua plant", "Broken plant", "Cycle one", "Cycle two"),
    accepted_plant_name_id = c("1", "1", "2", "4", "5", "6", "7", "7",
                               "4", "5", "99", "13", "12"),
    taxon_rank = c(rep("Species", 6), "Variety", rep("Species", 6)),
    taxon_status = c("Accepted", "Synonym", "Synonym", "Accepted", "Accepted",
                     "Accepted", "Accepted", rep("Synonym", 6)),
    stringsAsFactors = FALSE
  ))
}

wcvp_rec <- function(x, y, rank = "species") {
  reconcile_data(data.frame(species = x), data.frame(species = y),
                 authority = NULL, rank = rank, quiet = TRUE)
}

test_that("a synonym-labelled target is recovered by terminal accepted identity", {
  rec <- wcvp_rec(c("Glandularia verecunda", "Oldgenus verecunda"), "Verbena_verecunda")
  out <- reconcile_wcvp(rec, wcvp_fixture())
  expect_identical(out$mapping, rec$mapping)
  expect_setequal(out$candidates$name_x, c("Glandularia verecunda", "Oldgenus verecunda"))
  expect_true(all(out$candidates$name_y == "Verbena_verecunda"))
  expect_true(all(out$candidates$x_id == out$candidates$y_id))
  expect_true(all(!out$candidates$eligible))
  expect_true(all(out$candidates$review_status == "pending"))
  expect_identical(out$wcvp$version, "16")
  again <- reconcile_wcvp(out, wcvp_fixture())
  expect_identical(again$candidates, out$candidates)
  applied <- reconcile_apply(out, data = data.frame(species = rec$mapping$name_x[rec$mapping$in_x]),
                             species_col = "species", include_flagged = TRUE, drop_unresolved = TRUE)
  expect_equal(nrow(applied$data), 0L)
})

test_that("different accepted species, ambiguous names and broken chains do not generate pairs", {
  rec <- wcvp_rec(c("Adelia panamensis", "Ambigua plant", "Broken plant", "Cycle one"),
                  c("Adelia_triloba", "Cycle_two"))
  out <- reconcile_wcvp(rec, wcvp_fixture())
  expect_equal(nrow(out$candidates), 0L)
  expect_true(any(out$wcvp$input_audit$status == "ambiguous"))
  expect_true(any(out$wcvp$input_audit$status == "unresolved"))
})

test_that("infraspecific identity is preserved even for a species-level reconciliation", {
  rec <- wcvp_rec("Plant oldminor", "Plant_species")
  out <- reconcile_wcvp(rec, wcvp_fixture())
  expect_equal(nrow(out$candidates), 0L)
  same <- reconcile_wcvp(wcvp_rec("Plant oldminor", "Plant_species_var._minor"), wcvp_fixture())
  expect_equal(nrow(same$candidates), 1L)
  expect_identical(same$candidates$accepted_rank, "variety")
})

test_that("duplicate identities are withheld and occupied tips remain flagged for review", {
  ambiguous <- reconcile_wcvp(wcvp_rec("Oldgenus verecunda",
                   c("Verbena_verecunda", "Glandularia_verecunda")), wcvp_fixture())
  expect_equal(nrow(ambiguous$candidates), 0L)
  expect_true(any(ambiguous$wcvp$input_audit$target_status == "multiple_targets"))
  rec <- wcvp_rec(c("Oldgenus verecunda", "Verbena_verecunda"), "Verbena_verecunda")
  out <- reconcile_wcvp(rec, wcvp_fixture())
  pending <- .pr_pending_review_candidates(out)
  expect_true(pending$target_in_use[[1]])
  expect_identical(out$mapping, rec$mapping)
})

test_that("frozen source structure and duplicate record IDs are validated", {
  rec <- wcvp_rec("Oldgenus verecunda", "Verbena_verecunda")
  expect_error(reconcile_wcvp(rec, list()), "names.*version")
  bad <- wcvp_fixture(); bad$version <- ""
  expect_error(reconcile_wcvp(rec, bad), "version")
  bad <- wcvp_fixture(); bad$names$plant_name_id[2] <- "1"
  expect_error(reconcile_wcvp(rec, bad), "unique")
  bad <- wcvp_fixture(); bad$names$taxon_rank <- NULL
  expect_error(reconcile_wcvp(rec, bad), "taxon_rank")
})

test_that("TNRS can bridge a returned accepted name to a synonym tip with a local snapshot", {
  rec <- wcvp_rec("Typogenus verecunda", "Verbena_verecunda")
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) list(
      results = data.frame(Name_submitted = names, Accepted_name = "Glandularia verecunda",
                           Accepted_name_id = "service-id", Overall_score = .9),
      source_metadata = data.frame(sourceName = "WCVP", version = "service-version")),
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec, wcvp = wcvp_fixture())
  expect_identical(out$candidates$name_y, "Verbena_verecunda")
  expect_identical(out$mapping, rec$mapping)
  expect_identical(out$tnrs$source_version, "service-version")
  expect_identical(out$tnrs$target_reference_version, "16")
  expect_identical(out$candidates$identity_state, "unknown")
  expect_false(out$candidates$eligible)
  expect_error(reconcile_tnrs(rec, source = "wfo", wcvp = wcvp_fixture()), "wcvp")
})

test_that("local TNRS evidence cannot collapse ranks or override a known distinct identity", {
  rec <- wcvp_rec(c("Adelia panamensis", "Plant oldminor"), c("Adelia_triloba", "Plant_species"))
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) list(results = data.frame(
      Name_submitted = names, Accepted_name = c("Adelia triloba", "Plant species"))),
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec, wcvp = wcvp_fixture())
  expect_equal(nrow(out$candidates), 0L)
  expect_true(all(out$tnrs$target_audit$status == "wcvp_input_identity_conflict"))
})

test_that("invalid local references fail before any TNRS request", {
  rec <- wcvp_rec("Typogenus verecunda", "Verbena_verecunda")
  calls <- 0L
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(...) { calls <<- calls + 1L; stop("queried") },
    .package = "prepR4pcm"
  )
  expect_error(reconcile_tnrs(rec, wcvp = list()), "names.*version")
  expect_equal(calls, 0L)
})

test_that("empty and no-hit local references return usable audit tables", {
  rec <- wcvp_rec("Unknown plant", "Another_plant")
  empty <- wcvp_fixture(); empty$names <- empty$names[FALSE, ]
  for (w in list(empty, wcvp_fixture())) {
    out <- reconcile_wcvp(rec, w)
    expect_equal(nrow(out$candidates), 0L)
    expect_identical(out$wcvp$input_audit$status, "not_found")
    expect_identical(out$mapping, rec$mapping)
  }
})

test_that("changing the WCVP snapshot supersedes unsupported local candidates", {
  rec <- wcvp_rec("Oldgenus verecunda", "Verbena_verecunda")
  first <- reconcile_wcvp(rec, wcvp_fixture())
  revised <- wcvp_fixture(); revised$version <- "17"
  revised$names$accepted_plant_name_id[3] <- "4"
  second <- reconcile_wcvp(first, revised)
  expect_equal(nrow(.pr_pending_review_candidates(second)), 0L)
  expect_false(second$candidates$wcvp_supported[[1]])
  expect_identical(second$candidates$review_status, "superseded")
  expect_length(second$wcvp$queries, 2L)
  restored <- reconcile_wcvp(second, wcvp_fixture())
  expect_equal(nrow(.pr_pending_review_candidates(restored)), 1L)
  expect_true(restored$candidates$wcvp_supported[[1]])
  revised$version <- "16" # fingerprint changes even with the same version label
  same_label <- reconcile_wcvp(first, revised)
  expect_equal(nrow(.pr_pending_review_candidates(same_label)), 0L)
  first$candidates$review_status <- "rejected"
  rejected <- reconcile_wcvp(first, wcvp_fixture())
  expect_identical(rejected$candidates$review_status, "rejected")
})

test_that("current local support is structured regardless of evidence arrival order", {
  rec <- wcvp_rec("Oldgenus verecunda", "Verbena_verecunda")
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) list(results = data.frame(
      Name_submitted = names, Accepted_name = "Glandularia verecunda")),
    .package = "prepR4pcm"
  )
  a <- reconcile_tnrs(reconcile_wcvp(rec, wcvp_fixture()), wcvp = wcvp_fixture())
  b <- reconcile_wcvp(reconcile_tnrs(rec, wcvp = wcvp_fixture()), wcvp_fixture())
  fields <- c("wcvp_supported", "wcvp_version", "wcvp_fingerprint",
              "wcvp_accepted_id", "wcvp_accepted_rank")
  expect_identical(a$candidates[, fields], b$candidates[, fields])
  expect_true(a$candidates$wcvp_supported[[1]])
})

test_that("explicit Kew nomenclatural redirects remain reviewable with their status", {
  rec <- wcvp_rec("Glandularia verecunda", "Verbena_verecunda")
  for (status in c("Invalid", "Illegitimate", "Orthographic")) {
    w <- wcvp_fixture(); w$names$taxon_status[2] <- status
    out <- reconcile_wcvp(rec, w)
    expect_equal(nrow(out$wcvp$candidates), 1L)
    expect_match(out$wcvp$candidates$evidence_detail, status, fixed = TRUE)
    expect_identical(out$mapping, rec$mapping)
    w$names$taxon_status[2] <- "Unplaced"
    expect_equal(nrow(reconcile_wcvp(rec, w)$wcvp$candidates), 0L)
  }
})

test_that("Kew hybrid-rank abbreviations and subvarieties retain exact identity", {
  rec <- wcvp_rec("Plant oldminor", "Plant_species_var._minor")
  for (rank in c("nothosubsp.", "nothovar.", "nothof.", "Subvariety")) {
    w <- wcvp_fixture(); w$names$taxon_rank[7] <- rank
    out <- reconcile_wcvp(rec, w)
    expect_equal(nrow(out$wcvp$candidates), 1L)
    expect_identical(out$wcvp$input_audit$accepted_rank, tolower(rank))
  }
})
