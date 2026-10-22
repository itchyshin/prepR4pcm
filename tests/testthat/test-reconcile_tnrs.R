tnrs_test_reconciliation <- function(
    x = c("Trema tomentosa", "Trema orientalis"),
    y = c("Trema tomentosum", "Trema orientale"), fuzzy = FALSE) {
  reconcile_data(
    data.frame(species = x), data.frame(species = y),
    x_species = "species", y_species = "species", authority = NULL,
    fuzzy = fuzzy, quiet = TRUE
  )
}

tnrs_test_response <- function() {
  data.frame(
    Name_submitted = c("Trema tomentosa", "Trema orientalis", "Trema orientalis"),
    Name_matched = c("Trema tomentosum (Roxb.) H.Hara", "Trema orientale (L.) Blume", "Trema orientale (L.) Blume"),
    Name_matched_id = c("m1", "m2", "m2"),
    Accepted_name = c("Trema tomentosum (Roxb.) H.Hara", "Trema orientale (L.) Blume", "Absent species"),
    Accepted_name_id = c("a1", "a2", "a3"),
    Overall_score = c(0.92, 0.88, 0.51),
    Taxonomic_status = c("synonym", "accepted", "accepted"),
    Warnings = c("spelling", "", ""),
    WarningsEng = c("Spelling variant", "", ""),
    Source = "WCVP", stringsAsFactors = FALSE
  )
}

tnrs_test_client_result <- function(results = tnrs_test_response()) {
  list(
    results = results,
    source_metadata = data.frame(
      sourceName = c("WCVP", "WFO"), version = c("2026.1", "2025.2"),
      sourceReleaseDate = c("2026-01-01", "2025-01-01"),
      stringsAsFactors = FALSE
    ),
    service_metadata = data.frame(
      app_version = "4.1", db_version = "4.0", code_version = "4.1",
      api_version = "1", stringsAsFactors = FALSE
    ),
    client_version = "0.3.6"
  )
}

test_that("reconcile_tnrs validates source before calling the optional client", {
  rec <- tnrs_test_reconciliation()
  local_mocked_bindings(
    .pr_tnrs_query = function(...) stop("must not query"),
    .package = "prepR4pcm"
  )
  expect_error(reconcile_tnrs(rec, source = c("wcvp", "wfo")), "exactly one")
  expect_error(reconcile_tnrs(rec, source = "animals"), "source")
})

test_that("the TNRS client receives one source, all matches, and no score filtering", {
  captured <- list()
  spy <- function(taxonomic_names, sources, mode, matches, accuracy) {
    captured <<- list(
      names = taxonomic_names, sources = sources, mode = mode,
      matches = matches, accuracy = accuracy
    )
    data.frame(Name_submitted = taxonomic_names)
  }
  .pr_tnrs_client_call(c("Trema tomentosa", "Trema orientalis"), "wcvp", tnrs_fun = spy)
  expect_identical(captured$names, c("Trema tomentosa", "Trema orientalis"))
  expect_identical(captured$sources, "wcvp")
  expect_identical(captured$mode, "resolve")
  expect_identical(captured$matches, "all")
  expect_identical(captured$accuracy, 0)
})

test_that("the optional TNRS package is only required when the adapter is called", {
  rec <- tnrs_test_reconciliation()
  local_mocked_bindings(
    .pr_tnrs_client_available = function() FALSE,
    .package = "prepR4pcm"
  )
  expect_error(reconcile_tnrs(rec), "optional.*TNRS.*install")
  expect_identical(rec$mapping$name_x, tnrs_test_reconciliation()$mapping$name_x)
})

test_that("reconcile_tnrs retains all response rows but offers only exact accepted tree tips", {
  rec <- tnrs_test_reconciliation()
  calls <- list()
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) {
      calls$names <<- names
      calls$source <<- source
      tnrs_test_client_result()
    },
    .package = "prepR4pcm"
  )

  out <- reconcile_tnrs(rec, source = "wcvp")
  expect_setequal(calls$names, c("Trema tomentosa", "Trema orientalis"))
  expect_identical(calls$source, "wcvp")
  expect_identical(out$mapping, rec$mapping)
  expect_equal(nrow(out$tnrs$results), 3L)
  expect_identical(out$tnrs$source_version, "2026.1")
  expect_identical(out$tnrs$client_version, "0.3.6")
  expect_setequal(out$candidates$name_x, c("Trema tomentosa", "Trema orientalis"))
  expect_setequal(out$candidates$name_y, c("Trema tomentosum", "Trema orientale"))
  expect_true(all(out$candidates$reason == "tnrs_review_only"))
  expect_true(all(out$candidates$identity_state == "unknown"))
  expect_true(all(!out$candidates$eligible))
  expect_match(out$candidates$evidence_detail, "Overall score")
  expect_true(any(out$tnrs$target_audit$status == "accepted_name_absent_from_tips"))
})

test_that("TNRS suggestions never enter the mapping or apply through include_flagged", {
  rec <- tnrs_test_reconciliation()
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) tnrs_test_client_result(),
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec)
  expect_identical(out$mapping, rec$mapping)
  applied <- reconcile_apply(
    out, data = data.frame(species = c("Trema tomentosa", "Trema orientalis")),
    species_col = "species", include_flagged = TRUE, drop_unresolved = TRUE
  )
  expect_equal(nrow(applied$data), 0L)
})

test_that("existing local candidates are enriched without creating duplicate review pairs", {
  rec <- tnrs_test_reconciliation(fuzzy = TRUE)
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) tnrs_test_client_result(),
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec)
  key <- out$candidates$name_x == "Trema tomentosa" &
    out$candidates$name_y == "Trema tomentosum"
  expect_equal(sum(key), 1L)
  expect_match(out$candidates$evidence_detail[key], "TNRS/WCVP")
  expect_equal(nrow(out$tnrs$results), 3L)
})

test_that("no unresolved names avoids a service call and returns the same object", {
  rec <- tnrs_test_reconciliation(x = "Trema tomentosum", y = "Trema tomentosum")
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(...) stop("must not query"),
    .package = "prepR4pcm"
  )
  expect_identical(reconcile_tnrs(rec), rec)
})

test_that("service errors and malformed responses leave the input untouched", {
  rec <- tnrs_test_reconciliation()
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(...) stop("service offline"),
    .package = "prepR4pcm"
  )
  expect_error(reconcile_tnrs(rec), "TNRS query failed.*service offline")
  expect_false("tnrs" %in% names(rec))

  local_mocked_bindings(
    .pr_tnrs_query = function(...) list(results = "not a data frame"),
    .package = "prepR4pcm"
  )
  expect_error(reconcile_tnrs(rec), "data.frame")
  expect_identical(rec$mapping, tnrs_test_reconciliation()$mapping)
})

test_that("duplicate, blank, and missing submitted names are handled without misalignment", {
  rec <- tnrs_test_reconciliation(
    x = c("Trema tomentosa", "Trema tomentosa", "", NA_character_),
    y = c("Trema tomentosum", "Trema tomentosum", "Other plant", "Other plant")
  )
  calls <- list()
  response <- tnrs_test_response()[1, , drop = FALSE]
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) {
      calls$names <<- names
      response$Source <<- "WCVP"
      c(tnrs_test_client_result(response), list(warnings = character()))
    },
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec)
  expect_identical(calls$names, "Trema tomentosa")
  expect_equal(sum(out$candidates$name_y == "Trema tomentosum"), 1L)
})

test_that("partial responses are recorded and source versions may be explicitly unavailable", {
  rec <- tnrs_test_reconciliation()
  response <- tnrs_test_response()[1, , drop = FALSE]
  client_result <- tnrs_test_client_result(response)
  client_result$source_metadata <- NULL
  client_result$warnings <- "One service batch was incomplete"
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) client_result,
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec)
  expect_identical(out$tnrs$status, "partial")
  expect_true(is.na(out$tnrs$source_version))
  expect_setequal(out$tnrs$missing_submitted_names, "Trema orientalis")
  expect_identical(out$tnrs$warnings, "One service batch was incomplete")
  expect_true(all(is.na(out$candidates$version)))
})

test_that("accepted names with non-unique normalized target labels are withheld", {
  rec <- tnrs_test_reconciliation(y = c("Trema_tomentosum L.", "Trema tomentosum"))
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) {
      result <- tnrs_test_response()[1, , drop = FALSE]
      c(tnrs_test_client_result(result), list(warnings = character()))
    },
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec)
  expect_equal(nrow(out$candidates), 0L)
  expect_identical(out$tnrs$target_audit$status[[1]],
                   "accepted_name_matches_multiple_tips")
})

test_that("botanical author strings are removed only for tree-tip comparison", {
  rec <- tnrs_test_reconciliation()
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) {
      result <- data.frame(
        Name_submitted = "Bignonia ternata",
        Name_matched = "Bignonia ternata Vell.",
        Accepted_name = "Adenocalymma ternatum (Vell.) Netto ex Bureau & K.Schum.",
        Accepted_name_id = "109046-1", Overall_score = 1,
        Taxonomic_status = "Synonym", Warnings = NA_character_,
        WarningsEng = NA_character_, Source = "WCVP",
        stringsAsFactors = FALSE
      )
      metadata <- data.frame(sourceName = "WCVP", version = "2026.1")
      list(results = result, source_metadata = metadata,
           service_metadata = NULL, client_version = "0.3.6")
    },
    .package = "prepR4pcm"
  )
  rec <- tnrs_test_reconciliation(
    x = "Bignonia ternata", y = c("Adenocalymma ternatum", "Adenocalymma macrophyllum")
  )
  out <- reconcile_tnrs(rec)
  expect_identical(out$candidates$name_y, "Adenocalymma ternatum")
  expect_match(out$candidates$evidence_detail, "Adenocalymma ternatum \\(Vell\\.")
  expect_identical(out$tnrs$results$Accepted_name,
                   "Adenocalymma ternatum (Vell.) Netto ex Bureau & K.Schum.")
})

test_that("an occupied tree tip is visible as a review candidate but cannot be accepted", {
  rec <- tnrs_test_reconciliation(
    x = c("Trema tomentosa", "Trema tomentosum"), y = "Trema tomentosum"
  )
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) {
      result <- tnrs_test_response()[1, , drop = FALSE]
      c(tnrs_test_client_result(result), list(warnings = character()))
    },
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec)
  pending <- .pr_pending_review_candidates(out)
  expect_true(any(pending$name_x == "Trema tomentosa" & pending$target_in_use))
  expect_identical(out$mapping, rec$mapping)
})

test_that("a frozen live WCVP sample recovers useful tips and rejects five distractors", {
  sample <- utils::read.csv(
    testthat::test_path("fixtures", "tnrs-wcvp-public-examples.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  x <- sample$Name_submitted
  tips <- c(
    "Trema tomentosum", "Trema orientale",
    "Coelogyne cantonensis", "Coussarea impetiolaris",
    "Croton heterochrous", "Croton leucophyllus", "Croton stenosepalus",
    "Adenocalymma ternatum", "Adenocalymma macrophyllum", "Quercus robur"
  )
  rec <- tnrs_test_reconciliation(x = x, y = tips)
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) {
      list(results = sample, source_metadata = NULL,
           service_metadata = data.frame(app_version = "5.3.1"),
           client_version = "0.3.6", warnings = character())
    },
    .package = "prepR4pcm"
  )
  out <- reconcile_tnrs(rec, source = "wcvp")
  expect_setequal(out$candidates$name_y, c(
    "Trema tomentosum", "Trema orientale", "Adenocalymma ternatum", "Quercus robur"
  ))
  distractors <- c(
    "Coelogyne cantonensis", "Coussarea impetiolaris", "Croton heterochrous",
    "Croton leucophyllus", "Croton stenosepalus", "Adenocalymma macrophyllum"
  )
  expect_false(any(out$candidates$name_y %in% distractors))
  expect_false(any(out$mapping$in_x & out$mapping$in_y))
  expect_true(all(out$candidates$name_y %in% tips))
  expect_equal(nrow(out$tnrs$results), 9L)
})
