test_that("tree botanical options validate before the cascade", {
  local_mocked_bindings(pr_run_cascade = function(...) stop("cascade called"))
  x <- data.frame(species = "Rosa example")
  tree <- ape::read.tree(text = "(Rosa_other,Rosa_second);")
  for (bad in list(NA, 1, c(TRUE, FALSE), NULL)) {
    expect_error(reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                                tnrs = bad), "tnrs")
  }
  for (bad in list(NA_character_, "bad", c("wcvp", "wfo"), NULL)) {
    expect_error(reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                                tnrs_source = bad), "tnrs_source")
  }
  expect_error(reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                              tnrs = TRUE, tnrs_source = "wfo", wcvp = list()),
               "wcvp")
  expect_error(reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                              tnrs = TRUE, tnrs_source = "cact", wcvp = list()),
               "wcvp")
})

test_that("TNRS is opt-in and skips requests for resolved data", {
  local_mocked_bindings(
    .pr_tnrs_client_available = function() stop("unexpected client"),
    .pr_tnrs_query = function(...) stop("unexpected query")
  )
  tree <- ape::read.tree(text = "(Rosa_alpha,Rosa_beta);")
  unresolved <- data.frame(species = "Rosa unknown")
  resolved <- data.frame(species = "Rosa alpha")
  expect_s3_class(reconcile_tree(unresolved, tree, authority = NULL, quiet = TRUE),
                  "reconciliation")
  expect_s3_class(reconcile_tree(resolved, tree, authority = NULL, quiet = TRUE,
                                 tnrs = TRUE), "reconciliation")
})

test_that("tree integration chains botanical review without automatic mapping", {
  tree <- ape::read.tree(text = "(Rosa_alpha,Rosa_beta);")
  x <- data.frame(species = "Rosa unknown")
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(...) list(results = data.frame(), source_metadata = NULL)
  )
  manual <- reconcile_tree(x, tree, authority = NULL, quiet = TRUE)
  manual <- reconcile_tnrs(manual, source = "wfo")
  integrated <- reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                               tnrs = TRUE, tnrs_source = "wfo")
  expect_equal(integrated$mapping, manual$mapping)
  expect_equal(integrated$candidates, manual$candidates)
})

test_that("TNRS service errors propagate from tree integration", {
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(...) stop("service unavailable")
  )
  x <- data.frame(species = "Rosa unknown")
  tree <- ape::read.tree(text = "(Rosa_alpha,Rosa_beta);")
  expect_error(reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                              tnrs = TRUE), "service unavailable")
})

test_that("frozen WCVP is forwarded through both review stages", {
  snapshot <- list(version = "16", names = data.frame(
    plant_name_id = c("1", "2"),
    taxon_name = c("Glandularia verecunda", "Verbena verecunda"),
    accepted_plant_name_id = c("1", "1"),
    taxon_rank = c("Species", "Species"),
    taxon_status = c("Accepted", "Synonym")
  ), metadata = "frozen")
  x <- data.frame(species = c("Glandularia verecunda", "Typogenus verecunda"))
  tree <- ape::read.tree(text = "(Verbena_verecunda,Rosa_alpha);")
  local_mocked_bindings(
    .pr_tnrs_client_available = function() TRUE,
    .pr_tnrs_query = function(names, source) list(
      results = data.frame(Name_submitted = names,
                           Accepted_name = "Glandularia verecunda",
                           Accepted_name_id = "service-id", Overall_score = .9),
      source_metadata = data.frame(sourceName = "WCVP", version = "service-version"))
  )
  manual <- reconcile_tree(x, tree, authority = NULL, quiet = TRUE)
  manual <- reconcile_wcvp(manual, snapshot)
  manual <- reconcile_tnrs(manual, wcvp = snapshot)
  integrated <- reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                               wcvp = snapshot, tnrs = TRUE)
  expect_identical(integrated$mapping, manual$mapping)
  expect_equal(integrated$candidates, manual$candidates)
  integrated$wcvp$query_time <- manual$wcvp$query_time <- NULL
  integrated$wcvp$queries[[1]]$query_time <- NULL
  manual$wcvp$queries[[1]]$query_time <- NULL
  expect_identical(integrated$wcvp, manual$wcvp)
  expect_identical(integrated$tnrs$target_reference_version, "16")
  expect_true(all(!integrated$candidates$eligible))
  local_only <- reconcile_tree(x, tree, authority = NULL, quiet = TRUE, wcvp = snapshot)
  expect_identical(local_only$mapping, manual$mapping)
  expect_true(nrow(local_only$candidates) > 0)
})

test_that("invalid frozen WCVP fails before any TNRS request", {
  local_mocked_bindings(
    .pr_tnrs_client_available = function() stop("unexpected client"),
    .pr_tnrs_query = function(...) stop("unexpected query")
  )
  x <- data.frame(species = "Rosa unknown")
  tree <- ape::read.tree(text = "(Rosa_alpha,Rosa_beta);")
  expect_error(reconcile_tree(x, tree, authority = NULL, quiet = TRUE,
                              wcvp = list(), tnrs = TRUE), "names.*version")
})
