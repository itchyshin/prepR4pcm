# Regression tests: .pr_resolve_query() must realign tnrs_match_names()
# output to the (possibly duplicated) `normalised` vector.
#
# Bug: `rotl::tnrs_match_names()` de-duplicates its input, so when two
# distinct input names normalise to the same string (e.g. "Esox lucius"
# and "Esox_lucius" both normalise to "Esox lucius"), the TNRS result
# had fewer rows than `normalised`. Comparing `tnrs_res$unique_name`
# against `normalised` then recycled a short vector against a long one
# ("longer object length is not a multiple of shorter object length")
# and silently mis-matched species. The fix queries the unique
# normalised names and realigns by the lowercased `search_string`.

# tnrs_match_names() de-duplicates its input and returns one row per
# unique name, with a lowercased `search_string`. This mock mirrors
# that contract; `resolver` lets a test inject name substitutions.
fake_tnrs_factory <- function(resolver = identity) {
  function(names, ...) {
    u <- unique(names)
    data.frame(
      search_string = tolower(u),
      unique_name   = resolver(u),
      stringsAsFactors = FALSE
    )
  }
}

test_that(".pr_resolve_query realigns TNRS results when normalised names duplicate", {
  skip_if_not_installed("rotl")

  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(),  # no substitutions
    .package = "rotl"
  )

  # "Esox lucius" and "Esox_lucius" both normalise to "Esox lucius".
  input <- c("Esox lucius", "Esox_lucius", "Oncorhynchus mykiss")

  expect_no_warning(
    res <- .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  )
  expect_equal(res$original, input)
  expect_length(res$query, length(input))
  # each element resolves to its own species, not a shifted neighbour
  # (ignore_attr: pr_normalize_names() attaches a normalisation_log attr)
  expect_equal(res$query,
               c("Esox lucius", "Esox lucius", "Oncorhynchus mykiss"),
               ignore_attr = TRUE)
})

test_that(".pr_resolve_query maps a TNRS substitution to the correct species", {
  skip_if_not_installed("rotl")

  # TNRS rewrites "Esox lucius" -> "Esox reichertii" (stand-in synonym).
  # Both forms of Esox lucius must pick up the substitute; the unrelated
  # species must be left alone.
  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(
      function(u) ifelse(u == "Esox lucius", "Esox reichertii", u)
    ),
    .package = "rotl"
  )

  input <- c("Esox lucius", "Esox_lucius", "Oncorhynchus mykiss")
  # the TNRS-substitution cli warning is expected here; not under test
  res <- suppressWarnings(
    .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  )

  expect_equal(res$query,
               c("Esox reichertii", "Esox reichertii", "Oncorhynchus mykiss"),
               ignore_attr = TRUE)
  # replacement map is keyed by the ORIGINAL input strings
  expect_setequal(names(res$tnrs_replacements),
                  c("Esox lucius", "Esox_lucius"))
})

test_that(".pr_resolve_query is unaffected when no normalised names collide", {
  skip_if_not_installed("rotl")

  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(),
    .package = "rotl"
  )

  input <- c("Salmo salar", "Esox lucius", "Gadus morhua")
  res <- .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  expect_equal(res$query, input, ignore_attr = TRUE)
})
