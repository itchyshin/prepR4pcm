# Bundle G #1 — Authority parity ----------------------------------------
#
# These tests catch the "documentation lies about supported authorities"
# bug we hit in May 2026, where pr_valid_authorities() listed `tpl`,
# `fb`, `slb`, `wd` -- none of which are taxadb providers at all -- and
# `iucn`, which broke at taxadb v22.12.
#
# Three angles on the same claim:
#
# 1. Every authority that the package claims to support
#    (pr_valid_authorities()) is documented in the user-facing
#    @param authority block of ?reconcile_data.
# 2. Every authority documented in ?reconcile_data is in
#    pr_valid_authorities() -- no aspirational claims.
# 3. Every authority in pr_valid_authorities() is genuinely supported
#    by the underlying taxadb dependency. (Live; skipped on CRAN /
#    offline / without taxadb installed.)
#
# Sub-tests 1 and 2 are documentation-vs-code parity. Sub-test 3 is
# code-vs-dependency parity. All three can drift independently, so
# all three are tested.

.read_rd_source <- function(name) {
  candidates <- c(
    test_path("..", "..", "man"),
    file.path(getwd(), "man"),
    file.path(getwd(), "..", "man"),
    file.path(getwd(), "..", "..", "man")
  )
  for (c in candidates) {
    if (dir.exists(c) &&
        file.exists(file.path(dirname(c), "DESCRIPTION"))) {
      p <- file.path(c, paste0(name, ".Rd"))
      if (file.exists(p)) {
        return(paste(readLines(p, warn = FALSE), collapse = "\n"))
      }
    }
  }
  NA_character_
}


test_that("every entry in pr_valid_authorities() is documented in ?reconcile_data", {
  rd_text <- .read_rd_source("reconcile_data")
  expect_false(is.na(rd_text),
               info = "reconcile_data.Rd not found under man/")

  # Search the whole Rd source for the authority code as a quoted
  # string. We search the full Rd rather than extracting the @param
  # block specifically, because Rd source uses \item{authority}{...}
  # nesting that's awkward to slice with a flat regex; quoted authority
  # codes are unlikely to appear elsewhere in a data-rich help page.
  for (auth in pr_valid_authorities()) {
    expect_match(
      rd_text, paste0('["`]', auth, '["`]'),
      perl = TRUE,
      info = sprintf(
        "authority `%s` is in pr_valid_authorities() but not documented anywhere in ?reconcile_data. Update either the @param authority block to mention it, or remove it from pr_valid_authorities().",
        auth
      )
    )
  }
})


test_that("every authority listed as supported in ?reconcile_data is in pr_valid_authorities()", {
  rd_text <- .read_rd_source("reconcile_data")
  expect_false(is.na(rd_text), info = "reconcile_data.Rd not found")

  # The supported authorities are listed in a \describe{}{...} block
  # inside \item{authority}{...}. Authorities mentioned in narrative
  # prose elsewhere in the same @param (e.g. a deprecation note about
  # codes that used to be advertised) are NOT claims of support, so we
  # restrict the search to \item{...} entries WITHIN \describe{} blocks
  # only.
  describe_pattern <- "\\\\describe\\{((?:[^{}]|\\{[^{}]*\\})*)\\}"
  describe_blocks <- regmatches(
    rd_text,
    gregexpr(describe_pattern, rd_text, perl = TRUE)
  )[[1]]
  expect_gt(length(describe_blocks), 0,
            label = "no \\describe{} blocks found in reconcile_data.Rd; the doc structure may have changed")

  candidate_authorities <- c(
    "col", "itis", "gbif", "ncbi", "ott", "iucn", "itis_test",
    "tpl", "fb", "slb", "wd"
  )

  documented <- character()
  for (block in describe_blocks) {
    documented <- union(
      documented,
      candidate_authorities[
        vapply(candidate_authorities, function(a) {
          grepl(paste0('["`]', a, '["`]'), block, perl = TRUE)
        }, logical(1))
      ]
    )
  }

  for (auth in documented) {
    expect_true(
      auth %in% pr_valid_authorities(),
      info = sprintf(
        "authority `%s` is listed as supported in a \\describe{} block of ?reconcile_data but is missing from pr_valid_authorities() -- aspirational doc, not backed by code",
        auth
      )
    )
  }
})


test_that("every entry in pr_valid_authorities() actually works against taxadb (live)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("taxadb")

  for (auth in pr_valid_authorities()) {
    result <- tryCatch(
      taxadb::filter_name("Homo sapiens", provider = auth),
      error = function(e) e
    )
    expect_false(
      inherits(result, "error"),
      info = sprintf(
        "taxadb::filter_name fails for authority `%s` (`pr_valid_authorities()` claims this works but it does not): %s",
        auth,
        if (inherits(result, "error")) conditionMessage(result) else ""
      )
    )
    if (!inherits(result, "error")) {
      expect_gt(
        nrow(result), 0,
        label = sprintf(
          "taxadb::filter_name(`Homo sapiens`, provider = `%s`) returned 0 rows; the authority is technically callable but cannot resolve the most basic species name",
          auth
        )
      )
    }
  }
})
