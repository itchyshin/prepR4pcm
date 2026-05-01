# Bundle G #8 -- pkgdown search-index integrity
#
# Catches the issue #7 root cause: docs/search.json contains entries
# pointing to URLs that don't resolve to existing files under
# docs/reference/. This is precisely what was broken on the deployed
# site after the function-rename in April -- the search index was
# stale even though the .html files had been regenerated.
#
# The test inspects the COMMITTED docs/ tree (not a fresh build) so it
# fires when search.json drifts from the rest of the deployed assets.
# It does NOT rebuild pkgdown -- that's slow and would mask the bug we
# care about (drift between search.json and the rest of docs/).

test_that("docs/search.json paths all resolve to existing files in docs/", {
  skip_on_cran()  # docs/ may be excluded from the build tarball

  root <- NA_character_
  cands <- c(
    test_path("..", ".."),
    file.path(getwd()),
    file.path(getwd(), "..", "..")
  )
  for (c in cands) {
    if (dir.exists(file.path(c, "docs"))) {
      root <- normalizePath(c)
      break
    }
  }
  if (is.na(root)) skip("docs/ directory not found")

  search_path <- file.path(root, "docs", "search.json")
  if (!file.exists(search_path)) skip("docs/search.json not present")

  # Hand-parse the JSON paths -- avoids a hard dependency on jsonlite
  # in the test suite.
  src <- paste(readLines(search_path, warn = FALSE), collapse = "\n")
  paths <- regmatches(
    src,
    gregexpr('"path":"[^"]+"', src, perl = TRUE)
  )[[1]]
  paths <- gsub('^"path":"|"$', "", paths)

  expect_gt(length(paths), 0, label = "no path entries in search.json")

  # Site URL prefix that the search index uses. Every linked path
  # should start with this URL OR be a relative path. We strip both
  # forms and check the resulting docs-relative path resolves on disk.
  site_url <- "https://itchyshin.github.io/prepR4pcm/"

  bad <- character()
  for (p in unique(paths)) {
    relpath <- p
    if (startsWith(relpath, site_url)) {
      relpath <- sub(site_url, "", relpath, fixed = TRUE)
    }
    if (startsWith(relpath, "/")) {
      relpath <- sub("^/+", "", relpath)
    }
    # Strip URL fragments / query strings.
    relpath <- sub("[#?].*$", "", relpath)
    on_disk <- file.path(root, "docs", relpath)
    if (!file.exists(on_disk)) {
      bad <- c(bad, p)
    }
  }

  expect_equal(
    length(bad), 0,
    info = paste0(
      "search.json points to ",
      length(bad),
      " path(s) that don't resolve to a file under docs/. ",
      "Rebuild the site with pkgdown::build_site() to regenerate the ",
      "search index. Offending paths:\n  ",
      paste(head(bad, 10), collapse = "\n  "),
      if (length(bad) > 10) sprintf("\n  ... and %d more", length(bad) - 10)
    )
  )
})
