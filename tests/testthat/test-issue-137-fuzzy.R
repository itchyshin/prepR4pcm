test_that("fuzzy matching avoids a genus Cartesian distance matrix", {
  ids <- seq_len(1200) - 1L
  gx <- paste0("Genus", letters[ids %/% 676 + 1L],
               letters[(ids %/% 26) %% 26 + 1L], letters[ids %% 26 + 1L], " alba")
  trace("adist", where = asNamespace("utils"), print = FALSE,
        tracer = quote(if (length(x) > 1L && length(y) > 1L)
          stop("Cartesian edit-distance allocation")))
  on.exit(untrace("adist", where = asNamespace("utils")), add = TRUE)
  result <- pr_fuzzy_match(gx, gx, threshold = 1)
  expect_equal(nrow(result), length(gx))
})

test_that("fuzzy progress supports quiet suppression", {
  expect_true("quiet" %in% names(formals(pr_fuzzy_match)))
})

test_that("fuzzy scores and ordering match the predecessor", {
  baseline_path <- testthat::test_path("fixtures", "issue-137-fuzzy-predecessor.R")
  baseline_env <- new.env(parent = environment(pr_fuzzy_match))
  sys.source(baseline_path, envir = baseline_env)
  old <- baseline_env$pr_fuzzy_match
  set.seed(137)
  genera <- c("Acacia", "Acaciaa", "Acaciaaaa", "Abacia", "Aus", "Bus", "Pinus")
  epithets <- c("alba", "albaa", "rubra", "rubraa", "z", "longissima")
  y <- sample(as.vector(outer(genera, epithets, paste)))
  x <- c(sample(y, 80, replace = TRUE), "Aus", NA, "", "Bus alb")
  for (threshold in c(0, .7, .9, 1)) {
    expect_equal(suppressWarnings(pr_fuzzy_match(x, y, threshold, quiet = TRUE)),
                 suppressWarnings(old(x, y, threshold)))
  }
  expect_equal(pr_fuzzy_match(c("Aus alba", "Bus alba", "Aus alba"),
                             c("Cus alba", "Bus alba", "Aus alba"), quiet = TRUE),
               old(c("Aus alba", "Bus alba", "Aus alba"),
                   c("Cus alba", "Bus alba", "Aus alba")))
})

test_that("substantial fuzzy progress is visible and quiet is silent", {
  x <- rep("Aus alba", 1000)
  expect_message(pr_fuzzy_match(x, "Aus alba"), "Preparing fuzzy matching")
  expect_message(pr_fuzzy_match(x, "Aus alba"), "ETA")
  expect_message(pr_fuzzy_match(x, "Aus alba", quiet = TRUE), NA)
})

test_that("fuzzy notes format the unrounded score exactly as the predecessor", {
  baseline_env <- new.env(parent = environment(pr_fuzzy_match))
  sys.source(testthat::test_path("fixtures", "issue-137-fuzzy-predecessor.R"),
             envir = baseline_env)
  x <- c("Bulbophyllum longivagans", "Croton malvavisciifolius")
  y <- c("Bulbophyllum longivaginans", "Croton malvaviscifolius")
  expect_identical(pr_fuzzy_match(x, y, quiet = TRUE),
                   baseline_env$pr_fuzzy_match(x, y))
  for (i in seq_along(x)) {
    ex <- strsplit(x[i], " ")[[1]][2]
    ey <- strsplit(y[i], " ")[[1]][2]
    boundary <- 0.6 + 0.4 * (1 - as.numeric(utils::adist(ex, ey)) /
                              max(nchar(ex), nchar(ey)))
    for (threshold in boundary + c(-1, 0, 1) * .Machine$double.eps) {
      expect_identical(pr_fuzzy_match(x[i], y, threshold, quiet = TRUE),
                       baseline_env$pr_fuzzy_match(x[i], y, threshold))
    }
  }
})
