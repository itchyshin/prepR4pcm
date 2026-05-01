# Bundle G #36 -- vignette engine directives match the actual builder
#
# Catches "added a vignette but forgot the VignetteEngine directive"
# and "the directive does not match the build mode the vignette
# actually uses." Either case can produce a build that R CMD check
# accepts but pkgdown / CRAN refuses.

test_that("every vignette declares %\\VignetteIndexEntry / Engine / Encoding", {
  root <- NA_character_
  cands <- c(
    test_path("..", ".."),
    file.path(getwd()),
    file.path(getwd(), "..", "..")
  )
  for (c in cands) {
    if (dir.exists(file.path(c, "vignettes"))) {
      root <- normalizePath(c)
      break
    }
  }
  if (is.na(root)) skip("vignettes/ not found")

  vig_files <- list.files(file.path(root, "vignettes"),
                          pattern = "\\.Rmd$", full.names = TRUE)
  expect_gt(length(vig_files), 0, label = "no vignettes found")

  required <- c(
    "%\\VignetteIndexEntry{",
    "%\\VignetteEngine{knitr::rmarkdown}",
    "%\\VignetteEncoding{"
  )

  for (vig in vig_files) {
    src <- paste(readLines(vig, warn = FALSE), collapse = "\n")
    for (pat in required) {
      expect_true(
        grepl(pat, src, fixed = TRUE),
        info = sprintf(
          "%s is missing the directive %s. Vignettes need IndexEntry, Engine, and Encoding declared in the YAML or the leading roxygen so knitr / R CMD build pick them up.",
          basename(vig), pat
        )
      )
    }
  }
})


test_that("every vignette in vignettes/ appears in _pkgdown.yml Articles menu", {
  root <- NA_character_
  cands <- c(
    test_path("..", ".."),
    file.path(getwd()),
    file.path(getwd(), "..", "..")
  )
  for (c in cands) {
    if (file.exists(file.path(c, "_pkgdown.yml"))) {
      root <- normalizePath(c)
      break
    }
  }
  if (is.na(root)) skip("_pkgdown.yml not found")

  yml <- paste(readLines(file.path(root, "_pkgdown.yml"), warn = FALSE),
               collapse = "\n")

  vig_files <- list.files(file.path(root, "vignettes"),
                          pattern = "\\.Rmd$", full.names = FALSE)

  for (v in vig_files) {
    stem <- sub("\\.Rmd$", "", v)
    href <- sprintf("articles/%s.html", stem)
    expect_match(
      yml, href, fixed = TRUE,
      info = sprintf(
        "vignette %s exists but is not wired into _pkgdown.yml under navbar > components > articles. The pkgdown site won't show it.",
        v
      )
    )
  }
})
