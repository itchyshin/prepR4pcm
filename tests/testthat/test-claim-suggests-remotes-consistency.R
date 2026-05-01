# Bundle G #43 -- Suggests-vs-Remotes consistency
#
# CI failed in PR #43 / PR #44 because `clootl` and `rtrees` are
# GitHub-only Suggests but had no `Remotes:` entry, so pak couldn't
# resolve them during dependency setup. The fix was to declare them
# in the `Remotes:` field. This test catches the same drift in the
# future: every Suggests / Imports package that is NOT on CRAN and
# NOT in `Remotes:` will fail the build.
#
# We approximate "is on CRAN" via a static allowlist of known CRAN
# packages used by this project, since hitting the CRAN API on every
# test is slow and CI-fragile. If a NEW non-CRAN dependency is added
# that isn't on this allowlist, the test fails -- forcing the
# developer to either confirm it's on CRAN or add it to the
# `Remotes:` field.

# Packages we know are on CRAN (any release year). When adding a new
# Suggests / Imports package that's on CRAN, append it here.
.cran_allowlist <- c(
  "ape", "cli", "rlang", "tibble",
  "caper", "digest", "dplyr", "fishtree", "knitr", "MCMCglmm", "phytools",
  "pkgdown", "readr", "rmarkdown", "rotl", "spelling", "stringr", "taxadb",
  "testthat",
  # standard packages bundled with R
  "stats", "tools", "utils", "graphics", "grDevices", "methods"
)



test_that("every non-CRAN Suggests / Imports has a matching Remotes entry", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  parse_field <- function(name) {
    v <- desc[1, name]
    if (is.na(v)) return(character())
    parts <- trimws(strsplit(v, ",")[[1]])
    parts <- gsub("\\s*\\([^)]*\\)\\s*$", "", parts)  # strip "(>= ...)"
    parts <- gsub(",\\s*$", "", parts)
    parts[nzchar(parts)]
  }

  imports  <- parse_field("Imports")
  suggests <- parse_field("Suggests")
  depends  <- parse_field("Depends")
  # `Depends` may include `R (>= ...)` -- strip `R` itself
  depends <- depends[depends != "R"]
  remotes <- parse_field("Remotes")

  # Strip `github::owner/` prefix and any `@ref` suffix so we get
  # the bare package name from `Remotes:`.
  remotes_pkgs <- vapply(remotes, function(r) {
    r <- sub(".*::", "", r)         # drop `github::` etc.
    r <- sub("/.*$", "", sub("^[^/]+/", "", paste0(r, "/")))
    r
  }, character(1))
  # Better: just take the part after the last `/`.
  remotes_pkgs <- vapply(remotes, function(r) {
    r <- sub(".*::", "", r)
    parts <- strsplit(r, "/", fixed = TRUE)[[1]]
    parts[length(parts)]
  }, character(1))
  remotes_pkgs <- sub("@.*$", "", remotes_pkgs)

  declared <- c(imports, suggests, depends)

  unaccounted <- setdiff(declared, c(.cran_allowlist, remotes_pkgs))
  expect_equal(
    length(unaccounted), 0,
    info = paste0(
      "Imports / Suggests / Depends entries that are neither on the ",
      "CRAN allowlist nor in `Remotes:`. Either add them to the ",
      "allowlist (in this test file) or to the DESCRIPTION's ",
      "`Remotes:` field so pak can resolve them on CI:\n  ",
      paste(unaccounted, collapse = "\n  ")
    )
  )
})


# A Remotes entry can legitimately exist for a transitive
# dependency: pak needs to know where to fetch the transitive
# package even when our package doesn't directly Import / Suggest
# it. This allowlist holds those transitive-only Remotes; add
# entries when you add a Remotes that backs a transitive dep.
#
# As of 2026-05, `rtrees` 1.0+ ships its own `Remotes: daijiang/megatrees`
# entry upstream (see daijiang/rtrees#10), so we no longer carry
# `megatrees` here. Add new entries if a future Suggests' GitHub-only
# package has a transitive dep that pak still can't resolve from the
# upstream's Remotes.
.transitive_remotes_allowlist <- character(0)


test_that("every Remotes entry corresponds to a real Imports / Suggests / Depends or is allowlisted as transitive", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  parse_field <- function(name) {
    v <- desc[1, name]
    if (is.na(v)) return(character())
    parts <- trimws(strsplit(v, ",")[[1]])
    parts <- gsub("\\s*\\([^)]*\\)\\s*$", "", parts)
    parts <- gsub(",\\s*$", "", parts)
    parts[nzchar(parts)]
  }

  remotes <- parse_field("Remotes")
  if (length(remotes) == 0) skip("no Remotes field")

  declared <- c(parse_field("Imports"),
                parse_field("Suggests"),
                parse_field("Depends"))

  remotes_pkgs <- vapply(remotes, function(r) {
    r <- sub(".*::", "", r)
    parts <- strsplit(r, "/", fixed = TRUE)[[1]]
    parts[length(parts)]
  }, character(1))
  remotes_pkgs <- sub("@.*$", "", remotes_pkgs)

  orphans <- setdiff(remotes_pkgs,
                     c(declared, .transitive_remotes_allowlist))
  expect_equal(
    length(orphans), 0,
    info = paste0(
      "Packages in `Remotes:` that are NEITHER in Imports / Suggests / ",
      "Depends NOR on the transitive-Remotes allowlist (in this test ",
      "file). Each Remotes entry should back a real declared dependency, ",
      "or be flagged as transitive in `.transitive_remotes_allowlist`:\n  ",
      paste(orphans, collapse = "\n  ")
    )
  )
})
