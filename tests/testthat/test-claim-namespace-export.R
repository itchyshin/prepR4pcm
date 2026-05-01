# Bundle G #20 -- NAMESPACE export completeness
#
# Every R/ function that carries an `@export` roxygen tag has a
# corresponding `export(...)` line in NAMESPACE. devtools::document()
# normally keeps these in sync; this test catches stale builds and
# manual NAMESPACE edits that drop exports silently.

test_that("every @export'd function in R/ is exported in NAMESPACE", {
  root <- tryCatch(
    {
      cands <- c(
        test_path("..", ".."),
        file.path(getwd()),
        file.path(getwd(), "..", "..")
      )
      for (c in cands) if (file.exists(file.path(c, "NAMESPACE"))) return(normalizePath(c))
      NA_character_
    },
    error = function(e) NA_character_
  )
  if (is.na(root)) skip("NAMESPACE not found")

  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  ns_exports <- regmatches(
    ns,
    regexpr("(?<=^export\\()[^)]+(?=\\))", ns, perl = TRUE)
  )

  r_files <- list.files(file.path(root, "R"),
                        pattern = "\\.R$", full.names = TRUE)
  expect_gt(length(r_files), 0, label = "no R/*.R files found")

  declared_exports <- character()
  for (f in r_files) {
    src <- readLines(f, warn = FALSE)
    # Find `#' @export` lines and the function definition that follows.
    export_idx <- grep("^#'\\s*@export\\b", src)
    for (i in export_idx) {
      # Walk forward to the next non-roxygen line; that should be the
      # function declaration `name <- function(...)` or similar.
      j <- i + 1
      while (j <= length(src) && grepl("^#'", src[j])) j <- j + 1
      if (j > length(src)) next
      m <- regmatches(src[j], regexpr("^[A-Za-z_.][A-Za-z0-9_.]*", src[j]))
      if (length(m) == 1 && nzchar(m)) {
        declared_exports <- c(declared_exports, m)
      }
    }
  }
  declared_exports <- unique(declared_exports)
  expect_gt(length(declared_exports), 0,
            label = "no @export'd functions found in R/")

  missing <- setdiff(declared_exports, ns_exports)
  expect_equal(
    length(missing), 0,
    info = sprintf(
      "Functions tagged `@export` in R/ but missing from NAMESPACE: %s. Run devtools::document() to regenerate NAMESPACE.",
      paste(missing, collapse = ", ")
    )
  )
})
