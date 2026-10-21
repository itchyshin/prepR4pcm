#!/usr/bin/env Rscript

latin_review_ids <- function(manifest, group) {
  x <- jsonlite::fromJSON(manifest, simplifyVector = FALSE)
  ids <- vapply(Filter(function(z) group == "regression" || z$group == group,
                       x$scenarios), `[[`, character(1), "id")
  if (group == "regression" && !isTRUE(x$regression_includes_all_core))
    stop("manifest does not require all core scenarios in regression")
  ids
}

latin_review_class_counts <- function(results) {
  n <- c(success = 0L, skip = 0L, failure = 0L, error = 0L, warning = 0L)
  walk <- function(x) {
    if (inherits(x, "expectation_success")) n["success"] <<- n["success"] + 1L
    if (inherits(x, "expectation_skip")) n["skip"] <<- n["skip"] + 1L
    if (inherits(x, "expectation_failure")) n["failure"] <<- n["failure"] + 1L
    if (inherits(x, "expectation_error")) n["error"] <<- n["error"] + 1L
    if (inherits(x, "expectation_warning")) n["warning"] <<- n["warning"] + 1L
    if (is.list(x) && !inherits(x, "expectation")) lapply(x, walk)
  }
  walk(results)
  n
}

latin_review_test_names <- function(x) {
  vapply(x, function(z) if (is.null(z$test)) "" else as.character(z$test), character(1))
}

latin_review_validate <- function(results, required_ids, core = TRUE) {
  names <- latin_review_test_names(results)
  per <- lapply(results, function(z) latin_review_class_counts(z$results))
  ids <- sub(".*(LR-[RA][0-9]{2}).*", "\\1", names)
  has_id <- grepl("LR-[RA][0-9]{2}", names)
  issues <- character()
  for (id in required_ids) {
    ix <- which(has_id & ids == id)
    if (length(ix) != 1L) {
      issues <- c(issues, sprintf("%s executed %d times (required exactly once)", id, length(ix)))
      next
    }
    cc <- per[[ix]]
    if (cc[["success"]] < 1L) issues <- c(issues, sprintf("%s has no successful expectation", id))
    if (cc[["skip"]] || cc[["failure"]] || cc[["error"]])
      issues <- c(issues, sprintf("%s has skip/failure/error: %d/%d/%d", id, cc[["skip"]], cc[["failure"]], cc[["error"]]))
  }
  total <- Reduce(`+`, per, init = c(success=0L, skip=0L, failure=0L, error=0L, warning=0L))
  if (core && (total[["failure"]] || total[["error"]])) issues <- c(issues, "core tests contain failures or errors")
  if (!core && (total[["failure"]] || total[["error"]])) issues <- c(issues, "regression suite contains failures or errors")
  list(ok = !length(issues), issues = issues, counts = total,
       tests = data.frame(test=names, counts=I(per), stringsAsFactors=FALSE))
}

latin_review_source_hashes <- function(root) {
  dirs <- c("R", "tests", "tools", "man", "vignettes", "data", "inst")
  files <- unique(c(unlist(lapply(file.path(root, dirs), list.files,
    recursive = TRUE, full.names = TRUE)),
    file.path(root, c("DESCRIPTION", "NAMESPACE", ".Rbuildignore"))))
  files <- sort(files[file.exists(files) & !dir.exists(files)])
  hashes <- vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE)
  names(hashes) <- substring(files, nchar(root) + 2L)
  hashes
}

latin_review_self_test <- function() {
  td <- tempfile("lr-verifier-"); dir.create(td)
  on.exit(unlink(td, recursive=TRUE), add=TRUE)
  check <- function(label, source, required="LR-R01") {
    f <- file.path(td, paste0(label, ".R")); writeLines(source, f)
    r <- testthat::test_file(f, reporter="silent")
    latin_review_validate(r, required, core=TRUE)
  }
  missing <- check("missing", 'test_that("LR-R02 other", { expect_true(TRUE) })')
  skipped <- check("skipped", 'test_that("LR-R01 skipped", { skip("control"); expect_true(TRUE) })')
  failed <- check("failed", 'test_that("LR-R01 failing", { expect_true(FALSE) })')
  positive <- check("positive", 'test_that("LR-R01 positive", { expect_true(TRUE) })')
  if (missing$ok || skipped$ok || failed$ok) stop("negative verifier control unexpectedly passed")
  stopifnot(positive$ok, any(grepl("executed 0", missing$issues)),
    skipped$counts[["skip"]] == 1L, failed$counts[["failure"]] == 1L)
  cat("VERIFIER_NEGATIVE_CONTROLS_PASSED\n")
}

latin_review_run <- function(group, manifest, root=getwd()) {
  if (!requireNamespace("jsonlite", quietly=TRUE)) stop("jsonlite is required")
  if (!requireNamespace("devtools", quietly=TRUE)) stop("devtools is required")
  if (!requireNamespace("testthat", quietly=TRUE)) stop("testthat is required")
  root <- normalizePath(root, mustWork=TRUE); setwd(root)
  before <- latin_review_source_hashes(root)
  devtools::load_all(root, quiet=TRUE)
  required <- latin_review_ids(manifest, group)
  started <- proc.time()[["elapsed"]]
  core <- group != "regression"
  if (core) {
    results <- testthat::test_file(file.path(root,"tests/testthat/test-issue-139-review-recovery.R"), reporter="summary")
  } else {
    results <- devtools::test(root, reporter="summary")
  }
  elapsed <- proc.time()[["elapsed"]] - started
  validation <- latin_review_validate(results, required, core=core)
  if (!validation$ok) stop(paste(validation$issues, collapse="; "))
  after <- latin_review_source_hashes(root)
  if (!identical(before, after)) stop("source files changed during verification")
  outdir <- file.path(root,".unlazy/latin-review-recovery"); dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
  diff <- system2("git", c("diff","--binary","HEAD"), stdout=TRUE, stderr=TRUE)
  diff_hash <- digest::digest(paste(diff, collapse="\n"), algo="sha256", serialize=FALSE)
  rev <- system2("git", c("rev-parse","HEAD"), stdout=TRUE)
  test_names <- latin_review_test_names(results)
  receipt <- list(schema_version=1L, group=group, source_root=root,
    source_revision=paste(rev,collapse=""), working_diff_sha256=diff_hash,
    source_file_hashes=as.list(before),
    source_sha256=digest::digest(paste(names(before), before, collapse="\n"),
      algo="sha256", serialize=FALSE),
    R_version=R.version.string, package_version=as.character(utils::packageVersion("prepR4pcm")),
    loaded_package_path=normalizePath(find.package("prepR4pcm")),
    testthat_version=as.character(utils::packageVersion("testthat")),
    devtools_version=as.character(utils::packageVersion("devtools")),
    test_names=test_names, counts=as.list(validation$counts),
    test_counts=validation$tests, elapsed_seconds=elapsed,
    warnings_and_skips=validation$tests[vapply(validation$tests$counts, function(z) z[["warning"]] > 0L || z[["skip"]] > 0L, logical(1)),])
  saveRDS(results, file.path(outdir,paste0("tests-",group,".rds")))
  jsonlite::write_json(receipt, file.path(outdir,paste0("receipt-",group,".json")), auto_unbox=TRUE, pretty=TRUE, dataframe="rows")
  cat(switch(group, recovery="REVIEW_RECOVERY_TESTS_PASSED\n",
    apply="REVIEW_APPLY_COMPATIBILITY_PASSED\n",
    regression="PACKAGE_REGRESSION_SUITE_PASSED\n"))
  invisible(receipt)
}

latin_review_cli <- function(args=commandArgs(trailingOnly=TRUE)) {
  if ("--self-test" %in% args) return(latin_review_self_test())
  value <- function(flag) { i <- match(flag,args); if (is.na(i)||i==length(args)) stop("missing value for ",flag); args[[i+1L]] }
  group <- value("--group"); manifest <- value("--manifest")
  if (!group %in% c("recovery","apply","regression")) stop("invalid --group")
  latin_review_run(group, manifest)
}

if (sys.nframe() == 0L) {
  tryCatch(latin_review_cli(), error=function(e) { message("VERIFICATION_FAILED: ", conditionMessage(e)); quit(status=1L) })
}
