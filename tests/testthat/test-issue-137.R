test_that("post-lookup matching scales beyond a Cartesian scan", {
  n <- 5000L
  testthat::local_mocked_bindings(
    pr_lookup_authority = function(names, authority, db_version, quiet=FALSE) {
      tibble::tibble(input=names, accepted_name=paste("Accepted",names))
    }, .package="prepR4pcm")
  # The predecessor takes tens of seconds here. Indexed matching has a wide
  # margin below this limit; this is a guard against a quadratic regression.
  setTimeLimit(elapsed=5, transient=TRUE)
  on.exit(setTimeLimit(cpu=Inf, elapsed=Inf, transient=FALSE), add=TRUE)
  expect_equal(nrow(pr_resolve_synonyms(paste("X",seq_len(n)),
                                       paste("Y",seq_len(n)), quiet=TRUE)), 0L)
  setTimeLimit(cpu=Inf, elapsed=Inf, transient=FALSE)
})

test_that("synonym matching retains predecessor priorities and row order", {
  legacy <- eval(parse(test_path("fixtures", "issue137-legacy-synonyms.R")))
  environment(legacy) <- asNamespace("prepR4pcm")
  set.seed(137)
  accepted <- NULL
  testthat::local_mocked_bindings(
    pr_lookup_authority = function(names, authority, db_version, quiet=FALSE) {
      tibble::tibble(input=names, accepted_name=unname(accepted[names]))
    }, .package="prepR4pcm")
  for (trial in seq_len(50)) {
    x <- paste("X",1:20); y <- paste("Y",1:15)
    pool <- c(x,y,paste("Accepted",1:10),NA_character_, "")
    accepted <- setNames(sample(pool,length(c(x,y)),replace=TRUE),c(x,y))
    expect_identical(pr_resolve_synonyms(x,y,quiet=TRUE),legacy(x,y,quiet=TRUE))
  }
})

test_that("post-lookup stage transitions are visible and quiet is respected", {
  testthat::local_mocked_bindings(
    pr_lookup_authority=function(names,authority,db_version,quiet=FALSE) {
      tibble::tibble(input=names,accepted_name=NA_character_)
    }, .package="prepR4pcm")
  expect_message(pr_resolve_synonyms("Plant alpha","Plant beta"),
                 "Authority lookup complete")
  expect_message(pr_resolve_synonyms("Plant alpha","Plant beta",quiet=TRUE),NA)
})

test_that("applying many synonym candidates does not build one tibble per pair", {
  n<-10000L
  x<-paste('Genusa',paste0('name',seq_len(n)))
  y<-paste('Genusb',paste0('name',seq_len(n)))
  accepted<-setNames(c(y,y),c(x,y))
  testthat::local_mocked_bindings(pr_lookup_authority=function(names,authority,db_version,quiet=FALSE) {
    tibble::tibble(input=names,accepted_name=unname(accepted[names]))
  }, .package='prepR4pcm')
  setTimeLimit(elapsed=5,transient=TRUE)
  on.exit(setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE),add=TRUE)
  out<-pr_run_cascade(x,y,quiet=TRUE)
  setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE)
  expect_equal(nrow(out),n)
  expect_true(all(out$match_type=='synonym'))
})

test_that("cascade preserves predecessor greedy assignment and audit fields", {
  legacy<-eval(parse(test_path('fixtures','issue137-legacy-cascade.R')))
  legacy_fuzzy<-eval(parse(test_path('fixtures','issue137-legacy-fuzzy.R')))
  environment(legacy_fuzzy)<-asNamespace('prepR4pcm')
  environment(legacy)<-list2env(list(pr_fuzzy_match=legacy_fuzzy),parent=asNamespace('prepR4pcm'))
  accepted<-NULL
  testthat::local_mocked_bindings(pr_lookup_authority=function(names,authority,db_version,quiet=FALSE) {
    tibble::tibble(input=names,accepted_name=unname(accepted[names]))
  },.package='prepR4pcm')
  set.seed(137)
  for(i in seq_len(10)) {
    x<-paste('Genusa',paste0('name',1:20)); y<-paste('Genusb',paste0('name',1:15))
    pool<-c(x,y,paste('Accepted',1:10),NA_character_)
    accepted<-setNames(sample(pool,length(c(x,y)),replace=TRUE),c(x,y))
    expect_identical(pr_run_cascade(x,y,fuzzy=TRUE,quiet=TRUE),
                     legacy(x,y,fuzzy=TRUE,quiet=TRUE))
  }
})
