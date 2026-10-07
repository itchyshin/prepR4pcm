test_that('flagged pairs require explicit inclusion in apply and export', {
  df <- data.frame(species = c('Mus musculus', 'Spermophilus madrensis'))
  tree <- ape::read.tree(text='(Mus_musculus:1,Spermophilus_taurensis:1);')
  rec <- reconcile_tree(df, tree, x_species='species', authority=NULL,
                        fuzzy=TRUE, quiet=TRUE)
  expect_equal(rec$mapping$match_type[rec$mapping$name_x %in% 'Spermophilus madrensis'], 'flagged')
  expect_warning(a <- reconcile_apply(rec,df,tree,species_col='species',drop_unresolved=TRUE), 'flagged')
  expect_equal(a$data$species, 'Mus musculus')
  expect_setequal(a$data$species,a$tree$tip.label)
  expect_warning(b <- reconcile_apply(rec,df,tree,species_col='species'), 'flagged')
  expect_true('Spermophilus_taurensis' %in% b$tree$tip.label)
  expect_false('Spermophilus madrensis' %in% b$tree$tip.label)
  expect_warning(c <- reconcile_apply(rec,df,tree,species_col='species',drop_unresolved=TRUE,include_flagged=TRUE), 'flagged')
  expect_setequal(c$data$species,c$tree$tip.label)
  expect_true('Spermophilus madrensis' %in% c$tree$tip.label)
  expect_warning(d <- reconcile_apply(rec,data=df,species_col='species',drop_unresolved=TRUE), 'flagged')
  expect_equal(nrow(d$data),1L)
  expect_warning(e <- reconcile_apply(rec,tree=tree,drop_unresolved=TRUE), 'flagged')
  expect_equal(ape::Ntip(e$tree),1L)
  expect_error(reconcile_apply(rec,include_flagged=NA),'include_flagged')
  out <- tempfile(); on.exit(unlink(out,recursive=TRUE))
  expect_warning(paths <- reconcile_export(rec,df,tree,species_col='species',dir=out), 'flagged')
  expect_equal(nrow(read.csv(paths$data)),1L)
  expect_equal(ape::Ntip(ape::read.nexus(paths$tree)),1L)
  expect_warning(paths <- reconcile_export(rec,df,tree,species_col='species',dir=out,include_flagged=TRUE), 'flagged')
  expect_equal(nrow(read.csv(paths$data)),2L)
})

test_that('interactive review uses source names after override reorders rows', {
  df <- data.frame(species=c('Spermophilus madrensis','Neotragus moschatus'))
  tree <- ape::read.tree(text='(Spermophilus_taurensis:1,Nesotragus_moschatus:1);')
  rec <- reconcile_tree(df,tree,x_species='species',authority=NULL,fuzzy=TRUE,quiet=TRUE)
  expect_equal(sum(rec$mapping$match_type=='flagged'),2L)
  # A private environment supplies responses without mocking base primitives,
  # which R may inline when byte-compiling an installed package.
  review <- reconcile_review
  body(review) <- body(review)
  environment(review) <- new.env(parent = environment(review))
  environment(review)$interactive <- function() TRUE
  environment(review)$readline <- function(...) 'a'
  reviewed <- review(rec, quiet=TRUE)
  expect_equal(sum(reviewed$mapping$match_type=='manual'),2L)
  expect_equal(sum(reviewed$mapping$match_type=='flagged'),0L)
})
