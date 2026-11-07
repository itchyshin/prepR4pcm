pkgload::load_all('.', quiet = TRUE)
for (file in c('test-reconcile_wcvp.R', 'test-reconcile_tnrs.R',
               'test-reconcile_tree-botanical.R')) {
  testthat::test_file(file.path('tests/testthat', file), stop_on_failure = TRUE)
}
cat('BOTANICAL_FOCUSED_PASS\n')
