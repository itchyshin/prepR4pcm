test_that("pr_extract_tips returns tip labels from phylo object", {
  tree <- ape::read.tree(text = "(A:1,B:1,(C:1,D:1):1);")
  tips <- pr_extract_tips(tree)
  expect_equal(sort(tips), c("A", "B", "C", "D"))
})

test_that("pr_extract_tips reads a Newick file", {
  tmp <- tempfile(fileext = ".nwk")
  ape::write.tree(
    ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);"),
    file = tmp
  )
  tips <- pr_extract_tips(tmp)
  expect_equal(sort(tips), c("Homo_sapiens", "Pan_troglodytes"))
  unlink(tmp)
})

test_that("pr_extract_tips returns labels with underscores as-is", {
  tree <- ape::read.tree(text = "(Genus_sp1:1,Genus_sp2:1);")
  tips <- pr_extract_tips(tree)
  expect_true("Genus_sp1" %in% tips)
  expect_true("Genus_sp2" %in% tips)
})

test_that("pr_extract_tips errors on invalid file path", {
  expect_error(pr_extract_tips("/nonexistent/path/tree.nwk"),
               "not found")
})

test_that("pr_extract_tips errors on non-phylo non-path input", {
  expect_error(pr_extract_tips(42), "phylo")
})
