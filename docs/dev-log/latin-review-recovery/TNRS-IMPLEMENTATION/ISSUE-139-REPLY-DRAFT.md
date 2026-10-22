@ejlundgren, thanks for testing the matching against your examples and showing where the suggestions became too restrictive.

I have pushed a test branch that restores spelling-based suggestions for review and adds an optional botanical TNRS route. The TNRS route is separate: it only offers a review candidate when the service's accepted name matches exactly one tip in the supplied tree. It keeps the service result and score for inspection, and does not change the mapping or accept a suggestion automatically.

To install the branch in R:

```r
remotes::install_github(
  "itchyshin/prepR4pcm",
  ref = "codex/latin-review-recovery",
  upgrade = "never"
)
```

Ordinary review suggestions do not need the TNRS package. To try the optional botanical lookup, install `TNRS` from CRAN with `install.packages("TNRS")`, create the reconciliation as usual, then run `reconcile_tnrs(result, source = "wcvp")` and inspect the results with `reconcile_candidates()` or `reconcile_review()`.

I tested nine public names. WCVP returned *Trema tomentosum* for *Trema tomentosa* and *Trema orientale* for *Trema orientalis*. It returned *Adenocalymma ternatum* as the accepted name for *Bignonia ternata*, not *A. macrophyllum*. Since *A. ternatum* is absent from your tree, it remains audit evidence without a tree-tip candidate. The five similar-name pairs you raised were not supported as matches to those distinct WCVP taxa.

The focused adapter tests pass (51 tests), and the full package test suite passes (3,247 passed, 0 failed, 24 warnings, 47 skips). The package check still has two network-dependent failures in existing OpenTree examples and vignettes in this environment. I also could not run a live request through the TNRS R client here; the public examples came from the TNRS browser and are not a direct test of that client.

Could you install this branch and try your original data and tree? In particular, it would help to know whether the restored review suggestions behave as expected and whether the optional TNRS candidates are useful while remaining review-only.
