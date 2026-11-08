# Test coverage collection and Codecov upload

**Slice:** first Codecov coding pass (prepR4pcm / firstNext R lane only)  
**Approval:** Shinichi approved the 13-repo First+Next Codecov rollout; this
file records the pipeline, not a coverage percentage.

This is **code-coverage infrastructure** (covr + Codecov). It is not a
scientific result, not a CRAN claim, and not a README badge.

## What runs

| Workflow | Role | Shards | Uploads to Codecov? |
| --- | --- | --- | --- |
| `R-CMD-check.yaml` | package check | ubuntu by default; macOS+Windows only on `workflow_dispatch` `os=full` | **no** |
| `test-coverage.yaml` | `covr::package_coverage()` then one OIDC upload | ubuntu only | **yes, once** (`flags: ubuntu-covr`) |
| `pkgdown.yaml` | manual site rebuild | n/a | no |

Do **not** add covr to the R-CMD-check matrix. The default check shard is
already ubuntu; `os=full` would otherwise upload the same testthat suite
twice.

The coverage job re-runs the **existing** testthat suite under covr tracing.
`skip_on_cran()`, `skip_if_not_installed()`, `skip_if_offline()`, live
backend smokes, and claim tests are unchanged. Coverage numbers, when they
exist, describe that suite — not a new scientific test set.

Auth is documented Codecov OIDC (`use_oidc: true`) with job-scoped
`contents: read` and `id-token: write`. No `CODECOV_TOKEN`, and
`GITHUB_TOKEN` / `GH_TOKEN` is not used as a Codecov token.

Sibling-repo evidence: itchyshin/hsquared PR 364 successfully OIDC-uploaded
and processed a branch report with **no** owner grant and **no** repository
token. Do not assume a token is required here unless this repo's uploader
actually fails.

## Evidence stages (do not collapse)

Config on a branch is not functioning coverage. Keep these five claims
separate:

| Stage | What would prove it | This first pass |
| --- | --- | --- |
| 1. Local checks / local covr | `devtools::test()` / `covr::package_coverage()` on this host | **NOT_VERIFIED** — no R runtime here |
| 2. CI coverage finish | GitHub Actions `test-coverage` job produced `cobertura.xml` and a covr percent in the log / step summary | **NOT_VERIFIED** until that job completes |
| 3. Upload executed | Codecov action log shows a successful OIDC upload (`fail_ci_if_error: true`) | **NOT_VERIFIED** until that step succeeds |
| 4. Accepted commit report | Codecov commit URL for the **tested** SHA shows an accepted / processed report | **NOT_VERIFIED** until Codecov accepts it |
| 5. Default-branch baseline | https://app.codecov.io/gh/itchyshin/prepR4pcm (or the GitHub Codecov app) shows `main` | **NOT_VERIFIED**; needs an **authorized merge** to `main` plus a successful `push` run of `test-coverage.yaml` on `main` |

Stage 5 is why the workflow listens to `push` on `main` as well as
`pull_request`. GitHub Actions will run the PR workflow from this branch
without merging. The **main dashboard** will not exist until someone with
merge authority lands the workflow on `main` and that `push` job uploads.
This lane does not merge.

`R-CMD-check.yaml` still does **not** run on push to `main` (lean CI). That
is independent of the coverage `push` trigger.

## SHA distinction (do not conflate)

On `pull_request`, `actions/checkout@v6` checks out `github.sha`, which is
GitHub's **synthetic merge** of the PR head into the base. The branch HEAD
the reviewer pushes is `github.event.pull_request.head.sha`. codecov-action
attaches the upload to the PR head for pull_request events. Record both.
On `push` to `main`, checkout SHA, branch HEAD, and Codecov commit coincide.

## Architecture notes

- PR comments from Codecov, if the GitHub App is installed, can appear on
  this PR after a successful upload. That is stage 4-ish, still not stage 5.
- If OIDC upload fails, treat that as an actual uploader failure before
  asking the owner for a Codecov GitHub App grant or a repository token.
  Do not mint or extract tokens; do not put `GH_TOKEN` in the Codecov
  `token:` input.
- Coverage status checks are **informational** (`codecov.yml`). This pass
  does not fail PRs on an unknown baseline percentage.
- README Codecov badge is deferred until stage 4 exists. A badge is not
  evidence.
- First use of `codecov/codecov-action` on this repository may require a
  GitHub Actions allowlist click. That is a platform gate, not a token
  request.

## Local covr (when R is available)

```r
# Existing testthat suite, same skips as CI
cov <- covr::package_coverage()
print(cov)
covr::report(cov)
```

Do not treat a local percent as a Codecov dashboard number.
