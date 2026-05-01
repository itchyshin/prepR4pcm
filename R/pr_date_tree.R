# pr_date_tree() — time-calibrate a topology via datelife --------------
#
# Companion to `pr_get_tree()`. Where `pr_get_tree()` answers "I have
# only species names; give me a tree", `pr_date_tree()` answers "I have
# a topology; date it". Both return the same `pr_tree_result` shape so
# downstream consumers (notably `pigauto::multi_impute_trees()`) treat
# them interchangeably.

#' Time-calibrate a topology using the DateLife chronogram database
#'
#' Wraps \code{datelife::datelife_use()} to add divergence-time
#' calibrations to an existing `phylo` (or `multiPhylo`) using
#' DateLife's database of pre-computed chronograms (Sanchez Reyes et
#' al. 2024, \emph{Systematic Biology} 73:470). Returns a result with
#' the same shape as [pr_get_tree()] so downstream PCM workflows ---
#' including \href{https://itchyshin.github.io/pigauto/}{pigauto}'s
#' posterior-tree imputation --- can consume it without further glue
#' code.
#'
#' @section When to use this:
#' Use `pr_date_tree()` when you already have a topology (e.g. from a
#' published phylogeny or your own analysis) and want to attach
#' divergence times. Use [pr_get_tree()] with `source = "datelife"` if
#' you have only species names. Both end up calling \pkg{datelife},
#' but the starting point is different.
#'
#' @param tree An `ape::phylo` (or `multiPhylo`) object: the topology
#'   to calibrate.
#' @param n_dated A length-1 positive integer. How many calibrated
#'   trees to return. `1L` (default) returns a single dated topology;
#'   `> 1L` triggers `each = TRUE` so DateLife returns one chronogram
#'   per source (capped at `n_dated`).
#' @param dating_method A length-1 character vector. Forwarded to
#'   `datelife::datelife_use()`. One of `"bladj"` (default; fast,
#'   no calibration uncertainty) or `"mrbayes"` (Bayesian; slower,
#'   produces credible intervals).
#' @param ... Additional arguments forwarded to
#'   \code{datelife::datelife_use()}.
#'
#' @return A list with class `pr_tree_result` and components:
#' \describe{
#'   \item{`tree`}{The dated topology --- a `phylo` when `n_dated = 1`
#'     or a `multiPhylo` when `n_dated > 1`.}
#'   \item{`matched`}{Tip labels of the input that DateLife was able
#'     to date.}
#'   \item{`unmatched`}{Tip labels of the input absent from DateLife's
#'     database (returned with no calibration applied).}
#'   \item{`source`}{Always `"datelife"` (paired with `pr_get_tree()`'s
#'     dispatch).}
#'   \item{`backend_meta`}{Includes `dating_method`, `calibrations`
#'     (per-node calibration table from DateLife), and the standard
#'     `tree_provenance` list (one entry per returned tree).}
#' }
#'
#' @seealso [pr_get_tree()] for retrieval (species --> tree);
#'   [pr_cite_tree()] for formatting the citations of the result;
#'   [reconcile_augment()] for filling tip-level gaps in an existing
#'   tree (a complementary operation to dating).
#'
#' @references
#' Sanchez Reyes, L. L., McTavish, E. J., & O'Meara, B. (2024).
#' DateLife: Leveraging databases and analytical tools to reveal the
#' dated Tree of Life. \emph{Systematic Biology}, 73(2), 470--485.
#' \doi{10.1093/sysbio/syae015}
#'
#' @examples
#' \dontrun{
#'   # Example 1: date a single topology
#'   library(ape)
#'   tr  <- read.tree(text = "(Rhea_americana,(Pterocnemia_pennata,Struthio_camelus));")
#'   res <- pr_date_tree(tr)
#'   res$tree                       # phylo (chronogram)
#'   res$backend_meta$dating_method # "bladj"
#'
#'   # Example 2: request multiple per-source chronograms (one per
#'   # DateLife source)
#'   res <- pr_date_tree(tr, n_dated = 5)
#'   class(res$tree)                # multiPhylo
#'   length(res$backend_meta$tree_provenance)  # 1 entry per tree
#'
#'   # Example 3: hand the result to pigauto for multi-tree PCMs
#'   # mi  <- pigauto::multi_impute_trees(my_data, res$tree, m_per_tree = 5)
#' }
#'
#' @export
pr_date_tree <- function(tree, n_dated = 1L,
                          dating_method = "bladj", ...) {
  if (!inherits(tree, "phylo") && !inherits(tree, "multiPhylo")) {
    cli::cli_abort(c(
      "{.arg tree} must be a {.cls phylo} or {.cls multiPhylo} object.",
      "i" = "Got: {.cls {class(tree)[1]}}."
    ))
  }
  if (!is.numeric(n_dated) || length(n_dated) != 1L || n_dated < 1L) {
    cli::cli_abort(c(
      "{.arg n_dated} must be a length-1 positive integer.",
      "i" = "Got: {.val {n_dated}}."
    ))
  }
  n_dated <- as.integer(n_dated)

  res <- .pr_date_tree_datelife(tree,
                                 n_dated = n_dated,
                                 dating_method = dating_method,
                                 ...)

  # Standardise the result to the pr_tree_result contract used by
  # pr_get_tree().
  res$backend_meta <- .pr_ensure_tree_provenance(
    res$tree, res$backend_meta, source = "datelife"
  )
  out <- list(
    tree         = res$tree,
    matched      = res$matched,
    unmatched    = res$unmatched,
    source       = "datelife",
    backend_meta = res$backend_meta
  )
  class(out) <- "pr_tree_result"
  out
}


# Internal: do the work via datelife::datelife_use() ---------------------

.pr_date_tree_datelife <- function(tree, n_dated = 1L,
                                    dating_method = "bladj", ...) {
  if (!requireNamespace("datelife", quietly = TRUE)) {
    cli::cli_abort(
      c("{.fn pr_date_tree} requires the {.pkg datelife} package.",
        "i" = 'Install with: {.code pak::pak("phylotastic/datelife")} (GitHub-only; archived from CRAN in 2024).',
        ">" = "See {.url https://github.com/phylotastic/datelife} for details.")
    )
  }

  each <- n_dated > 1L
  out <- datelife::datelife_use(
    input         = tree,
    each          = each,
    dating_method = dating_method,
    ...
  )

  # `each = TRUE` may return a multiPhylo with more (or fewer) elements
  # than n_dated. Cap if needed.
  if (inherits(out, "multiPhylo") && length(out) > n_dated) {
    out <- out[seq_len(n_dated)]
  }

  # Tip labels of the input are the union of "matched" + "unmatched":
  # DateLife dates whichever it can find in its database. We approximate
  # matched / unmatched by intersecting the input tip set with the
  # output tip set (under-approximate if the dating algorithm prunes).
  input_tips <- if (inherits(tree, "multiPhylo")) {
    tree[[1]]$tip.label
  } else {
    tree$tip.label
  }
  output_tips <- if (inherits(out, "multiPhylo")) {
    out[[1]]$tip.label
  } else {
    out$tip.label
  }
  matched   <- intersect(input_tips, output_tips)
  unmatched <- setdiff(input_tips, output_tips)

  list(
    tree         = out,
    matched      = matched,
    unmatched    = unmatched,
    backend_meta = list(
      backend       = "datelife",
      version       = as.character(utils::packageVersion("datelife")),
      dating_method = dating_method,
      calibrations  = attr(out, "datelife_calibrations"),
      n_returned    = if (inherits(out, "multiPhylo")) length(out) else 1L,
      reference     = "Sanchez Reyes et al. (2024) Syst. Biol. 73:470 (doi:10.1093/sysbio/syae015)"
    )
  )
}
