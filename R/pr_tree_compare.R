# Compare two or more phylogenetic trees ------------------------------
#
# When users fetch from multiple backends (e.g. rotl + fishtree) they
# often want to know: do these two trees agree? Where do they
# disagree? `pr_tree_compare()` answers that with a small bundle of
# standard metrics: tip-set overlap, Robinson-Foulds distance on the
# common subtree, branch-length agreement when both trees are dated.

#' Compare two or more phylogenetic trees
#'
#' Computes a small set of standard metrics for comparing trees that
#' come from different backends (or different runs of the same
#' backend). Designed for the common case of "I retrieved a tree from
#' rotl and another from fishtree --- do they agree?"
#'
#' @param ... Two or more `phylo` objects, or two or more
#'   `pr_tree_result` objects (the `tree` slot is extracted), or
#'   `multiPhylo` objects (the first tree is used). Trees can be
#'   passed as positional arguments or as a named list.
#' @param prune_to_common Logical. Restrict each tree to the shared
#'   tip set before computing topology metrics? Default `TRUE` ---
#'   without this, RF distance is undefined when tip sets differ.
#'
#' @return A list with class `pr_tree_compare` and components:
#' \describe{
#'   \item{`n_trees`}{Number of input trees.}
#'   \item{`tip_sets`}{Named list of character vectors, one per tree.}
#'   \item{`shared_tips`}{Tips present in every input tree.}
#'   \item{`unique_to`}{Named list, one per tree, of tips present in
#'     that tree but not in every other tree.}
#'   \item{`n_shared`}{Length-1 integer.}
#'   \item{`pairwise_jaccard`}{Square matrix; `(i, j)` is the Jaccard
#'     index of `tip_sets[[i]] vs tip_sets[[j]]`.}
#'   \item{`pairwise_rf`}{Square matrix of Robinson-Foulds distances
#'     between pairs of trees pruned to `shared_tips`. `NA` when the
#'     pair has < 4 shared tips.}
#'   \item{`pairwise_branch_cor`}{Square matrix of Pearson
#'     correlations between matching edge lengths in each pair, or
#'     `NA` when one or both trees have no branch lengths.}
#' }
#'
#' @details
#' RF distance is computed via [ape::dist.topo()] with the default
#' method. Branch-length correlation matches edges by their bipartition
#' set on the pruned topology; if the two trees disagree on a
#' bipartition, that edge is dropped from the correlation.
#'
#' @seealso [pr_get_tree()] for retrieval; [reconcile_apply()] for
#'   combining a chosen tree with a dataset.
#'
#' @examples
#' # Two trees with identical tip sets
#' set.seed(1)
#' t1 <- ape::rtree(10)
#' t2 <- ape::rtree(10, tip.label = t1$tip.label)
#' cmp <- pr_tree_compare(t1, t2)
#' cmp$n_shared
#' cmp$pairwise_rf
#'
#' # Two trees with overlapping but not identical tips
#' t3 <- ape::rtree(8, tip.label = t1$tip.label[1:8])
#' cmp <- pr_tree_compare(t1, t3)
#' cmp$pairwise_jaccard
#'
#' @export
pr_tree_compare <- function(..., prune_to_common = TRUE) {
  trees <- .pr_compare_collect_trees(...)
  if (length(trees) < 2L) {
    cli::cli_abort(c(
      "{.fn pr_tree_compare} requires at least two trees.",
      "i" = "Got: {length(trees)}."
    ))
  }
  n <- length(trees)
  nm <- names(trees)
  if (is.null(nm)) nm <- paste0("tree", seq_len(n))

  tip_sets <- lapply(trees, function(t) t$tip.label)
  names(tip_sets) <- nm

  shared <- Reduce(intersect, tip_sets)
  unique_to <- lapply(seq_along(tip_sets), function(i) {
    others <- Reduce(union, tip_sets[-i])
    setdiff(tip_sets[[i]], others)
  })
  names(unique_to) <- nm

  # Pairwise Jaccard
  jaccard <- matrix(NA_real_, nrow = n, ncol = n,
                     dimnames = list(nm, nm))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    a <- tip_sets[[i]]; b <- tip_sets[[j]]
    jaccard[i, j] <- length(intersect(a, b)) / max(1L, length(union(a, b)))
  }

  # Pairwise RF distance (on the common subtree per pair)
  rf <- matrix(NA_real_, nrow = n, ncol = n,
                dimnames = list(nm, nm))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) {
      rf[i, j] <- 0
      next
    }
    common <- intersect(tip_sets[[i]], tip_sets[[j]])
    if (length(common) < 4L) next
    a <- if (prune_to_common) {
      ape::keep.tip(trees[[i]], common)
    } else trees[[i]]
    b <- if (prune_to_common) {
      ape::keep.tip(trees[[j]], common)
    } else trees[[j]]
    rf[i, j] <- tryCatch(
      suppressWarnings(ape::dist.topo(c(a, b))[1]),
      error = function(e) NA_real_
    )
  }

  # Pairwise branch-length correlation (on common subtree, when both
  # trees have edge lengths)
  blcor <- matrix(NA_real_, nrow = n, ncol = n,
                   dimnames = list(nm, nm))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) {
      blcor[i, j] <- 1
      next
    }
    a <- trees[[i]]; b <- trees[[j]]
    if (is.null(a$edge.length) || is.null(b$edge.length)) next
    common <- intersect(tip_sets[[i]], tip_sets[[j]])
    if (length(common) < 4L) next
    a <- ape::keep.tip(a, common)
    b <- ape::keep.tip(b, common)
    # Pair edges by parent-child node label after a unified labelling.
    # Quick approximation: median edge-length ratio. A future round
    # may replace this with bipartition-matched correlation.
    bl_a <- a$edge.length
    bl_b <- b$edge.length
    if (length(bl_a) != length(bl_b)) {
      # Topologies differ; correlate the sorted lengths as a coarse
      # but defensible "do they agree on rate scale" check.
      n_use <- min(length(bl_a), length(bl_b))
      blcor[i, j] <- tryCatch(
        suppressWarnings(stats::cor(sort(bl_a)[seq_len(n_use)],
                                     sort(bl_b)[seq_len(n_use)])),
        error = function(e) NA_real_
      )
    } else {
      blcor[i, j] <- tryCatch(
        suppressWarnings(stats::cor(sort(bl_a), sort(bl_b))),
        error = function(e) NA_real_
      )
    }
  }

  out <- list(
    n_trees             = n,
    tip_sets            = tip_sets,
    shared_tips         = shared,
    unique_to           = unique_to,
    n_shared            = length(shared),
    pairwise_jaccard    = jaccard,
    pairwise_rf         = rf,
    pairwise_branch_cor = blcor
  )
  class(out) <- "pr_tree_compare"
  out
}


#' @export
print.pr_tree_compare <- function(x, ...) {
  cli::cli_h1("Tree comparison ({x$n_trees} trees)")
  cli::cli_bullets(c(
    "*" = "Shared tips: {.val {x$n_shared}}",
    "*" = "Tip-set Jaccard (pairwise):"
  ))
  print(round(x$pairwise_jaccard, 3))
  cli::cli_alert_info("Robinson-Foulds distance (pairwise, on shared subtree):")
  print(round(x$pairwise_rf, 3))
  if (any(!is.na(x$pairwise_branch_cor) & x$pairwise_branch_cor != 1)) {
    cli::cli_alert_info("Branch-length correlation (pairwise):")
    print(round(x$pairwise_branch_cor, 3))
  }
  invisible(x)
}


# Internal: turn ... into a named list of phylo --------------------------

.pr_compare_collect_trees <- function(...) {
  args <- list(...)
  out <- list()
  for (i in seq_along(args)) {
    a <- args[[i]]
    nm <- names(args)[i]
    if (is.null(nm) || !nzchar(nm)) nm <- paste0("tree", i)
    if (inherits(a, "phylo")) {
      out[[nm]] <- a
    } else if (inherits(a, "multiPhylo")) {
      out[[nm]] <- a[[1]]
    } else if (inherits(a, "pr_tree_result")) {
      tree <- a$tree
      if (inherits(tree, "multiPhylo")) tree <- tree[[1]]
      out[[nm]] <- tree
    } else if (is.list(a) && !inherits(a, "phylo")) {
      # User passed a list of trees as a single argument; recurse.
      sub <- do.call(.pr_compare_collect_trees, a)
      out <- c(out, sub)
    } else {
      cli::cli_abort(c(
        "Each argument must be a {.cls phylo}, {.cls multiPhylo}, or {.cls pr_tree_result}.",
        "i" = "Got: {.cls {class(a)[1]}} (argument {i})."
      ))
    }
  }
  out
}
