# Tree augmentation --------------------------------------------------------

#' Graft missing species onto a phylogenetic tree (genus-level placement)
#'
#' When a reconciliation identifies species that are present in your data
#' but missing from the tree, `reconcile_augment()` attaches each missing
#' species as sister to a congener --- i.e., a species in the same genus
#' already present in the tree. The result is a tree that contains every
#' species in your dataset, at the cost of making a strong assumption
#' about where the new tips sit.
#'
#' @section When to use this:
#' Genus-level placement is an \emph{exploratory} convenience, not a
#' substitute for a properly inferred phylogeny. It assumes the missing
#' species diverged in roughly the same way as its congeners, which is
#' often wrong in detail. Use it to keep exploratory PCMs running while
#' you decide how to handle orphan species, and always:
#' \enumerate{
#'   \item Report exactly which species were augmented (see `$augmented`
#'     in the return value).
#'   \item Run sensitivity analyses with and without the augmented tips.
#'   \item Prefer a published imputed phylogeny (e.g. the PhyloMaker or
#'     TACT approaches) when grafting many species.
#' }
#'
#' @param reconciliation A [reconciliation] object, typically from
#'   [reconcile_tree()].
#' @param tree An `ape::phylo` object. Must be the same tree used to
#'   build `reconciliation` (or a tree with the same tip set).
#' @param where Character(1). Where to attach each new tip:
#'   \describe{
#'     \item{`"genus"` (default)}{Attach as sister to a single congener
#'       chosen at random from the genus. Simpler; recommended when the
#'       genus has only one or two representatives in the tree, or when
#'       you want variation across runs for sensitivity analyses.}
#'     \item{`"near"`}{Attach at the most recent common ancestor (MRCA)
#'       of all congeners in the tree. Better when the genus is
#'       well-represented, because the new tip is not arbitrarily tied
#'       to one sister taxon.}
#'   }
#' @param branch_length Character(1). How to set the terminal branch
#'   length of each newly added tip:
#'   \describe{
#'     \item{`"congener_median"` (default)}{Median terminal branch length
#'       of the species' congeners. Uses the average "how long since
#'       this group diverged" for the genus. Recommended for
#'       time-calibrated trees because it preserves approximate
#'       branch-length scale.}
#'     \item{`"half_terminal"`}{Half the sister tip's terminal branch.
#'       A conservative alternative that places the new tip as a recent
#'       split from its sister. Useful when the genus is sparsely
#'       sampled and the median is unreliable.}
#'     \item{`"zero"`}{Zero-length branch, producing a polytomy with the
#'       sister taxon (or MRCA). Use for exploratory sensitivity checks
#'       where you want to see the effect of adding species without
#'       assuming any divergence time.}
#'   }
#' @param seed Integer or `NULL`. Random seed for reproducibility when
#'   `where = "genus"` picks a congener at random. Set to a fixed
#'   integer in real analyses so results are reproducible. Default
#'   `NULL`.
#' @param quiet Logical. Suppress progress messages? Default `FALSE`.
#'
#' @return A list with:
#'   \describe{
#'     \item{tree}{The augmented `phylo` object.}
#'     \item{original}{The original (unmodified) `phylo` object, for
#'       easy comparison.}
#'     \item{augmented}{A tibble documenting each added species:
#'       `species`, `genus`, `placed_near` (sister tip or MRCA node),
#'       `branch_length`, `method`, `n_congeners`.}
#'     \item{skipped}{A tibble of species that could not be placed,
#'       with the reason (e.g. "no congener in tree").}
#'     \item{meta}{Provenance metadata: placement strategy, branch
#'       length rule, counts.}
#'   }
#'
#' @family reconciliation functions
#' @seealso [reconcile_tree()] for the reconciliation step;
#'   [reconcile_apply()] for the non-augmenting alternative (prune data
#'   and tree to the intersection).
#'
#' @references
#' Paradis, E. & Schliep, K. (2019) ape 5.0: an environment for modern
#' phylogenetics and evolutionary analyses in R. \emph{Bioinformatics}
#' 35:526--528. \doi{10.1093/bioinformatics/bty633}
#'
#' @examples
#' # --- Example 1: genus-level placement with congener_median branch lengths ---
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#'
#' aug <- reconcile_augment(result, tree_jetz, seed = 42)
#'
#' # Compare original vs augmented tree
#' cat("Original tips:", ape::Ntip(tree_jetz), "\n")
#' cat("Augmented tips:", ape::Ntip(aug$tree), "\n")
#' cat("Added:", nrow(aug$augmented), "| Skipped:", nrow(aug$skipped), "\n")
#'
#' # Inspect which species were added and where they were placed
#' head(aug$augmented[, c("species", "genus", "placed_near",
#'                        "branch_length", "n_congeners")])
#'
#' # Species skipped (no congener in tree)
#' head(aug$skipped)
#'
#' # --- Example 2: MRCA placement with zero-length branches ---
#' aug_near <- reconcile_augment(result, tree_jetz,
#'                               where = "near",
#'                               branch_length = "zero",
#'                               seed = 42, quiet = TRUE)
#'
#' cat("\nMRCA placement (zero branches):\n")
#' cat("  Added:", nrow(aug_near$augmented), "\n")
#' # Compare: MRCA placement shows genus-level context
#' head(aug_near$augmented[, c("species", "placed_near", "method")])
#'
#' @export
reconcile_augment <- function(reconciliation,
                               tree,
                               where = c("genus", "near"),
                               branch_length = c("congener_median",
                                                  "half_terminal",
                                                  "zero"),
                               seed = NULL,
                               quiet = FALSE) {

  validate_reconciliation(reconciliation)
  where <- match.arg(where)
  branch_length <- match.arg(branch_length)
  tree <- pr_load_tree(tree)
  original_tree <- tree

  # Find unresolved species (in data, not in tree)
  mapping <- reconciliation$mapping
  unresolved <- mapping[mapping$match_type == "unresolved" &
                          mapping$in_x & !mapping$in_y, ]
  species_to_add <- unresolved$name_x
  species_to_add <- species_to_add[!is.na(species_to_add)]

  if (length(species_to_add) == 0) {
    if (!quiet) cli_alert_info("No unresolved species to augment.")
    return(list(
      tree      = tree,
      original  = original_tree,
      augmented = tibble(species = character(), genus = character(),
                         placed_near = character(), branch_length = numeric(),
                         method = character(), n_congeners = integer()),
      skipped   = tibble(species = character(), genus = character(),
                         reason = character()),
      meta      = list(n_augmented = 0L, n_skipped = 0L,
                        where = where, branch_length_method = branch_length)
    ))
  }

  if (!is.null(seed)) set.seed(seed)

  if (!quiet) {
    cli_alert_info(
      "Augmenting tree with {length(species_to_add)} unresolved species..."
    )
  }

  # Extract genera from species names (normalise underscores to match tree tips)
  genera <- pr_extract_genus(gsub("_", " ", species_to_add))

  # Get current tip genera for matching
  tip_genera <- pr_extract_genus(gsub("_", " ", tree$tip.label))

  augmented_rows <- list()
  skipped_rows <- list()

  for (i in seq_along(species_to_add)) {
    sp <- species_to_add[i]
    genus <- genera[i]

    if (is.na(genus) || nchar(genus) == 0) {
      skipped_rows[[length(skipped_rows) + 1]] <- tibble(
        species = sp, genus = NA_character_,
        reason = "Could not extract genus from name"
      )
      next
    }

    # Find congeners in current tree
    # Update tip_genera after each addition
    current_tips <- tree$tip.label
    current_genera <- pr_extract_genus(gsub("_", " ", current_tips))
    congener_idx <- which(current_genera == genus)

    if (length(congener_idx) == 0) {
      skipped_rows[[length(skipped_rows) + 1]] <- tibble(
        species = sp, genus = genus,
        reason = "No congener in tree"
      )
      next
    }

    congener_tips <- current_tips[congener_idx]

    # Calculate branch length
    bl <- pr_calc_augment_bl(tree, congener_tips, branch_length)

    if (bl == 0 && branch_length != "zero") {
      cli_alert_warning(
        "Branch length is 0 for '{sp}'; added tip creates a polytomy."
      )
    }

    # Add species to tree
    sp_label <- gsub(" ", "_", sp)
    result <- pr_bind_species(tree, sp_label, congener_tips, where, bl)
    tree <- result$tree

    augmented_rows[[length(augmented_rows) + 1]] <- tibble(
      species       = sp,
      genus         = genus,
      placed_near   = gsub("_", " ", result$placed_near),
      branch_length = bl,
      method        = paste0(where, "/", branch_length),
      n_congeners   = length(congener_tips)
    )
  }

  # Build result tibbles
  augmented <- if (length(augmented_rows) > 0) {
    do.call(rbind, augmented_rows)
  } else {
    tibble(species = character(), genus = character(),
           placed_near = character(), branch_length = numeric(),
           method = character(), n_congeners = integer())
  }

  skipped <- if (length(skipped_rows) > 0) {
    do.call(rbind, skipped_rows)
  } else {
    tibble(species = character(), genus = character(),
           reason = character())
  }

  if (!quiet) {
    cli_alert_success(
      "Added {nrow(augmented)} species to tree ({nrow(skipped)} skipped)"
    )
    if (nrow(skipped) > 0) {
      cli_alert_warning(
        "Skipped species had no congener in the tree. See $skipped for details."
      )
    }
  }

  list(
    tree      = tree,
    original  = original_tree,
    augmented = augmented,
    skipped   = skipped,
    meta      = list(
      n_augmented          = nrow(augmented),
      n_skipped            = nrow(skipped),
      where                = where,
      branch_length_method = branch_length,
      seed                 = seed,
      original_n_tips      = ape::Ntip(original_tree),
      augmented_n_tips     = ape::Ntip(tree)
    )
  )
}


# Internal helpers ---------------------------------------------------------

#' Extract genus from binomial species names
#'
#' Takes the first word of each name as the genus.
#'
#' @param names Character vector of species names.
#' @return Character vector of genus names.
#' @keywords internal
pr_extract_genus <- function(names) {
  vapply(strsplit(names, "\\s+"), function(parts) {
    if (length(parts) >= 1 && nchar(parts[1]) > 0) parts[1] else NA_character_
  }, character(1))
}


#' Calculate branch length for an augmented tip
#'
#' @param tree phylo object.
#' @param congener_tips Character vector of congener tip labels.
#' @param method Branch length strategy.
#' @return Numeric(1) branch length.
#' @keywords internal
pr_calc_augment_bl <- function(tree, congener_tips, method) {
  switch(method,
    congener_median = {
      # Get terminal branch lengths of congeners
      tip_idx <- match(congener_tips, tree$tip.label)
      tip_idx <- tip_idx[!is.na(tip_idx)]
      if (length(tip_idx) == 0) return(0)
      edge_idx <- match(tip_idx, tree$edge[, 2])
      edge_idx <- edge_idx[!is.na(edge_idx)]
      if (length(edge_idx) == 0 || is.null(tree$edge.length)) return(0)
      bls <- tree$edge.length[edge_idx]
      stats::median(bls, na.rm = TRUE)
    },
    half_terminal = {
      # Half the first congener's terminal branch
      tip_idx <- match(congener_tips[1], tree$tip.label)
      if (is.na(tip_idx) || is.null(tree$edge.length)) return(0)
      edge_idx <- match(tip_idx, tree$edge[, 2])
      if (is.na(edge_idx)) return(0)
      tree$edge.length[edge_idx] / 2
    },
    zero = 0,
    0
  )
}


#' Bind a species to a tree as sister to a congener
#'
#' Uses `phytools::bind.tip()` if available, otherwise falls back to a
#' pure-ape implementation using `ape::bind.tree()`.
#'
#' @param tree phylo object.
#' @param sp_label Character(1). Tip label to add (underscore format).
#' @param congener_tips Character vector of congener tip labels.
#' @param where Placement strategy.
#' @param bl Branch length for the new tip.
#' @return List with `tree` and `placed_near`.
#' @keywords internal
pr_bind_species <- function(tree, sp_label, congener_tips, where, bl) {

  if (where == "genus") {
    # Pick a random congener as sister
    sister <- sample(congener_tips, 1)
    sister_idx <- which(tree$tip.label == sister)
    edge_idx <- match(sister_idx, tree$edge[, 2])

    # Attachment point: midway along the sister's terminal branch
    sister_bl <- if (!is.null(tree$edge.length) && !is.na(edge_idx)) {
      tree$edge.length[edge_idx]
    } else {
      1
    }
    attach_point <- sister_bl / 2

    tree <- pr_bind_tip(tree, sp_label,
                        where = sister_idx,
                        position = attach_point,
                        edge.length = bl)

    return(list(tree = tree, placed_near = sister))
  }

  if (where == "near") {
    if (length(congener_tips) == 1) {
      # Monotypic genus — same as "genus" strategy
      return(pr_bind_species(tree, sp_label, congener_tips, "genus", bl))
    }

    # Attach at MRCA of all congeners
    congener_idx <- match(congener_tips, tree$tip.label)
    congener_idx <- congener_idx[!is.na(congener_idx)]
    mrca_node <- ape::getMRCA(tree, congener_idx)

    if (is.null(mrca_node)) {
      # Fallback to genus strategy
      return(pr_bind_species(tree, sp_label, congener_tips, "genus", bl))
    }

    tree <- pr_bind_tip(tree, sp_label,
                        where = mrca_node,
                        position = 0,
                        edge.length = bl)

    return(list(tree = tree,
                placed_near = paste("MRCA of",
                                    paste(utils::head(congener_tips, 3),
                                          collapse = ", "))))
  }

  abort(paste0("Unknown placement strategy: '", where, "'"),
        call = caller_env())
}


#' Bind a tip to a tree
#'
#' Wrapper that uses `phytools::bind.tip()` if available, otherwise
#' uses a pure-ape implementation via `ape::bind.tree()`.
#'
#' @param tree phylo object.
#' @param tip_label Character(1). Label for the new tip.
#' @param where Integer. Node or tip index to bind near.
#' @param position Numeric. How far back from the node to attach.
#' @param edge.length Numeric. Branch length of the new tip.
#' @return A modified phylo object.
#' @keywords internal
pr_bind_tip <- function(tree, tip_label, where, position = 0,
                         edge.length = 0) {
  # Try phytools first
 if (requireNamespace("phytools", quietly = TRUE)) {
    return(phytools::bind.tip(tree, tip_label,
                               where = where,
                               position = position,
                               edge.length = edge.length))
  }

  # Pure-ape fallback: create a 1-tip tree and bind it
  new_tip <- list(
    edge        = matrix(c(2L, 1L), 1, 2),
    tip.label   = tip_label,
    edge.length = edge.length,
    Nnode       = 1L
  )
  class(new_tip) <- "phylo"

  # Find the edge leading to `where`
  edge_row <- which(tree$edge[, 2] == where)
  if (length(edge_row) == 0) {
    # `where` is the root — bind at root
    tree <- ape::bind.tree(tree, new_tip)
    return(tree)
  }

  parent_bl <- tree$edge.length[edge_row]
  if (is.null(parent_bl)) parent_bl <- 1

  if (position > 0 && position < parent_bl) {
    # Split the edge: shorten the existing edge, then bind
    tree$edge.length[edge_row] <- parent_bl - position
    tree <- ape::bind.tree(tree, new_tip, where = where, position = position)
  } else {
    tree <- ape::bind.tree(tree, new_tip, where = where)
  }

  tree
}
