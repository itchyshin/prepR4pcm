# Tree utilities -----------------------------------------------------------

#' Extract tip labels from a tree
#'
#' Accepts an `ape::phylo` object or a file path (Newick or Nexus,
#' auto-detected). Returns the tip labels as a character vector.
#'
#' @param tree An `ape::phylo` object, or a character(1) file path to a
#'   Newick (.nwk, .tre, .tree, .newick) or Nexus (.nex, .nexus) file.
#'
#' @return Character vector of tip labels.
#'
#' @examples
#' data(tree_jetz)
#' tips <- pr_extract_tips(tree_jetz)
#' head(tips)
#'
#' @export
pr_extract_tips <- function(tree) {
  tree <- pr_load_tree(tree)
  tree$tip.label
}


#' Load a phylogenetic tree
#'
#' If `tree` is already a `phylo` object, returns it. If it is a file path,
#' attempts to read it as Newick first, then Nexus.
#'
#' @param tree An `ape::phylo` object or a character(1) file path.
#' @return An `ape::phylo` object.
#' @keywords internal
pr_load_tree <- function(tree) {
  if (inherits(tree, "phylo")) {
    return(tree)
  }

  if (is.character(tree) && length(tree) == 1) {
    if (!file.exists(tree)) {
      abort(
        c("Tree file not found.", "x" = paste0("Path: ", tree)),
        call = caller_env()
      )
    }

    ext <- tolower(tools::file_ext(tree))

    # Try Nexus for .nex/.nexus files
    if (ext %in% c("nex", "nexus")) {
      tryCatch(
        return(ape::read.nexus(tree)),
        error = function(e) {
          abort(
            c("Failed to read Nexus tree file.",
              "x" = conditionMessage(e)),
            call = caller_env()
          )
        }
      )
    }

    # Try Newick for everything else
    tryCatch(
      return(ape::read.tree(tree)),
      error = function(e) {
        # If Newick fails, try Nexus as fallback
        tryCatch(
          return(ape::read.nexus(tree)),
          error = function(e2) {
            abort(
              c("Failed to read tree file as Newick or Nexus.",
                "x" = paste0("Newick error: ", conditionMessage(e)),
                "x" = paste0("Nexus error: ", conditionMessage(e2))),
              call = caller_env()
            )
          }
        )
      }
    )
  }

  abort(
    c("`tree` must be an ape::phylo object or a file path.",
      "i" = paste0("Got: ", class(tree)[1])),
    call = caller_env()
  )
}


#' Align a tree to a reconciliation mapping
#'
#' Renames and/or prunes tip labels according to the reconciliation mapping.
#'
#' @param tree An `ape::phylo` object.
#' @param mapping A mapping tibble from a reconciliation object.
#' @param drop_unresolved Logical. Drop tips with no match? Default `FALSE`.
#'
#' @return A modified `ape::phylo` object.
#' @keywords internal
pr_align_tree <- function(tree, mapping, drop_unresolved = FALSE) {
  tips <- tree$tip.label

  # Build a lookup: normalised tip -> matched name
  tip_norm <- pr_normalize_names(tips)
  matched <- mapping[mapping$in_x & mapping$in_y, ]

  # Rename tips where the y-name (tree side) differs from x-name (data side)
  for (i in seq_len(nrow(matched))) {
    y_name <- matched$name_y[i]
    x_name <- matched$name_x[i]
    if (!is.na(y_name) && !is.na(x_name)) {
      # Find the tip that corresponds to y_name (original or normalised)
      idx <- which(tips == y_name | tip_norm == pr_normalize_names(y_name))
      if (length(idx) > 0) {
        tree$tip.label[idx[1]] <- x_name
      }
    }
  }

  # Drop unresolved tips
  if (drop_unresolved) {
    unresolved_y <- mapping$name_y[mapping$match_type == "unresolved" &
                                     !mapping$in_x & mapping$in_y]
    unresolved_y <- unresolved_y[!is.na(unresolved_y)]
    if (length(unresolved_y) > 0) {
      # Match against current tip labels
      to_drop <- intersect(tree$tip.label, unresolved_y)
      if (length(to_drop) > 0) {
        tree <- ape::drop.tip(tree, to_drop)
      }
    }
  }

  tree
}
