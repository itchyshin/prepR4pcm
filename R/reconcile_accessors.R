# Accessor and action functions for reconciliation objects -----------------

#' Extract the mapping table from a reconciliation
#'
#' Returns the full mapping tibble, suitable for downstream joins.
#'
#' @param x A `reconciliation` object.
#' @return A tibble with columns: `name_x`, `name_y`, `name_resolved`,
#'   `match_type`, `match_score`, `match_source`, `in_x`, `in_y`, `notes`.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#' mapping <- reconcile_mapping(result)
#' # Filter to matched species
#' matched <- mapping[mapping$in_x & mapping$in_y, ]
#' nrow(matched)
#'
#' @export
reconcile_mapping <- function(x) {
  validate_reconciliation(x)
  x$mapping
}


#' Add a manual override to a reconciliation
#'
#' Creates an updated reconciliation object with a manual name correction
#' applied.
#'
#' @param x A `reconciliation` object.
#' @param name_x Character(1). Name from source x.
#' @param name_y Character(1) or NULL. Corrected match in source y. Use `NULL`
#'   to mark the name as deliberately unresolvable.
#' @param action Character(1). One of `"accept"` (confirm match), `"reject"`
#'   (remove a match), or `"replace"` (set a new match).
#' @param note Character(1). Justification for the override. Stored in the
#'   provenance log.
#'
#' @return An updated `reconciliation` object.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#' # Manually assign an unresolved species to a tree tip
#' unresolved <- reconcile_mapping(result)
#' unresolved <- unresolved[unresolved$match_type == "unresolved" &
#'                            unresolved$in_x, ]
#' if (nrow(unresolved) > 0) {
#'   result <- reconcile_override(
#'     result,
#'     name_x = unresolved$name_x[1],
#'     name_y = tree_jetz$tip.label[1],
#'     note = "Manual assignment for demonstration"
#'   )
#' }
#'
#' @export
reconcile_override <- function(x, name_x, name_y = NULL,
                               action = c("accept", "reject", "replace"),
                               note = "") {

  validate_reconciliation(x)
  action <- match.arg(action)

  # Record the override
  new_override <- tibble(
    name_x    = name_x,
    name_y    = name_y %||% NA_character_,
    action    = action,
    user_note = note,
    timestamp = Sys.time()
  )
  x$overrides <- rbind(x$overrides, new_override)

  # Apply the override to the mapping
  mapping <- x$mapping

  if (action == "accept" || action == "replace") {
    if (is.null(name_y)) {
      abort("Must provide `name_y` for 'accept' or 'replace' actions.",
            call = caller_env())
    }

    # Remove any existing row for name_x
    mapping <- mapping[!(mapping$name_x %in% name_x & !is.na(mapping$name_x)), ]

    # Also remove name_y from unresolved y-only rows
    mapping <- mapping[!(mapping$name_y %in% name_y &
                           mapping$match_type == "unresolved" &
                           !mapping$in_x), ]

    # Add the manual match
    mapping <- rbind(mapping, tibble(
      name_x        = name_x,
      name_y        = name_y,
      name_resolved = NA_character_,
      match_type    = "manual",
      match_score   = 1.0,
      match_source  = "user_override",
      in_x          = TRUE,
      in_y          = TRUE,
      notes         = note
    ))

  } else if (action == "reject") {
    # Mark the match as unresolved
    idx <- which(mapping$name_x == name_x & !is.na(mapping$name_x))
    if (length(idx) > 0) {
      old_y <- mapping$name_y[idx[1]]
      mapping$match_type[idx[1]] <- "unresolved"
      mapping$match_score[idx[1]] <- NA_real_
      mapping$match_source[idx[1]] <- NA_character_
      mapping$in_y[idx[1]] <- FALSE
      mapping$name_y[idx[1]] <- NA_character_
      mapping$notes[idx[1]] <- paste("Rejected:", note)

      # Re-add the y name as unresolved if it was matched
      if (!is.na(old_y) && !old_y %in% mapping$name_y) {
        mapping <- rbind(mapping, tibble(
          name_x        = NA_character_,
          name_y        = old_y,
          name_resolved = NA_character_,
          match_type    = "unresolved",
          match_score   = NA_real_,
          match_source  = NA_character_,
          in_x          = FALSE,
          in_y          = TRUE,
          notes         = "Re-added after rejection"
        ))
      }
    }
  }

  x$mapping <- mapping
  x$counts <- pr_compute_counts(mapping)

  cli_alert_success("Override applied: '{name_x}' -> '{name_y %||% 'NA'}' ({action})")

  x
}


#' Apply a reconciliation to produce aligned data and tree
#'
#' Uses the reconciliation mapping to rename and/or subset a data frame and
#' phylogenetic tree so that species names match.
#'
#' @param x A `reconciliation` object.
#' @param data A data frame to align. If `NULL`, returns only the mapping.
#' @param tree An `ape::phylo` object to align. If `NULL`, returns only the
#'   data frame.
#' @param species_col Character(1). Column name in `data` containing species
#'   names. Auto-detected if `NULL`.
#' @param drop_unresolved Logical. Drop species with no match? Default `FALSE`
#'   (keeps them with a warning).
#'
#' @return A list with components `$data` (aligned data frame) and `$tree`
#'   (aligned phylo object). Either may be `NULL` if not provided.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#' aligned <- reconcile_apply(result,
#'                            data = avonet_subset, tree = tree_jetz,
#'                            species_col = "Species1",
#'                            drop_unresolved = TRUE)
#' cat("Aligned data:", nrow(aligned$data), "rows\n")
#' cat("Aligned tree:", ape::Ntip(aligned$tree), "tips\n")
#'
#' @export
reconcile_apply <- function(x, data = NULL, tree = NULL,
                            species_col = NULL,
                            drop_unresolved = FALSE) {

  validate_reconciliation(x)
  mapping <- x$mapping

  # Get matched names (in both x and y)
  matched <- mapping[mapping$in_x & mapping$in_y, ]

  result <- list(data = NULL, tree = NULL)

  # Align data frame

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      abort("`data` must be a data frame.", call = caller_env())
    }

    if (is.null(species_col)) {
      species_col <- pr_detect_species_column(data, "species_col")
    }

    data_names <- as.character(data[[species_col]])

    if (drop_unresolved) {
      keep <- data_names %in% matched$name_x
      data <- data[keep, , drop = FALSE]
      n_dropped <- sum(!keep)
      if (n_dropped > 0) {
        cli_alert_warning("Dropped {n_dropped} rows with unresolved species from data")
      }
    }

    result$data <- data
  }

  # Align tree
  if (!is.null(tree)) {
    tree <- pr_load_tree(tree)
    tree <- pr_align_tree(tree, mapping, drop_unresolved = drop_unresolved)

    if (drop_unresolved) {
      n_tips <- length(tree$tip.label)
      cli_alert_info("Tree has {n_tips} tips after alignment")
    }

    result$tree <- tree
  }

  result
}


#' Load overrides from a data frame or file path
#'
#' @param overrides A data frame, file path to CSV, or NULL.
#' @return A data frame with columns `name_x`, `name_y`, and optionally
#'   `user_note`, or NULL.
#' @keywords internal
pr_load_overrides <- function(overrides) {
  if (is.null(overrides)) return(NULL)

  if (is.character(overrides) && length(overrides) == 1) {
    if (!file.exists(overrides)) {
      abort(
        c("Overrides file not found.", "x" = paste0("Path: ", overrides)),
        call = caller_env()
      )
    }
    overrides <- utils::read.csv(overrides, stringsAsFactors = FALSE)
  }

  if (!is.data.frame(overrides)) {
    abort("`overrides` must be a data frame or a file path to a CSV.",
          call = caller_env())
  }

  if (!"name_x" %in% names(overrides) || !"name_y" %in% names(overrides)) {
    abort(
      c(
        "`overrides` must have columns `name_x` and `name_y`.",
        "i" = paste0("Columns found: ", paste(names(overrides), collapse = ", "))
      ),
      call = caller_env()
    )
  }

  if (!"user_note" %in% names(overrides)) {
    overrides$user_note <- "Pre-built override"
  }

  overrides
}
