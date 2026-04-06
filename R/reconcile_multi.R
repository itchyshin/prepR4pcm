#' Reconcile multiple datasets against one phylogenetic tree
#'
#' Calls [reconcile_tree()] on each dataset and combines the results into a
#' single reconciliation object. Species that appear in multiple datasets are
#' reconciled once; the combined mapping shows which datasets each species
#' appears in.
#'
#' @param datasets A named list of data frames. Names are used as dataset
#'   labels in the output.
#' @param tree An `ape::phylo` object or a file path.
#' @param species_cols Character vector or NULL. Column names for species in
#'   each dataset (recycled if length 1). Auto-detected if `NULL`.
#' @inheritParams reconcile_data
#'
#' @return A `reconciliation` object. The mapping table includes all unique
#'   species across all datasets.
#'
#' @examples
#' data(avonet_subset)
#' data(nesttrait_subset)
#' data(tree_jetz)
#' datasets <- list(
#'   morpho = avonet_subset,
#'   nests  = nesttrait_subset
#' )
#' result <- reconcile_multi(datasets, tree_jetz,
#'                           species_cols = c("Species1", "Scientific_name"),
#'                           authority = NULL)
#' print(result)
#'
#' @export
reconcile_multi <- function(datasets, tree,
                            species_cols = NULL,
                            authority = "col",
                            rank = c("species", "subspecies"),
                            overrides = NULL,
                            db_version = NULL,
                            fuzzy = FALSE,
                            fuzzy_threshold = 0.9,
                            resolve = c("flag", "first"),
                            quiet = FALSE) {

  rank <- match.arg(rank)
  resolve <- match.arg(resolve)

  # Validate inputs
  if (!is.list(datasets) || length(datasets) == 0) {
    abort("`datasets` must be a non-empty named list of data frames.",
          call = caller_env())
  }

  if (is.null(names(datasets))) {
    names(datasets) <- paste0("dataset_", seq_along(datasets))
  }

  for (nm in names(datasets)) {
    if (!is.data.frame(datasets[[nm]])) {
      abort(paste0("Element '", nm, "' of `datasets` is not a data frame."),
            call = caller_env())
    }
  }

  # Handle species_cols
  if (!is.null(species_cols)) {
    if (length(species_cols) == 1) {
      species_cols <- rep(species_cols, length(datasets))
    }
    if (length(species_cols) != length(datasets)) {
      abort(
        paste0("`species_cols` must have length 1 or ", length(datasets), "."),
        call = caller_env()
      )
    }
  }

  # Load tree once
  tree_obj <- pr_load_tree(tree)
  tips <- tree_obj$tip.label

  tree_source <- if (is.character(tree)) {
    basename(tree)
  } else {
    sprintf("phylo (%d tips)", length(tips))
  }

  # Collect all unique species names across datasets
  all_names <- character()

  for (i in seq_along(datasets)) {
    df <- datasets[[i]]
    col <- if (!is.null(species_cols)) {
      species_cols[i]
    } else {
      pr_detect_species_column(df, paste0("species in '", names(datasets)[i], "'"))
    }
    all_names <- c(all_names, as.character(df[[col]]))
  }

  all_names <- unique(all_names[!is.na(all_names)])

  if (!quiet) {
    cli_alert_info(
      "Reconciling {length(all_names)} unique names from {length(datasets)} datasets vs {length(tips)} tree tips"
    )
  }

  # Load overrides
  overrides_df <- pr_load_overrides(overrides)

  # Run cascade on combined names
  mapping <- pr_run_cascade(
    names_x         = all_names,
    names_y         = tips,
    authority       = authority,
    db_version      = db_version,
    rank            = rank,
    overrides       = overrides_df,
    fuzzy           = fuzzy,
    fuzzy_threshold = fuzzy_threshold,
    resolve         = resolve,
    quiet           = quiet
  )

  # Build metadata
  meta <- list(
    call             = match.call(),
    type             = "multi",
    timestamp        = Sys.time(),
    authority        = authority %||% "none",
    db_version       = db_version %||% "latest",
    fuzzy            = fuzzy,
    fuzzy_threshold  = if (fuzzy) fuzzy_threshold else NA_real_,
    fuzzy_method     = if (fuzzy) "component_levenshtein" else NA_character_,
    resolve          = resolve,
    prepR4pcm_version = as.character(utils::packageVersion("prepR4pcm")),
    x_source         = paste(names(datasets), collapse = ", "),
    y_source         = tree_source,
    rank             = rank,
    dataset_names    = names(datasets)
  )

  result <- new_reconciliation(mapping = mapping, meta = meta)

  if (!quiet) {
    n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
    cli_alert_success("Matched {n_matched}/{length(all_names)} unique names to tree tips")
  }

  result
}
