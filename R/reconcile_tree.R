#' Reconcile species names between a dataset and a phylogenetic tree
#'
#' Compares species names in a data frame against tip labels of a phylogenetic
#' tree. This is the core function for preparing data for phylogenetic
#' comparative methods (PGLS, phylogenetic mixed models, etc.).
#'
#' @param x A data frame.
#' @param tree An `ape::phylo` object, or a character(1) file path to a
#'   Newick or Nexus tree file.
#' @param x_species Character(1). Column name in `x` containing species names.
#'   Auto-detected if `NULL`.
#' @inheritParams reconcile_data
#'
#' @return A `reconciliation` object.
#'
#' @examples
#' \dontrun{
#' result <- reconcile_tree(my_data, "my_tree.nwk", x_species = "species")
#' print(result)
#' aligned <- reconcile_apply(result, drop_unresolved = TRUE)
#' }
#'
#' @export
reconcile_tree <- function(x, tree,
                           x_species = NULL,
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

  # Validate data frame
  if (!is.data.frame(x)) abort("`x` must be a data frame.", call = caller_env())

  if (!is.null(authority)) {
    authority <- tolower(authority)
    if (!authority %in% pr_valid_authorities()) {
      abort(
        c(
          paste0("Unknown authority: '", authority, "'."),
          "i" = paste0("Valid options: ",
                       paste(pr_valid_authorities(), collapse = ", "))
        ),
        call = caller_env()
      )
    }
  }

  # Detect species column
  if (is.null(x_species)) x_species <- pr_detect_species_column(x, "x_species")
  if (!x_species %in% names(x)) {
    abort(paste0("Column '", x_species, "' not found in `x`."),
          call = caller_env())
  }

  # Load tree
  tree_obj <- pr_load_tree(tree)
  tips <- tree_obj$tip.label

  names_x <- as.character(x[[x_species]])

  # Describe tree source for metadata
  tree_source <- if (is.character(tree)) {
    basename(tree)
  } else {
    sprintf("phylo (%d tips)", length(tips))
  }

  # Load overrides
  overrides_df <- pr_load_overrides(overrides)

  if (!quiet) {
    cli_alert_info(
      "Reconciling {length(unique(names_x))} data names vs {length(tips)} tree tips"
    )
  }

  # Run cascade
  mapping <- pr_run_cascade(
    names_x         = names_x,
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
    type             = "data_tree",
    timestamp        = Sys.time(),
    authority        = authority %||% "none",
    db_version       = db_version %||% "latest",
    fuzzy            = fuzzy,
    fuzzy_threshold  = if (fuzzy) fuzzy_threshold else NA_real_,
    fuzzy_method     = if (fuzzy) "component_levenshtein" else NA_character_,
    resolve          = resolve,
    prepR4pcm_version = as.character(utils::packageVersion("prepR4pcm")),
    x_source         = deparse(substitute(x)),
    y_source         = tree_source,
    rank             = rank
  )

  result <- new_reconciliation(mapping = mapping, meta = meta)

  if (!quiet) {
    n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
    n_total <- result$counts$n_x
    cli_alert_success("Matched {n_matched}/{n_total} data names to tree tips")
  }

  result
}
