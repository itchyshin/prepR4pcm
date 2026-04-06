#' Export reconciled data and tree to files
#'
#' Applies a reconciliation and writes the aligned dataset, pruned tree,
#' and full mapping table to disk. This produces publication-ready files
#' with a documented provenance trail.
#'
#' @param x A `reconciliation` object.
#' @param data A data frame to align. If `NULL`, only the tree and mapping
#'   are written.
#' @param tree An `ape::phylo` object or file path. If `NULL`, only the
#'   data and mapping are written.
#' @param species_col Character(1). Column name in `data` containing
#'   species names. Auto-detected if `NULL`.
#' @param dir Character(1). Output directory. Created if it does not exist.
#'   Default `"."`.
#' @param prefix Character(1). File name prefix. Default `"reconciled"`.
#' @param tree_format Character(1). Tree output format: `"nexus"` (default)
#'   or `"newick"`.
#' @param drop_unresolved Logical. Drop unresolved species? Default `TRUE`.
#'
#' @return A named list of file paths (invisibly):
#'   `$data`, `$tree`, `$mapping`.
#'
#' @examples
#' \dontrun{
#' result <- reconcile_tree(my_data, my_tree)
#' files <- reconcile_export(result, data = my_data, tree = my_tree,
#'                            dir = "output", prefix = "pgls_ready")
#' files$data     # "output/pgls_ready_data.csv"
#' files$tree     # "output/pgls_ready_tree.nex"
#' files$mapping  # "output/pgls_ready_mapping.csv"
#' }
#'
#' @export
reconcile_export <- function(x, data = NULL, tree = NULL,
                              species_col = NULL,
                              dir = ".",
                              prefix = "reconciled",
                              tree_format = c("nexus", "newick"),
                              drop_unresolved = TRUE) {

  validate_reconciliation(x)
  tree_format <- match.arg(tree_format)

  # Create output directory
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  # Apply the reconciliation
  aligned <- reconcile_apply(
    x,
    data = data,
    tree = tree,
    species_col = species_col,
    drop_unresolved = drop_unresolved
  )

  paths <- list(data = NULL, tree = NULL, mapping = NULL)

  # Write aligned data
  if (!is.null(aligned$data)) {
    path_data <- file.path(dir, paste0(prefix, "_data.csv"))
    utils::write.csv(aligned$data, path_data, row.names = FALSE)
    paths$data <- path_data
    cli_alert_success("Data written to {.path {path_data}}")
  }

  # Write aligned tree
  if (!is.null(aligned$tree)) {
    ext <- if (tree_format == "nexus") ".nex" else ".nwk"
    path_tree <- file.path(dir, paste0(prefix, "_tree", ext))

    if (tree_format == "nexus") {
      ape::write.nexus(aligned$tree, file = path_tree)
    } else {
      ape::write.tree(aligned$tree, file = path_tree)
    }
    paths$tree <- path_tree
    cli_alert_success("Tree written to {.path {path_tree}}")
  }

  # Write mapping table
  path_mapping <- file.path(dir, paste0(prefix, "_mapping.csv"))
  utils::write.csv(
    as.data.frame(x$mapping),
    path_mapping,
    row.names = FALSE
  )
  paths$mapping <- path_mapping
  cli_alert_success("Mapping written to {.path {path_mapping}}")

  invisible(paths)
}
