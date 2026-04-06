#' Merge two reconciled datasets
#'
#' After reconciling two datasets with [reconcile_data()], use this function
#' to join them into a single analysis-ready data frame. The reconciliation
#' mapping table provides the species-level join key, so names that differ
#' between the two datasets (due to formatting, synonyms, or typos) are
#' correctly linked.
#'
#' @param x A `reconciliation` object (typically from [reconcile_data()]).
#' @param data_x The first data frame (source x in the reconciliation).
#' @param data_y The second data frame (source y in the reconciliation).
#' @param species_col_x Character(1). Species column in `data_x`.
#'   Auto-detected if `NULL`.
#' @param species_col_y Character(1). Species column in `data_y`.
#'   Auto-detected if `NULL`.
#' @param how Character(1). Join type:
#'   - `"inner"` (default): keep only species matched in both datasets.
#'   - `"left"`: keep all species from `data_x`.
#'   - `"full"`: keep all species from both datasets.
#' @param suffix Character(2). Suffixes to disambiguate columns with the
#'   same name in both datasets. Default `c("_x", "_y")`.
#' @param drop_unresolved Logical. Drop unresolved species? Default `TRUE`.
#'   Only relevant for `how = "left"` or `how = "full"`.
#'
#' @return A data frame with a `species_resolved` column as the join key,
#'   plus all columns from both datasets (with suffixes where needed).
#'
#' @examples
#' data(avonet_subset)
#' data(nesttrait_subset)
#'
#' rec <- reconcile_data(avonet_subset, nesttrait_subset,
#'                       x_species = "Species1",
#'                       y_species = "Scientific_name",
#'                       authority = NULL, quiet = TRUE)
#'
#' merged <- reconcile_merge(rec, avonet_subset, nesttrait_subset,
#'                           species_col_x = "Species1",
#'                           species_col_y = "Scientific_name")
#' cat(sprintf("Merged: %d rows, %d cols\n", nrow(merged), ncol(merged)))
#' head(merged[, c("species_resolved", "Family1", "Common_name")])
#'
#' @export
reconcile_merge <- function(x, data_x, data_y,
                            species_col_x = NULL,
                            species_col_y = NULL,
                            how = c("inner", "left", "full"),
                            suffix = c("_x", "_y"),
                            drop_unresolved = TRUE) {

  validate_reconciliation(x)
  how <- match.arg(how)

  if (!is.data.frame(data_x)) abort("`data_x` must be a data frame.", call = caller_env())
  if (!is.data.frame(data_y)) abort("`data_y` must be a data frame.", call = caller_env())

  if (length(suffix) != 2) {
    abort("`suffix` must be a character vector of length 2.", call = caller_env())
  }

  # Detect species columns
  if (is.null(species_col_x)) {
    species_col_x <- pr_detect_species_column(data_x, "species_col_x")
  }
  if (is.null(species_col_y)) {
    species_col_y <- pr_detect_species_column(data_y, "species_col_y")
  }

  if (!species_col_x %in% names(data_x)) {
    abort(paste0("Column '", species_col_x, "' not found in `data_x`."),
          call = caller_env())
  }
  if (!species_col_y %in% names(data_y)) {
    abort(paste0("Column '", species_col_y, "' not found in `data_y`."),
          call = caller_env())
  }

  mapping <- x$mapping

  # Build the join key from the mapping table
  matched <- mapping[mapping$in_x & mapping$in_y, ]

  if (drop_unresolved) {
    matched <- matched[matched$match_type != "unresolved", ]
  }

  # Create a lookup: name_x -> resolved name (use name_y as the resolved key)
  join_key <- data.frame(
    name_x           = matched$name_x,
    name_y           = matched$name_y,
    species_resolved = ifelse(!is.na(matched$name_resolved),
                              matched$name_resolved,
                              matched$name_y),
    stringsAsFactors = FALSE
  )

  # Attach the join key to each dataset
  data_x$..join_key.. <- join_key$species_resolved[
    match(as.character(data_x[[species_col_x]]), join_key$name_x)
  ]

  data_y$..join_key.. <- join_key$species_resolved[
    match(as.character(data_y[[species_col_y]]), join_key$name_y)
  ]

  # Handle duplicate column names with suffixes
  common_cols <- intersect(names(data_x), names(data_y))
  common_cols <- setdiff(common_cols, "..join_key..")

  if (length(common_cols) > 0) {
    for (col in common_cols) {
      names(data_x)[names(data_x) == col] <- paste0(col, suffix[1])
      names(data_y)[names(data_y) == col] <- paste0(col, suffix[2])
    }
  }

  # Perform the merge
  all_x <- how %in% c("left", "full")
  all_y <- how == "full"

  merged <- merge(data_x, data_y,
                  by = "..join_key..",
                  all.x = all_x,
                  all.y = all_y)

  # Rename join key to species_resolved
  names(merged)[names(merged) == "..join_key.."] <- "species_resolved"

  # Drop rows with NA join key if inner join
  if (how == "inner") {
    merged <- merged[!is.na(merged$species_resolved), , drop = FALSE]
  }

  # Reorder: species_resolved first
  col_order <- c("species_resolved",
                 setdiff(names(merged), "species_resolved"))
  merged <- merged[, col_order, drop = FALSE]

  n_merged <- sum(!is.na(merged$species_resolved))
  cli_alert_success("Merged {n_merged} species ({how} join)")

  merged
}
