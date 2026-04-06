# Batch overrides -----------------------------------------------------------

#' Apply multiple overrides to a reconciliation
#'
#' A convenience wrapper around [reconcile_override()] that applies several
#' overrides at once from a data frame or CSV file.
#'
#' @param x A `reconciliation` object.
#' @param overrides A data frame or a character(1) file path to a CSV.
#'   Required columns: `name_x`. Optional columns: `action` (one of
#'   `"accept"`, `"reject"`, `"replace"`; defaults to `"accept"` if absent),
#'   `name_y` (required when action is `"accept"` or `"replace"`), `note`.
#' @param quiet Logical. Suppress per-override messages? Default `FALSE`.
#'
#' @return An updated `reconciliation` object with all overrides applied.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#' # Create a batch of overrides
#' batch <- data.frame(
#'   name_x = reconcile_mapping(result)$name_x[
#'     reconcile_mapping(result)$match_type == "unresolved" &
#'     reconcile_mapping(result)$in_x][1:2],
#'   name_y = tree_jetz$tip.label[1:2],
#'   action = "accept",
#'   note = "Batch demo",
#'   stringsAsFactors = FALSE
#' )
#' batch <- batch[!is.na(batch$name_x), ]
#' if (nrow(batch) > 0) {
#'   result2 <- reconcile_override_batch(result, batch)
#' }
#'
#' @export
reconcile_override_batch <- function(x, overrides, quiet = FALSE) {

  validate_reconciliation(x)


  # --- Load overrides from CSV if needed ---
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

  # --- Validate columns ---
  if (!"name_x" %in% names(overrides)) {
    abort(
      c(
        "`overrides` must have a `name_x` column.",
        "i" = paste0("Columns found: ", paste(names(overrides), collapse = ", "))
      ),
      call = caller_env()
    )
  }

  # Default action to "accept" if column is absent
  if (!"action" %in% names(overrides)) {
    overrides$action <- "accept"
  }

  # Validate action values
  valid_actions <- c("accept", "reject", "replace")
  bad_actions <- setdiff(unique(overrides$action), valid_actions)
  if (length(bad_actions) > 0) {
    abort(
      c(
        "Invalid action values found.",
        "x" = paste0("Invalid: ", paste(bad_actions, collapse = ", ")),
        "i" = paste0("Must be one of: ", paste(valid_actions, collapse = ", "))
      ),
      call = caller_env()
    )
  }

  # Default note if absent
  if (!"note" %in% names(overrides)) {
    overrides$note <- ""
  }

  # Default name_y if absent
  if (!"name_y" %in% names(overrides)) {
    overrides$name_y <- NA_character_
  }

  # --- Apply overrides one by one ---
  n_applied <- 0

  for (i in seq_len(nrow(overrides))) {
    row <- overrides[i, ]

    name_y_val <- if (is.na(row$name_y)) NULL else row$name_y

    x <- reconcile_override(
      x,
      name_x = row$name_x,
      name_y = name_y_val,
      action = row$action,
      note   = row$note
    )

    n_applied <- n_applied + 1
  }

  if (!quiet) {
    cli_alert_success("Applied {n_applied} override{?s}.")
  }

  x
}
