# Compare two reconciliation objects ----------------------------------------

#' Compare two reconciliation objects
#'
#' Identifies what changed between two reconciliation runs by comparing
#' their mapping tables on `name_x`. Reports species that gained or lost
#' a match, species whose match type changed, and species whose match
#' target changed.
#'
#' @param x A `reconciliation` object (the "before" state).
#' @param y A `reconciliation` object (the "after" state).
#' @param quiet Logical. If `TRUE`, suppress console output. Default `FALSE`.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{gained}{Tibble of species matched in `y` but unresolved in `x`.}
#'     \item{lost}{Tibble of species matched in `x` but unresolved in `y`.}
#'     \item{type_changed}{Tibble of species whose `match_type` differs.}
#'     \item{target_changed}{Tibble of species whose `name_y` differs.}
#'     \item{summary}{A one-row tibble with counts: `n_gained`, `n_lost`,
#'       `n_type_changed`, `n_target_changed`, `n_shared`.}
#'   }
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' # Without crosswalk
#' r1 <- reconcile_tree(avonet_subset, tree_jetz,
#'                      x_species = "Species1", authority = NULL)
#' # With crosswalk overrides
#' data(crosswalk_birdlife_birdtree)
#' overrides <- reconcile_crosswalk(crosswalk_birdlife_birdtree,
#'                                   from_col = "Species1", to_col = "Species3",
#'                                   match_type_col = "Match.type")
#' r2 <- reconcile_tree(avonet_subset, tree_jetz,
#'                      x_species = "Species1", authority = NULL,
#'                      overrides = overrides)
#' d <- reconcile_diff(r1, r2)
#' cat("Gained:", nrow(d$gained), "| Lost:", nrow(d$lost), "\n")
#'
#' @export
reconcile_diff <- function(x, y, quiet = FALSE) {

  validate_reconciliation(x)

  validate_reconciliation(y)

  map_x <- x$mapping
  map_y <- y$mapping


  # Keep only species from source x (in_x == TRUE)
  mx <- map_x[map_x$in_x & !is.na(map_x$name_x), ]
  my <- map_y[map_y$in_x & !is.na(map_y$name_x), ]

  # Deduplicate by name_x (keep first occurrence)
  mx <- mx[!duplicated(mx$name_x), ]
  my <- my[!duplicated(my$name_x), ]

  # Identify shared species

  shared_names <- intersect(mx$name_x, my$name_x)

  mx_shared <- mx[match(shared_names, mx$name_x), ]
  my_shared <- my[match(shared_names, my$name_x), ]

  resolved_types <- c("exact", "normalized", "synonym", "fuzzy",
                       "manual", "augmented")

  matched_x <- mx_shared$match_type %in% resolved_types
  matched_y <- my_shared$match_type %in% resolved_types

  # Gained: unresolved/flagged in x, resolved in y
  gained_idx <- !matched_x & matched_y
  gained <- tibble(
    name_x         = my_shared$name_x[gained_idx],
    name_y_new     = my_shared$name_y[gained_idx],
    match_type_old = mx_shared$match_type[gained_idx],
    match_type_new = my_shared$match_type[gained_idx],
    match_score    = my_shared$match_score[gained_idx]
  )

  # Lost: resolved in x, unresolved/flagged in y
  lost_idx <- matched_x & !matched_y
  lost <- tibble(
    name_x         = mx_shared$name_x[lost_idx],
    name_y_old     = mx_shared$name_y[lost_idx],
    match_type_old = mx_shared$match_type[lost_idx],
    match_type_new = my_shared$match_type[lost_idx]
  )

  # Type changed: both resolved but different match_type
  both_matched <- matched_x & matched_y
  type_diff <- mx_shared$match_type != my_shared$match_type & both_matched
  type_changed <- tibble(
    name_x         = mx_shared$name_x[type_diff],
    match_type_old = mx_shared$match_type[type_diff],
    match_type_new = my_shared$match_type[type_diff],
    name_y         = my_shared$name_y[type_diff]
  )

  # Target changed: both resolved but different name_y
  name_y_x <- ifelse(is.na(mx_shared$name_y), "", mx_shared$name_y)
  name_y_y <- ifelse(is.na(my_shared$name_y), "", my_shared$name_y)
  target_diff <- name_y_x != name_y_y & both_matched
  target_changed <- tibble(
    name_x     = mx_shared$name_x[target_diff],
    name_y_old = mx_shared$name_y[target_diff],
    name_y_new = my_shared$name_y[target_diff]
  )

  # Summary
  summary_tbl <- tibble(
    n_gained         = nrow(gained),
    n_lost           = nrow(lost),
    n_type_changed   = nrow(type_changed),
    n_target_changed = nrow(target_changed),
    n_shared         = length(shared_names)
  )

  # Console output
  if (!quiet) {
    cli_h2("Reconciliation diff")
    cli_bullets(c(
      " " = "Shared species (in both x and y): {.val {length(shared_names)}}",
      "*" = "Gained matches:    {.val {nrow(gained)}}",
      "!" = "Lost matches:      {.val {nrow(lost)}}",
      "*" = "Type changed:      {.val {nrow(type_changed)}}",
      "*" = "Target changed:    {.val {nrow(target_changed)}}"
    ))

    if (nrow(gained) > 0) {
      n_show <- min(nrow(gained), 5)
      cli_h2("Gained (first {n_show})")
      for (i in seq_len(n_show)) {
        cli_alert_success(
          '"{gained$name_x[i]}" -> "{gained$name_y_new[i]}" ({gained$match_type_new[i]})'
        )
      }
      if (nrow(gained) > 5) {
        cli_alert_info("... and {nrow(gained) - 5} more")
      }
    }

    if (nrow(lost) > 0) {
      n_show <- min(nrow(lost), 5)
      cli_h2("Lost (first {n_show})")
      for (i in seq_len(n_show)) {
        cli_alert_warning(
          '"{lost$name_x[i]}" was "{lost$name_y_old[i]}" ({lost$match_type_old[i]})'
        )
      }
      if (nrow(lost) > 5) {
        cli_alert_info("... and {nrow(lost) - 5} more")
      }
    }
  }

  list(
    gained         = gained,
    lost           = lost,
    type_changed   = type_changed,
    target_changed = target_changed,
    summary        = summary_tbl
  )
}
