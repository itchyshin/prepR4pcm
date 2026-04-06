#' Convert a taxonomy crosswalk to an overrides table
#'
#' Parses a curated taxonomy crosswalk (e.g., the BirdLife-BirdTree
#' crosswalk) into a data frame compatible with the `overrides` parameter
#' of [reconcile_tree()], [reconcile_data()], and other reconciliation
#' functions.
#'
#' This is useful when you have an authoritative mapping between two
#' naming systems and want to use it instead of (or in addition to)
#' automated synonym resolution.
#'
#' @param crosswalk A data frame or a file path to a CSV.
#' @param from_col Character(1). Column name for source names (e.g.,
#'   `"Species1"` for BirdLife names).
#' @param to_col Character(1). Column name for target names (e.g.,
#'   `"Species3"` for BirdTree names).
#' @param match_type_col Character(1) or NULL. Column containing the match
#'   type (e.g., `"1BL to 1BT"`, `"Many BL to 1BT"`). Used to annotate
#'   notes and filter.
#' @param notes_col Character(1) or NULL. Column containing additional
#'   notes.
#' @param one_to_one_only Logical. If `TRUE`, keeps only one-to-one
#'   matches (e.g., `"1BL to 1BT"`). Default `FALSE`.
#'
#' @return A data frame with columns `name_x`, `name_y`, and `user_note`,
#'   suitable for passing to the `overrides` parameter.
#'
#' @examples
#' data(crosswalk_birdlife_birdtree)
#' overrides <- reconcile_crosswalk(
#'   crosswalk_birdlife_birdtree,
#'   from_col = "Species1",
#'   to_col = "Species3",
#'   match_type_col = "Match.type"
#' )
#' head(overrides)
#'
#' @export
reconcile_crosswalk <- function(crosswalk,
                                 from_col,
                                 to_col,
                                 match_type_col = NULL,
                                 notes_col = NULL,
                                 one_to_one_only = FALSE) {

  # Load from file if needed
  if (is.character(crosswalk) && length(crosswalk) == 1) {
    if (!file.exists(crosswalk)) {
      abort(
        c("Crosswalk file not found.", "x" = paste0("Path: ", crosswalk)),
        call = caller_env()
      )
    }
    crosswalk <- utils::read.csv(crosswalk, stringsAsFactors = FALSE)
  }

  if (!is.data.frame(crosswalk)) {
    abort("`crosswalk` must be a data frame or a file path to a CSV.",
          call = caller_env())
  }

  # Validate columns
  if (!from_col %in% names(crosswalk)) {
    abort(
      c(paste0("Column '", from_col, "' not found in crosswalk."),
        "i" = paste0("Available: ", paste(names(crosswalk), collapse = ", "))),
      call = caller_env()
    )
  }
  if (!to_col %in% names(crosswalk)) {
    abort(
      c(paste0("Column '", to_col, "' not found in crosswalk."),
        "i" = paste0("Available: ", paste(names(crosswalk), collapse = ", "))),
      call = caller_env()
    )
  }

  # Build overrides
  result <- data.frame(
    name_x    = as.character(crosswalk[[from_col]]),
    name_y    = as.character(crosswalk[[to_col]]),
    stringsAsFactors = FALSE
  )

  # Build notes from match_type and notes columns
  note_parts <- rep("crosswalk", nrow(result))
  if (!is.null(match_type_col) && match_type_col %in% names(crosswalk)) {
    note_parts <- paste0("crosswalk [", crosswalk[[match_type_col]], "]")
  }
  if (!is.null(notes_col) && notes_col %in% names(crosswalk)) {
    extra <- as.character(crosswalk[[notes_col]])
    has_extra <- !is.na(extra) & nchar(extra) > 0
    note_parts[has_extra] <- paste0(note_parts[has_extra], ": ", extra[has_extra])
  }
  result$user_note <- note_parts

  # Remove rows where from or to is NA/empty
  keep <- !is.na(result$name_x) & nchar(result$name_x) > 0 &
    !is.na(result$name_y) & nchar(result$name_y) > 0
  result <- result[keep, ]

  # Filter to one-to-one if requested
  if (one_to_one_only && !is.null(match_type_col) &&
      match_type_col %in% names(crosswalk)) {
    types <- crosswalk[[match_type_col]][keep]
    # Identify 1:1 matches — match type contains "1" on both sides
    # Common patterns: "1BL to 1BT", "1-to-1", etc.
    is_1to1 <- grepl("^1.*to.*1", types, ignore.case = TRUE)
    n_removed <- sum(!is_1to1)
    if (n_removed > 0) {
      cli_alert_warning(
        "Removed {n_removed} non-one-to-one entries from crosswalk"
      )
    }
    result <- result[is_1to1, ]
  }

  # Warn about many-to-one and one-to-many
  if (!is.null(match_type_col) && match_type_col %in% names(crosswalk)) {
    types <- crosswalk[[match_type_col]][keep]
    n_m2o <- sum(grepl("Many.*to.*1", types, ignore.case = TRUE))
    n_o2m <- sum(grepl("1.*to.*many", types, ignore.case = TRUE))
    if (n_m2o > 0) {
      cli_alert_info("{n_m2o} many-to-one entries (lumps) included")
    }
    if (n_o2m > 0) {
      cli_alert_info("{n_o2m} one-to-many entries (splits) included")
    }
  }

  # Remove identical name pairs (no reconciliation needed)
  different <- result$name_x != result$name_y
  n_same <- sum(!different)
  result_filtered <- result[different, ]

  cli_alert_success(
    "Crosswalk: {nrow(result_filtered)} overrides ({n_same} identical pairs skipped)"
  )

  rownames(result_filtered) <- NULL
  result_filtered
}
