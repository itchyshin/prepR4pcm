# Interactive review of reconciliation matches ------------------------------

#' Interactively review reconciliation matches
#'
#' Presents matches one at a time for manual accept/reject decisions in
#' an interactive R session. Each accepted or rejected match is applied
#' via [reconcile_override()], updating the reconciliation object
#' in place. Useful for auditing fuzzy or flagged matches in the console
#' or RStudio.
#'
#' @param reconciliation A [reconciliation] object returned by
#'   [reconcile_tree()], [reconcile_data()], or a related matcher.
#' @param type A length-1 character vector. Which matches to review:
#'   \describe{
#'     \item{`"flagged"`}{Flagged matches and pending candidate pairs for
#'       unresolved names (default).}
#'     \item{`"fuzzy"`}{Fuzzy and flagged matches, then pending candidates.}
#'     \item{`"all_unresolved"`}{All unresolved species.}
#'     \item{`"identity_candidates"`}{Pairs withheld by the identity guard.}
#'   }
#' @param suggest Logical. If `TRUE` and `type = "all_unresolved"`, show
#'   the closest fuzzy candidate (if any) alongside unresolved names.
#'   Default `TRUE`.
#' @param quiet Logical. If `TRUE`, suppress the end-of-review summary.
#'   Default `FALSE`.
#'
#' @return An updated [reconciliation] object reflecting accepted and
#'   rejected decisions.
#'
#' @family reconciliation functions
#' @seealso [reconcile_override()] and [reconcile_override_batch()] for
#'   non-interactive corrections; [reconcile_suggest()] for shortlisting
#'   unresolved species before review.
#'
#' @details
#' This function requires an interactive session. In non-interactive
#' contexts (e.g., scripts, CI), it warns and returns `reconciliation`
#' unchanged.
#'
#' At each prompt the user may enter:
#' \describe{
#'   \item{`a`}{Accept the proposed match (calls [reconcile_override()]
#'     with `action = "accept"`).}
#'   \item{`r`}{Reject the match (calls [reconcile_override()] with
#'     `action = "reject"`).}
#'   \item{`s`}{Skip -- move to the next item without changes.}
#'   \item{`q`}{Quit -- return the current state immediately.}
#' }
#'
#' @examples
#' if (interactive()) {
#'   # Interactive review in RStudio console:
#'   result <- reconcile_review(result, type = "flagged")
#' }
#'
#' @export
reconcile_review <- function(reconciliation,
                             type = c("flagged", "fuzzy", "all_unresolved",
                                      "identity_candidates"),
                             suggest = TRUE, quiet = FALSE) {

  validate_reconciliation(reconciliation)
  type <- match.arg(type)

  if (!interactive()) {
    warn(
      paste0(
        "`reconcile_review()` requires an interactive session. ",
        "Returning `reconciliation` unchanged."
      )
    )
    return(reconciliation)
  }

  review_candidates <- function(reconciliation, available_only = TRUE) {
    # Initialise the stored table before an override updates one row. Otherwise
    # partial column creation would leave untouched legacy alternatives as NA.
    if (!is.null(reconciliation$candidates) && nrow(reconciliation$candidates)) {
      if (!"review_status" %in% names(reconciliation$candidates)) {
        reconciliation$candidates$review_status <- "pending"
      }
      if (!"review_note" %in% names(reconciliation$candidates)) {
        reconciliation$candidates$review_note <- NA_character_
      }
    }
    candidates <- if (available_only) {
      .pr_pending_review_candidates(reconciliation)
    } else reconciliation$candidates
    if (is.null(candidates) || !nrow(candidates)) return(reconciliation)
    if (!"review_status" %in% names(candidates)) {
      candidates$review_status <- "pending"
    }
    review_idx <- which(candidates$review_status == "pending")
    if (!length(review_idx)) {
      cli_alert_info("No pending identity candidates to review.")
      return(reconciliation)
    }
    cli_h1("Interactive identity review: {length(review_idx)} candidate pairs")
    accepted <- rejected <- skipped <- 0L
    current <- candidates
    for (i in review_idx) {
      candidate <- candidates[i, , drop = FALSE]
      if (available_only) {
        if (!any(current$name_x == candidate$name_x &
                 current$name_y == candidate$name_y)) next
      }
      cat(sprintf(
        "\n[%d/%d] %s -> %s [%s; %s / %s; %s@%s]\n",
        match(i, review_idx), length(review_idx), candidate$name_x,
        candidate$name_y, candidate$reason, candidate$x_id, candidate$y_id,
        candidate$provider, candidate$version
      ))
      if ("evidence_detail" %in% names(candidate) &&
          !is.na(candidate$evidence_detail)) {
        cat("  Evidence: ", candidate$evidence_detail, "\n", sep = "")
      }
      target_taken <- any(reconciliation$mapping$in_x &
        reconciliation$mapping$in_y &
        reconciliation$mapping$name_y == candidate$name_y &
        reconciliation$mapping$name_x != candidate$name_x, na.rm = TRUE)
      if (target_taken) {
        cat("  Target already assigned to another input name; resolve that assignment before accepting this pair.\n")
      }
      response <- ""
      while (!response %in% c("a", "r", "s", "q")) {
        response <- tolower(trimws(readline(
          "  Accept pair [a], Reject pair [r], Skip [s], Quit [q]: "
        )))
      }
      if (response == "q") break
      if (response == "a") {
        if (target_taken) {
          cat("  Cannot accept: target already assigned. Skipping this pair.\n")
          skipped <- skipped + 1L
          next
        }
        note <- paste0("Accepted after identity review; evidence: ", candidate$reason,
                       " (", candidate$x_id, " vs ", candidate$y_id, ")")
        reconciliation <- reconcile_override(
          reconciliation, candidate$name_x, candidate$name_y,
          action = "accept", note = note
        )
        accepted <- accepted + 1L
      } else if (response == "r") {
        note <- paste0("Rejected identity candidate: ", candidate$reason)
        reconciliation <- reconcile_override(
          reconciliation, candidate$name_x, candidate$name_y,
          action = "reject", note = note
        )
        rejected <- rejected + 1L
      } else {
        skipped <- skipped + 1L
      }
      if (available_only && response %in% c("a", "r")) {
        current <- .pr_pending_review_candidates(reconciliation)
      }
    }
    if (!quiet) {
      cli_alert_success("Identity review: {accepted} accepted, {rejected} rejected, {skipped} skipped")
    }
    return(reconciliation)
  }

  if (type == "identity_candidates") {
    return(review_candidates(reconciliation, available_only = FALSE))
  }

  mapping <- reconciliation$mapping

  # Select rows to review based on type
  if (type == "flagged") {
    review_idx <- which(mapping$match_type == "flagged")
  } else if (type == "fuzzy") {
    review_idx <- which(mapping$match_type %in% c("fuzzy", "flagged"))
  } else {
    # all_unresolved
    review_idx <- which(
      mapping$match_type %in% c("unresolved", "flagged") & mapping$in_x
    )
  }

  review_names <- mapping$name_x[review_idx]
  n_total <- length(review_names)
  if (n_total == 0) {
    if (!nrow(.pr_pending_review_candidates(reconciliation))) {
      cli_alert_info("No matches to review for type = '{type}'.")
      return(reconciliation)
    }
    return(review_candidates(reconciliation))
  }

  cli_h1("Interactive review ({type}): {n_total} items")

  n_accepted <- 0L
  n_rejected <- 0L
  n_skipped  <- 0L

  for (i in seq_len(n_total)) {
    # Re-read mapping each iteration because overrides may mutate it
    mapping <- reconciliation$mapping
    idx <- which(!is.na(mapping$name_x) & mapping$in_x &
                   mapping$name_x == review_names[i])
    row <- mapping[idx[1], ]

    name_x     <- row$name_x
    name_y     <- row$name_y
    score      <- row$match_score
    match_type <- row$match_type

    # Format the prompt line
    score_str <- if (is.na(score)) "NA" else sprintf("%.2f", score)
    target_str <- if (is.na(name_y) || nchar(name_y) == 0) "<none>" else name_y

    cat(sprintf(
      '\n[%d/%d] "%s" -> "%s" (score: %s, %s)\n',
      i, n_total, name_x, target_str, score_str, match_type
    ))

    # For unresolved items with suggest = TRUE, show suggestion hint
    if (suggest && match_type == "unresolved" && (is.na(name_y) || nchar(name_y) == 0)) {
      cat("  (No current match target)\n")
    }

    response <- ""
    while (!response %in% c("a", "r", "s", "q")) {
      response <- tolower(trimws(readline("  Accept [a], Reject [r], Skip [s], Quit [q]: ")))
    }

    if (response == "q") {
      cli_alert_info("Quit early at item {i}/{n_total}.")
      return(reconciliation)
    } else if (response == "a") {
      if (!is.na(name_y) && nchar(name_y) > 0) {
        reconciliation <- reconcile_override(
          reconciliation, name_x = name_x, name_y = name_y,
          action = "accept",
          note = "Accepted during interactive review"
        )
        n_accepted <- n_accepted + 1L
      } else {
        cat("  Cannot accept: no target name. Skipping.\n")
        n_skipped <- n_skipped + 1L
      }
    } else if (response == "r") {
      reconciliation <- reconcile_override(
        reconciliation, name_x = name_x,
        action = "reject",
        note = "Rejected during interactive review"
      )
      n_rejected <- n_rejected + 1L
    } else {
      n_skipped <- n_skipped + 1L
    }
  }

  if (!quiet) {
    cli_alert_success(
      "Reviewed {n_total} matches: {n_accepted} accepted, {n_rejected} rejected, {n_skipped} skipped"
    )
  }

  review_candidates(reconciliation)
}
