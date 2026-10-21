#' Extract candidates withheld by the taxonomic identity guard
#'
#' @param reconciliation A `reconciliation` object.
#' @return A tibble with candidate names, the evidence status, provider/version,
#'   IDs when available, and the cascade stage that considered the pair.
#' @export
reconcile_candidates <- function(reconciliation) {
  validate_reconciliation(reconciliation)
  reconciliation$candidates
}

# Pending pairs that can be reviewed without replacing an existing match.
# Keep these separate from the mapping: merely displaying a suggestion must
# never assign a target or make include_flagged apply it.
.pr_pending_review_candidates <- function(reconciliation) {
  candidates <- reconciliation$candidates
  if (is.null(candidates) || !nrow(candidates)) return(tibble::tibble())
  pending <- if ("review_status" %in% names(candidates)) {
    candidates$review_status %in% "pending"
  } else rep(TRUE, nrow(candidates))
  mapping <- reconciliation$mapping
  available_x <- mapping$name_x[mapping$in_x &
    mapping$match_type == "unresolved" & !is.na(mapping$name_x)]
  candidates <- candidates[pending & candidates$name_x %in% available_x, , drop = FALSE]
  taken <- mapping[mapping$in_x & mapping$in_y & !is.na(mapping$name_y), , drop = FALSE]
  assigned <- match(candidates$name_y, taken$name_y)
  candidates$target_in_use <- !is.na(assigned)
  candidates$target_assigned_to <- taken$name_x[assigned]
  candidates
}
