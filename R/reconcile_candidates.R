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
