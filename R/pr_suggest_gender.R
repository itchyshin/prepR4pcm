#' Suggest reference names with different grammatical endings
#'
#' Find possible specific-epithet gender variants in a supplied reference.
#' This is a review tool, not a taxonomic correction or an additional automatic
#' matching stage. Names already present after normalisation are omitted.
#'
#' @details
#' Common ending groups `-us/-a/-um` and `-is/-e` are compared only within
#' the same genus. At subspecies rank, the third epithet must also agree
#' exactly; optional `subsp.` / `ssp.` markers are accepted and only the
#' specific epithet is varied. All candidate reference
#' spellings are retained, including ambiguous alternatives.
#'
#' Endings alone cannot distinguish an adjective from a noun, establish genus
#' gender, or prove that two names identify the same species. Nouns in apposition
#' and genitives need not change endings. Review every suggestion against an
#' authority before using [reconcile_override()] or [reconcile_override_batch()].
#' [pr_normalize_names()] deliberately preserves these spelling differences.
#'
#' The implementation generates at most two alternatives per query and joins
#' them to the reference; it does not construct a pairwise distance matrix.
#'
#' @param names Character vector of query names. Missing names are ignored.
#' @param reference Character vector of candidate reference names. Original
#'   spellings are returned. Missing names and duplicate spellings are ignored.
#' @param rank Normalisation rank, `"species"` or `"subspecies"`.
#' @return A tibble with `input`, `suggestion`, `ending_group`, `ambiguous`
#'   (more than one reference spelling for that query), and `requires_review`
#'   (always `TRUE`). No input vector or reconciliation is changed.
#' @references International Code of Nomenclature, Article 23.5:
#'   <https://www.iapt-taxon.org/nomen/pages/main/art_23.html>.
#' @family name utilities
#' @examples
#' candidates <- pr_suggest_gender("Plant albus", c("Plant alba", "Plant album"))
#' candidates
#' # Check candidates against a taxonomic authority before accepting a pair.
#' @export
pr_suggest_gender <- function(names, reference, rank = c("species", "subspecies")) {
  rank <- match.arg(rank)
  if (!is.character(names) || !is.character(reference)) {
    cli::cli_abort("{.arg names} and {.arg reference} must be character vectors.")
  }
  empty <- tibble::tibble(input = character(), suggestion = character(),
                         ending_group = character(), ambiguous = logical(),
                         requires_review = logical())
  names <- unique(names[!is.na(names)])
  reference <- unique(reference[!is.na(reference)])
  if (!length(names) || !length(reference)) return(empty)
  nx <- as.character(pr_normalize_names(names, rank = rank))
  ny <- as.character(pr_normalize_names(reference, rank = rank))
  if (rank == "subspecies") {
    # Normalisation canonicalises ssp. to subsp.; marked and unmarked
    # trinomials describe the same comparison shape. Keep the final epithet.
    marker <- "^([^ ]+ [^ ]+) subsp\\. ([^ ]+)$"
    nx <- sub(marker, "\\1 \\2", nx)
    ny <- sub(marker, "\\1 \\2", ny)
  }
  parts <- strsplit(nx, " ", fixed = TRUE)
  valid <- lengths(parts) %in% c(2L, 3L) & !nx %in% ny
  idx <- which(valid)
  if (!length(idx)) return(empty)
  genus <- vapply(parts[idx], `[`, character(1), 1L)
  epithet <- vapply(parts[idx], `[`, character(1), 2L)
  tail <- vapply(parts[idx], function(p) {
    if (length(p) == 3L) paste0(" ", p[3L]) else ""
  }, character(1))
  candidates <- list()
  groups <- list(c("us", "a", "um"), c("is", "e"))
  for (endings in groups) {
    pattern <- paste0("(", paste(endings, collapse = "|"), ")$")
    eligible <- which(grepl(pattern, epithet))
    stem <- sub(pattern, "", epithet[eligible])
    eligible <- eligible[nzchar(stem)]
    if (!length(eligible)) next
    stem <- sub(pattern, "", epithet[eligible])
    for (ending in endings) {
      candidate <- paste0(genus[eligible], " ", stem, ending, tail[eligible])
      keep <- candidate != nx[idx[eligible]]
      candidates[[length(candidates) + 1L]] <- data.frame(
        query_id = idx[eligible][keep], candidate = candidate[keep],
        ending_group = rep(paste(endings, collapse = "/"), sum(keep)))
    }
  }
  if (!length(candidates)) return(empty)
  candidates <- do.call(rbind, candidates)
  hits <- merge(candidates,
                data.frame(candidate = ny, reference_id = seq_along(reference)),
                by = "candidate", sort = FALSE)
  if (!nrow(hits)) return(empty)
  hits <- hits[order(hits$query_id, hits$reference_id), , drop = FALSE]
  counts <- tabulate(hits$query_id, nbins = length(names))
  tibble::tibble(input = names[hits$query_id],
                 suggestion = reference[hits$reference_id],
                 ending_group = hits$ending_group,
                 ambiguous = counts[hits$query_id] > 1L,
                 requires_review = TRUE)
}
