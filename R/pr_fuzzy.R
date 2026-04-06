# Fuzzy matching -----------------------------------------------------------

#' Fuzzy-match two sets of species names
#'
#' Uses component-based similarity: the genus and epithet are matched
#' separately, then combined with weights (genus 0.6, epithet 0.4) to
#' reflect that genus-level errors are more informative. Uses base R
#' `utils::adist()` for Levenshtein distance — no extra dependencies.
#'
#' @param names_x Character vector.
#' @param names_y Character vector.
#' @param threshold Numeric (0--1). Minimum similarity score. Default 0.9.
#' @param rank Character. `"species"` or `"subspecies"`.
#'
#' @return A tibble with columns: `name_x`, `name_y`, `score`, `notes`.
#' @keywords internal
pr_fuzzy_match <- function(names_x, names_y, threshold = 0.9,
                            rank = "species") {

  # Normalise for comparison
  norm_x <- as.character(pr_normalize_names(names_x, rank = rank))
  norm_y <- as.character(pr_normalize_names(names_y, rank = rank))

  results <- list()

  for (i in seq_along(norm_x)) {
    nx <- norm_x[i]
    if (is.na(nx) || nchar(nx) == 0) next

    parts_x <- strsplit(nx, "\\s+")[[1]]
    if (length(parts_x) < 2) next

    genus_x <- parts_x[1]
    epithet_x <- parts_x[2]

    best_score <- -1
    best_j <- NA_integer_

    for (j in seq_along(norm_y)) {
      ny <- norm_y[j]
      if (is.na(ny) || nchar(ny) == 0) next

      parts_y <- strsplit(ny, "\\s+")[[1]]
      if (length(parts_y) < 2) next

      genus_y <- parts_y[1]
      epithet_y <- parts_y[2]

      # Levenshtein similarity: 1 - (edit_distance / max_length)
      genus_dist <- utils::adist(genus_x, genus_y)[1, 1]
      genus_max <- max(nchar(genus_x), nchar(genus_y))
      genus_sim <- if (genus_max == 0) 1 else 1 - genus_dist / genus_max

      epithet_dist <- utils::adist(epithet_x, epithet_y)[1, 1]
      epithet_max <- max(nchar(epithet_x), nchar(epithet_y))
      epithet_sim <- if (epithet_max == 0) 1 else 1 - epithet_dist / epithet_max

      # Weighted score: genus more important than epithet
      score <- 0.6 * genus_sim + 0.4 * epithet_sim

      if (score > best_score) {
        best_score <- score
        best_j <- j
      }
    }

    if (!is.na(best_j) && best_score >= threshold) {
      results[[length(results) + 1]] <- tibble(
        name_x = names_x[i],
        name_y = names_y[best_j],
        score  = round(best_score, 4),
        notes  = sprintf("Fuzzy match (score %.3f): '%s' ~ '%s'",
                          best_score, norm_x[i], norm_y[best_j])
      )
    }
  }

  if (length(results) == 0) {
    return(tibble(
      name_x = character(),
      name_y = character(),
      score  = numeric(),
      notes  = character()
    ))
  }

  do.call(rbind, results)
}
