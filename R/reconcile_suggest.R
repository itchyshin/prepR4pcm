# Suggest matches for unresolved species -----------------------------------

#' Suggest fuzzy matches for unresolved species
#'
#' For each unresolved species (present in x but not matched to y), finds the
#' `n` closest matches in source y using Levenshtein-based similarity. This
#' uses the same component-based approach as [pr_fuzzy_match()] (genus 0.6,
#' epithet 0.4) but returns the top-`n` candidates per unresolved name rather
#' than a single best match.
#'
#' Genus pre-filtering is applied to keep computation tractable for large
#' datasets: only names whose genus is within 2 edits are compared.
#'
#' @param x A `reconciliation` object.
#' @param n Integer. Maximum number of suggestions per unresolved species.
#'   Default 3.
#' @param threshold Numeric (0--1). Minimum similarity score to include a
#'   suggestion. Default 0.7.
#' @param quiet Logical. Suppress informational messages? Default `FALSE`.
#'
#' @return A tibble with columns: `unresolved`, `suggestion`, `score`, sorted
#'   by `unresolved` (ascending), then `score` (descending).
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#' suggestions <- reconcile_suggest(result, n = 2)
#' head(suggestions, 10)
#'
#' @export
reconcile_suggest <- function(x, n = 3, threshold = 0.7, quiet = FALSE) {

  validate_reconciliation(x)

  mapping <- x$mapping

  # Unresolved x-only names

  unresolved_rows <- mapping[mapping$match_type == "unresolved" &
                               mapping$in_x & !mapping$in_y, ]
  unresolved_names <- unique(unresolved_rows$name_x)
  unresolved_names <- unresolved_names[!is.na(unresolved_names)]

  # All y names (both matched and unresolved)
  y_names <- unique(mapping$name_y[!is.na(mapping$name_y)])

  # Early return if nothing to suggest
  if (length(unresolved_names) == 0) {
    if (!quiet) {
      cli_alert_info("No unresolved species found.")
    }
    return(tibble(
      unresolved = character(),
      suggestion = character(),
      score      = numeric()
    ))
  }

  if (length(y_names) == 0) {
    if (!quiet) {
      cli_alert_warning("No y names available for matching.")
    }
    return(tibble(
      unresolved = character(),
      suggestion = character(),
      score      = numeric()
    ))
  }

  # Normalise for comparison
  rank <- x$meta$rank %||% "species"
  norm_unresolved <- as.character(pr_normalize_names(unresolved_names, rank = rank))
  norm_y <- as.character(pr_normalize_names(y_names, rank = rank))

  # Parse into genus + epithet
  parse_binomial <- function(nms) {
    parts <- strsplit(nms, "\\s+")
    genus   <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
    epithet <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
    list(genus = genus, epithet = epithet)
  }

  px <- parse_binomial(norm_unresolved)
  py <- parse_binomial(norm_y)

  valid_y <- !is.na(py$genus) & !is.na(py$epithet)
  idx_y <- which(valid_y)

  # Build genus index for y names
  genus_y_groups <- split(idx_y, py$genus[idx_y])
  unique_genus_y <- names(genus_y_groups)

  # Compute genus distance matrix (unique unresolved genera x unique y genera)
  unique_genus_x <- unique(px$genus[!is.na(px$genus)])
  genus_dist_mat <- utils::adist(unique_genus_x, unique_genus_y)
  rownames(genus_dist_mat) <- unique_genus_x

  max_genus_edits <- 2L
  results <- list()

  for (i in seq_along(unresolved_names)) {
    gx <- px$genus[i]
    ex <- px$epithet[i]
    if (is.na(gx) || is.na(ex)) next

    # Find candidate y genera within max_genus_edits
    gx_row <- which(unique_genus_x == gx)
    if (length(gx_row) == 0) next

    close_genus_mask <- genus_dist_mat[gx_row, ] <= max_genus_edits
    candidate_genera <- unique_genus_y[close_genus_mask]
    if (length(candidate_genera) == 0) next

    cand_y <- unlist(genus_y_groups[candidate_genera], use.names = FALSE)
    if (length(cand_y) == 0) next

    # Vectorised distance computation
    genera_y  <- py$genus[cand_y]
    epithets_y <- py$epithet[cand_y]

    g_dists  <- utils::adist(gx, genera_y)[1, ]
    g_maxlen <- pmax(nchar(gx), nchar(genera_y))
    g_sim    <- ifelse(g_maxlen == 0, 1, 1 - g_dists / g_maxlen)

    ep_dists  <- utils::adist(ex, epithets_y)[1, ]
    ep_maxlen <- pmax(nchar(ex), nchar(epithets_y))
    ep_sim    <- ifelse(ep_maxlen == 0, 1, 1 - ep_dists / ep_maxlen)

    scores <- 0.6 * g_sim + 0.4 * ep_sim

    # Keep top n above threshold
    above <- which(scores >= threshold)
    if (length(above) == 0) next

    top_idx <- above[order(scores[above], decreasing = TRUE)]
    top_idx <- top_idx[seq_len(min(n, length(top_idx)))]

    results[[length(results) + 1]] <- tibble(
      unresolved = rep(unresolved_names[i], length(top_idx)),
      suggestion = y_names[cand_y[top_idx]],
      score      = round(scores[top_idx], 4)
    )
  }

  if (length(results) == 0) {
    if (!quiet) {
      cli_alert_info(
        "No suggestions above threshold {threshold} for {length(unresolved_names)} unresolved species."
      )
    }
    return(tibble(
      unresolved = character(),
      suggestion = character(),
      score      = numeric()
    ))
  }

  out <- do.call(rbind, results)

  # Sort by unresolved name (ascending), then score (descending)
  out <- out[order(out$unresolved, -out$score), ]
  rownames(out) <- NULL

  if (!quiet) {
    n_with_suggestions <- length(unique(out$unresolved))
    cli_alert_success(
      "Found suggestions for {n_with_suggestions} of {length(unresolved_names)} unresolved species."
    )
  }

  out
}
