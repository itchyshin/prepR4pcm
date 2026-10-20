# Fuzzy matching -----------------------------------------------------------

#' Fuzzy-match two sets of species names
#'
#' Uses component-based similarity: the genus and epithet are matched
#' separately, then combined with weights (genus 0.6, epithet 0.4) to
#' reflect that genus-level errors are more informative. Uses base R
#' `utils::adist()` for Levenshtein distance — no extra dependencies.
#'
#' Genus pre-filtering is applied: only names whose genus is within 2
#' edits of each other are compared. This reduces the number of pairwise
#' comparisons dramatically for large datasets.
#'
#' @param names_x Character vector.
#' @param names_y Character vector.
#' @param threshold Numeric (0--1). Minimum similarity score. Default 0.9.
#' @param rank Character. `"species"` or `"subspecies"`.
#'
#' @param quiet Logical. Suppress progress messages.
#' @return A tibble with columns: `name_x`, `name_y`, `score`, `notes`.
#' @keywords internal
pr_fuzzy_match <- function(names_x, names_y, threshold = 0.9,
                            rank = "species", quiet = FALSE,
                            identity_x = NULL, identity_y = NULL) {

  if (!quiet && length(names_x) >= 1000L) {
    cli::cli_inform("Preparing fuzzy matching for {length(names_x)} names against {length(names_y)} reference names.")
  }

  # Normalise for comparison
  norm_x <- as.character(pr_normalize_names(names_x, rank = rank))
  norm_y <- as.character(pr_normalize_names(names_y, rank = rank))

  # Parse into genus + epithet
  parse_binomial <- function(nms) {
    parts <- strsplit(nms, "\\s+")
    genus   <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
    epithet <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
    list(genus = genus, epithet = epithet)
  }

  px <- parse_binomial(norm_x)
  py <- parse_binomial(norm_y)

  # Filter out entries without valid binomials
  valid_x <- !is.na(px$genus) & !is.na(px$epithet)
  valid_y <- !is.na(py$genus) & !is.na(py$epithet)

  n_skip_x <- sum(!valid_x)
  n_skip_y <- sum(!valid_y)
  if (n_skip_x > 0 || n_skip_y > 0) {
    cli_alert_warning(
      paste0(
        "{n_skip_x} non-binomial name{?s} in x and {n_skip_y} in y ",
        "excluded from fuzzy matching."
      )
    )
  }

  idx_x <- which(valid_x)
  idx_y <- which(valid_y)

  identity_guard <- !is.null(identity_x) || !is.null(identity_y)
  if (identity_guard && (is.null(identity_x) || is.null(identity_y) ||
                         nrow(identity_x) != length(names_x) ||
                         nrow(identity_y) != length(names_y))) {
    cli::cli_abort("Fuzzy identity evidence must contain one aligned row for every input name.")
  }

  if (length(idx_x) == 0 || length(idx_y) == 0) {
    return(tibble(
      name_x = character(),
      name_y = character(),
      score  = numeric(),
      notes  = character()
    ))
  }

  # Build genus-level index: for each genus in y, store indices
  genus_y_groups <- split(idx_y, py$genus[idx_y])

  # Work one query genus at a time: never allocate the Cartesian genus matrix.
  # agrep is a conservative substring prefilter; adist below validates the
  # whole-string edit distance, preserving the original <= 2 rule.
  unique_genus_x <- unique(px$genus[idx_x])
  unique_genus_y <- names(genus_y_groups)
  groups_x <- split(idx_x, factor(px$genus[idx_x], levels = unique_genus_x))
  results <- vector("list", length(idx_x))
  result_count <- 0L
  blocked <- list()
  blocked_count <- 0L
  completed <- 0L
  started <- proc.time()[["elapsed"]]
  last_progress <- started
  show_progress <- !quiet && length(idx_x) >= 1000L
  if (show_progress) {
    cli::cli_inform("Fuzzy matching {length(idx_x)} names against {length(idx_y)} names ({length(unique_genus_x)} query genera).")
    progress_id <- cli::cli_progress_bar(
      name = "Fuzzy matching", total = length(idx_x),
      format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} ETA {cli::pb_eta}",
      clear = FALSE)
    on.exit(cli::cli_progress_done(id = progress_id), add = TRUE)
  }

  for (gx in unique_genus_x) {
    possible <- which(abs(nchar(unique_genus_y) - nchar(gx)) <= 2L)
    close <- agrep(gx, unique_genus_y[possible], fixed = TRUE,
                   max.distance = list(all = 2L, insertions = 2L,
                                       deletions = 2L, substitutions = 2L))
    possible <- possible[close]
    gd <- as.numeric(utils::adist(gx, unique_genus_y[possible]))
    possible <- possible[gd <= 2L]
    gd <- gd[gd <= 2L]
    candidate_genera <- unique_genus_y[possible]
    cand_y <- unlist(genus_y_groups[candidate_genera], use.names = FALSE)
    # Keep genus-alphabetical / original-within-genus ordering for score ties.
    g_dist <- rep(gd, lengths(genus_y_groups[candidate_genera]))
    g_maxlen <- pmax(nchar(gx), nchar(py$genus[cand_y]))
    g_sim <- ifelse(g_maxlen == 0, 1, 1 - g_dist / g_maxlen)
    epithets_y <- py$epithet[cand_y]
    cache <- new.env(hash = TRUE, parent = emptyenv())
    for (i in groups_x[[gx]]) {
      ex <- px$epithet[i]
      cache_key <- paste0("epithet:", ex)
      if (length(cand_y)) {
        if (identity_guard) {
          ep_dists <- as.numeric(utils::adist(ex, epithets_y))
          ep_maxlen <- pmax(nchar(ex), nchar(epithets_y))
          ep_sim <- ifelse(ep_maxlen == 0, 1, 1 - ep_dists / ep_maxlen)
          scores <- 0.6 * g_sim + 0.4 * ep_sim
          known_x <- identity_x$identity_status[i] == "known"
          known_y <- identity_y$identity_status[cand_y] == "known"
          same_source <- identity_x$provider[i] == identity_y$provider[cand_y] &
            identity_x$version[i] == identity_y$version[cand_y] &
            identity_x$rank[i] == identity_y$rank[cand_y]
          same_id <- !is.na(identity_x$accepted_id[i]) &
            !is.na(identity_y$accepted_id[cand_y]) &
            identity_x$accepted_id[i] == identity_y$accepted_id[cand_y]
          eligible <- known_x & known_y & same_source & same_id
          above_threshold <- which(scores >= threshold)
          blocked_idx <- above_threshold[!eligible[above_threshold]]
          if (length(blocked_idx)) {
            blocked_count <- blocked_count + 1L
            blocked[[blocked_count]] <- tibble(
              name_x = names_x[i], name_y = names_y[cand_y[blocked_idx]],
              score = round(scores[blocked_idx], 4),
              identity_state = ifelse(
                known_x & known_y[blocked_idx] & same_source[blocked_idx],
                "conflict", "unknown"
              ),
              eligible = FALSE,
              reason = ifelse(
                known_x & known_y[blocked_idx] & same_source[blocked_idx],
                "distinct_accepted_ids", "identity_not_established"
              ),
              x_id = if (known_x) rep(identity_x$accepted_id[i], length(blocked_idx)) else NA_character_,
              y_id = ifelse(known_y[blocked_idx],
                            identity_y$accepted_id[cand_y[blocked_idx]], NA_character_),
              provider = ifelse(same_source[blocked_idx],
                                identity_x$provider[i], NA_character_),
              version = ifelse(same_source[blocked_idx],
                               identity_x$version[i], NA_character_)
            )
          }
          eligible_idx <- which(eligible & scores >= threshold)
          if (length(eligible_idx)) {
            best_k <- eligible_idx[which.max(scores[eligible_idx])]
            best <- c(index = cand_y[best_k], score = scores[best_k])
          } else {
            best <- c(index = NA_real_, score = -Inf)
          }
        } else if (exists(cache_key, cache, inherits = FALSE)) {
          best <- get(cache_key, cache, inherits = FALSE)
        } else {
          ep_dists <- as.numeric(utils::adist(ex, epithets_y))
          ep_maxlen <- pmax(nchar(ex), nchar(epithets_y))
          ep_sim <- ifelse(ep_maxlen == 0, 1, 1 - ep_dists / ep_maxlen)
          scores <- 0.6 * g_sim + 0.4 * ep_sim
          best_k <- which.max(scores)
          best <- c(index = cand_y[best_k], score = scores[best_k])
          assign(cache_key, best, cache)
        }
        if (is.finite(best[["score"]]) && best[["score"]] >= threshold) {
          j <- as.integer(best[["index"]])
          result_count <- result_count + 1L
          results[[result_count]] <- list(i = i, j = j, score = best[["score"]])
        }
      }
      completed <- completed + 1L
      now <- proc.time()[["elapsed"]]
      if (show_progress && (now - last_progress >= 5 || completed == length(idx_x))) {
        cli::cli_progress_update(id = progress_id, set = completed, force = TRUE)
        eta <- (now - started) / completed * (length(idx_x) - completed)
        cli::cli_inform("Fuzzy matching: {completed}/{length(idx_x)} names; ETA {round(eta)} s.")
        last_progress <- now
      }
    }
  }
  results <- results[seq_len(result_count)]

  if (length(results) == 0) {
    out <- tibble(
      name_x = character(),
      name_y = character(),
      score  = numeric(),
      notes  = character()
    )
    attr(out, "identity_candidates") <- if (length(blocked)) dplyr::bind_rows(blocked) else NULL
    return(out)
  }

  i <- vapply(results, `[[`, integer(1), "i")
  j <- vapply(results, `[[`, integer(1), "j")
  raw_score <- vapply(results, `[[`, numeric(1), "score")
  order_x <- order(i)
  out <- tibble(name_x = names_x[i], name_y = names_y[j], score = round(raw_score, 4),
                notes = sprintf("Fuzzy match (score %.3f): '%s' ~ '%s'",
                                raw_score, norm_x[i], norm_y[j]))[order_x, ]
  attr(out, "identity_candidates") <- if (length(blocked)) dplyr::bind_rows(blocked) else NULL
  out
}
