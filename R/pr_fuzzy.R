# Fuzzy matching -----------------------------------------------------------

#' Fuzzy-match two sets of species names
#'
#' Uses component-based similarity: genus and epithet are matched separately,
#' then combined with weights (genus 0.6, species epithet 0.4; at subspecies
#' rank, genus 0.55, species epithet 0.30, and infraspecific epithet 0.15).
#' Uses base R `utils::adist()` for Levenshtein distance, with no extra
#' dependencies.
#'
#' Candidate generation starts with names whose genus is within 2 edits.
#' When comparable authority identity is available, same-ID names in other
#' genera can also be considered. The existing [pr_suggest_gender()] helper
#' identifies its limited `-us/-a/-um` and `-is/-e` ending pairs. These pairs
#' are withheld without identity evidence and can match only when provider,
#' snapshot, rank, and accepted ID agree. The adjusted score treats the
#' supported species-epithet ending as equivalent; at subspecies rank, the
#' infraspecific epithet must also be sufficiently similar. This does not
#' establish grammar or identity.
#' Other fuzzy matches retain their spelling-based behavior.
#'
#' The `identity_x` and `identity_y` arguments are internal evidence inputs
#' supplied by the reconciliation cascade. They are not part of the exported
#' user interface.
#'
#' @param names_x Character vector.
#' @param names_y Character vector.
#' @param threshold Numeric (0--1). Minimum similarity score. Default 0.9.
#' @param rank Character. `"species"` or `"subspecies"`.
#' @param quiet Logical. Suppress progress messages.
#' @param identity_x Internal authority identity evidence aligned to `names_x`.
#'   Used only by the reconciliation cascade.
#' @param identity_y Internal authority identity evidence aligned to `names_y`.
#'   Used only by the reconciliation cascade.
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

  # Parse genus, specific epithet, and (at subspecies rank) the
  # infraspecific epithet.
  parse_binomial <- function(nms) {
    parts <- strsplit(nms, "\\s+")
    genus   <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
    epithet <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
    infraspecific <- rep(NA_character_, length(parts))
    if (rank == "subspecies") {
      infraspecific <- vapply(parts, function(p) {
        if (length(p) < 3L) return(NA_character_)
        pos <- if (p[3L] %in% c("subsp.", "var.", "f.")) 4L else 3L
        if (length(p) >= pos) p[pos] else NA_character_
      }, character(1))
    }
    list(genus = genus, epithet = epithet, infraspecific = infraspecific)
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
  gender_suggestions <- pr_suggest_gender(names_x, names_y, rank = rank)
  gender_targets_by_input <- new.env(hash = TRUE, parent = emptyenv())
  if (nrow(gender_suggestions)) {
    gender_target_groups <- split(gender_suggestions$suggestion,
                                  gender_suggestions$input)
    for (j in seq_along(gender_target_groups)) {
      assign(names(gender_target_groups)[[j]], gender_target_groups[[j]],
             gender_targets_by_input)
    }
  }
  identity_y_groups <- new.env(hash = TRUE, parent = emptyenv())
  if (identity_guard) {
    known_y <- intersect(idx_y, which(identity_y$identity_status == "known" &
                                        !is.na(identity_y$accepted_id) &
                                        !is.na(identity_y$provider) &
                                        !is.na(identity_y$version) &
                                        !is.na(identity_y$rank)))
    if (length(known_y)) {
      identity_key <- function(x) {
        paste(x$provider, x$version, x$rank, x$accepted_id, sep = "\034")
      }
      identity_groups <- split(known_y, identity_key(identity_y[known_y, ]))
      for (j in seq_along(identity_groups)) {
        assign(names(identity_groups)[[j]], identity_groups[[j]],
               identity_y_groups)
      }
    }
  }
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
    candidate_genera <- unique_genus_y[possible]
    cand_y <- unlist(genus_y_groups[candidate_genera], use.names = FALSE)
    # Keep genus-alphabetical / original-within-genus ordering for score ties.
    base_g_dist <- as.numeric(utils::adist(gx, py$genus[cand_y]))
    base_g_maxlen <- pmax(nchar(gx), nchar(py$genus[cand_y]))
    base_g_sim <- ifelse(base_g_maxlen == 0, 1,
                         1 - base_g_dist / base_g_maxlen)
    # Retain only the most recent score vector for repeated epithets. Keeping
    # one full vector per unique epithet grows with query epithets × targets.
    cached_key <- NULL
    cached_scores <- NULL
    for (i in groups_x[[gx]]) {
      ex <- px$epithet[i]
      gender_targets <- if (!is.na(names_x[i]) &&
                              exists(names_x[i], gender_targets_by_input,
                                     inherits = FALSE)) {
        get(names_x[i], gender_targets_by_input, inherits = FALSE)
      } else {
        character()
      }
      candidate_y <- cand_y
      appended_y <- integer()
      cross_fallback_y <- integer()
      known_x <- FALSE
      if (identity_guard) {
        known_x <- isTRUE(identity_x$identity_status[i] == "known") &&
          !is.na(identity_x$accepted_id[i]) && !is.na(identity_x$provider[i]) &&
          !is.na(identity_x$version[i]) && !is.na(identity_x$rank[i])
        if (known_x) {
          key <- paste(
            identity_x$provider[i], identity_x$version[i],
            identity_x$rank[i], identity_x$accepted_id[i], sep = "\034"
          )
          same_id_y <- if (exists(key, identity_y_groups, inherits = FALSE)) {
            get(key, identity_y_groups, inherits = FALSE)
          } else {
            integer()
          }
          if (length(same_id_y)) {
            # Every authority-confirmed cross-genus target follows the same
            # eligibility rule, whether or not its genus entered the spelling
            # prefilter. Append only the targets not already in that pool.
            cross_fallback_y <- same_id_y[py$genus[same_id_y] != gx]
            appended_y <- setdiff(cross_fallback_y, cand_y)
            candidate_y <- c(cand_y, appended_y)
          }
        }
      }

      if (length(candidate_y)) {
        cache_key <- paste0("epithet:", ex, "\034",
                            paste(sort(gender_targets), collapse = "\034"))
        if (!identity_guard && rank == "species" &&
              identical(cache_key, cached_key)) {
          scores <- cached_scores
          g_sim_i <- base_g_sim
        } else {
          if (identity_guard) {
            if (length(appended_y)) {
              g_dist_extra <- as.numeric(utils::adist(gx, py$genus[appended_y]))
              g_maxlen_extra <- pmax(nchar(gx), nchar(py$genus[appended_y]))
              g_sim_i <- c(
                base_g_sim,
                ifelse(g_maxlen_extra == 0, 1,
                       1 - g_dist_extra / g_maxlen_extra)
              )
            } else {
              g_sim_i <- base_g_sim
            }
          } else {
            g_sim_i <- base_g_sim
          }
          ep_dists <- as.numeric(utils::adist(ex, py$epithet[candidate_y]))
          ep_maxlen <- pmax(nchar(ex), nchar(py$epithet[candidate_y]))
          ep_sim <- ifelse(ep_maxlen == 0, 1, 1 - ep_dists / ep_maxlen)
          if (rank == "subspecies") {
            sub_x <- px$infraspecific[i]
            sub_y <- py$infraspecific[candidate_y]
            both_missing <- is.na(sub_x) & is.na(sub_y)
            one_missing <- xor(is.na(sub_x), is.na(sub_y))
            sub_sim <- numeric(length(candidate_y))
            sub_sim[both_missing] <- 1
            comparable <- which(!is.na(sub_x) & !is.na(sub_y))
            if (length(comparable)) {
              sub_d <- as.numeric(utils::adist(sub_x, sub_y[comparable]))
              sub_maxlen <- pmax(nchar(sub_x), nchar(sub_y[comparable]))
              sub_sim[comparable] <- ifelse(
                sub_maxlen == 0, 1, 1 - sub_d / sub_maxlen
              )
            }
            sub_sim[one_missing] <- 0
            scores <- 0.55 * g_sim_i + 0.30 * ep_sim + 0.15 * sub_sim
          } else {
            sub_sim <- rep(1, length(candidate_y))
            scores <- 0.6 * g_sim_i + 0.4 * ep_sim
          }
          if (!identity_guard && rank == "species") {
            cached_key <- cache_key
            cached_scores <- scores
          }
        }
        gender_variant <- names_y[candidate_y] %in% gender_targets
        # The helper encodes only the two supported ending groups. Treat
        # those endings as equivalent for the spelling score, while keeping
        # authority identity as a separate eligibility requirement.
        if (rank == "subspecies") {
          scores[gender_variant] <- 0.55 * g_sim_i[gender_variant] +
            0.30 + 0.15 * sub_sim[gender_variant]
        } else {
          scores[gender_variant] <- 0.6 * g_sim_i[gender_variant] + 0.4
        }

        if (identity_guard) {
          known_y <- !is.na(identity_y$identity_status[candidate_y]) &
            identity_y$identity_status[candidate_y] == "known"
          known_y[is.na(known_y)] <- FALSE
          same_source <- !is.na(identity_x$provider[i]) &
            !is.na(identity_y$provider[candidate_y]) &
            identity_x$provider[i] == identity_y$provider[candidate_y] &
            identity_x$version[i] == identity_y$version[candidate_y] &
            identity_x$rank[i] == identity_y$rank[candidate_y]
          same_source[is.na(same_source)] <- FALSE
          same_id <- !is.na(identity_x$accepted_id[i]) &
            !is.na(identity_y$accepted_id[candidate_y]) &
            identity_x$accepted_id[i] == identity_y$accepted_id[candidate_y]
          same_id[is.na(same_id)] <- FALSE
          eligible <- known_x & known_y & same_source & same_id
          blocked_idx <- which(
            !eligible & (scores >= threshold | gender_variant)
          )
        } else {
          known_y <- rep(FALSE, length(candidate_y))
          same_source <- rep(FALSE, length(candidate_y))
          eligible <- rep(TRUE, length(candidate_y))
          blocked_idx <- which(gender_variant)
        }

        if (length(blocked_idx)) {
          conflict <- if (identity_guard) {
            known_x & known_y[blocked_idx] & same_source[blocked_idx]
          } else {
            rep(FALSE, length(blocked_idx))
          }
          x_id <- if (known_x) {
            rep(identity_x$accepted_id[i], length(blocked_idx))
          } else {
            NA_character_
          }
          y_id <- identity_y$accepted_id[candidate_y[blocked_idx]]
          y_id[!known_y[blocked_idx]] <- NA_character_
          blocked_count <- blocked_count + 1L
          blocked[[blocked_count]] <- tibble(
            name_x = names_x[i], name_y = names_y[candidate_y[blocked_idx]],
            score = round(scores[blocked_idx], 4),
            identity_state = ifelse(conflict, "conflict", "unknown"),
            eligible = FALSE,
            reason = ifelse(conflict, "distinct_accepted_ids",
                            "identity_not_established"),
            x_id = x_id,
            y_id = y_id,
            provider = ifelse(same_source[blocked_idx],
                              identity_x$provider[i], NA_character_),
            version = ifelse(same_source[blocked_idx],
                             identity_x$version[i], NA_character_)
          )
        }

        eligible_idx <- which(
          eligible & (scores >= threshold | candidate_y %in% cross_fallback_y)
        )
        if (!identity_guard) {
          rank_compatible <- if (rank == "subspecies") {
            sub_sim >= threshold
          } else {
            rep(TRUE, length(scores))
          }
          eligible_idx <- which(scores >= threshold & !gender_variant &
                                  rank_compatible)
        }
        if (length(eligible_idx)) {
          best_k <- eligible_idx[which.max(scores[eligible_idx])]
          j <- candidate_y[best_k]
          result_count <- result_count + 1L
          result_provider <- if (identity_guard && eligible[best_k]) {
            identity_x$provider[i]
          } else {
            NA_character_
          }
          result_version <- if (identity_guard && eligible[best_k]) {
            identity_x$version[i]
          } else {
            NA_character_
          }
          results[[result_count]] <- list(
            i = i, j = j, score = scores[best_k],
            gender_variant = gender_variant[best_k],
            cross_genus = j %in% cross_fallback_y,
            provider = result_provider,
            version = result_version
          )
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
  gender_variant <- vapply(results, `[[`, logical(1), "gender_variant")
  cross_genus <- vapply(results, `[[`, logical(1), "cross_genus")
  provider <- vapply(results, `[[`, character(1), "provider")
  version <- vapply(results, `[[`, character(1), "version")
  order_x <- order(i)
  notes <- sprintf("Fuzzy match (score %.3f): '%s' ~ '%s'",
                   raw_score, norm_x[i], norm_y[j])
  if (any(gender_variant)) {
    notes[gender_variant] <- paste0(
      notes[gender_variant], "; Latin-aware ending candidate confirmed by ",
      provider[gender_variant], "@", version[gender_variant]
    )
  }
  if (any(cross_genus)) {
    notes[cross_genus] <- paste0(
      notes[cross_genus], "; authority-confirmed cross-genus candidate (",
      provider[cross_genus], "@", version[cross_genus], ")"
    )
  }
  out <- tibble(name_x = names_x[i], name_y = names_y[j], score = round(raw_score, 4),
                notes = notes)[order_x, ]
  attr(out, "identity_candidates") <- if (length(blocked)) dplyr::bind_rows(blocked) else NULL
  out
}
