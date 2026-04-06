# Matching cascade ---------------------------------------------------------

#' Run the matching cascade
#'
#' The central engine behind all `reconcile_*` functions. Applies matching
#' stages in strict order of decreasing confidence: exact -> normalised ->
#' synonym -> fuzzy. Each stage only operates on names not yet matched.
#'
#' @param names_x Character vector. Names from source x.
#' @param names_y Character vector. Names from source y.
#' @param authority Character(1) or NULL. Taxonomic authority for synonym
#'   lookup. NULL skips stage 3.
#' @param db_version Character(1) or NULL.
#' @param rank Character(1). `"species"` or `"subspecies"`.
#' @param overrides A data.frame with columns `name_x` and `name_y` for
#'   pre-built overrides, or NULL.
#' @param fuzzy Logical. Enable fuzzy matching? Default `FALSE`.
#' @param fuzzy_threshold Numeric. Minimum similarity (0--1) for fuzzy
#'   matches. Default `0.9` (conservative).
#' @param resolve Character(1). How to handle low-confidence matches:
#'   `"flag"` (default) marks fuzzy matches below 0.95 and indirect
#'   synonym matches as `match_type = "flagged"` for manual review.
#'   `"first"` accepts all matches at face value.
#' @param quiet Logical.
#'
#' @return A tibble with the full mapping table.
#' @keywords internal
pr_run_cascade <- function(names_x, names_y,
                           authority = "col",
                           db_version = NULL,
                           rank = "species",
                           overrides = NULL,
                           fuzzy = FALSE,
                           fuzzy_threshold = 0.9,
                           resolve = "flag",
                           quiet = FALSE) {

  # Deduplicate inputs
  unique_x <- unique(names_x[!is.na(names_x)])
  unique_y <- unique(names_y[!is.na(names_y)])

  # Track which names are matched at each stage
  matched_x <- character()
  matched_y <- character()
  rows <- list()

  # --- Pre-stage: Apply manual overrides ---
  if (!is.null(overrides) && nrow(overrides) > 0) {
    for (i in seq_len(nrow(overrides))) {
      ox <- overrides$name_x[i]
      oy <- overrides$name_y[i]
      if (ox %in% unique_x && oy %in% unique_y) {
        rows[[length(rows) + 1]] <- tibble(
          name_x        = ox,
          name_y        = oy,
          name_resolved = NA_character_,
          match_type    = "manual",
          match_score   = 1.0,
          match_source  = "user_override",
          in_x          = TRUE,
          in_y          = TRUE,
          notes         = overrides$user_note[i] %||% "Manual override"
        )
        matched_x <- c(matched_x, ox)
        matched_y <- c(matched_y, oy)
      }
    }
  }

  # --- Stage 1: Exact match ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  exact_both <- intersect(remaining_x, remaining_y)

  if (length(exact_both) > 0) {
    rows[[length(rows) + 1]] <- tibble(
      name_x        = exact_both,
      name_y        = exact_both,
      name_resolved = NA_character_,
      match_type    = "exact",
      match_score   = 1.0,
      match_source  = "exact_string",
      in_x          = TRUE,
      in_y          = TRUE,
      notes         = ""
    )
    matched_x <- c(matched_x, exact_both)
    matched_y <- c(matched_y, exact_both)
  }

  # --- Stage 2: Normalised match ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  if (length(remaining_x) > 0 && length(remaining_y) > 0) {
    norm_x <- pr_normalize_names(remaining_x, rank = rank)
    norm_y <- pr_normalize_names(remaining_y, rank = rank)

    # Build lookup: normalised -> original
    lookup_y <- stats::setNames(remaining_y, norm_y)

    for (i in seq_along(remaining_x)) {
      nx <- norm_x[i]
      if (nx %in% names(lookup_y)) {
        oy <- lookup_y[nx][[1]]
        # Check not already matched
        if (!(remaining_x[i] %in% matched_x) && !(oy %in% matched_y)) {
          rows[[length(rows) + 1]] <- tibble(
            name_x        = remaining_x[i],
            name_y        = oy,
            name_resolved = NA_character_,
            match_type    = "normalized",
            match_score   = 1.0,
            match_source  = "normalisation",
            in_x          = TRUE,
            in_y          = TRUE,
            notes         = sprintf("'%s' normalised to '%s'",
                                    remaining_x[i], nx)
          )
          matched_x <- c(matched_x, remaining_x[i])
          matched_y <- c(matched_y, oy)
        }
      }
    }
  }

  # --- Stage 3: Synonym resolution ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  if (!is.null(authority) && length(remaining_x) > 0 &&
      length(remaining_y) > 0) {

    # Normalise remaining names before synonym lookup
    norm_remaining_x <- as.character(pr_normalize_names(remaining_x,
                                                         rank = rank))
    norm_remaining_y <- as.character(pr_normalize_names(remaining_y,
                                                         rank = rank))

    syn_matches <- pr_resolve_synonyms(
      unmatched_x = norm_remaining_x,
      unmatched_y = norm_remaining_y,
      authority   = authority,
      db_version  = db_version,
      quiet       = quiet
    )

    if (nrow(syn_matches) > 0) {
      # Map normalised names back to originals
      lookup_orig_x <- stats::setNames(remaining_x, norm_remaining_x)
      lookup_orig_y <- stats::setNames(remaining_y, norm_remaining_y)

      for (i in seq_len(nrow(syn_matches))) {
        orig_x <- lookup_orig_x[syn_matches$name_x[i]]
        orig_y <- lookup_orig_y[syn_matches$name_y[i]]

        if (!is.na(orig_x) && !is.na(orig_y) &&
            !(orig_x %in% matched_x) && !(orig_y %in% matched_y)) {
          # Flag indirect matches (both names are synonyms) when resolve = "flag"
          is_indirect <- grepl("^Both synonyms", syn_matches$notes[i])
          mtype <- if (resolve == "flag" && is_indirect) "flagged" else "synonym"

          rows[[length(rows) + 1]] <- tibble(
            name_x        = unname(orig_x),
            name_y        = unname(orig_y),
            name_resolved = syn_matches$name_resolved[i],
            match_type    = mtype,
            match_score   = 0.95,
            match_source  = syn_matches$match_source[i],
            in_x          = TRUE,
            in_y          = TRUE,
            notes         = syn_matches$notes[i]
          )
          matched_x <- c(matched_x, unname(orig_x))
          matched_y <- c(matched_y, unname(orig_y))
        }
      }
    }
  }

  # --- Stage 4: Fuzzy matching ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  if (fuzzy && length(remaining_x) > 0 && length(remaining_y) > 0) {
    if (!quiet) {
      cli_alert_info("Running fuzzy matching on {length(remaining_x)} x {length(remaining_y)} remaining names...")
    }

    fuzzy_matches <- pr_fuzzy_match(
      remaining_x, remaining_y,
      threshold = fuzzy_threshold,
      rank = rank
    )

    if (nrow(fuzzy_matches) > 0) {
      for (i in seq_len(nrow(fuzzy_matches))) {
        fx <- fuzzy_matches$name_x[i]
        fy <- fuzzy_matches$name_y[i]
        if (!(fx %in% matched_x) && !(fy %in% matched_y)) {
          # Flag low-confidence fuzzy matches when resolve = "flag"
          fscore <- fuzzy_matches$score[i]
          mtype <- if (resolve == "flag" && fscore < 0.95) "flagged" else "fuzzy"

          rows[[length(rows) + 1]] <- tibble(
            name_x        = fx,
            name_y        = fy,
            name_resolved = NA_character_,
            match_type    = mtype,
            match_score   = fscore,
            match_source  = "fuzzy_match",
            in_x          = TRUE,
            in_y          = TRUE,
            notes         = fuzzy_matches$notes[i]
          )
          matched_x <- c(matched_x, fx)
          matched_y <- c(matched_y, fy)
        }
      }
    }
  }

  # --- Unresolved names ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  if (length(remaining_x) > 0) {
    rows[[length(rows) + 1]] <- tibble(
      name_x        = remaining_x,
      name_y        = NA_character_,
      name_resolved = NA_character_,
      match_type    = "unresolved",
      match_score   = NA_real_,
      match_source  = NA_character_,
      in_x          = TRUE,
      in_y          = FALSE,
      notes         = "No match found in source y"
    )
  }

  if (length(remaining_y) > 0) {
    rows[[length(rows) + 1]] <- tibble(
      name_x        = NA_character_,
      name_y        = remaining_y,
      name_resolved = NA_character_,
      match_type    = "unresolved",
      match_score   = NA_real_,
      match_source  = NA_character_,
      in_x          = FALSE,
      in_y          = TRUE,
      notes         = "No match found in source x"
    )
  }

  # Combine all rows
  if (length(rows) == 0) {
    return(tibble(
      name_x = character(), name_y = character(),
      name_resolved = character(), match_type = character(),
      match_score = numeric(), match_source = character(),
      in_x = logical(), in_y = logical(), notes = character()
    ))
  }

  do.call(rbind, rows)
}
