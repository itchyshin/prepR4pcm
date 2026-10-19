function (names_x, names_y, authority = "col", db_version = NULL,
    rank = "species", overrides = NULL, fuzzy = FALSE, fuzzy_threshold = 0.9,
    flag_threshold = 0.95, resolve = "flag", multi_x = FALSE,
    quiet = FALSE)
{
    n_na_x <- sum(is.na(names_x))
    n_na_y <- sum(is.na(names_y))
    if (!quiet && (n_na_x > 0 || n_na_y > 0)) {
        cli_alert_warning("Removed {n_na_x} NA name{?s} from x and {n_na_y} from y before matching.")
    }
    unique_x <- unique(names_x[!is.na(names_x)])
    unique_y <- unique(names_y[!is.na(names_y)])
    use_progress <- !quiet && length(unique_x) > 500
    do_synonym <- !is.null(authority)
    do_fuzzy <- isTRUE(fuzzy)
    n_stages <- 2L + do_synonym + do_fuzzy
    stage_no <- 0L
    next_stage_label <- function(name) {
        stage_no <<- stage_no + 1L
        sprintf("Stage %d/%d: %s", stage_no, n_stages, name)
    }
    if (use_progress) {
        cli_alert_info("Matching {length(unique_x)} x {length(unique_y)} names through {n_stages} stage{?s}...")
    }
    matched_x <- character()
    matched_y <- character()
    rows <- list()
    unused_overrides <- list()
    if (!is.null(overrides) && nrow(overrides) > 0) {
        norm_ux <- as.character(pr_normalize_names(unique_x,
            rank = rank))
        norm_uy <- as.character(pr_normalize_names(unique_y,
            rank = rank))
        lookup_orig_ux <- stats::setNames(unique_x, norm_ux)
        lookup_orig_uy <- stats::setNames(unique_y, norm_uy)
        for (i in seq_len(nrow(overrides))) {
            ox <- overrides$name_x[i]
            oy <- overrides$name_y[i]
            ox_norm <- as.character(pr_normalize_names(ox, rank = rank))
            oy_norm <- as.character(pr_normalize_names(oy, rank = rank))
            ox_orig <- lookup_orig_ux[ox_norm]
            oy_orig <- lookup_orig_uy[oy_norm]
            ox_missing <- is.na(ox_orig)
            oy_missing <- is.na(oy_orig)
            if (!ox_missing && !oy_missing && !(ox_orig %in%
                matched_x) && !(oy_orig %in% matched_y)) {
                rows[[length(rows) + 1]] <- tibble(name_x = unname(ox_orig),
                  name_y = unname(oy_orig), name_resolved = NA_character_,
                  match_type = "manual", match_score = 1, match_source = "user_override",
                  in_x = TRUE, in_y = TRUE, notes = overrides$user_note[i] %||%
                    "Manual override")
                matched_x <- c(matched_x, unname(ox_orig))
                matched_y <- c(matched_y, unname(oy_orig))
            }
            else {
                reason <- if (ox_missing && oy_missing) {
                  "name_x_not_in_data"
                }
                else if (ox_missing) {
                  "name_x_not_in_data"
                }
                else if (oy_missing) {
                  "name_y_not_in_target"
                }
                else {
                  "already_matched"
                }
                unused_overrides[[length(unused_overrides) +
                  1]] <- tibble(name_x = ox, name_y = oy, reason = reason)
            }
        }
    }
    if (use_progress) {
        cli_alert_info(next_stage_label("Exact matching..."))
    }
    remaining_x <- setdiff(unique_x, matched_x)
    remaining_y <- setdiff(unique_y, matched_y)
    exact_both <- intersect(remaining_x, remaining_y)
    if (length(exact_both) > 0) {
        rows[[length(rows) + 1]] <- tibble(name_x = exact_both,
            name_y = exact_both, name_resolved = NA_character_,
            match_type = "exact", match_score = 1, match_source = "exact_string",
            in_x = TRUE, in_y = TRUE, notes = "")
        matched_x <- c(matched_x, exact_both)
        matched_y <- c(matched_y, exact_both)
    }
    if (use_progress) {
        cli_alert_info(paste0(next_stage_label("Normalised matching"),
            " ({length(matched_x)} matched so far)..."))
    }
    remaining_x <- setdiff(unique_x, matched_x)
    remaining_y <- if (multi_x)
        unique_y
    else setdiff(unique_y, matched_y)
    if (length(remaining_x) > 0 && length(remaining_y) > 0) {
        norm_x <- pr_normalize_names(remaining_x, rank = rank)
        norm_y <- pr_normalize_names(remaining_y, rank = rank)
        dup_norm_y <- norm_y[duplicated(norm_y) & !is.na(norm_y)]
        if (length(dup_norm_y) > 0 && !quiet) {
            cli_alert_warning("{length(dup_norm_y)} name{?s} in y have identical normalised forms; only the first of each will be matched.")
        }
        lookup_y <- stats::setNames(remaining_y, norm_y)
        hit_mask <- norm_x %in% names(lookup_y)
        if (any(hit_mask)) {
            x_hits <- remaining_x[hit_mask]
            nx_hits <- as.character(norm_x[hit_mask])
            y_hits <- unname(lookup_y[nx_hits])
            if (!multi_x) {
                dup_y <- duplicated(y_hits)
                x_hits <- x_hits[!dup_y]
                nx_hits <- nx_hits[!dup_y]
                y_hits <- y_hits[!dup_y]
            }
            rows[[length(rows) + 1]] <- tibble(name_x = x_hits,
                name_y = y_hits, name_resolved = NA_character_,
                match_type = "normalized", match_score = 1, match_source = "normalisation",
                in_x = TRUE, in_y = TRUE, notes = sprintf("'%s' normalised to '%s'",
                  x_hits, nx_hits))
            matched_x <- c(matched_x, x_hits)
            matched_y <- c(matched_y, y_hits)
        }
    }
    remaining_x <- setdiff(unique_x, matched_x)
    remaining_y <- setdiff(unique_y, matched_y)
    if (do_synonym) {
        if (use_progress) {
            cli_alert_info(paste0(next_stage_label("Synonym resolution"),
                " ({length(matched_x)} matched so far)..."))
        }
    }
    if (do_synonym && length(remaining_x) > 0 && length(remaining_y) >
        0) {
        norm_remaining_x <- as.character(pr_normalize_names(remaining_x,
            rank = rank))
        norm_remaining_y <- as.character(pr_normalize_names(remaining_y,
            rank = rank))
        syn_matches <- pr_resolve_synonyms(unmatched_x = norm_remaining_x,
            unmatched_y = norm_remaining_y, authority = authority,
            db_version = db_version, quiet = quiet)
        if (nrow(syn_matches) > 0) {
            lookup_orig_x <- stats::setNames(remaining_x, norm_remaining_x)
            lookup_orig_y <- stats::setNames(remaining_y, norm_remaining_y)
            for (i in seq_len(nrow(syn_matches))) {
                orig_x <- lookup_orig_x[syn_matches$name_x[i]]
                orig_y <- lookup_orig_y[syn_matches$name_y[i]]
                if (!is.na(orig_x) && !is.na(orig_y) && !(orig_x %in%
                  matched_x) && !(orig_y %in% matched_y)) {
                  is_indirect <- grepl("^Both synonyms", syn_matches$notes[i])
                  mtype <- if (resolve == "flag" && is_indirect) {
                    "flagged"
                  }
                  else {
                    "synonym"
                  }
                  rows[[length(rows) + 1]] <- tibble(name_x = unname(orig_x),
                    name_y = unname(orig_y), name_resolved = syn_matches$name_resolved[i],
                    match_type = mtype, match_score = 0.95, match_source = syn_matches$match_source[i],
                    in_x = TRUE, in_y = TRUE, notes = syn_matches$notes[i])
                  matched_x <- c(matched_x, unname(orig_x))
                  matched_y <- c(matched_y, unname(orig_y))
                }
            }
        }
    }
    remaining_x <- setdiff(unique_x, matched_x)
    remaining_y <- setdiff(unique_y, matched_y)
    if (do_fuzzy) {
        if (use_progress) {
            cli_alert_info(paste0(next_stage_label("Fuzzy matching"),
                " ({length(matched_x)} matched so far)..."))
        }
    }
    if (do_fuzzy && length(remaining_x) > 0 && length(remaining_y) >
        0) {
        if (!quiet) {
            cli_alert_info("Running fuzzy matching on {length(remaining_x)} x {length(remaining_y)} remaining names...")
        }
        fuzzy_matches <- pr_fuzzy_match(remaining_x, remaining_y,
            threshold = fuzzy_threshold, rank = rank)
        if (nrow(fuzzy_matches) > 0) {
            for (i in seq_len(nrow(fuzzy_matches))) {
                fx <- fuzzy_matches$name_x[i]
                fy <- fuzzy_matches$name_y[i]
                if (!(fx %in% matched_x) && !(fy %in% matched_y)) {
                  fscore <- fuzzy_matches$score[i]
                  mtype <- if (resolve == "flag" && fscore <
                    flag_threshold) {
                    "flagged"
                  }
                  else {
                    "fuzzy"
                  }
                  rows[[length(rows) + 1]] <- tibble(name_x = fx,
                    name_y = fy, name_resolved = NA_character_,
                    match_type = mtype, match_score = fscore,
                    match_source = "fuzzy_match", in_x = TRUE,
                    in_y = TRUE, notes = fuzzy_matches$notes[i])
                  matched_x <- c(matched_x, fx)
                  matched_y <- c(matched_y, fy)
                }
            }
        }
    }
    remaining_x <- setdiff(unique_x, matched_x)
    remaining_y <- setdiff(unique_y, matched_y)
    if (length(remaining_x) > 0) {
        rows[[length(rows) + 1]] <- tibble(name_x = remaining_x,
            name_y = NA_character_, name_resolved = NA_character_,
            match_type = "unresolved", match_score = NA_real_,
            match_source = NA_character_, in_x = TRUE, in_y = FALSE,
            notes = "No match found in source y")
    }
    if (length(remaining_y) > 0) {
        rows[[length(rows) + 1]] <- tibble(name_x = NA_character_,
            name_y = remaining_y, name_resolved = NA_character_,
            match_type = "unresolved", match_score = NA_real_,
            match_source = NA_character_, in_x = FALSE, in_y = TRUE,
            notes = "No match found in source x")
    }
    if (length(unused_overrides) == 0) {
        unused_tbl <- tibble(name_x = character(), name_y = character(),
            reason = character())
    }
    else {
        unused_tbl <- do.call(rbind, unused_overrides)
    }
    if (length(rows) == 0) {
        mapping <- tibble(name_x = character(), name_y = character(),
            name_resolved = character(), match_type = character(),
            match_score = numeric(), match_source = character(),
            in_x = logical(), in_y = logical(), notes = character())
    }
    else {
        mapping <- do.call(rbind, rows)
    }
    attr(mapping, "unused_overrides") <- unused_tbl
    mapping
}
