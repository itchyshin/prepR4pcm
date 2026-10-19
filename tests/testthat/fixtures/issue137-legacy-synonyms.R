function (unmatched_x, unmatched_y, authority = "col", db_version = NULL,
    quiet = FALSE)
{
    if (length(unmatched_x) == 0 || length(unmatched_y) == 0) {
        return(tibble(name_x = character(), name_y = character(),
            name_resolved = character(), match_source = character(),
            notes = character()))
    }
    if (!quiet) {
        cli_alert_info("Looking up {length(unmatched_x)} + {length(unmatched_y)} names in {.val {toupper(authority)}}...")
    }
    lookup_x <- pr_lookup_authority(unmatched_x, authority, db_version,
        quiet = quiet)
    lookup_y <- pr_lookup_authority(unmatched_y, authority, db_version,
        quiet = quiet)
    results <- list()
    for (i in seq_len(nrow(lookup_x))) {
        acc_x <- lookup_x$accepted_name[i]
        if (is.na(acc_x))
            next
        match_idx <- which(unmatched_y == acc_x)
        if (length(match_idx) > 0) {
            results[[length(results) + 1]] <- tibble(name_x = lookup_x$input[i],
                name_y = unmatched_y[match_idx[1]], name_resolved = acc_x,
                match_source = paste0(authority, "_synonym"),
                notes = sprintf("Synonym per %s: '%s' -> accepted '%s'",
                  toupper(authority), lookup_x$input[i], acc_x))
        }
    }
    for (j in seq_len(nrow(lookup_y))) {
        acc_y <- lookup_y$accepted_name[j]
        if (is.na(acc_y))
            next
        already_matched_y <- vapply(results, function(r) r$name_y,
            character(1))
        if (lookup_y$input[j] %in% already_matched_y)
            next
        match_idx <- which(unmatched_x == acc_y)
        if (length(match_idx) > 0) {
            already_matched_x <- vapply(results, function(r) r$name_x,
                character(1))
            if (unmatched_x[match_idx[1]] %in% already_matched_x)
                next
            results[[length(results) + 1]] <- tibble(name_x = unmatched_x[match_idx[1]],
                name_y = lookup_y$input[j], name_resolved = acc_y,
                match_source = paste0(authority, "_synonym"),
                notes = sprintf("Synonym per %s: '%s' -> accepted '%s'",
                  toupper(authority), lookup_y$input[j], acc_y))
        }
    }
    for (i in seq_len(nrow(lookup_x))) {
        acc_x <- lookup_x$accepted_name[i]
        if (is.na(acc_x))
            next
        already_matched_x <- if (length(results) > 0) {
            vapply(results, function(r) r$name_x, character(1))
        }
        else {
            character()
        }
        if (lookup_x$input[i] %in% already_matched_x)
            next
        for (j in seq_len(nrow(lookup_y))) {
            acc_y <- lookup_y$accepted_name[j]
            if (is.na(acc_y))
                next
            already_matched_y <- if (length(results) > 0) {
                vapply(results, function(r) r$name_y, character(1))
            }
            else {
                character()
            }
            if (lookup_y$input[j] %in% already_matched_y)
                next
            if (acc_x == acc_y) {
                results[[length(results) + 1]] <- tibble(name_x = lookup_x$input[i],
                  name_y = lookup_y$input[j], name_resolved = acc_x,
                  match_source = paste0(authority, "_synonym"),
                  notes = sprintf("Both synonyms per %s: '%s' and '%s' -> accepted '%s'",
                    toupper(authority), lookup_x$input[i], lookup_y$input[j],
                    acc_x))
                break
            }
        }
    }
    if (length(results) == 0) {
        return(tibble(name_x = character(), name_y = character(),
            name_resolved = character(), match_source = character(),
            notes = character()))
    }
    do.call(rbind, results)
}
