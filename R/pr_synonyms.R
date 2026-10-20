# Synonym resolution -------------------------------------------------------

#' Resolve synonyms between two name sets
#'
#' For names that remain unmatched after exact and normalised matching, queries
#' a taxonomic authority to find cases where both names resolve to the same
#' accepted name, or where one name is a synonym of the other.
#'
#' @param unmatched_x A character vector. Unmatched names from
#'   source x.
#' @param unmatched_y A character vector. Unmatched names from
#'   source y.
#' @param authority A length-1 character vector. Authority code, one
#'   of `pr_valid_authorities()`.
#' @param db_version A length-1 character vector, or `NULL`.
#' @param quiet Logical. Suppresses progress messages when `TRUE`.
#'
#' @return A tibble with columns: `name_x`, `name_y`, `name_resolved`,
#'   `match_source`, `notes`.
#' @keywords internal
pr_resolve_synonyms <- function(unmatched_x, unmatched_y,
                                authority = "col", db_version = NULL,
                                quiet = FALSE, lookup_x = NULL,
                                lookup_y = NULL) {

  if (length(unmatched_x) == 0 || length(unmatched_y) == 0) {
    return(tibble(
      name_x        = character(),
      name_y        = character(),
      name_resolved = character(),
      match_source  = character(),
      notes         = character()
    ))
  }

  if (!quiet) {
    cli_alert_info(
      "Looking up {length(unmatched_x)} + {length(unmatched_y)} names in {.val {toupper(authority)}}..."
    )
  }

  # Look up both sets
  if (is.null(lookup_x)) {
    lookup_x <- pr_lookup_authority(unmatched_x, authority, db_version, quiet = quiet)
  }
  if (is.null(lookup_y)) {
    lookup_y <- pr_lookup_authority(unmatched_y, authority, db_version, quiet = quiet)
  }

  if (!quiet) cli_alert_info("Authority lookup complete; matching accepted names...")
  empty <- tibble(name_x = character(), name_y = character(),
                  name_resolved = character(), match_source = character(),
                  notes = character())
  rows <- list()
  identity_blocked <- tibble(name_x = character(), name_y = character())
  make_rows <- function(x, y, accepted, indirect = FALSE) {
    tibble(
      name_x = x, name_y = y, name_resolved = accepted,
      match_source = paste0(authority, "_synonym"),
      notes = if (indirect) {
        sprintf("Both synonyms per %s: '%s' and '%s' -> accepted '%s'",
                toupper(authority), x, y, accepted)
      } else {
        sprintf("Synonym per %s: '%s' -> accepted '%s'",
                toupper(authority), x,
                accepted)
      }
    )
  }

  # Keep predecessor priority and order: x->y, then y->x, then shared
  # accepted names. match() preserves its first-target rule without scans.
  identity_context <- all(c("identity_status", "accepted_id", "provider", "version", "rank") %in%
                            names(lookup_x)) &&
    all(c("identity_status", "accepted_id", "provider", "version", "rank") %in%
          names(lookup_y))
  if (identity_context) {
    context_x <- unique(lookup_x[c("provider", "version", "rank")])
    context_y <- unique(lookup_y[c("provider", "version", "rank")])
    identity_context <- nrow(context_x) == 1L && nrow(context_y) == 1L &&
      identical(as.character(context_x[1, ]), as.character(context_y[1, ]))
  }
  pair_eligible <- function(ix, iy) {
    if (!identity_context) return(rep(TRUE, length(ix)))
    eligible <- lookup_x$identity_status[ix] == "known" &
      lookup_y$identity_status[iy] == "known" &
      !is.na(lookup_x$accepted_id[ix]) & !is.na(lookup_y$accepted_id[iy]) &
      lookup_x$accepted_id[ix] == lookup_y$accepted_id[iy]
    eligible[is.na(eligible)] <- FALSE
    eligible
  }
  target_y <- match(lookup_x$accepted_name, unmatched_y)
  i <- which(!is.na(lookup_x$accepted_name) & !is.na(target_y))
  if (length(i)) {
    eligible <- pair_eligible(i, target_y[i])
    if (any(!eligible)) {
      blocked_i <- i[!eligible]
      identity_blocked <- rbind(identity_blocked, tibble(
        name_x = lookup_x$input[blocked_i],
        name_y = unmatched_y[target_y[blocked_i]]
      ))
    }
    i <- i[eligible]
  }
  used_x <- lookup_x$input[i]
  used_y <- unmatched_y[target_y[i]]
  if (length(i)) {
    rows[[1L]] <- make_rows(used_x, used_y, lookup_x$accepted_name[i])
  }

  target_x <- match(lookup_y$accepted_name, unmatched_x)
  j <- which(!is.na(lookup_y$accepted_name) & !is.na(target_x) &
               !lookup_y$input %in% used_y &
               !unmatched_x[target_x] %in% used_x)
  if (length(j)) {
    eligible <- pair_eligible(target_x[j], j)
    if (any(!eligible)) {
      blocked_j <- j[!eligible]
      identity_blocked <- rbind(identity_blocked, tibble(
        name_x = unmatched_x[target_x[blocked_j]],
        name_y = lookup_y$input[blocked_j]
      ))
    }
    j <- j[eligible]
  }
  # The reverse strategy consumes each source at its first eligible y.
  j <- j[!duplicated(unmatched_x[target_x[j]])]
  if (length(j)) {
    rx <- unmatched_x[target_x[j]]
    ry <- lookup_y$input[j]
    rows[[length(rows) + 1L]] <- tibble(
      name_x = rx, name_y = ry,
      name_resolved = lookup_y$accepted_name[j],
      match_source = paste0(authority, "_synonym"),
      notes = sprintf("Synonym per %s: '%s' -> accepted '%s'",
                      toupper(authority), ry, lookup_y$accepted_name[j])
    )
    used_x <- c(used_x, rx)
    used_y <- c(used_y, ry)
  }

  # Each lookup has one row per unique input. When both lookups carry
  # comparable identity evidence, pair shared accepted-name groups by the
  # canonical ID first. Pairing by row order and checking identity afterward
  # can consume names in crossed-ID groups and hide valid matches.
  ix <- which(!is.na(lookup_x$accepted_name) & !lookup_x$input %in% used_x)
  iy <- which(!is.na(lookup_y$accepted_name) & !lookup_y$input %in% used_y)
  if (length(ix) && length(iy)) {
    groups <- unique(lookup_y$accepted_name[iy])
    if (!identity_context) {
      # Preserve the exact predecessor ordering when there is no comparable
      # identity evidence. This lexical route has no IDs to reorder by.
      gy <- match(lookup_y$accepted_name[iy], groups)
      gx <- match(lookup_x$accepted_name[ix], groups)
      valid <- which(!is.na(gx))
      if (length(valid)) {
        gid <- gx[valid]
        ordinal <- stats::ave(seq_along(gid), gid, FUN = seq_along)
        counts <- tabulate(gy, nbins = length(groups))
        keep <- ordinal <= counts[gid]
        valid <- valid[keep]
        gid <- gid[keep]
        ordinal <- ordinal[keep]
        starts <- c(1L, utils::head(cumsum(counts), -1L) + 1L)
        chosen_y <- iy[order(gy, method = "radix")[starts[gid] + ordinal - 1L]]
        chosen_x <- ix[valid]
      } else {
        chosen_x <- chosen_y <- integer()
      }
    } else {
      chosen_x <- chosen_y <- integer()
      for (accepted in groups) {
        group_x <- ix[lookup_x$accepted_name[ix] == accepted]
        group_y <- iy[lookup_y$accepted_name[iy] == accepted]
        if (!length(group_x) || !length(group_y)) next

        paired_x <- paired_y <- integer()
        known_x <- group_x[lookup_x$identity_status[group_x] == "known" &
                             !is.na(lookup_x$accepted_id[group_x]) &
                             nzchar(lookup_x$accepted_id[group_x])]
        known_y <- group_y[lookup_y$identity_status[group_y] == "known" &
                             !is.na(lookup_y$accepted_id[group_y]) &
                             nzchar(lookup_y$accepted_id[group_y])]
        for (x_i in known_x) {
          eligible_y <- known_y[
            lookup_y$accepted_id[known_y] == lookup_x$accepted_id[x_i] &
              !known_y %in% paired_y
          ]
          if (length(eligible_y)) {
            paired_x <- c(paired_x, x_i)
            paired_y <- c(paired_y, eligible_y[[1L]])
          }
        }

        # Preserve order for remaining candidates; identity-supported pairs
        # have already been removed from both sides.
        remaining_x <- group_x[!group_x %in% paired_x]
        remaining_y <- group_y[!group_y %in% paired_y]
        n_pairs <- min(length(remaining_x), length(remaining_y))
        if (n_pairs) {
          paired_x <- c(paired_x, remaining_x[seq_len(n_pairs)])
          paired_y <- c(paired_y, remaining_y[seq_len(n_pairs)])
        }
        chosen_x <- c(chosen_x, paired_x)
        chosen_y <- c(chosen_y, paired_y)
      }
    }

    if (length(chosen_x)) {
      order_x <- order(chosen_x)
      chosen_x <- chosen_x[order_x]
      chosen_y <- chosen_y[order_x]
      rows[[length(rows) + 1L]] <- make_rows(
        lookup_x$input[chosen_x], lookup_y$input[chosen_y],
        lookup_x$accepted_name[chosen_x], indirect = TRUE
      )
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else empty
  if (nrow(identity_blocked)) {
    attr(out, "identity_candidates") <- identity_blocked
  }
  if (!quiet) cli_alert_info("Synonym matching complete: {nrow(out)} candidate pairs.")
  out
}
