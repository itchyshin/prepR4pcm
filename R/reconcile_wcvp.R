#' Add synonym review candidates from a frozen WCVP names table
#'
#' Resolve unmatched input names and target labels against the same WCVP
#' release. Pairs with the same unambiguous accepted taxon ID and rank are
#' retained for review, including targets written as synonyms. No mapping
#' rows are changed and candidates are never accepted automatically.
#'
#' @param reconciliation A [reconciliation] object.
#' @param wcvp A list with `names`, a raw Kew WCVP names data frame (such as
#'   the `wcvp_names` table from rWCVPdata), and `version`, a non-empty character label for
#'   that frozen release. Required columns are `plant_name_id`, `taxon_name`,
#'   `taxon_status`, `taxon_rank`, and `accepted_plant_name_id`.
#'
#' @details
#' Only spaces, underscores and case are normalized for local lookup.
#' Infraspecific names retain their rank and accepted identity even when the
#' reconciliation uses `rank = "species"`. Names with multiple possible
#' accepted identities, unresolved records, missing links, cyclic synonym
#' chains, or multiple target labels for one identity are audit-only. An
#' occupied unique target can be suggested but must be resolved during review.
#' Kew's explicit accepted-name links for invalid, illegitimate and orthographic
#' names are retained as review evidence, with their nomenclatural status shown.
#'
#' This function is offline and does not require TNRS or rWCVPdata to be
#' installed if the caller supplies a names table. It records the release,
#' table fingerprint, input and target resolution, and candidate evidence in
#' `result$wcvp`. It supplements review evidence; it does not resolve conflicts
#' in existing mappings made using another taxonomic reference.
#'
#' @return The reconciliation with a `wcvp` audit record and pending candidates
#'   available through [reconcile_candidates()] and [reconcile_review()].
#' @seealso [reconcile_tnrs()], [reconcile_tree()]
#' @export
reconcile_wcvp <- function(reconciliation, wcvp) {
  validate_reconciliation(reconciliation)
  mapping <- reconciliation$mapping
  input <- unique(mapping$name_x[mapping$in_x & mapping$match_type == "unresolved" &
                                  !is.na(mapping$name_x)])
  targets <- unique(mapping$name_y[mapping$in_y & !is.na(mapping$name_y)])
  index <- .pr_wcvp_index(wcvp, unique(c(input, targets)))
  x <- .pr_wcvp_lookup(input, index)
  y <- .pr_wcvp_lookup(targets, index)
  target_lookup <- split(y$name[y$status == "resolved"],
                         y$identity[y$status == "resolved"])
  hits <- unname(target_lookup[x$identity])
  counts <- lengths(hits)
  x$target_status <- ifelse(x$status != "resolved", "unresolved_identity",
                            ifelse(counts == 0L, "absent", ifelse(counts == 1L,
                              "candidate", "multiple_targets")))
  keep <- which(x$target_status == "candidate")
  additions <- tibble::tibble(name_x = character(), name_y = character(),
                              x_id = character(), accepted_rank = character())
  if (length(keep)) {
    xk <- x[keep, , drop = FALSE]
    tips <- vapply(hits[keep], `[[`, character(1), 1L)
    target_status <- y$name_status[match(tips, y$name)]
    additions <- tibble::tibble(
      name_x = xk$name, name_y = tips, identity_state = "same_taxon",
      eligible = FALSE, reason = "wcvp_review_only",
      x_id = paste0("WCVP:", xk$accepted_id), y_id = paste0("WCVP:", xk$accepted_id),
      provider = "WCVP", version = wcvp$version, accepted_rank = xk$accepted_rank,
      stage = "wcvp", review_status = "pending", review_note = NA_character_,
      evidence_detail = paste0("WCVP@", wcvp$version, "; accepted ID: ", xk$accepted_id,
        "; accepted name: ", xk$accepted_name, "; rank: ", xk$accepted_rank,
        "; input status: ", xk$name_status, "; target status: ", target_status,
        "; exact local identity; review only")
    )
  }
  candidates <- .pr_merge_review_candidates(reconciliation$candidates, additions)
  pair <- function(x) if (nrow(x)) paste(x$name_x, x$name_y, sep = "\034") else character()
  support <- match(pair(candidates), pair(additions))
  supported <- !is.na(support)
  # Keep primary evidence and review decisions, but make support from the
  # current local snapshot explicit even when an ordinary/TNRS pair came first.
  candidates$wcvp_supported <- supported
  candidates$wcvp_version <- rep(wcvp$version, nrow(candidates))
  candidates$wcvp_fingerprint <- rep(index$fingerprint, nrow(candidates))
  candidates$wcvp_accepted_id <- additions$x_id[support]
  candidates$wcvp_accepted_rank <- additions$accepted_rank[support]
  if (nrow(candidates)) {
    local <- candidates$stage %in% "wcvp"
    pending <- candidates$review_status %in% c("pending", "superseded")
    candidates$review_status[local & pending & !supported] <- "superseded"
    candidates$review_status[local & pending & supported] <- "pending"
    for (column in c("identity_state", "x_id", "y_id", "provider", "version", "accepted_rank")) {
      rows <- which(local & supported)
      candidates[[column]][rows] <- additions[[column]][support[rows]]
    }
  }
  reconciliation$candidates <- candidates
  previous <- reconciliation$wcvp$queries %||% list()
  receipt <- list(
    version = wcvp$version, source = "WCVP", fingerprint = index$fingerprint,
    record_count = index$record_count, query_time = Sys.time(),
    input_audit = x, target_audit = y, candidates = additions
  )
  reconciliation$wcvp <- receipt
  reconciliation$wcvp$queries <- c(previous, list(receipt))
  reconciliation
}

# Preserve rank tokens. Collapsing to species here can manufacture identity.
.pr_wcvp_key <- function(x) {
  tolower(trimws(gsub("[[:space:]]+", " ", gsub("_", " ", as.character(x), fixed = TRUE))))
}

.pr_validate_wcvp <- function(wcvp) {
  if (!is.list(wcvp) || !all(c("names", "version") %in% names(wcvp)) ||
      !is.data.frame(wcvp$names)) {
    abort("`wcvp` must contain names (a data frame) and version (a frozen label).")
  }
  if (!is.character(wcvp$version) || length(wcvp$version) != 1L ||
      is.na(wcvp$version) || !nzchar(trimws(wcvp$version))) {
    abort("`wcvp$version` must be one non-empty frozen version label.")
  }
  columns <- c("plant_name_id", "taxon_name", "taxon_status", "taxon_rank",
               "accepted_plant_name_id")
  missing <- setdiff(columns, names(wcvp$names))
  if (length(missing)) abort(paste0("`wcvp$names` is missing: ", paste(missing, collapse = ", ")))
  w <- as.data.frame(lapply(wcvp$names[, columns, drop = FALSE], as.character),
                     stringsAsFactors = FALSE)
  if (anyNA(w$plant_name_id) || any(!nzchar(trimws(w$plant_name_id))) ||
      anyDuplicated(w$plant_name_id)) abort("WCVP plant_name_id values must be non-empty and unique.")
  w
}

.pr_wcvp_index <- function(wcvp, requested) {
  w <- .pr_validate_wcvp(wcvp)
  status <- tolower(trimws(w$taxon_status))
  terminal <- match(w$accepted_plant_name_id, w$plant_name_id)
  accepted <- status %in% "accepted"
  terminal[accepted] <- which(accepted)
  terminal[!status %in% c("accepted", "synonym", "invalid", "illegitimate", "orthographic")] <- NA_integer_
  # Pointer jumping resolves chains in batches. A cycle or excessive chain
  # remains unresolved after this finite bound, never an invented identity.
  for (iteration in seq_len(100L)) {
    pending <- which(!is.na(terminal) & !accepted[terminal])
    if (!length(pending)) break
    terminal[pending] <- terminal[terminal[pending]]
  }
  terminal[!is.na(terminal) & !accepted[terminal]] <- NA_integer_
  rank <- tolower(trimws(w$taxon_rank[terminal]))
  valid_rank <- rank %in% c("species", "subspecies", "variety", "subvariety", "form",
                           "nothospecies", "nothosubspecies", "nothovariety", "nothoform",
                           "nothosubsp.", "nothovar.", "nothof.")
  terminal[!valid_rank] <- NA_integer_
  keys <- .pr_wcvp_key(w$taxon_name)
  needed <- unique(.pr_wcvp_key(requested))
  rows <- which(!is.na(keys) & nzchar(keys) & keys %in% needed)
  # Summarize ambiguity in vectors rather than constructing one data frame
  # per name. Full inventories contain hundreds of thousands of groups.
  pairs <- unique(data.frame(key = keys[rows], id = w$plant_name_id[terminal[rows]],
                             stringsAsFactors = FALSE))
  first <- rows[!duplicated(keys[rows])]
  at <- terminal[first]
  table <- data.frame(key = keys[first], status = rep("resolved", length(first)),
    accepted_id = w$plant_name_id[at], accepted_name = w$taxon_name[at],
    accepted_rank = rank[first], stringsAsFactors = FALSE)
  ambiguous <- unique(pairs$key[duplicated(pairs$key) | duplicated(pairs$key, fromLast = TRUE)])
  table$status[table$key %in% ambiguous] <- "ambiguous"
  table$status[table$key %in% pairs$key[is.na(pairs$id)]] <- "unresolved"
  table$accepted_id[table$status != "resolved"] <- NA_character_
  table$accepted_name[table$status != "resolved"] <- NA_character_
  statuses <- unique(data.frame(key = keys[rows], status = w$taxon_status[rows]))
  table$name_status <- statuses$status[match(table$key, statuses$key)]
  repeated <- duplicated(statuses$key) | duplicated(statuses$key, fromLast = TRUE)
  if (any(repeated)) {
    combined <- vapply(split(statuses$status[repeated], statuses$key[repeated]),
                       paste, collapse = ";", FUN.VALUE = character(1))
    at <- match(names(combined), table$key)
    table$name_status[at] <- unname(combined)
  }
  # Rank is meaningful only for a uniquely resolved accepted identity.
  table$accepted_rank[table$status != "resolved"] <- NA_character_
  table$identity <- ifelse(table$status == "resolved",
                           paste(table$accepted_id, table$accepted_rank, sep = ":"), NA_character_)
  if (requireNamespace("digest", quietly = TRUE)) {
    fingerprint <- paste0("sha256:", digest::digest(w, algo = "sha256"))
  } else {
    tmp <- tempfile("prepr-wcvp-")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(w, tmp, compress = FALSE, version = 2)
    fingerprint <- paste0("md5:", unname(tools::md5sum(tmp)))
  }
  list(table = table, fingerprint = fingerprint, record_count = nrow(w))
}

.pr_wcvp_lookup <- function(names, index) {
  key <- .pr_wcvp_key(names)
  rows <- match(key, index$table$key)
  out <- index$table[rows, , drop = FALSE]
  out$key <- key
  out$name <- as.character(names)
  out$status[is.na(rows)] <- "not_found"
  rownames(out) <- NULL
  tibble::as_tibble(out)
}
