# Identity evidence supplied by the caller ---------------------------------

utils::globalVariables(".data", package = "prepR4pcm")

.pr_validate_identity_backbone <- function(backbone, rank) {
  required <- c("records", "provider", "version", "rank")
  if (!is.list(backbone) || !all(required %in% names(backbone))) {
    cli::cli_abort("`backbone` must be a list with records, provider, version, and rank.")
  }
  if (!is.data.frame(backbone$records)) {
    cli::cli_abort("`backbone$records` must be a data frame.")
  }
  required_columns <- c("input", "accepted_name", "accepted_id", "status")
  missing <- setdiff(required_columns, names(backbone$records))
  if (length(missing)) {
    cli::cli_abort("`backbone$records` is missing required columns: {paste(missing, collapse = ', ')}.")
  }
  if (!is.character(backbone$provider) || length(backbone$provider) != 1L ||
      is.na(backbone$provider) || !nzchar(backbone$provider)) {
    cli::cli_abort("`backbone$provider` must be one non-empty character value.")
  }
  if (!is.character(backbone$version) || length(backbone$version) != 1L ||
      is.na(backbone$version) || !nzchar(backbone$version)) {
    cli::cli_abort("`backbone$version` must be one non-empty frozen version label.")
  }
  if (!identical(as.character(backbone$rank), as.character(rank))) {
    cli::cli_abort("`backbone$rank` must match the requested comparison rank ({rank}).")
  }

  records <- tibble::as_tibble(backbone$records)
  records$input <- as.character(records$input)
  records$accepted_name <- as.character(records$accepted_name)
  records$accepted_id <- as.character(records$accepted_id)
  records$status <- tolower(as.character(records$status))
  if (!"side" %in% names(records)) records$side <- "both"
  records$side <- as.character(records$side)
  if (anyNA(records$input) || any(!nzchar(records$input))) {
    cli::cli_abort("`backbone$records$input` cannot contain missing or empty names.")
  }
  if (anyNA(records$side) || any(!records$side %in% c("x", "y", "both"))) {
    cli::cli_abort("`backbone$records$side` must contain only `x`, `y`, or `both`.")
  }
  records$key <- as.character(pr_normalize_names(records$input, rank = rank))
  if (anyNA(records$key) || any(!nzchar(records$key))) {
    cli::cli_abort("Every backbone input must have a non-empty normalised name.")
  }

  # Fingerprint the exact validated table, including record order and side.
  if (requireNamespace("digest", quietly = TRUE)) {
    fingerprint <- digest::digest(records, algo = "sha256")
  } else {
    tmp <- tempfile("prepr4pcm-backbone-")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(records, tmp, version = 2)
    fingerprint <- unname(tools::md5sum(tmp))
  }

  structure(list(
    records = records,
    provider = backbone$provider,
    version = backbone$version,
    rank = as.character(rank),
    fingerprint = fingerprint,
    index = new.env(parent = emptyenv())
  ), class = "pr_identity_backbone")
}

.pr_identity_side_index <- function(backbone, side) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    cli::cli_abort("Identity-backbone matching requires the suggested package {.pkg dplyr}.")
  }
  cached <- get0(side, envir = backbone$index, inherits = FALSE)
  if (!is.null(cached)) return(cached)

  records <- backbone$records[
    backbone$records$side %in% c("both", side), , drop = FALSE
  ]
  valid <- records$status %in% c("accepted", "synonym")
  has_id <- !is.na(records$accepted_id) & nzchar(records$accepted_id)
  records$identity_evidence <- valid & has_id
  summary <- dplyr::summarise(
    dplyr::group_by(records, .data$key),
    ids = list(unique(.data$accepted_id[
      .data$identity_evidence
    ])),
    has_valid = any(.data$identity_evidence),
    has_unsupported = any(!.data$status %in%
                            c("accepted", "synonym", "not_found")) |
      any(.data$status %in% c("accepted", "synonym") &
            (is.na(.data$accepted_id) | !nzchar(.data$accepted_id))),
    has_not_found = any(.data$status %in% "not_found"),
    has_accepted = any(.data$status %in% "accepted"),
    .groups = "drop"
  )
  valid_records <- records[valid & has_id, , drop = FALSE]
  names_by_id <- dplyr::summarise(
    dplyr::group_by(valid_records, .data$key, .data$accepted_id),
    accepted_names = list(unique(.data$accepted_name[
      !is.na(.data$accepted_name) & nzchar(.data$accepted_name)
    ])),
    .groups = "drop"
  )
  cached <- list(summary = summary, names_by_id = names_by_id)
  assign(side, cached, envir = backbone$index)
  cached
}

.pr_identity_lookup <- function(names, backbone, side, rank) {
  stopifnot(inherits(backbone, "pr_identity_backbone"))
  side <- match.arg(side, c("x", "y"))
  key <- as.character(pr_normalize_names(names, rank = rank))
  if (!length(names)) {
    return(tibble::tibble(
      input = character(), key = character(), accepted_name = character(),
      status = character(), accepted_id = character(), identity_status = character(),
      provider = character(), version = character(), rank = character(), authority = character()
    ))
  }

  # Build one grouped side index and reuse it across exact, normalized,
  # synonym, and fuzzy stages. Re-grouping a large supplied backbone for every
  # stage dominates full-tree workloads.
  side_index <- .pr_identity_side_index(backbone, side)
  summary <- side_index$summary

  index <- match(key, summary$key)
  has_hits <- !is.na(index)
  ids <- rep(list(character()), length(names))
  ids[has_hits] <- summary$ids[index[has_hits]]
  n_ids <- lengths(ids)
  has_valid <- has_unsupported <- has_not_found <- has_accepted <-
    rep(FALSE, length(names))
  has_valid[has_hits] <- summary$has_valid[index[has_hits]]
  has_unsupported[has_hits] <- summary$has_unsupported[index[has_hits]]
  has_not_found[has_hits] <- summary$has_not_found[index[has_hits]]
  has_accepted[has_hits] <- summary$has_accepted[index[has_hits]]

  identity_status <- ifelse(
    !has_hits, "missing",
    ifelse(has_unsupported, "unknown",
    ifelse(n_ids > 1L, "ambiguous",
           ifelse(n_ids == 1L & has_valid, "known", "unknown"))
    )
  )
  status <- ifelse(
    !has_hits, "not_found",
    ifelse(has_unsupported, "unknown",
    ifelse(n_ids > 1L, "ambiguous",
           ifelse(n_ids == 0L | !has_valid,
                  ifelse(has_not_found, "not_found", "unknown"),
                  ifelse(has_accepted, "accepted", "synonym"))))
  )
  accepted_id <- rep(NA_character_, length(names))
  single_id <- which(n_ids == 1L & identity_status == "known")
  if (length(single_id)) {
    accepted_id[single_id] <- vapply(ids[single_id], `[[`, character(1), 1L)
  }

  names_by_id <- side_index$names_by_id
  pair_key <- function(keys, ids) paste(keys, ids, sep = "\034")
  name_index <- match(
    pair_key(key, accepted_id),
    pair_key(names_by_id$key, names_by_id$accepted_id)
  )
  accepted_name <- rep(NA_character_, length(names))
  has_name_index <- !is.na(name_index) & identity_status == "known"
  accepted_name[has_name_index] <- vapply(
    names_by_id$accepted_names[name_index[has_name_index]],
    function(x) if (length(x) == 1L) x[[1L]] else NA_character_,
    character(1)
  )

  tibble::tibble(
    input = as.character(names), key = key,
    accepted_name = accepted_name, status = status,
    accepted_id = accepted_id, identity_status = identity_status,
    provider = backbone$provider, version = backbone$version,
    rank = backbone$rank, authority = backbone$provider
  )
}

.pr_authority_identity_lookup <- function(names, authority, db_version, rank,
                                          quiet = TRUE) {
  rows <- pr_lookup_authority(names, authority, db_version, quiet = quiet)
  resolved_version <- attr(rows, "resolved_version")
  requested <- as.character(names)
  row_key <- as.character(pr_normalize_names(rows$input, rank = rank))
  requested_key <- as.character(pr_normalize_names(requested, rank = rank))
  rows <- rows[match(requested_key, row_key), , drop = FALSE]
  rows$input <- requested
  rows$key <- as.character(pr_normalize_names(rows$input, rank = rank))
  rows$accepted_id <- if ("taxon_id" %in% names(rows)) {
    as.character(rows$taxon_id)
  } else {
    rep(NA_character_, nrow(rows))
  }
  if (!"status" %in% names(rows)) rows$status <- rep("unknown", nrow(rows))
  rows$provider <- as.character(authority)
  if (is.null(resolved_version)) resolved_version <- db_version %||% "latest"
  rows$version <- rep(as.character(resolved_version), length.out = nrow(rows))
  rows$rank <- as.character(rank)
  # GNverifier's recordId is not a canonical accepted-taxon ID. Until an
  # adapter proves otherwise, it contributes synonym names only, not identity.
  rows$status[is.na(rows$status)] <- "unknown"
  comparable <- !identical(authority, "gnverifier") &
    rows$status %in% c("accepted", "synonym") &
    !is.na(rows$accepted_id) & nzchar(rows$accepted_id) &
    !is.na(rows$version) & nzchar(rows$version)
  rows$identity_status <- ifelse(comparable, "known", "unknown")
  rows
}

.pr_check_identity_candidates <- function(candidates, evidence_x, evidence_y) {
  if (!nrow(candidates)) {
    candidates$identity_state <- character()
    candidates$eligible <- logical()
    candidates$reason <- character()
    candidates$x_id <- character()
    candidates$y_id <- character()
    candidates$provider <- character()
    candidates$version <- character()
    return(candidates)
  }

  get_evidence <- function(names, evidence) {
    if (is.null(evidence) || !nrow(evidence)) return(rep(NA_integer_, length(names)))
    match(as.character(pr_normalize_names(names, rank = evidence$rank[[1L]])), evidence$key)
  }
  ix <- get_evidence(candidates$name_x, evidence_x)
  iy <- get_evidence(candidates$name_y, evidence_y)
  x_known <- !is.na(ix) & evidence_x$identity_status[ix] == "known"
  y_known <- !is.na(iy) & evidence_y$identity_status[iy] == "known"
  same_source <- rep(FALSE, nrow(candidates))
  has_both <- !is.na(ix) & !is.na(iy)
  source_x <- evidence_x$provider[ix[has_both]]
  source_y <- evidence_y$provider[iy[has_both]]
  version_x <- evidence_x$version[ix[has_both]]
  version_y <- evidence_y$version[iy[has_both]]
  rank_x <- evidence_x$rank[ix[has_both]]
  rank_y <- evidence_y$rank[iy[has_both]]
  same_source[has_both] <- !is.na(source_x) & !is.na(source_y) &
    !is.na(version_x) & !is.na(version_y) & !is.na(rank_x) & !is.na(rank_y) &
    source_x == source_y & version_x == version_y & rank_x == rank_y
  x_id <- rep(NA_character_, nrow(candidates))
  y_id <- rep(NA_character_, nrow(candidates))
  x_id[x_known] <- evidence_x$accepted_id[ix[x_known]]
  y_id[y_known] <- evidence_y$accepted_id[iy[y_known]]
  comparable <- x_known & y_known & same_source
  same <- comparable & !is.na(x_id) & !is.na(y_id) & x_id == y_id
  conflict <- comparable & !same
  state <- ifelse(conflict, "conflict", ifelse(same, "same", "unknown"))
  out <- candidates
  out$identity_state <- state
  out$eligible <- same
  out$reason <- ifelse(conflict, "distinct_accepted_ids", "identity_not_established")
  out$x_id <- x_id
  out$y_id <- y_id
  out$provider <- ifelse(has_both & same_source,
                         evidence_x$provider[pmax(ix, 1L)], NA_character_)
  out$version <- ifelse(has_both & same_source,
                        evidence_x$version[pmax(ix, 1L)], NA_character_)
  out
}
