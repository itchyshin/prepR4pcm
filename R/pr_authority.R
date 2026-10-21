# Taxonomic authority lookup -----------------------------------------------

# Session-level cache for synonym lookups
.pr_cache <- new.env(parent = emptyenv())

#' Valid taxonomic authorities
#'
#' Returns the set of authority codes that the package accepts when
#' resolving species-name synonyms. Most are served by \pkg{taxadb}
#' (a local database mirroring the providers documented in
#' `?taxadb::td_create`); `"gnverifier"` is the one HTTP-backed
#' authority, calling the Global Names Architecture
#' [verifier service](https://verifier.globalnames.org/) instead of a
#' local database.
#'
#' \describe{
#'   \item{`"col"`}{Catalogue of Life. The default and a sensible
#'     starting point for most taxa.}
#'   \item{`"itis"`}{Integrated Taxonomic Information System. Strong
#'     coverage for North American vertebrates and plants.}
#'   \item{`"gbif"`}{GBIF backbone. Wider coverage; captures more
#'     recent synonymy.}
#'   \item{`"ncbi"`}{NCBI Taxonomy. Best when you are working with
#'     sequence data.}
#'   \item{`"ott"`}{Open Tree of Life synthetic taxonomy. Useful when
#'     your downstream phylogeny is from the Open Tree synthesis. We
#'     restrict the schema to `"dwc"` (Darwin Core) when calling
#'     `taxadb::td_create()` because the `"common"` schema does not
#'     ship for OTT under \pkg{taxadb} v22.12.}
#'   \item{`"itis_test"`}{A small bundled subset of ITIS, cached
#'     locally with \pkg{taxadb} for testing. Intended for examples
#'     and unit tests; not for analysis.}
#'   \item{`"gnverifier"`}{Global Names verifier --- HTTP-backed
#'     verification against ~100 authoritative sources (Catalogue of
#'     Life, ITIS, GBIF, NCBI, Open Tree, ...). No local database is
#'     downloaded; requires network access and the \pkg{httr2} package.
#'     Useful when you want broader source coverage than any single
#'     taxadb provider, or want to avoid the ~100 MB taxadb download.}
#' }
#'
#' Five authority codes that previous versions of the package
#' advertised --- `iucn`, `tpl`, `fb`, `slb`, `wd` --- are not on this
#' list. Empirical testing against \pkg{taxadb} v22.12 showed that
#' `iucn` errors with a schema mismatch and the other four are not
#' \pkg{taxadb} providers at all. Anyone who was passing one of those
#' values was getting a hard error; passing them now produces a
#' helpful migration message instead.
#'
#' @keywords internal
pr_valid_authorities <- function() {
  c("col", "itis", "gbif", "ncbi", "ott", "itis_test", "gnverifier")
}

# Authorities that earlier versions of the package incorrectly listed
# as supported. We keep the set so that we can produce a targeted
# migration error if a user passes one of these (rather than the
# generic "not a valid authority" message).
.pr_removed_authorities <- function() {
  c("iucn", "tpl", "fb", "slb", "wd")
}


#' Validate a user-supplied authority string
#'
#' Used by every entry-point function that accepts `authority`.
#' Lower-cases the input, returns it unchanged if `NULL` (synonym
#' resolution skipped), errors with a helpful message if the value
#' was previously listed but is no longer supported, or with a
#' standard "unknown authority" message otherwise.
#'
#' @param authority A length-1 character vector or NULL. The user-supplied value.
#' @param call Calling environment, for `cli_abort(call = ...)`.
#' @return The lower-cased, validated authority (or NULL).
#' @keywords internal
pr_validate_authority <- function(authority, call = caller_env()) {
  if (is.null(authority)) return(NULL)
  authority <- tolower(authority)

  if (authority %in% pr_valid_authorities()) {
    return(authority)
  }

  if (authority %in% .pr_removed_authorities()) {
    removed <- .pr_removed_authorities()
    valid <- pr_valid_authorities()
    cli::cli_abort(
      c(
        "{.val {authority}} is not a supported authority.",
        "x" = paste0(
          "{.val {authority}} was listed in earlier versions of the ",
          "package but is not actually supported by {.pkg taxadb} ",
          "v22.12 (the database we test against)."
        ),
        "i" = "Removed authorities: {.val {removed}}.",
        ">" = "Switch to one of: {.val {valid}}.",
        ">" = "Or pass {.code authority = NULL} to skip synonym resolution."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "Unknown authority: {.val {authority}}.",
      "i" = "Valid options: {.val {pr_valid_authorities()}}."
    ),
    call = call
  )
}

#' Ensure the taxadb local database is available
#'
#' Downloads the database for the specified authority if not already cached.
#'
#' @param authority A length-1 character vector. Taxonomic authority code.
#' @param db_version A length-1 character vector or NULL. Database version.
#'
#' @return Invisibly returns the authority string.
#' @keywords internal
pr_ensure_db <- function(authority, db_version = NULL) {
  if (!requireNamespace("taxadb", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Synonym resolution requires the {.pkg taxadb} package.",
        "i" = 'Install with: {.code install.packages("taxadb")}'
      ),
      call = caller_env()
    )
  }

  # dbplyr may be installed but fail to load (e.g. dbplyr 2.6.0 with
  # dplyr 1.1.4). Check the namespace before taxadb's interactive installer
  # mistakes that failure for a missing package and repeatedly prompts.
  .pr_check_dbplyr()

  # We restrict to the Darwin Core ("dwc") schema rather than letting
  # taxadb default to schema = c("dwc", "common"). The cascade only
  # consumes scientific names (dwc); the `common` schema (vernacular
  # names) is unused. More importantly, the "common" schema does not
  # ship for OTT under taxadb v22.12, so the default would error on
  # otherwise-valid OTT calls. Restricting to "dwc" keeps every
  # authority on pr_valid_authorities() callable.
  args <- list(provider = authority, schema = "dwc")
  if (!is.null(db_version)) args$version <- db_version

  tryCatch(
    {
      cli_alert_info(
        "Ensuring local {.val {toupper(authority)}} database is available..."
      )
      do.call(taxadb::td_create, args)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to create/access taxadb database.",
          "x" = conditionMessage(e),
          "i" = 'Try running {.code taxadb::td_create("{authority}", schema = "dwc")} manually.'
        ),
        call = caller_env()
      )
    }
  )

  invisible(authority)
}

.pr_check_dbplyr <- function(load = loadNamespace) {
  tryCatch(
    load("dbplyr"),
    error = function(e) {
      cli::cli_abort(
        c(
          "Synonym resolution could not load {.pkg dbplyr}.",
          "x" = conditionMessage(e),
          "i" = 'Install or update both packages with {.code install.packages(c("dplyr", "dbplyr"))}.',
          "i" = "Restart R, then retry synonym resolution."
        ),
        call = caller_env()
      )
    }
  )
  invisible(NULL)
}


#' Look up names in a taxonomic authority
#'
#' For each name, queries the configured authority and returns the accepted
#' name, taxonomic status, and taxon ID. Most authorities are backed by a
#' local \pkg{taxadb} database; `authority = "gnverifier"` calls the
#' Global Names HTTP verifier instead.
#'
#' @param names Character vector of scientific names.
#' @param authority A length-1 character vector. Authority code (e.g.,
#'   `"col"`). Pass `"gnverifier"` for HTTP-backed verification against
#'   ~100 sources; see `vignette("getting-started")` for the trade-off.
#' @param db_version A length-1 character vector or NULL. Ignored when
#'   `authority = "gnverifier"` (the GNverifier service does not expose
#'   per-snapshot versions); a non-NULL value emits a single warning.
#'
#' @param quiet Logical. Suppress lookup progress when `TRUE`. More than 500
#'   uncached names show batch counts and an interactive progress bar. Queries
#'   run in batches of at most 1000 names; completed batches enter the cache.
#' @return A tibble with columns: `input`, `accepted_name`, `status`,
#'   `taxon_id`, `authority`.
#' @keywords internal
pr_lookup_authority <- function(names, authority = "col", db_version = NULL,
                                quiet = FALSE) {
  resolved_version <- if (authority == "gnverifier") NA_character_ else {
    .pr_resolved_taxadb_version(authority, db_version)
  }
  effective_version <- if (!is.null(db_version)) {
    as.character(db_version)
  } else if (length(resolved_version) == 1L && !is.na(resolved_version)) {
    resolved_version
  } else {
    NULL
  }
  if (authority != "gnverifier") {
    pr_ensure_db(authority, effective_version)
  }

  unique_names <- unique(names[!is.na(names)])

  if (length(unique_names) == 0) {
    return(tibble(
      input         = character(),
      accepted_name = character(),
      status        = character(),
      taxon_id      = character(),
      authority     = character()
    ))
  }

  # Check session cache
  cache_version <- if (length(resolved_version) == 1L &&
                       !is.na(resolved_version) && nzchar(resolved_version)) {
    resolved_version
  } else {
    "latest_unresolved"
  }
  cache_key <- paste0("lookup_", authority, "_", cache_version)
  cached <- if (exists(cache_key, envir = .pr_cache)) {
    get(cache_key, envir = .pr_cache)
  } else {
    NULL
  }

  # Identify which names need lookup
  if (!is.null(cached)) {
    already_done <- intersect(unique_names, cached$input)
    to_lookup <- setdiff(unique_names, cached$input)
  } else {
    already_done <- character()
    to_lookup <- unique_names
  }

  if (length(to_lookup) > 0) {
    # Bounded queries also bound taxadb's per-name result scans and HTTP payloads.
    # Cache each completed batch so an interruption does not discard all work.
    n_lookup <- length(to_lookup)
    show_progress <- !quiet && n_lookup > 500L
    if (show_progress) {
      progress <- cli::cli_progress_bar(
        name = paste(toupper(authority), "lookup"), total = n_lookup,
        .auto_close = FALSE
      )
      completed <- FALSE
      on.exit(cli::cli_progress_done(id = progress,
                                    result = if (completed) "done" else "failed"),
              add = TRUE)
    }
    for (start in seq.int(1L, n_lookup, by = 1000L)) {
      end <- min(start + 999L, n_lookup)
      batch <- to_lookup[start:end]
      if (show_progress) {
        cli_alert_info("{toupper(authority)} lookup: querying names {start}-{end} of {n_lookup} ({start - 1L} completed; {length(already_done)} cached).")
      }
      new_results_df <- if (authority == "gnverifier") {
        .pr_lookup_gnverifier(batch, if (start == 1L) db_version else NULL)
      } else {
        .pr_lookup_taxadb(batch, authority, effective_version)
      }
      cached <- if (is.null(cached)) new_results_df else rbind(cached, new_results_df)
      assign(cache_key, cached, envir = .pr_cache)
      if (show_progress) cli::cli_progress_update(id = progress, set = end)
    }
    if (show_progress) {
      completed <- TRUE
      cli_alert_info("{toupper(authority)} lookup: {n_lookup} names completed.")
    }
  }

  # Return results for all requested names
  full_cache <- get(cache_key, envir = .pr_cache)
  out <- full_cache[full_cache$input %in% unique_names, ]
  attr(out, "resolved_version") <- resolved_version
  out
}

.pr_resolved_taxadb_version <- function(authority, db_version = NULL) {
  if (!is.null(db_version)) return(as.character(db_version))
  if (!requireNamespace("taxadb", quietly = TRUE)) return(NA_character_)
  cache_key <- paste0("resolved_taxadb_version_", authority)
  if (exists(cache_key, envir = .pr_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .pr_cache, inherits = FALSE))
  }
  version <- tryCatch({
    if (!"latest_version" %in% getNamespaceExports("taxadb")) return(NA_character_)
    taxadb::latest_version()
  }, error = function(e) NA_character_)
  if (length(version) != 1L || is.na(version) || !nzchar(version)) {
    version <- NA_character_
  }
  assign(cache_key, as.character(version), envir = .pr_cache)
  as.character(version)
}

.pr_taxadb_accepted_target <- function(accepted_hit, accepted_id) {
  if (is.null(accepted_hit) || nrow(accepted_hit) == 0L ||
      !all(c("taxonomicStatus", "taxonID", "scientificName") %in%
           names(accepted_hit))) {
    return(NULL)
  }
  status <- tolower(as.character(accepted_hit$taxonomicStatus))
  if (anyNA(status) || any(status != "accepted")) return(NULL)

  target_ids <- unique(as.character(accepted_hit$taxonID))
  target_ids <- target_ids[!is.na(target_ids) & nzchar(target_ids)]
  target_names <- unique(as.character(accepted_hit$scientificName))
  target_names <- target_names[!is.na(target_names) & nzchar(target_names)]
  if (length(target_ids) != 1L || length(target_names) != 1L ||
      !identical(target_ids[[1L]], as.character(accepted_id))) {
    return(NULL)
  }

  list(accepted_name = target_names[[1L]], accepted_id = target_ids[[1L]])
}

.pr_taxadb_status_class <- function(status) {
  status <- tolower(as.character(status))
  ifelse(is.na(status), "unknown",
         ifelse(status == "accepted", "accepted",
                ifelse(status == "synonym", "synonym", "unknown")))
}


#' Look up names in a taxadb-backed authority
#'
#' Internal helper extracted from `pr_lookup_authority()` so the
#' taxadb path can sit alongside the gnverifier path without
#' duplicating the cache machinery in `pr_lookup_authority()`.
#'
#' @param to_lookup Character vector of names to look up.
#' @param authority A length-1 character vector. Authority code.
#' @param db_version A length-1 character vector or NULL.
#' @return A tibble with the same 5 columns as `pr_lookup_authority()`.
#' @keywords internal
.pr_lookup_taxadb <- function(to_lookup, authority, db_version = NULL) {
    lookup_failed <- FALSE
    # Batch query: taxadb::filter_name accepts a character vector.
    # Only forward `version` when the caller supplied one — taxadb 0.2.1
    # errors on `version = NULL` because parse_schema() runs
    # `if (version == "latest")` and gets logical(0). Omitting the
    # argument lets taxadb fall back to latest_version().
    filter_args <- list(to_lookup, provider = authority)
    if (!is.null(db_version)) filter_args$version <- db_version
    all_hits <- tryCatch(
      do.call(taxadb::filter_name, filter_args),
      error = function(e) {
        lookup_failed <<- TRUE
        cli_alert_warning(
          "taxadb lookup failed for {.val {authority}}: {conditionMessage(e)}. Names will be recorded as lookup errors."
        )
        NULL
      }
    )

    # Process results per name
    new_results <- lapply(to_lookup, function(name) {
      if (lookup_failed) {
        return(tibble(
          input         = name,
          accepted_name = NA_character_,
          status        = "lookup_error",
          taxon_id      = NA_character_,
          authority     = authority
        ))
      }
      if (is.null(all_hits) || nrow(all_hits) == 0) {
        return(tibble(
          input = name, accepted_name = NA_character_, status = "not_found",
          taxon_id = NA_character_, authority = authority
        ))
      }

      # Match on scientificName, and on `input` only if taxadb provides
      # that column. taxadb 0.2.1 returns scientificName but not input,
      # so unconditionally referencing all_hits$input returns NULL and
      # `NULL == name` yields logical(0), which then collides with the
      # length-N scientificName mask under tibble's strict subscripting.
      match_mask <- !is.na(all_hits$scientificName) & all_hits$scientificName == name
      if ("input" %in% names(all_hits)) {
        match_mask <- match_mask | (!is.na(all_hits$input) & all_hits$input == name)
      }
      hits <- all_hits[match_mask, , drop = FALSE]

      if (nrow(hits) == 0) {
        return(tibble(
          input         = name,
          accepted_name = NA_character_,
          status        = "not_found",
          taxon_id      = NA_character_,
          authority     = authority
        ))
      }

      # A record is identity-safe only when every matching accepted/synonym
      # row resolves to one canonical accepted taxon. Never choose the first
      # hit when the source supplies conflicting IDs.
      status_class <- .pr_taxadb_status_class(hits$taxonomicStatus)
      accepted <- hits[status_class == "accepted", , drop = FALSE]
      synonym <- hits[status_class == "synonym", , drop = FALSE]
      unsupported <- hits[status_class == "unknown", , drop = FALSE]
      if (nrow(unsupported) > 0L) {
        # COL's "ambiguous synonym" records can still point to a single
        # accepted-name target. Preserve that text only as a review hint:
        # the status remains unknown and no canonical ID is exposed, so the
        # identity guard cannot use it for an automatic match.
        candidate_name <- NA_character_
        unsupported_status <- tolower(trimws(as.character(
          hits$taxonomicStatus[status_class == "unknown"]
        )))
        if (all(status_class == "unknown") &&
            length(unsupported_status) > 0L &&
            !anyNA(unsupported_status) &&
            all(unsupported_status == "ambiguous synonym") &&
            "acceptedNameUsageID" %in% names(hits)) {
          target_ids <- unique(as.character(
            hits$acceptedNameUsageID[status_class == "unknown"]
          ))
          target_ids <- target_ids[!is.na(target_ids) & nzchar(target_ids)]
          if (length(target_ids) == 1L) {
            target <- tryCatch({
              id_args <- list(target_ids[[1L]], provider = authority)
              if (!is.null(db_version)) id_args$version <- db_version
              .pr_taxadb_accepted_target(
                do.call(taxadb::filter_id, id_args), target_ids[[1L]]
              )
            }, error = function(e) NULL)
            if (!is.null(target)) candidate_name <- target$accepted_name
          }
        }
        return(tibble(input = name, accepted_name = candidate_name,
                      status = "unknown", taxon_id = NA_character_,
                      authority = authority))
      }

      has_complete_ids <- function(records, column) {
        if (nrow(records) == 0L) return(TRUE)
        if (!column %in% names(records)) return(FALSE)
        ids <- as.character(records[[column]])
        length(ids) == nrow(records) && !anyNA(ids) && all(nzchar(ids))
      }
      if (!has_complete_ids(accepted, "taxonID") ||
          !has_complete_ids(synonym, "acceptedNameUsageID")) {
        return(tibble(input = name, accepted_name = NA_character_,
                      status = "unknown", taxon_id = NA_character_,
                      authority = authority))
      }

      accepted_ids <- unique(as.character(accepted$taxonID))
      accepted_ids <- accepted_ids[!is.na(accepted_ids) & nzchar(accepted_ids)]
      synonym_ids <- unique(as.character(synonym$acceptedNameUsageID))
      synonym_ids <- synonym_ids[!is.na(synonym_ids) & nzchar(synonym_ids)]
      canonical_ids <- unique(c(accepted_ids, synonym_ids))
      if (length(canonical_ids) > 1L) {
        return(tibble(input = name, accepted_name = NA_character_,
                      status = "ambiguous", taxon_id = NA_character_,
                      authority = authority))
      }

      if (length(accepted_ids) == 1L) {
        accepted_names <- unique(as.character(accepted$scientificName))
        accepted_names <- accepted_names[!is.na(accepted_names) & nzchar(accepted_names)]
        if (length(accepted_names) != 1L) {
          return(tibble(input = name, accepted_name = NA_character_,
                        status = "ambiguous", taxon_id = NA_character_,
                        authority = authority))
        }
        return(tibble(
          input         = name,
          accepted_name = accepted_names[[1L]],
          status        = "accepted",
          taxon_id      = accepted_ids[[1L]],
          authority     = authority
        ))
      }

      # Name is a synonym — follow acceptedNameUsageID
      if (length(synonym_ids) != 1L) {
        return(tibble(input = name, accepted_name = NA_character_,
                      status = if (nrow(synonym)) "unresolved_synonym" else "unknown",
                      taxon_id = NA_character_, authority = authority))
      }
      accepted_id <- synonym_ids[[1L]]

      accepted_target <- if (!is.na(accepted_id) && nzchar(accepted_id)) {
        tryCatch({
          id_args <- list(accepted_id, provider = authority)
          if (!is.null(db_version)) id_args$version <- db_version
          .pr_taxadb_accepted_target(
            do.call(taxadb::filter_id, id_args), accepted_id
          )
        }, error = function(e) NULL)
      } else NULL
      if (!is.null(accepted_target)) {
        return(tibble(
          input         = name,
          accepted_name = accepted_target$accepted_name,
          status        = "synonym",
          taxon_id      = accepted_target$accepted_id,
          authority     = authority
        ))
      }

      # Could not resolve
      tibble(
        input         = name,
        accepted_name = NA_character_,
        status        = "unresolved_synonym",
        taxon_id      = NA_character_,
        authority     = authority
      )
    })

    do.call(rbind, new_results)
}


#' Look up names via the Global Names verifier (HTTP)
#'
#' Internal helper for `pr_lookup_authority(authority = "gnverifier")`.
#' POSTs the input vector to the
#' [Global Names verifier](https://verifier.globalnames.org/) and maps
#' each `bestResult` back to the 5-column tibble contract used by the
#' taxadb path. Returns all-rows-`not_found` and emits a single
#' warning on network failure, mirroring the taxadb branch's
#' degradation behaviour so the cascade above keeps running.
#'
#' @param names Character vector of names to verify.
#' @param db_version Ignored; emits a single warning if non-NULL.
#' @return A tibble with the same 5 columns as `pr_lookup_authority()`.
#' @keywords internal
.pr_lookup_gnverifier <- function(names, db_version = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.code authority = \"gnverifier\"} requires the {.pkg httr2} package.",
      "i" = 'Install with {.code install.packages("httr2")}.',
      ">" = "Or pick a taxadb-backed authority (e.g. {.val col})."
    ))
  }

  if (!is.null(db_version)) {
    cli::cli_warn(c(
      "{.arg db_version} is ignored when {.code authority = \"gnverifier\"}.",
      "i" = "The Global Names verifier does not expose per-snapshot versions."
    ))
  }

  not_found <- function(n) {
    tibble(
      input         = n,
      accepted_name = NA_character_,
      status        = rep("not_found", length(n)),
      taxon_id      = NA_character_,
      authority     = rep("gnverifier", length(n))
    )
  }

  body <- list(nameStrings = as.list(names))

  resp <- tryCatch(
    {
      req <- httr2::request("https://verifier.globalnames.org/api/v1/verifications")
      req <- httr2::req_body_json(req, body)
      req <- httr2::req_user_agent(
        req,
        "prepR4pcm (https://github.com/itchyshin/prepR4pcm)"
      )
      httr2::req_perform(req)
    },
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    cli::cli_warn(c(
      "GNverifier lookup failed: {conditionMessage(resp)}.",
      "i" = "Names will be recorded as not found; the rest of the cascade still runs."
    ))
    return(not_found(names))
  }

  parsed <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    cli::cli_warn(c(
      "GNverifier returned an unparseable response: {conditionMessage(parsed)}.",
      "i" = "Names will be recorded as not found."
    ))
    return(not_found(names))
  }

  # The verifier returns `{"names": [...]}` (since v1). Older deployments
  # used `namesList`. Accept either so a transparent server update does
  # not break us.
  results <- parsed[["names"]] %||% parsed[["namesList"]]
  if (is.null(results) || length(results) != length(names)) {
    cli::cli_warn(c(
      "GNverifier response shape did not match the request.",
      "i" = "Names will be recorded as not found."
    ))
    return(not_found(names))
  }

  rows <- lapply(seq_along(names), function(i) {
    entry <- results[[i]]
    best  <- entry[["bestResult"]]
    if (is.null(best)) {
      return(tibble(
        input         = names[[i]],
        accepted_name = NA_character_,
        status        = "not_found",
        taxon_id      = NA_character_,
        authority     = "gnverifier"
      ))
    }

    accepted <- best[["currentCanonicalSimple"]] %||%
                best[["currentCanonicalFull"]]   %||%
                best[["matchedCanonicalSimple"]] %||%
                NA_character_
    accepted <- if (length(accepted) && nzchar(accepted)) accepted else NA_character_

    match_type <- best[["matchType"]] %||% ""
    current    <- best[["currentName"]] %||% best[["currentCanonicalSimple"]] %||% ""
    matched    <- best[["matchedName"]] %||% best[["matchedCanonicalSimple"]] %||% ""

    status <- if (identical(match_type, "Exact") &&
                  nzchar(current) && nzchar(matched) &&
                  identical(current, matched)) {
      "accepted"
    } else {
      "synonym"
    }

    tibble(
      input         = names[[i]],
      accepted_name = as.character(accepted),
      status        = status,
      taxon_id      = as.character(best[["recordId"]] %||%
                                   best[["taxonId"]]  %||%
                                   NA_character_),
      authority     = "gnverifier"
    )
  })

  do.call(rbind, rows)
}
