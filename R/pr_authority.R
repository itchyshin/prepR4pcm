# Taxonomic authority lookup -----------------------------------------------

# Session-level cache for synonym lookups
.pr_cache <- new.env(parent = emptyenv())

#' Valid taxonomic authorities
#'
#' Returns the set of authority codes that the package accepts when
#' resolving species-name synonyms via \pkg{taxadb}. The list mirrors
#' \pkg{taxadb}'s own documented providers (see `?taxadb::td_create`)
#' so that every value here is genuinely callable in the underlying
#' database:
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
  c("col", "itis", "gbif", "ncbi", "ott", "itis_test")
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
#' @param authority Character(1) or NULL. The user-supplied value.
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
    cli::cli_abort(
      c(
        "{.val {authority}} is not a supported authority.",
        "x" = paste0(
          "{.val {authority}} was listed in earlier versions of the ",
          "package but is not actually supported by {.pkg taxadb} ",
          "v22.12 (the database we test against)."
        ),
        "i" = "Removed authorities: {.val {.pr_removed_authorities()}}.",
        ">" = "Switch to one of: {.val {pr_valid_authorities()}}.",
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
#' @param authority Character(1). Taxonomic authority code.
#' @param db_version Character(1) or NULL. Database version.
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


#' Look up names in a taxonomic authority
#'
#' For each name, queries the local taxadb database and returns the accepted
#' name, taxonomic status, and taxon ID.
#'
#' @param names Character vector of scientific names.
#' @param authority Character(1). Authority code (e.g., `"col"`).
#' @param db_version Character(1) or NULL.
#'
#' @return A tibble with columns: `input`, `accepted_name`, `status`,
#'   `taxon_id`, `authority`.
#' @keywords internal
pr_lookup_authority <- function(names, authority = "col", db_version = NULL) {
  pr_ensure_db(authority, db_version)

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
  cache_key <- paste0("lookup_", authority, "_",
                       db_version %||% "latest")
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
    # Batch query: taxadb::filter_name accepts a character vector
    all_hits <- tryCatch(
      taxadb::filter_name(
        to_lookup,
        provider = authority,
        version = db_version
      ),
      error = function(e) {
        cli_alert_warning(
          "taxadb lookup failed for {.val {authority}}: {conditionMessage(e)}. Names will be recorded as not found."
        )
        NULL
      }
    )

    # Process results per name
    new_results <- lapply(to_lookup, function(name) {
      if (is.null(all_hits) || nrow(all_hits) == 0) {
        return(tibble(
          input         = name,
          accepted_name = NA_character_,
          status        = "not_found",
          taxon_id      = NA_character_,
          authority     = authority
        ))
      }

      hits <- all_hits[all_hits$scientificName == name |
                         all_hits$input == name, , drop = FALSE]

      # taxadb may use an 'input' column to match query names
      if (nrow(hits) == 0 && "input" %in% names(all_hits)) {
        hits <- all_hits[all_hits$input == name, , drop = FALSE]
      }

      if (nrow(hits) == 0) {
        return(tibble(
          input         = name,
          accepted_name = NA_character_,
          status        = "not_found",
          taxon_id      = NA_character_,
          authority     = authority
        ))
      }

      # Prefer accepted name
      accepted <- hits[hits$taxonomicStatus == "accepted", ]

      if (nrow(accepted) > 0) {
        return(tibble(
          input         = name,
          accepted_name = accepted$scientificName[1],
          status        = "accepted",
          taxon_id      = accepted$taxonID[1],
          authority     = authority
        ))
      }

      # Name is a synonym — follow acceptedNameUsageID
      syn <- hits[1, ]
      accepted_id <- syn$acceptedNameUsageID

      if (!is.na(accepted_id) && nchar(accepted_id) > 0) {
        tryCatch({
          accepted_hit <- taxadb::filter_id(
            accepted_id,
            provider = authority,
            version = db_version
          )
          if (!is.null(accepted_hit) && nrow(accepted_hit) > 0) {
            return(tibble(
              input         = name,
              accepted_name = accepted_hit$scientificName[1],
              status        = syn$taxonomicStatus[1] %||% "synonym",
              taxon_id      = accepted_id,
              authority     = authority
            ))
          }
        }, error = function(e) NULL)
      }

      # Could not resolve
      tibble(
        input         = name,
        accepted_name = NA_character_,
        status        = syn$taxonomicStatus[1] %||% "unknown",
        taxon_id      = syn$taxonID[1],
        authority     = authority
      )
    })

    new_results_df <- do.call(rbind, new_results)

    # Update cache
    if (!is.null(cached)) {
      assign(cache_key, rbind(cached, new_results_df), envir = .pr_cache)
    } else {
      assign(cache_key, new_results_df, envir = .pr_cache)
    }
  }

  # Return results for all requested names
  full_cache <- get(cache_key, envir = .pr_cache)
  full_cache[full_cache$input %in% unique_names, ]
}
