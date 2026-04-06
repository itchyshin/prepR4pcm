# Taxonomic authority lookup -----------------------------------------------

# Session-level cache for synonym lookups
.pr_cache <- new.env(parent = emptyenv())

#' Valid taxonomic authorities
#'
#' @keywords internal
pr_valid_authorities <- function() {
  c("col", "itis", "gbif", "ncbi", "ott", "tpl", "fb", "slb", "wd", "iucn")
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
    abort(
      c(
        "Synonym resolution requires the {.pkg taxadb} package.",
        "i" = 'Install with: {.code install.packages("taxadb")}'
      ),
      call = caller_env()
    )
  }

  # taxadb::td_create downloads the DB if not present
  args <- list(provider = authority)
  if (!is.null(db_version)) args$version <- db_version

  tryCatch(
    {
      cli_alert_info(
        "Ensuring local {.val {toupper(authority)}} database is available..."
      )
      do.call(taxadb::td_create, args)
    },
    error = function(e) {
      abort(
        c(
          "Failed to create/access taxadb database.",
          "x" = conditionMessage(e),
          "i" = "Try running {.code taxadb::td_create(\"{authority}\")} manually."
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
      error = function(e) NULL
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
