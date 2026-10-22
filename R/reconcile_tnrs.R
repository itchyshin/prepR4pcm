#' Add botanical TNRS suggestions to a reconciliation for review
#'
#' Resolve currently unmatched input names with one selected botanical TNRS
#' reference source, then compare each returned accepted name with the actual
#' target-name inventory. Only exact normalized matches to one target name are
#' added as pending review candidates. TNRS suggestions do not become mapping
#' rows and are never applied automatically.
#'
#' @param reconciliation A [reconciliation] object from a reconciliation
#'   function.
#' @param source One botanical TNRS source: `"wcvp"`, `"wfo"`, or `"cact"`.
#'   The source is explicit so results from different taxonomic backbones are
#'   not combined.
#'
#' @return The reconciliation with a `tnrs` audit record and any eligible
#'   review-only candidates added to `reconcile_candidates()`.
#'
#' @details
#' This function is opt-in and requires the optional CRAN package `TNRS`. It
#' requests all service matches with the lowest supported score threshold so
#' that alternatives are retained. It records the raw response, warnings,
#' source and service metadata, client version, and query time. A target is
#' suggested only when the service's accepted name matches exactly, after the
#' package's ordinary name normalization, to one name in the reconciliation's
#' target inventory. Names absent from the tree and ambiguous normalized tree
#' labels remain in the audit record but are not candidates.
#'
#' The returned candidates have unknown identity status and are ineligible for
#' automatic mapping. Review them with [reconcile_review()]. Service resolution
#' is evidence for review, not proof that the returned taxonomic name is
#' correct.
#'
#' @examples
#' if (interactive() && requireNamespace("TNRS", quietly = TRUE)) {
#'   # Explicitly opt in to WCVP; inspect suggestions before accepting any.
#'   # resolved <- reconcile_tnrs(existing_reconciliation, source = "wcvp")
#'   # reconcile_review(resolved, type = "flagged")
#' }
#'
#' @export
reconcile_tnrs <- function(reconciliation, source = "wcvp") {
  validate_reconciliation(reconciliation)
  if (length(source) != 1L || is.na(source) || !is.character(source)) {
    abort("`source` must name exactly one TNRS source: 'wcvp', 'wfo', or 'cact'.",
          call = caller_env())
  }
  source <- tolower(trimws(source))
  if (!source %in% c("wcvp", "wfo", "cact")) {
    abort("`source` must be one of 'wcvp', 'wfo', or 'cact'.",
          call = caller_env())
  }

  mapping <- reconciliation$mapping
  input_names <- unique(mapping$name_x[
    mapping$in_x & mapping$match_type == "unresolved" &
      !is.na(mapping$name_x) & nzchar(trimws(mapping$name_x))
  ])
  if (!length(input_names)) return(reconciliation)

  if (!.pr_tnrs_client_available()) {
    abort(
      "The optional TNRS client is required for `reconcile_tnrs()`; install the CRAN package TNRS first.",
      call = caller_env()
    )
  }

  response <- tryCatch(
    .pr_tnrs_query(input_names, source),
    error = function(e) {
      abort(paste0("TNRS query failed: ", conditionMessage(e)), call = caller_env())
    }
  )
  if (is.null(response$results) || !is.data.frame(response$results)) {
    abort("TNRS returned an invalid response; expected a data.frame of results.",
          call = caller_env())
  }
  if (nrow(response$results) && !"Name_submitted" %in% names(response$results)) {
    abort("TNRS returned results without `Name_submitted`; cannot align its rows safely.",
          call = caller_env())
  }

  rank <- reconciliation$meta$rank %||% "species"
  norm <- function(x) .pr_tnrs_name_key(x, rank = rank)
  target_names <- unique(mapping$name_y[
    mapping$in_y & !is.na(mapping$name_y) & nzchar(trimws(mapping$name_y))
  ])
  target_norm <- norm(target_names)
  result <- response$results
  submitted <- if (nrow(result)) as.character(result$Name_submitted) else character()
  accepted <- if ("Accepted_name" %in% names(result)) {
    as.character(result$Accepted_name)
  } else rep(NA_character_, nrow(result))
  accepted_id <- if ("Accepted_name_id" %in% names(result)) {
    as.character(result$Accepted_name_id)
  } else rep(NA_character_, nrow(result))
  score <- if ("Overall_score" %in% names(result)) {
    as.character(result$Overall_score)
  } else rep(NA_character_, nrow(result))
  status <- if ("Taxonomic_status" %in% names(result)) {
    as.character(result$Taxonomic_status)
  } else rep(NA_character_, nrow(result))
  warnings <- if ("WarningsEng" %in% names(result)) {
    as.character(result$WarningsEng)
  } else rep(NA_character_, nrow(result))
  warning_codes <- if ("Warnings" %in% names(result)) {
    as.character(result$Warnings)
  } else rep(NA_character_, nrow(result))
  matched <- if ("Name_matched" %in% names(result)) {
    as.character(result$Name_matched)
  } else rep(NA_character_, nrow(result))

  source_version <- .pr_tnrs_source_version(response$source_metadata, source)
  source_version_label <- if (is.na(source_version) || !nzchar(source_version)) {
    NA_character_
  } else source_version
  audit <- tibble::tibble(
    row = seq_len(nrow(result)),
    name_submitted = submitted,
    accepted_name = accepted,
    target_tip = NA_character_,
    status = NA_character_
  )
  new_candidates <- list()
  for (i in seq_len(nrow(result))) {
    x_norm <- norm(submitted[[i]])
    x_hits <- input_names[norm(input_names) == x_norm]
    y_norm <- norm(accepted[[i]])
    y_hits <- target_names[target_norm == y_norm]
    if (is.na(accepted[[i]]) || !nzchar(trimws(accepted[[i]]))) {
      audit$status[[i]] <- "no_accepted_name"
      next
    }
    if (!length(x_hits)) {
      audit$status[[i]] <- "submitted_name_not_in_query"
      next
    }
    if (!length(y_hits)) {
      audit$status[[i]] <- "accepted_name_absent_from_tips"
      next
    }
    if (length(y_hits) != 1L) {
      audit$status[[i]] <- "accepted_name_matches_multiple_tips"
      next
    }
    audit$target_tip[[i]] <- y_hits[[1L]]
    audit$status[[i]] <- "candidate"

    warning_text <- unique(c(warnings[[i]], warning_codes[[i]]))
    warning_text <- warning_text[!is.na(warning_text) & nzchar(trimws(warning_text))]
    detail <- paste0(
      "TNRS/", toupper(source), "; accepted name: ", accepted[[i]],
      if (!is.na(matched[[i]]) && nzchar(matched[[i]])) {
        paste0("; matched name: ", matched[[i]])
      } else "",
      if (!is.na(score[[i]]) && nzchar(score[[i]])) {
        paste0("; Overall score: ", score[[i]])
      } else "; Overall score: not reported",
      if (!is.na(status[[i]]) && nzchar(status[[i]])) {
        paste0("; taxonomic status: ", status[[i]])
      } else "",
      if (length(warning_text)) paste0("; warning: ", paste(warning_text, collapse = "; ")) else ""
    )
    for (name_x in x_hits) {
      new_candidates[[length(new_candidates) + 1L]] <- tibble::tibble(
        name_x = name_x, name_y = y_hits[[1L]],
        identity_state = "unknown", eligible = FALSE,
        reason = "tnrs_review_only", x_id = NA_character_,
        y_id = if (!is.na(accepted_id[[i]]) && nzchar(accepted_id[[i]])) {
          paste0(toupper(source), ":", accepted_id[[i]])
        } else NA_character_,
        provider = paste0("TNRS/", toupper(source)),
        version = source_version_label %||% NA_character_,
        stage = "tnrs", review_status = "pending", review_note = NA_character_,
        evidence_detail = detail
      )
    }
  }

  candidates <- reconciliation$candidates
  if (is.null(candidates)) candidates <- tibble::tibble()
  if (!"evidence_detail" %in% names(candidates)) {
    candidates$evidence_detail <- rep(NA_character_, nrow(candidates))
  }
  if (length(new_candidates)) {
    additions <- tibble::as_tibble(do.call(rbind, new_candidates))
    for (i in seq_len(nrow(additions))) {
      key <- !is.na(candidates$name_x) & !is.na(candidates$name_y) &
        candidates$name_x == additions$name_x[[i]] &
        candidates$name_y == additions$name_y[[i]]
      existing <- which(key)
      if (length(existing)) {
        for (j in existing) {
          old <- candidates$evidence_detail[[j]]
          candidates$evidence_detail[[j]] <- paste(
            unique(c(old, additions$evidence_detail[[i]]))[!is.na(
              unique(c(old, additions$evidence_detail[[i]])))], collapse = " | "
          )
        }
      } else {
        candidates <- .pr_tnrs_bind_rows(candidates, additions[i, , drop = FALSE])
      }
    }
  }
  reconciliation$candidates <- tibble::as_tibble(candidates)

  query_warnings <- response$warnings %||% character()
  returned <- unique(norm(submitted[!is.na(submitted)]))
  queried <- unique(norm(input_names))
  missing_names <- input_names[!queried %in% returned]
  receipt <- list(
    source = source, source_version = source_version_label,
    source_metadata = response$source_metadata %||% NULL,
    service_metadata = response$service_metadata %||% NULL,
    client_version = response$client_version %||% NA_character_,
    queried_names = input_names, query_time = Sys.time(), results = result,
    target_audit = audit, warnings = query_warnings,
    missing_submitted_names = missing_names,
    status = if (length(missing_names) || length(query_warnings)) "partial" else "complete"
  )
  previous <- reconciliation$tnrs$queries %||% list()
  if (!length(previous) && !is.null(reconciliation$tnrs)) {
    previous <- list(reconciliation$tnrs)
  }
  receipt$queries <- NULL
  reconciliation$tnrs <- receipt
  reconciliation$tnrs$queries <- c(previous, list(receipt))
  reconciliation
}

.pr_tnrs_client_available <- function() {
  requireNamespace("TNRS", quietly = TRUE)
}

.pr_tnrs_query <- function(names, source) {
  if (!.pr_tnrs_client_available()) {
    abort("The optional TNRS client is not installed.", call = caller_env())
  }
  warnings <- character()
  results <- tryCatch(
    withCallingHandlers(
      .pr_tnrs_client_call(names, source),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) abort(conditionMessage(e), call = caller_env())
  )
  if (is.null(results)) {
    abort("The TNRS client returned no result; check internet access and service availability.",
          call = caller_env())
  }
  source_metadata <- tryCatch(.pr_tnrs_source_metadata(), error = function(e) NULL)
  service_metadata <- tryCatch(.pr_tnrs_service_metadata(), error = function(e) NULL)
  list(
    results = results, source_metadata = source_metadata,
    service_metadata = service_metadata,
    client_version = as.character(utils::packageVersion("TNRS")),
    warnings = warnings
  )
}

.pr_tnrs_client_call <- function(names, source, tnrs_fun = TNRS::TNRS) {
  tnrs_fun(
    taxonomic_names = names, sources = source,
    mode = "resolve", matches = "all", accuracy = 0
  )
}

.pr_tnrs_source_metadata <- function() TNRS::TNRS_sources()

.pr_tnrs_service_metadata <- function() TNRS::TNRS_version()

.pr_tnrs_source_version <- function(metadata, source) {
  if (is.null(metadata) || !is.data.frame(metadata) || !nrow(metadata)) {
    return(NA_character_)
  }
  source_col <- intersect(c("sourceName", "source", "name"), names(metadata))[1L]
  version_col <- intersect(c("version", "sourceVersion", "source_version"), names(metadata))[1L]
  if (is.na(source_col) || is.na(version_col)) return(NA_character_)
  rows <- which(tolower(as.character(metadata[[source_col]])) == source)
  if (length(rows) != 1L) return(NA_character_)
  value <- as.character(metadata[[version_col]][[rows]])
  if (is.na(value) || !nzchar(trimws(value))) NA_character_ else value
}

# TNRS accepted names may append botanical author strings in forms such as
# "Trema tomentosum (Roxb.) H.Hara". The general normalizer handles common
# trailing author/year forms, but this parenthetical + trailing-author form
# must be removed before comparing a canonical taxon name with a tree tip.
.pr_tnrs_name_key <- function(names, rank) {
  names <- as.character(names)
  names <- sub(
    "^(\\S+\\s+\\S+(?:\\s+\\S+){0,3})\\s+\\([^()]*\\)\\s+.+$",
    "\\1", names, perl = TRUE
  )
  as.character(pr_normalize_names(names, rank = rank))
}

.pr_tnrs_bind_rows <- function(x, y) {
  columns <- union(names(x), names(y))
  fill <- function(data, template) {
    for (column in setdiff(columns, names(data))) {
      data[[column]] <- template[[column]][rep(NA_integer_, nrow(data))]
    }
    data[, columns, drop = FALSE]
  }
  tibble::as_tibble(rbind(fill(x, y), fill(y, x)))
}
