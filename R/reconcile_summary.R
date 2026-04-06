#' Generate a reconciliation summary report
#'
#' Produces a detailed, human-readable report of the reconciliation results,
#' breaking down matches by type and listing unresolved names.
#'
#' @param x A `reconciliation` object.
#' @param detail Character(1). Level of detail: `"full"` (default) shows all
#'   matches by category, `"brief"` shows counts only, `"mismatches_only"`
#'   shows only non-exact matches and unresolved names.
#' @param format Character(1). Output format: `"console"` (default) prints
#'   to screen, `"data.frame"` returns a list of tibbles silently.
#' @param file Character(1) or NULL. If non-NULL, writes the console report
#'   to this file path.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `reconciliation_summary` object (invisibly for console output).
#'
#' @examples
#' \dontrun{
#' result <- reconcile_tree(my_data, my_tree)
#' reconcile_summary(result)
#' reconcile_summary(result, detail = "mismatches_only")
#' }
#'
#' @export
reconcile_summary <- function(x,
                              detail = c("full", "brief", "mismatches_only"),
                              format = c("console", "data.frame"),
                              file = NULL,
                              ...) {

  validate_reconciliation(x)
  detail <- match.arg(detail)
  format <- match.arg(format)

  mapping <- x$mapping
  meta    <- x$meta
  counts  <- x$counts

  # Build sub-tables
  by_type <- tibble(
    match_type = c("exact", "normalized", "synonym", "fuzzy", "manual",
                   "unresolved", "flagged"),
    count = c(counts$n_exact, counts$n_normalized, counts$n_synonym,
              counts$n_fuzzy, counts$n_manual,
              counts$n_unresolved_x + counts$n_unresolved_y,
              counts$n_flagged)
  )

  unresolved <- mapping[mapping$match_type == "unresolved", ]
  flagged    <- mapping[mapping$match_type == "flagged", ]
  synonyms   <- mapping[mapping$match_type == "synonym", ]
  normalized <- mapping[mapping$match_type == "normalized", ]
  fuzzy_m    <- mapping[mapping$match_type == "fuzzy", ]
  manual_m   <- mapping[mapping$match_type == "manual", ]

  # Console output
  if (format == "console") {
    lines <- character()

    lines <- c(lines, "")
    lines <- c(lines, "=== Reconciliation Report ===")
    lines <- c(lines, sprintf("Type: %s", meta$type))
    lines <- c(lines, sprintf("Timestamp: %s",
                               format(meta$timestamp, "%Y-%m-%d %H:%M:%S")))
    lines <- c(lines, sprintf("Package: prepR4pcm %s", meta$prepR4pcm_version))
    lines <- c(lines, sprintf("Authority: %s (version: %s)",
                               toupper(meta$authority), meta$db_version))
    lines <- c(lines, sprintf("Rank: %s", meta$rank %||% "species"))
    lines <- c(lines, "")

    lines <- c(lines, "--- Match Summary ---")
    lines <- c(lines, sprintf("  Exact:       %d / %d", counts$n_exact, counts$n_x))
    lines <- c(lines, sprintf("  Normalized:  %d / %d", counts$n_normalized, counts$n_x))
    lines <- c(lines, sprintf("  Synonym:     %d / %d", counts$n_synonym, counts$n_x))
    lines <- c(lines, sprintf("  Fuzzy:       %d / %d", counts$n_fuzzy, counts$n_x))
    lines <- c(lines, sprintf("  Manual:      %d / %d", counts$n_manual, counts$n_x))
    lines <- c(lines, sprintf("  Unresolved:  %d (x only) + %d (y only)",
                               counts$n_unresolved_x, counts$n_unresolved_y))
    lines <- c(lines, "")

    if (detail != "brief") {
      # Normalised matches
      if (nrow(normalized) > 0 && detail != "mismatches_only") {
        lines <- c(lines, sprintf("--- Normalized Matches (%d) ---", nrow(normalized)))
        for (i in seq_len(min(nrow(normalized), 20))) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [%s]',
                                     normalized$name_x[i],
                                     normalized$name_y[i],
                                     normalized$notes[i]))
        }
        if (nrow(normalized) > 20) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(normalized) - 20))
        }
        lines <- c(lines, "")
      }

      # Synonym matches
      if (nrow(synonyms) > 0) {
        lines <- c(lines, sprintf("--- Synonym Matches (%d) ---", nrow(synonyms)))
        for (i in seq_len(min(nrow(synonyms), 20))) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [%s]',
                                     synonyms$name_x[i],
                                     synonyms$name_y[i],
                                     synonyms$notes[i]))
        }
        if (nrow(synonyms) > 20) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(synonyms) - 20))
        }
        lines <- c(lines, "")
      }

      # Manual overrides
      if (nrow(manual_m) > 0) {
        lines <- c(lines, sprintf("--- Manual Overrides (%d) ---", nrow(manual_m)))
        for (i in seq_len(min(nrow(manual_m), 20))) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [%s]',
                                     manual_m$name_x[i],
                                     manual_m$name_y[i],
                                     manual_m$notes[i]))
        }
        lines <- c(lines, "")
      }

      # Unresolved (x only)
      unres_x <- unresolved[unresolved$in_x & !unresolved$in_y, ]
      if (nrow(unres_x) > 0) {
        lines <- c(lines, sprintf("--- Unresolved: In x But Not In y (%d) ---",
                                   nrow(unres_x)))
        show_n <- min(nrow(unres_x), 30)
        for (i in seq_len(show_n)) {
          lines <- c(lines, sprintf("  %s", unres_x$name_x[i]))
        }
        if (nrow(unres_x) > 30) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(unres_x) - 30))
        }
        lines <- c(lines, "")
      }

      # Unresolved (y only)
      unres_y <- unresolved[!unresolved$in_x & unresolved$in_y, ]
      if (nrow(unres_y) > 0) {
        lines <- c(lines, sprintf("--- Unresolved: In y But Not In x (%d) ---",
                                   nrow(unres_y)))
        show_n <- min(nrow(unres_y), 30)
        for (i in seq_len(show_n)) {
          lines <- c(lines, sprintf("  %s", unres_y$name_y[i]))
        }
        if (nrow(unres_y) > 30) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(unres_y) - 30))
        }
        lines <- c(lines, "")
      }
    }

    # Print or write
    text <- paste(lines, collapse = "\n")

    if (!is.null(file)) {
      writeLines(text, file)
      cli_alert_success("Report written to {.path {file}}")
    } else {
      cat(text, "\n")
    }
  }

  # Build summary object
  summary_obj <- structure(
    list(
      tables = list(
        by_type    = by_type,
        unresolved = unresolved,
        flagged    = flagged,
        synonyms   = synonyms,
        normalized = normalized
      ),
      meta = meta
    ),
    class = "reconciliation_summary"
  )

  if (format == "data.frame") {
    return(summary_obj)
  }

  invisible(summary_obj)
}


#' @export
print.reconciliation_summary <- function(x, ...) {
  cat("Reconciliation summary object\n")
  cat(sprintf("  Match types: %s\n",
              paste(x$tables$by_type$match_type[x$tables$by_type$count > 0],
                    collapse = ", ")))
  cat(sprintf("  Unresolved: %d names\n", nrow(x$tables$unresolved)))
  cat("Use $tables to access individual match tables.\n")
  invisible(x)
}
