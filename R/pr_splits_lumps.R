# Split / lump detection ----------------------------------------------------

#' Detect taxonomic splits and lumps in a reconciliation mapping
#'
#' Examines a reconciliation mapping for cases where:
#' - **Splits**: one name in x maps to multiple names in y (or vice versa)
#'   via synonym resolution, indicating a taxonomic split.
#' - **Lumps**: multiple names in x map to a single name in y (or vice versa)
#'   via synonym resolution, indicating a taxonomic lump.
#'
#' Detection uses the `name_resolved` column: when multiple rows share
#' the same accepted name but differ in the original names, the accepted
#' name has been split or lumped between the two sources.
#'
#' @param mapping A mapping tibble from a `reconciliation` object
#'   (i.e., `result$mapping`).
#'
#' @return A list with two tibbles:
#'   \describe{
#'     \item{splits}{Cases where one name in x maps to multiple names in y
#'       (or one resolved name covers multiple y names).}
#'     \item{lumps}{Cases where multiple names in x map to one name in y
#'       (or multiple x names share one resolved name).}
#'   }
#'   Each tibble has columns: `name_resolved`, `names_x`, `names_y`,
#'   `n_x`, `n_y`, `type`.
#' @keywords internal
pr_detect_splits_lumps <- function(mapping) {

  # Only look at rows that were matched via synonym resolution
  syn_rows <- mapping[mapping$match_type == "synonym" & !is.na(mapping$name_resolved), ]

  if (nrow(syn_rows) == 0) {
    empty <- tibble(
      name_resolved = character(),
      names_x       = list(),
      names_y       = list(),
      n_x           = integer(),
      n_y           = integer(),
      type          = character()
    )
    return(list(splits = empty, lumps = empty))
  }

  # Group by resolved name
  resolved_names <- unique(syn_rows$name_resolved)
  results <- list()

  for (rn in resolved_names) {
    rows <- syn_rows[syn_rows$name_resolved == rn, ]
    ux <- unique(rows$name_x[!is.na(rows$name_x)])
    uy <- unique(rows$name_y[!is.na(rows$name_y)])

    if (length(ux) > 1 || length(uy) > 1) {
      sl_type <- if (length(uy) > 1 && length(ux) == 1) {
        "split"
      } else if (length(ux) > 1 && length(uy) == 1) {
        "lump"
      } else {
        "split_lump"
      }

      results[[length(results) + 1]] <- tibble(
        name_resolved = rn,
        names_x       = list(ux),
        names_y       = list(uy),
        n_x           = length(ux),
        n_y           = length(uy),
        type          = sl_type
      )
    }
  }

  if (length(results) == 0) {
    empty <- tibble(
      name_resolved = character(),
      names_x       = list(),
      names_y       = list(),
      n_x           = integer(),
      n_y           = integer(),
      type          = character()
    )
    return(list(splits = empty, lumps = empty))
  }

  all_sl <- do.call(rbind, results)
  splits <- all_sl[all_sl$type %in% c("split", "split_lump"), ]
  lumps  <- all_sl[all_sl$type %in% c("lump", "split_lump"), ]

  list(splits = splits, lumps = lumps)
}


#' Summarise splits and lumps for a reconciliation object
#'
#' User-facing function that wraps [pr_detect_splits_lumps()] with
#' formatted console output.
#'
#' @param x A `reconciliation` object.
#' @param quiet Logical. Suppress console output? Default `FALSE`.
#'
#' @return A list with `$splits` and `$lumps` tibbles, invisibly.
#'
#' @examples
#' \dontrun{
#' result <- reconcile_tree(my_data, my_tree, authority = "col")
#' sl <- reconcile_splits_lumps(result)
#' sl$splits
#' sl$lumps
#' }
#'
#' @export
reconcile_splits_lumps <- function(x, quiet = FALSE) {

  if (!inherits(x, "reconciliation")) {
    abort("`x` must be a <reconciliation> object.", call = caller_env())
  }

  sl <- pr_detect_splits_lumps(x$mapping)

  if (!quiet) {
    n_splits <- nrow(sl$splits)
    n_lumps  <- nrow(sl$lumps)

    if (n_splits == 0 && n_lumps == 0) {
      cli_alert_info("No taxonomic splits or lumps detected.")
    } else {
      if (n_splits > 0) {
        cli_alert_warning("{n_splits} potential split(s) detected:")
        for (i in seq_len(n_splits)) {
          xs <- paste(sl$splits$names_x[[i]], collapse = ", ")
          ys <- paste(sl$splits$names_y[[i]], collapse = ", ")
          cli_bullets(c(
            " " = "  {sl$splits$name_resolved[i]}: [{xs}] -> [{ys}]"
          ))
        }
      }
      if (n_lumps > 0) {
        cli_alert_warning("{n_lumps} potential lump(s) detected:")
        for (i in seq_len(n_lumps)) {
          xs <- paste(sl$lumps$names_x[[i]], collapse = ", ")
          ys <- paste(sl$lumps$names_y[[i]], collapse = ", ")
          cli_bullets(c(
            " " = "  {sl$lumps$name_resolved[i]}: [{xs}] -> [{ys}]"
          ))
        }
      }
    }
  }

  invisible(sl)
}
