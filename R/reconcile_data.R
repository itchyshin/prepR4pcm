#' Reconcile species names between two datasets
#'
#' Compares species names in two data frames and produces a reconciliation
#' mapping documenting exact matches, normalised matches, synonym-based
#' matches, and unresolved names.
#'
#' @param x A data frame (first dataset).
#' @param y A data frame (second dataset).
#' @param x_species Character(1). Column name in `x` containing species names.
#'   Auto-detected if `NULL`.
#' @param y_species Character(1). Column name in `y` containing species names.
#'   Auto-detected if `NULL`.
#' @param authority Character(1). Taxonomic authority for synonym resolution:
#'   `"col"` (default), `"itis"`, `"gbif"`, `"ncbi"`, etc. Set to `NULL` to
#'   skip synonym lookup entirely.
#' @param rank Character(1). `"species"` (default) strips infraspecific
#'   epithets during normalisation. `"subspecies"` retains trinomials.
#' @param overrides A data frame with at least columns `name_x` and `name_y`
#'   containing pre-built name corrections, or a file path to a CSV. Optional
#'   column `user_note` for documentation. Set to `NULL` (default) for no
#'   overrides.
#' @param db_version Character(1). taxadb database version. `NULL` uses the
#'   latest available.
#' @param fuzzy Logical. Enable fuzzy matching for likely typos? Default
#'   `FALSE`. When `TRUE`, names that remain unmatched after synonym
#'   resolution are compared using component-based string similarity.
#' @param fuzzy_threshold Numeric (0--1). Minimum similarity score for
#'   fuzzy matches. Default `0.9` (conservative; catches obvious typos).
#' @param resolve Character(1). How to handle low-confidence matches:
#'   `"flag"` (default) marks fuzzy matches below 0.95 and indirect
#'   synonym matches as `match_type = "flagged"` for manual review.
#'   `"first"` accepts all matches at face value.
#' @param quiet Logical. Suppress progress messages? Default `FALSE`.
#'
#' @return A `reconciliation` object. Use [print()] for a summary,
#'   [reconcile_summary()] for details, [reconcile_mapping()] to extract the
#'   mapping table, and [reconcile_apply()] to produce aligned data.
#'
#' @references
#' Norman, K.E., Chamberlain, S. & Boettiger, C. (2020) taxadb: A
#' high-performance local taxonomic database interface.
#' \emph{Methods in Ecology and Evolution} 11:1153--1159.
#' \doi{10.1111/2041-210X.13440}
#'
#' @examples
#' data(avonet_subset)
#' data(nesttrait_subset)
#' result <- reconcile_data(avonet_subset, nesttrait_subset,
#'                          x_species = "Species1",
#'                          y_species = "Scientific_name",
#'                          authority = NULL)
#' print(result)
#'
#' @export
reconcile_data <- function(x, y,
                           x_species = NULL,
                           y_species = NULL,
                           authority = "col",
                           rank = c("species", "subspecies"),
                           overrides = NULL,
                           db_version = NULL,
                           fuzzy = FALSE,
                           fuzzy_threshold = 0.9,
                           resolve = c("flag", "first"),
                           quiet = FALSE) {

  rank <- match.arg(rank)
  resolve <- match.arg(resolve)

  # Validate inputs
  if (!is.data.frame(x)) abort("`x` must be a data frame.", call = caller_env())
  if (!is.data.frame(y)) abort("`y` must be a data frame.", call = caller_env())

  if (!is.null(authority)) {
    authority <- tolower(authority)
    if (!authority %in% pr_valid_authorities()) {
      abort(
        c(
          paste0("Unknown authority: '", authority, "'."),
          "i" = paste0("Valid options: ",
                       paste(pr_valid_authorities(), collapse = ", "))
        ),
        call = caller_env()
      )
    }
  }

  # Detect species columns
  if (is.null(x_species)) x_species <- pr_detect_species_column(x, "x_species")
  if (is.null(y_species)) y_species <- pr_detect_species_column(y, "y_species")

  if (!x_species %in% names(x)) {
    abort(paste0("Column '", x_species, "' not found in `x`."),
          call = caller_env())
  }
  if (!y_species %in% names(y)) {
    abort(paste0("Column '", y_species, "' not found in `y`."),
          call = caller_env())
  }

  # Input guards: empty data, all-NA species, factor columns
  if (nrow(x) == 0) abort("`x` has 0 rows.", call = caller_env())
  if (nrow(y) == 0) abort("`y` has 0 rows.", call = caller_env())

  if (is.factor(x[[x_species]])) {
    cli_alert_warning("Converting factor column '{x_species}' in `x` to character.")
    x[[x_species]] <- as.character(x[[x_species]])
  }
  if (is.factor(y[[y_species]])) {
    cli_alert_warning("Converting factor column '{y_species}' in `y` to character.")
    y[[y_species]] <- as.character(y[[y_species]])
  }

  names_x <- as.character(x[[x_species]])
  names_y <- as.character(y[[y_species]])

  if (all(is.na(names_x))) {
    abort("All species names in `x` are NA.", call = caller_env())
  }
  if (all(is.na(names_y))) {
    abort("All species names in `y` are NA.", call = caller_env())
  }

  # Load overrides
  overrides_df <- pr_load_overrides(overrides)

  if (!quiet) {
    cli_alert_info("Reconciling {length(unique(names_x))} names (x) vs {length(unique(names_y))} names (y)")
  }

  # Run cascade
  mapping <- pr_run_cascade(
    names_x         = names_x,
    names_y         = names_y,
    authority       = authority,
    db_version      = db_version,
    rank            = rank,
    overrides       = overrides_df,
    fuzzy           = fuzzy,
    fuzzy_threshold = fuzzy_threshold,
    resolve         = resolve,
    quiet           = quiet
  )

  # Build metadata
  meta <- list(
    call             = match.call(),
    type             = "data_data",
    timestamp        = Sys.time(),
    authority        = authority %||% "none",
    db_version       = db_version %||% "latest",
    fuzzy            = fuzzy,
    fuzzy_threshold  = if (fuzzy) fuzzy_threshold else NA_real_,
    fuzzy_method     = if (fuzzy) "component_levenshtein" else NA_character_,
    resolve          = resolve,
    prepR4pcm_version = as.character(utils::packageVersion("prepR4pcm")),
    x_source         = deparse(substitute(x)),
    y_source         = deparse(substitute(y)),
    rank             = rank
  )

  result <- new_reconciliation(mapping = mapping, meta = meta)

  if (!quiet) {
    n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
    n_total <- result$counts$n_x
    cli_alert_success("Matched {n_matched}/{n_total} names from x")
  }

  result
}
