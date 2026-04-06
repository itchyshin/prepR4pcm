# Base R plots for reconciliation results -----------------------------------

#' Plot reconciliation match composition
#'
#' Produces a bar chart or pie chart summarising how species names were
#' matched during reconciliation. Uses base R graphics only (no ggplot2
#' dependency).
#'
#' @param x A `reconciliation` object.
#' @param type Character(1). Plot type: `"bar"` (default) draws a horizontal
#'   stacked bar chart; `"pie"` draws a pie chart.
#' @param ... Additional arguments passed to [graphics::barplot()] or
#'   [graphics::pie()].
#'
#' @return `x`, invisibly, so the function can be used in a pipeline.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#' reconcile_plot(result)
#' reconcile_plot(result, type = "pie")
#'
#' @export
reconcile_plot <- function(x, type = c("bar", "pie"), ...) {

  validate_reconciliation(x)

  type <- match.arg(type)

  counts <- x$counts

  # Categories, colours, and counts (in display order)
  categories <- c(
    Exact      = counts$n_exact,
    Normalized = counts$n_normalized,
    Synonym    = counts$n_synonym,
    Fuzzy      = counts$n_fuzzy,
    Manual     = counts$n_manual,
    Flagged    = counts$n_flagged,
    Unresolved = counts$n_unresolved_x + counts$n_unresolved_y

)

  palette <- c(
    Exact      = "#4CAF50",
    Normalized = "#2196F3",
    Synonym    = "#9C27B0",
    Fuzzy      = "#FF9800",
    Manual     = "#009688",
    Flagged    = "#FFC107",
    Unresolved = "#F44336"
  )

  # Keep only categories with count > 0
  keep <- categories > 0
  categories <- categories[keep]
  palette    <- palette[names(categories)]

  n_matched <- sum(categories) - (counts$n_unresolved_x + counts$n_unresolved_y)
  n_total_x <- counts$n_x

  if (type == "bar") {
    pr_plot_bar(categories, palette, n_matched, n_total_x, ...)
  } else {
    pr_plot_pie(categories, palette, ...)
  }

  invisible(x)
}


# Internal plotting helpers ------------------------------------------------

#' Draw horizontal stacked bar chart
#' @keywords internal
#' @noRd
pr_plot_bar <- function(categories, palette, n_matched, n_total_x, ...) {

  vals <- matrix(categories, ncol = 1)
  old_par <- graphics::par(mar = c(4, 1, 3, 1))
  on.exit(graphics::par(old_par), add = TRUE)

  dots <- list(...)
  bar_args <- list(
    height    = vals,
    horiz     = TRUE,
    col       = palette,
    border    = NA,
    axes      = FALSE,
    names.arg = "",
    main      = dots$main %||% "Match composition",
    sub       = sprintf("Matched: %d / %d", n_matched, n_total_x)
  )
  # Merge extra args (excluding main to avoid duplicate)
  dots$main <- NULL
  bar_args <- c(bar_args, dots)

  do.call(graphics::barplot, bar_args)

  graphics::legend(
    "bottomright",
    legend = sprintf("%s (%d)", names(categories), categories),
    fill   = palette,
    border = NA,
    bty    = "n",
    cex    = 0.85
  )
}

#' Draw pie chart
#' @keywords internal
#' @noRd
pr_plot_pie <- function(categories, palette, ...) {
  labels <- sprintf("%s (%d)", names(categories), categories)

  dots <- list(...)
  pie_args <- list(
    x      = categories,
    labels = labels,
    col    = palette,
    border = NA,
    main   = dots$main %||% "Match composition"
  )
  dots$main <- NULL
  pie_args <- c(pie_args, dots)

  do.call(graphics::pie, pie_args)
}
