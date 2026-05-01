# Pluggable tree retrieval ------------------------------------------------
#
# `pr_get_tree()` connects a reconciled species list to an external
# phylogenetic resource and returns a pruned candidate tree plus a small
# matching report. Designed to slot in between `reconcile_*` and any
# downstream PCM workflow:
#
#     rec    <- reconcile_data(my_data, ref, ...)
#     tree   <- pr_get_tree(rec, source = "rotl")
#     align  <- reconcile_apply(rec, my_data, tree$tree)
#
# Issue #42 (Ayumi Mizuno).


#' Retrieve a candidate phylogeny for a species list
#'
#' Connects reconciled species names to an external phylogenetic resource
#' and returns a pruned candidate tree plus a report of which species
#' were matched and which were dropped. Intended as the bridge between
#' the package's reconciliation cascade and any downstream comparative
#' analysis: feed the result of `reconcile_data()` / `reconcile_tree()`
#' (or any character vector of cleaned names) into `pr_get_tree()` and
#' get back a `phylo` ready for `reconcile_apply()`.
#'
#' Each backend is provided by an external R package that we list in
#' `Suggests` rather than `Imports`, so installing **prepR4pcm** does
#' not pull them in automatically. The error message tells you what
#' to install if you ask for a backend you don't have.
#'
#' @param x One of:
#'   \describe{
#'     \item{a `reconciliation` object}{returned by [reconcile_tree()]
#'       or [reconcile_data()]; species are taken from the reconciled
#'       `name_y` column with `NA`s and unresolved entries dropped.}
#'     \item{a character vector}{used directly after deduplication and
#'       NA removal.}
#'     \item{a data frame}{`species_col` must name a character column;
#'       its unique non-NA values are used.}
#'   }
#' @param source A length-1 character vector. Which external backend
#'   to use. One of:
#'   \describe{
#'     \item{\code{"rotl"}}{Open Tree of Life synthesis tree, via the
#'       CRAN package \pkg{rotl}. Universal taxonomic coverage; calls
#'       \code{tnrs_match_names()} to resolve names to OTT ids and
#'       then \code{tol_induced_subtree()}.}
#'     \item{\code{"rtrees"}}{Taxon-specific mega-trees (bird, mammal,
#'       fish, amphibian, reptile, plant, shark/ray, bee, butterfly)
#'       via the GitHub package \pkg{rtrees}
#'       (\url{https://daijiang.github.io/rtrees/}). Requires
#'       \code{taxon = "<group>"}. Calls \code{get_tree()}. Install
#'       with \code{pak::pak("daijiang/rtrees")} (GitHub-only).}
#'     \item{\code{"clootl"}}{Bird-only phylogenies in current
#'       Clements taxonomy, via the GitHub package \pkg{clootl}
#'       (\url{https://github.com/eliotmiller/clootl}). Calls
#'       \code{extractTree()}. Install with
#'       \code{pak::pak("eliotmiller/clootl")}.}
#'   }
#' @param species_col A length-1 character vector. Required when `x`
#'   is a data frame; ignored otherwise.
#' @param taxon A length-1 character vector. Required when
#'   `source = "rtrees"`. One of `"bird"`, `"mammal"`, `"fish"`,
#'   `"amphibian"`, `"reptile"`, `"plant"`, `"shark_ray"`, `"bee"`,
#'   `"butterfly"` (see the \pkg{rtrees} package help for
#'   \code{get_tree}). Ignored for other backends.
#' @param ... Backend-specific arguments forwarded to the underlying
#'   call. See the help page of the underlying function in the
#'   relevant backend package (\code{tol_induced_subtree} in
#'   \pkg{rotl}, \code{extractTree} in \pkg{clootl},
#'   \code{get_tree} in \pkg{rtrees}) for the full list.
#'
#' @return A list with class `pr_tree_result` and components:
#' \describe{
#'   \item{`tree`}{A `phylo` object from the chosen backend, pruned to
#'     the matched species.}
#'   \item{`matched`}{A character vector of species names that were
#'     successfully placed on the returned tree.}
#'   \item{`unmatched`}{A character vector of species names that the
#'     backend could not resolve. Inspect these and consider running
#'     them back through [reconcile_suggest()] / a manual override.}
#'   \item{`source`}{The backend that produced the tree.}
#'   \item{`backend_meta`}{A backend-specific named list of
#'     diagnostic information (e.g. `clootl::getCitations()` output
#'     for the `clootl` backend; OTT tip-id table for the `rotl`
#'     backend).}
#' }
#'
#' @seealso [reconcile_tree()] / [reconcile_data()] for producing the
#'   reconciled species list that feeds this function;
#'   [reconcile_apply()] for combining the returned `phylo` with the
#'   data frame ready for analysis.
#'
#' @examples
#' \dontrun{
#'   # Example 1: drive from a reconciliation object (rotl, universal)
#'   rec  <- reconcile_data(your_data, reference_dataset,
#'                          x_species = "species")
#'   res  <- pr_get_tree(rec, source = "rotl")
#'   res$tree                  # phylo
#'   length(res$matched)       # how many species placed
#'   head(res$unmatched)       # unresolved species, if any
#'
#'   # Example 2: birds, via clootl (Clements taxonomy)
#'   res <- pr_get_tree(c("Corvus corax", "Pica pica"),
#'                      source = "clootl")
#'
#'   # Example 3: fish via rtrees (taxon-specific mega-tree)
#'   res <- pr_get_tree(c("Salmo salar", "Esox lucius"),
#'                      source = "rtrees", taxon = "fish")
#'
#'   # Example 4: from a data frame with custom species column
#'   res <- pr_get_tree(my_df, source = "rotl",
#'                      species_col = "scientific_name")
#' }
#'
#' @export
pr_get_tree <- function(x,
                        source = c("rotl", "rtrees", "clootl"),
                        species_col = NULL,
                        taxon = NULL,
                        ...) {
  source <- match.arg(source)
  species <- .pr_extract_species_for_tree(x, species_col)

  if (length(species) == 0) {
    cli::cli_abort(
      c("No species names available to query the backend.",
        "i" = "If you passed a {.cls reconciliation} object, ensure {.code mapping$name_y} contains resolved names.")
    )
  }

  result <- switch(
    source,
    rotl   = .pr_get_tree_rotl(species, ...),
    rtrees = .pr_get_tree_rtrees(species, taxon = taxon, ...),
    clootl = .pr_get_tree_clootl(species, ...)
  )

  out <- list(
    tree         = result$tree,
    matched      = result$matched,
    unmatched    = result$unmatched,
    source       = source,
    backend_meta = result$backend_meta
  )
  class(out) <- "pr_tree_result"
  out
}


# Internal helpers --------------------------------------------------------

# Pull a clean character vector of species names from `x`. Accepts a
# reconciliation object, a character vector, or a data frame.
.pr_extract_species_for_tree <- function(x, species_col = NULL) {
  if (inherits(x, "reconciliation")) {
    if (!is.null(species_col)) {
      cli::cli_alert_info(
        "{.arg species_col} ignored for {.cls reconciliation} input; using {.code mapping$name_y}."
      )
    }
    nm <- x$mapping$name_y
    return(unique(stats::na.omit(as.character(nm))))
  }
  if (is.character(x)) {
    return(unique(stats::na.omit(x)))
  }
  if (is.data.frame(x)) {
    if (is.null(species_col)) {
      species_col <- pr_detect_species_column(x)
    }
    if (!species_col %in% names(x)) {
      cli::cli_abort(c(
        "Column {.val {species_col}} not found in {.arg x}.",
        "i" = "Available columns: {.val {names(x)}}."
      ))
    }
    return(unique(stats::na.omit(as.character(x[[species_col]]))))
  }
  cli::cli_abort(c(
    "{.arg x} must be a {.cls reconciliation}, character vector, or data frame.",
    "i" = "Got: {.cls {class(x)[1]}}."
  ))
}


# rotl backend: resolve names via TNRS, then induced subtree -------------

.pr_get_tree_rotl <- function(species, ...) {
  if (!requireNamespace("rotl", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.val rotl} backend requires the {.pkg rotl} package.",
        "i" = 'Install with: {.code install.packages("rotl")}.')
    )
  }

  # Step 1: TNRS name match -> OTT ids.
  tnrs <- rotl::tnrs_match_names(species)
  matched_idx <- !is.na(tnrs$ott_id)
  ott_ids <- tnrs$ott_id[matched_idx]
  matched_names <- tnrs$search_string[matched_idx]
  unmatched <- tnrs$search_string[!matched_idx]

  if (length(ott_ids) == 0) {
    cli::cli_abort(
      "{.pkg rotl} returned 0 matches for {length(species)} species; cannot build a tree."
    )
  }

  # Step 2: induced subtree.
  tree <- rotl::tol_induced_subtree(ott_ids = ott_ids, ...)

  list(
    tree         = tree,
    matched      = matched_names,
    unmatched    = unmatched,
    backend_meta = list(
      tnrs_table = tnrs,
      n_queried  = length(species),
      n_matched  = length(ott_ids)
    )
  )
}


# clootl backend: bird-only, Clements taxonomy --------------------------

.pr_get_tree_clootl <- function(species, ...) {
  if (!requireNamespace("clootl", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.val clootl} backend requires the {.pkg clootl} package.",
        "i" = 'Install with: {.code pak::pak("eliotmiller/clootl")} (GitHub-only).',
        ">" = "See {.url https://github.com/eliotmiller/clootl} for details.")
    )
  }

  # clootl::extractTree returns a phylo (or list of phylo if multiple
  # versions). The default `force = FALSE` returns only exact matches.
  tree <- clootl::extractTree(species = species, ...)

  # Determine matched / unmatched by intersecting the requested species
  # against the returned tree's tip labels. clootl uses underscores in
  # tip labels; normalise both sides for comparison.
  norm_req <- pr_normalize_names(species)
  norm_tip <- pr_normalize_names(tree$tip.label)
  in_tree  <- norm_req %in% norm_tip

  # Gather citation block via clootl::getCitations() if present.
  citations <- tryCatch(
    if (exists("getCitations", envir = asNamespace("clootl"), inherits = FALSE)) {
      get("getCitations", envir = asNamespace("clootl"))(tree)
    } else {
      NULL
    },
    error = function(e) NULL
  )

  list(
    tree         = tree,
    matched      = species[in_tree],
    unmatched    = species[!in_tree],
    backend_meta = list(
      n_queried  = length(species),
      n_matched  = sum(in_tree),
      citations  = citations
    )
  )
}


# rtrees backend: taxon-specific mega-trees ------------------------------

.pr_get_tree_rtrees <- function(species, taxon = NULL, ...) {
  if (!requireNamespace("rtrees", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.val rtrees} backend requires the {.pkg rtrees} package.",
        "i" = 'Install with: {.code pak::pak("daijiang/rtrees")} (GitHub-only).',
        ">" = "See {.url https://daijiang.github.io/rtrees/} for details.")
    )
  }

  if (is.null(taxon) || !nzchar(taxon)) {
    cli::cli_abort(
      c("The {.val rtrees} backend requires a {.arg taxon} argument.",
        "i" = "Choose one of: {.val bird}, {.val mammal}, {.val fish}, {.val amphibian}, {.val reptile}, {.val plant}, {.val shark_ray}, {.val bee}, {.val butterfly}.",
        ">" = "Example: {.code pr_get_tree(x, source = \"rtrees\", taxon = \"bird\")}.")
    )
  }

  # rtrees::get_tree wants a character vector OR a data.frame with cols
  # `species`, `genus`, `family`. We pass the simple character form.
  tree <- rtrees::get_tree(
    sp_list      = species,
    taxon        = taxon,
    show_grafted = TRUE,
    ...
  )

  # rtrees returns a phylo when only one source tree was used, and a
  # multiPhylo when many were sampled (e.g. 100 trees from the bird /
  # mammal posterior). Pull the tip-label set from whichever shape we
  # got -- for a multiPhylo all trees share the same tip set so the
  # first one is enough.
  ref_tips <- if (inherits(tree, "multiPhylo")) {
    tree[[1]]$tip.label
  } else {
    tree$tip.label
  }

  # Determine matched / unmatched. rtrees may graft species at higher
  # taxonomic nodes (genus/family); we report both placement types.
  # Strip the trailing `*` rtrees adds to grafted tips before
  # normalising.
  ref_tips_clean <- sub("\\*$", "", ref_tips)
  norm_req <- pr_normalize_names(species)
  norm_tip <- pr_normalize_names(ref_tips_clean)
  in_tree  <- norm_req %in% norm_tip

  # If show_grafted = TRUE rtrees flags grafted tips with a `*`. Surface
  # the grafted set so users can see which species were placed on
  # higher-rank stand-ins rather than at their actual position.
  grafted <- grep("\\*$", ref_tips, value = TRUE)

  list(
    tree         = tree,
    matched      = species[in_tree],
    unmatched    = species[!in_tree],
    backend_meta = list(
      taxon       = taxon,
      n_queried   = length(species),
      n_matched   = sum(in_tree),
      n_grafted   = length(grafted),
      grafted_tips = grafted
    )
  )
}


# Print method -----------------------------------------------------------

#' @export
print.pr_tree_result <- function(x, ...) {
  cli::cli_h1("Tree retrieval result")

  # Some backends (rtrees, clootl) can return a multiPhylo when more
  # than one source tree was used. Print a one-tree summary if it's a
  # single phylo, else summarise the list.
  is_multi <- inherits(x$tree, "multiPhylo")
  n_tips_str <- if (is_multi) {
    sprintf("%d tree%s", length(x$tree),
            if (length(x$tree) == 1) "" else "s")
  } else {
    n_tips <- ape::Ntip(x$tree)
    sprintf("%d tip%s",
            n_tips, if (n_tips == 1) "" else "s")
  }
  n_matched <- length(x$matched)
  n_unmatched <- length(x$unmatched)

  cli::cli_bullets(c(
    "*" = "Source:    {.val {x$source}}",
    "*" = "Tree:      {n_tips_str}",
    "*" = "Matched:   {.val {n_matched}} species",
    "!" = "Unmatched: {.val {n_unmatched}} species"
  ))

  if (n_unmatched > 0) {
    show <- utils::head(x$unmatched, 5)
    cli::cli_alert_info(
      "First {length(show)} unmatched: {.val {show}}"
    )
    cli::cli_alert_info("Use {.code reconcile_suggest()} to find likely candidates.")
  }
  invisible(x)
}
