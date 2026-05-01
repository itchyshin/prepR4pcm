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
#'     \item{\code{"fishtree"}}{Fish-only time-calibrated phylogeny
#'       (Rabosky et al. 2018), via the CRAN package \pkg{fishtree}.
#'       Calls \code{fishtree_phylogeny()} (single tree) or
#'       \code{fishtree_complete_phylogeny()} (multi-tree posterior;
#'       triggered by \code{n_tree > 1}). Requires exact name
#'       matches against the Fish Tree of Life taxonomy --- pre-clean
#'       with [reconcile_data()] (with a `taxadb` authority) for best
#'       results.}
#'     \item{\code{"datelife"}}{Universal database of pre-computed
#'       chronograms (Sanchez Reyes et al. 2024, *Syst. Biol.*
#'       73:470), via the GitHub package \pkg{datelife}
#'       (\url{https://github.com/phylotastic/datelife}). Returns a
#'       single SDM-summary chronogram by default; with
#'       \code{n_tree > 1}, returns a multiPhylo of up to that many
#'       per-source candidate chronograms. Install with
#'       \code{pak::pak("phylotastic/datelife")} (the package was
#'       archived from CRAN in 2024).}
#'   }
#' @param species_col A length-1 character vector. Required when `x`
#'   is a data frame; ignored otherwise.
#' @param taxon A length-1 character vector. Required when
#'   `source = "rtrees"`. One of `"bird"`, `"mammal"`, `"fish"`,
#'   `"amphibian"`, `"reptile"`, `"plant"`, `"shark_ray"`, `"bee"`,
#'   `"butterfly"` (see the \pkg{rtrees} package help for
#'   \code{get_tree}). Ignored for other backends.
#' @param n_tree A length-1 positive integer. How many trees to
#'   request from the backend. Default `1L` (single phylo for
#'   back-compat). Each backend negotiates this differently:
#'   \describe{
#'     \item{`"rotl"`}{Always returns 1 (the synthesis tree). A
#'       one-shot warning is emitted if `n_tree > 1`.}
#'     \item{`"rtrees"`}{Passes through to
#'       `rtrees::get_tree(n_tree = ...)`. Requires `taxon`.}
#'     \item{`"clootl"`}{Passes through to
#'       `clootl::extractTree(sample.size = n_tree)` so you get
#'       multiple Clements posterior samples.}
#'     \item{`"fishtree"`}{Single phylo via `fishtree_phylogeny()`
#'       when `n_tree = 1`; switches to
#'       `fishtree_complete_phylogeny()` returning a multiPhylo of
#'       stochastically polytomy-resolved trees when `n_tree > 1`.}
#'     \item{`"datelife"`}{`summary_format = "phylo_sdm"` (single
#'       summary chronogram) when `n_tree = 1`; switches to
#'       `summary_format = "phylo_all"` (one chronogram per source,
#'       capped at `n_tree`) when `n_tree > 1`.}
#'   }
#'   When the request returns a multiPhylo, the result's `tree` slot
#'   is `multiPhylo`; otherwise `phylo`.
#' @param ... Backend-specific arguments forwarded to the underlying
#'   call. See the help page of the underlying function in the
#'   relevant backend package (\code{tol_induced_subtree} in
#'   \pkg{rotl}, \code{extractTree} in \pkg{clootl},
#'   \code{get_tree} in \pkg{rtrees}, \code{fishtree_phylogeny} /
#'   \code{fishtree_complete_phylogeny} in \pkg{fishtree},
#'   \code{datelife_search} in \pkg{datelife}) for the full list.
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
#'     backend; the `fishtree_phylogeny()` warning text and tree
#'     `type` for the `fishtree` backend; the chronogram source
#'     citations for the `datelife` backend). Always contains
#'     `tree_provenance`, a list with one entry per returned tree
#'     (so `tree[[i]]` pairs with `backend_meta$tree_provenance[[i]]`
#'     when `tree` is a `multiPhylo`).}
#' }
#'
#' @seealso [reconcile_tree()] / [reconcile_data()] for producing the
#'   reconciled species list that feeds this function;
#'   [reconcile_apply()] for combining the returned `phylo` with the
#'   data frame ready for analysis;
#'   [reconcile_augment()] for filling gaps in an existing tree
#'   (a tree-aware alternative to retrieving a fresh tree);
#'   [pr_date_tree()] for time-calibrating an existing topology;
#'   [pr_cite_tree()] for formatting citations for a tree result.
#'   The companion package
#'   \href{https://itchyshin.github.io/pigauto/}{pigauto} consumes a
#'   `multiPhylo` directly via `multi_impute_trees()` for posterior-
#'   tree PCMs --- request a posterior sample with `n_tree > 1`.
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
#'   # Example 4: fish via fishtree (Rabosky et al. 2018, time-calibrated)
#'   res <- pr_get_tree(c("Salmo salar", "Esox lucius"),
#'                      source = "fishtree")
#'
#'   # Example 5: from a data frame with custom species column
#'   res <- pr_get_tree(my_df, source = "rotl",
#'                      species_col = "scientific_name")
#' }
#'
#' @export
pr_get_tree <- function(x,
                        source = c("rotl", "rtrees", "clootl",
                                   "fishtree", "datelife"),
                        species_col = NULL,
                        taxon = NULL,
                        n_tree = 1L,
                        ...) {
  source <- match.arg(source)
  if (!is.numeric(n_tree) || length(n_tree) != 1L || n_tree < 1L) {
    cli::cli_abort(
      c("{.arg n_tree} must be a length-1 positive integer.",
        "i" = "Got: {.val {n_tree}}.")
    )
  }
  n_tree <- as.integer(n_tree)
  species <- .pr_extract_species_for_tree(x, species_col)

  if (length(species) == 0) {
    cli::cli_abort(
      c("No species names available to query the backend.",
        "i" = "If you passed a {.cls reconciliation} object, ensure {.code mapping$name_y} contains resolved names.")
    )
  }

  result <- switch(
    source,
    rotl     = .pr_get_tree_rotl(species, n_tree = n_tree, ...),
    rtrees   = .pr_get_tree_rtrees(species, n_tree = n_tree,
                                    taxon = taxon, ...),
    clootl   = .pr_get_tree_clootl(species, n_tree = n_tree, ...),
    fishtree = .pr_get_tree_fishtree(species, n_tree = n_tree, ...),
    datelife = .pr_get_tree_datelife(species, n_tree = n_tree, ...)
  )

  # Ensure backend_meta$tree_provenance is always present as a list with
  # one entry per returned tree, so downstream consumers (e.g. pigauto)
  # can pair tree[[i]] with backend_meta$tree_provenance[[i]].
  result$backend_meta <- .pr_ensure_tree_provenance(
    result$tree, result$backend_meta, source
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

.pr_get_tree_rotl <- function(species, n_tree = 1L, ...) {
  if (!requireNamespace("rotl", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.val rotl} backend requires the {.pkg rotl} package.",
        "i" = 'Install with: {.code install.packages("rotl")}.')
    )
  }

  if (n_tree > 1L) {
    cli::cli_warn(c(
      "{.pkg rotl} returns the Open Tree of Life {.emph synthesis} tree (single).",
      "i" = "{.arg n_tree} = {n_tree} ignored; returning n = 1.",
      ">" = "For posterior samples, try {.code source = \"datelife\"} or {.code source = \"rtrees\"}."
    ))
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

.pr_get_tree_clootl <- function(species, n_tree = 1L, ...) {
  if (!requireNamespace("clootl", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.val clootl} backend requires the {.pkg clootl} package.",
        "i" = 'Install with: {.code pak::pak("eliotmiller/clootl")} (GitHub-only).',
        ">" = "See {.url https://github.com/eliotmiller/clootl} for details.")
    )
  }

  # When n_tree > 1, request multiple posterior samples via
  # clootl::extractTree(sample.size = n_tree). Default sample.size = 1.
  call_args <- list(...)
  if (n_tree > 1L && is.null(call_args$sample.size)) {
    call_args$sample.size <- n_tree
  }
  call_args$species <- species

  tree <- do.call(clootl::extractTree, call_args)

  # Determine matched / unmatched by intersecting the requested species
  # against the returned tree's tip labels. For multiPhylo, all trees
  # share the same tip set, so the first one suffices.
  ref_tips <- if (inherits(tree, "multiPhylo")) {
    tree[[1]]$tip.label
  } else {
    tree$tip.label
  }
  norm_req <- pr_normalize_names(species)
  norm_tip <- pr_normalize_names(ref_tips)
  in_tree  <- norm_req %in% norm_tip

  # Gather citation block via clootl::getCitations() if present.
  citations <- tryCatch(
    if (exists("getCitations", envir = asNamespace("clootl"), inherits = FALSE)) {
      get("getCitations", envir = asNamespace("clootl"))(
        if (inherits(tree, "multiPhylo")) tree[[1]] else tree
      )
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
      n_queried   = length(species),
      n_matched   = sum(in_tree),
      n_returned  = if (inherits(tree, "multiPhylo")) length(tree) else 1L,
      citations   = citations
    )
  )
}


# rtrees backend: taxon-specific mega-trees ------------------------------

.pr_get_tree_rtrees <- function(species, taxon = NULL, n_tree = 1L, ...) {
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

  # rtrees::get_tree has its own n_tree argument. Pass it through if the
  # user didn't already set it via ... so n_tree on pr_get_tree() works
  # uniformly across backends.
  call_args <- list(...)
  if (is.null(call_args$n_tree)) call_args$n_tree <- n_tree
  call_args$sp_list      <- species
  call_args$taxon        <- taxon
  call_args$show_grafted <- TRUE

  tree <- do.call(rtrees::get_tree, call_args)

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


# fishtree backend: fish-only, time-calibrated --------------------------

.pr_get_tree_fishtree <- function(species, n_tree = 1L, ...) {
  if (!requireNamespace("fishtree", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.val fishtree} backend requires the {.pkg fishtree} package.",
        "i" = 'Install with: {.code install.packages("fishtree")}.',
        ">" = "Reference: Rabosky et al. (2018) {.emph Nature} 559:392 ({.href [doi:10.1038/s41586-018-0273-1](https://doi.org/10.1038/s41586-018-0273-1)}).")
    )
  }

  # When n_tree > 1, switch to fishtree_complete_phylogeny() which
  # returns a multiPhylo of stochastically polytomy-resolved trees.
  # Otherwise fishtree_phylogeny() returns the single best-guess
  # chronogram.
  warns <- character()
  multi <- n_tree > 1L

  tree <- withCallingHandlers(
    if (multi) {
      fishtree::fishtree_complete_phylogeny(species = species, ...)
    } else {
      fishtree::fishtree_phylogeny(species = species, ...)
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # If the user requested fewer than fishtree_complete_phylogeny()
  # produced, cap to n_tree.
  if (multi && inherits(tree, "multiPhylo") && length(tree) > n_tree) {
    tree <- tree[seq_len(n_tree)]
  }

  # fishtree uses underscore-form tip labels; for multiPhylo all trees
  # share the same tip set so the first one is enough for matching.
  ref_tips <- if (inherits(tree, "multiPhylo")) {
    tree[[1]]$tip.label
  } else {
    tree$tip.label
  }
  tip_sp    <- gsub("_", " ", ref_tips)
  norm_req  <- pr_normalize_names(species)
  norm_tip  <- pr_normalize_names(tip_sp)
  in_tree   <- norm_req %in% norm_tip

  # Pull `type` from the call if user supplied it; default is chronogram.
  call_args <- list(...)
  type_used <- if (!is.null(call_args$type)) call_args$type else "chronogram"

  list(
    tree         = tree,
    matched      = species[in_tree],
    unmatched    = species[!in_tree],
    backend_meta = list(
      backend    = "fishtree",
      type       = type_used,
      n_queried  = length(species),
      n_matched  = sum(in_tree),
      n_returned = if (inherits(tree, "multiPhylo")) length(tree) else 1L,
      warnings   = warns,
      reference  = "Rabosky et al. (2018) Nature 559:392 (doi:10.1038/s41586-018-0273-1)"
    )
  )
}


# datelife backend: chronograms from a published database --------------

.pr_get_tree_datelife <- function(species, n_tree = 1L,
                                   summary_format = NULL,
                                   use_tnrs = FALSE, ...) {
  if (!requireNamespace("datelife", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.val datelife} backend requires the {.pkg datelife} package.",
        "i" = 'Install with: {.code pak::pak("phylotastic/datelife")} (GitHub-only; archived from CRAN in 2024).',
        ">" = "See {.url https://github.com/phylotastic/datelife} for details.")
    )
  }

  # Default summary_format: single SDM tree when n_tree = 1; all per-source
  # candidates when n_tree > 1.
  if (is.null(summary_format)) {
    summary_format <- if (n_tree > 1L) "phylo_all" else "phylo_sdm"
  }

  # Build a make_datelife_query result so we know the matched / unmatched
  # set independent of which summary format is used. use_tnrs = FALSE
  # keeps this offline; users who want TNRS pass use_tnrs = TRUE.
  query <- datelife::make_datelife_query(input = species,
                                          use_tnrs = use_tnrs)
  matched_names <- query$cleaned_names
  if (is.null(matched_names)) matched_names <- character()
  unmatched <- setdiff(species, matched_names)

  res <- datelife::datelife_search(
    input          = query,
    summary_format = summary_format,
    use_tnrs       = use_tnrs,
    ...
  )

  # Coerce return to phylo or multiPhylo per our contract.
  tree <- if (inherits(res, "phylo")) {
    res
  } else if (inherits(res, "multiPhylo")) {
    if (length(res) > n_tree) res[seq_len(n_tree)] else res
  } else if (is.list(res) &&
             all(vapply(res, inherits, logical(1), what = "phylo"))) {
    # phylo_all returns a named list of phylo: coerce to multiPhylo
    out <- res
    if (length(out) > n_tree) out <- out[seq_len(n_tree)]
    class(out) <- "multiPhylo"
    out
  } else {
    cli::cli_abort(c(
      "Unexpected return type from {.code datelife::datelife_search}.",
      "i" = "Got class: {.cls {class(res)[1]}}.",
      ">" = "Expected: {.cls phylo}, {.cls multiPhylo}, or a list of {.cls phylo}."
    ))
  }

  # Per-source citations come from the names of the multiPhylo (datelife
  # uses the source citation as the name).
  source_citations <- if (inherits(tree, "multiPhylo") &&
                          !is.null(names(tree))) {
    names(tree)
  } else {
    NULL
  }

  list(
    tree         = tree,
    matched      = matched_names,
    unmatched    = unmatched,
    backend_meta = list(
      backend          = "datelife",
      version          = as.character(utils::packageVersion("datelife")),
      summary_format   = summary_format,
      n_queried        = length(species),
      n_matched        = length(matched_names),
      n_returned       = if (inherits(tree, "multiPhylo")) length(tree) else 1L,
      source_citations = source_citations,
      reference        = "Sanchez Reyes et al. (2024) Syst. Biol. 73:470 (doi:10.1093/sysbio/syae015)"
    )
  )
}


# Per-tree provenance helper --------------------------------------------
#
# Build a per-tree provenance list so downstream consumers (e.g. pigauto)
# can pair tree[[i]] with backend_meta$tree_provenance[[i]]. For a
# single phylo, the list has one element; for a multiPhylo, one per tree.

.pr_ensure_tree_provenance <- function(tree, backend_meta, source) {
  is_multi <- inherits(tree, "multiPhylo")
  n <- if (is_multi) length(tree) else 1L

  # Helper: pick first non-null. Local closure, not a global operator.
  null_or <- function(a, b) if (is.null(a)) b else a

  base_ref <- switch(
    source,
    rotl     = "Open Tree of Life synthesis (OTT)",
    rtrees   = "Daijiang Li, rtrees package (taxon-specific reference)",
    clootl   = null_or(backend_meta$citations, "Clements taxonomy (clootl)"),
    fishtree = null_or(backend_meta$reference,
                       "Rabosky et al. (2018) Nature 559:392 (doi:10.1038/s41586-018-0273-1)"),
    datelife = null_or(backend_meta$reference,
                       "Sanchez Reyes et al. (2024) Syst. Biol. 73:470"),
    "(unknown)"
  )

  # For datelife multiPhylo, prefer the per-source citation (in tree names).
  per_tree_citations <- if (source == "datelife" && is_multi) {
    citations <- names(tree)
    if (is.null(citations) || length(citations) != n) {
      rep(base_ref, n)
    } else {
      citations
    }
  } else {
    rep(base_ref, n)
  }

  calibration_method <- switch(
    source,
    rotl     = "topology only (no calibration)",
    rtrees   = "graft / no recalibration",
    clootl   = "Clements posterior sample",
    fishtree = if (n > 1) "stochastic polytomy resolution" else "best-guess chronogram",
    datelife = null_or(backend_meta$summary_format, "datelife summary"),
    NA_character_
  )

  prov <- vector("list", n)
  for (i in seq_len(n)) {
    prov[[i]] <- list(
      source_index       = i,
      citation           = per_tree_citations[[i]],
      calibration_method = calibration_method,
      n_tips             = if (is_multi) ape::Ntip(tree[[i]])
                            else ape::Ntip(tree)
    )
  }

  backend_meta$tree_provenance <- prov
  backend_meta
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
