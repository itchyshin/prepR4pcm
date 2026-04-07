#' prepR4pcm: Prepare Data and Trees for Phylogenetic Comparative Methods
#'
#' Reconcile species names across datasets and phylogenetic trees for
#' comparative biology workflows. Detects mismatches due to formatting
#' differences, taxonomic synonymy, and spelling errors, and produces
#' documented, reproducible alignments between data and trees.
#'
#' Synonym resolution uses the \pkg{taxadb} package (Norman et al. 2020)
#' for local taxonomic database lookups. Phylogenetic trees are handled
#' via the \pkg{ape} package (Paradis & Schliep 2019).
#'
#' @references
#' Mizuno, A., Drobniak, S.M., Williams, C., Lagisz, M. & Nakagawa, S.
#' (2025) Promoting the use of phylogenetic multinomial generalised
#' mixed-effects model to understand the evolution of discrete traits.
#' \emph{Journal of Evolutionary Biology} 38:1699--1715.
#' \doi{10.1093/jeb/voaf116}
#'
#' Norman, K.E., Chamberlain, S. & Boettiger, C. (2020) taxadb: A
#' high-performance local taxonomic database interface.
#' \emph{Methods in Ecology and Evolution} 11:1153--1159.
#' \doi{10.1111/2041-210X.13440}
#'
#' Paradis, E. & Schliep, K. (2019) ape 5.0: an environment for modern
#' phylogenetics and evolutionary analyses in R. \emph{Bioinformatics}
#' 35:526--528. \doi{10.1093/bioinformatics/bty633}
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang abort inform warn check_required caller_env
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning
#'   cli_alert_danger cli_h1 cli_h2 cli_bullets cli_progress_bar
#'   cli_progress_update cli_progress_done
#' @importFrom tibble tibble as_tibble
## usethis namespace: end
NULL
