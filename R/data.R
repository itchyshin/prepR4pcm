# Dataset documentation ----------------------------------------------------

#' AVONET morphological trait data (subset)
#'
#' A subset of ~920 bird species from the AVONET database (BirdLife
#' taxonomy), covering 12 passerine families within the Corvoidea and
#' allied clades. Contains morphological measurements and ecological
#' traits.
#'
#' @format A data frame with ~920 rows and 16 columns:
#' \describe{
#'   \item{Species1}{Scientific name (BirdLife taxonomy)}
#'   \item{Family1}{Family}
#'   \item{Order1}{Order}
#'   \item{Beak.Length_Culmen}{Beak length from culmen (mm)}
#'   \item{Beak.Length_Nares}{Beak length from nares (mm)}
#'   \item{Beak.Width}{Beak width (mm)}
#'   \item{Beak.Depth}{Beak depth (mm)}
#'   \item{Tarsus.Length}{Tarsus length (mm)}
#'   \item{Wing.Length}{Wing length (mm)}
#'   \item{Mass}{Body mass (g)}
#'   \item{Habitat}{Primary habitat code}
#'   \item{Habitat.Density}{Habitat density code}
#'   \item{Migration}{Migration status}
#'   \item{Trophic.Level}{Trophic level}
#'   \item{Trophic.Niche}{Trophic niche}
#'   \item{Primary.Lifestyle}{Primary lifestyle}
#' }
#'
#' @source Tobias et al. (2022) AVONET: morphological, ecological and
#'   geographical data for all birds. *Ecology Letters* 25:581--597.
#'   \doi{10.1111/ele.13898}
"avonet_subset"


#' Nest trait data (subset)
#'
#' A subset of ~920 bird species from the global nest trait database (v2),
#' covering the same Corvoidea + allied families as [avonet_subset].
#' Contains nest site and structure information.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Scientific_name}{Scientific name (HBW/BirdLife v5 taxonomy)}
#'   \item{Order}{Order}
#'   \item{Family}{Family}
#'   \item{Common_name}{English common name}
#'   \item{NestSite_ground}{Ground nesting (0/1)}
#'   \item{NestSite_tree}{Tree nesting (0/1)}
#'   \item{NestSite_nontree}{Non-tree elevated nesting (0/1)}
#'   \item{NestSite_cliff_bank}{Cliff/bank nesting (0/1)}
#'   \item{NestStr_scrape}{Scrape nest (0/1)}
#'   \item{NestStr_platform}{Platform nest (0/1)}
#'   \item{NestStr_cup}{Cup nest (0/1)}
#'   \item{NestStr_dome}{Dome nest (0/1)}
#'   \item{NestStr_primary_cavity}{Primary cavity nester (0/1)}
#'   \item{NestStr_second_cavity}{Secondary cavity nester (0/1)}
#' }
#'
#' @source Chia et al. (2023) A global database of bird nest traits.
#'   *Scientific Data* 10:923. \doi{10.1038/s41597-023-02837-1}
"nesttrait_subset"


#' Plumage lightness data (subset)
#'
#' A subset of ~650 passerine species from Delhey et al. (2019), with
#' plumage lightness measurements and climate variables. Covers species
#' from the same families as [avonet_subset] that have plumage data.
#' Note that species names use underscores (e.g.,
#' `"Corvus_corax"`), making this useful for demonstrating name
#' normalisation.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{TipLabel}{Species name with underscores (tree tip label format)}
#'   \item{family}{Family name}
#'   \item{annual_mean_temperature}{Annual mean temperature at range centroid}
#'   \item{annual_precipitation}{Annual precipitation at range centroid}
#'   \item{lightness_male}{Mean plumage lightness, males}
#'   \item{lightness_female}{Mean plumage lightness, females}
#' }
#'
#' @source Delhey et al. (2019) Reconciling ecogeographical rules: rainfall
#'   and temperature predict global colour variation in the largest bird
#'   radiation. *American Naturalist* 194:13--27. \doi{10.1086/703588}
"delhey_subset"


#' BirdLife-BirdTree taxonomy crosswalk
#'
#' A crosswalk mapping species names between BirdLife International taxonomy
#' and the BirdTree (Jetz et al. 2012) taxonomy. This is useful as a
#' pre-built override table for reconciling datasets that use BirdLife names
#' against phylogenies that use BirdTree names. See
#' [reconcile_crosswalk()] to convert this into an overrides table.
#'
#' @format A data frame with ~11,000 rows and 4 columns:
#' \describe{
#'   \item{Species1}{Species name in BirdLife taxonomy}
#'   \item{Species3}{Species name in BirdTree taxonomy}
#'   \item{Match.type}{Type of match: `"1BL to 1BT"` (one-to-one),
#'     `"Many BL to 1BT"` (lump), `"1BL to many BT"` (split),
#'     `"Extinct"`, `"Newly described species"`, `"Invalid taxon"`}
#'   \item{Match.notes}{Additional notes on the match}
#' }
#'
#' @source Tobias et al. (2022) AVONET: morphological, ecological and
#'   geographical data for all birds. *Ecology Letters* 25:581--597.
#'   \doi{10.1111/ele.13898}
"crosswalk_birdlife_birdtree"


#' Jetz (2012) phylogenetic tree (subset)
#'
#' A pruned version of the BirdTree Stage 2 maximum clade credibility tree
#' (Hackett backbone), containing ~660 species from the Corvoidea and
#' allied passerine families. Deliberately smaller than [avonet_subset]
#' (~920 species) so that reconciliation produces unresolved species
#' suitable for [reconcile_augment()]. Tip labels use underscores.
#'
#' @format An object of class `phylo` (from the ape package).
#'
#' @source Jetz et al. (2012) The global diversity of birds in space and
#'   time. *Nature* 491:444--448. \doi{10.1038/nature11631}
"tree_jetz"


#' Clements 2025 phylogenetic tree (subset)
#'
#' A pruned version of the Clements 2025 taxonomy phylogenetic tree,
#' containing ~850 species from the same families. Larger than
#' [tree_jetz] because the Clements taxonomy recognises more species
#' in these clades. Tip labels use underscores.
#'
#' @format An object of class `phylo` (from the ape package).
#'
#' @source Clements et al. (2025) eBird/Clements Checklist of Birds of
#'   the World, v2025.
"tree_clements25"
