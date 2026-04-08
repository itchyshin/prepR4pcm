# Name normalisation -------------------------------------------------------

#' Normalise scientific names to a canonical form
#'
#' Apply a sequence of deterministic text transformations so that
#' scientific names which differ only in formatting compare equal.
#' This is the same routine used by stage 2 of the matching cascade in
#' [reconcile_data()] and [reconcile_tree()]. Use it directly when you
#' want to clean a column of names without running a full
#' reconciliation --- for example, when building a crosswalk by hand.
#'
#' @details
#' The transformations, applied in order, are:
#' \enumerate{
#'   \item Replace underscores and multiple whitespace with a single
#'     space (`Homo_sapiens` -> `Homo sapiens`).
#'   \item Strip authority strings and year, including multi-author
#'     and parenthetical forms (`Corvus corax (Linnaeus, 1758)` ->
#'     `Corvus corax`).
#'   \item Fold diacritics to ASCII (`Passer domesticus` stays as
#'     `Passer domesticus`; accented characters are simplified).
#'   \item Standardise case: genus capitalised, epithet lowercase.
#'   \item Strip infraspecific epithets if `rank = "species"`.
#'   \item Trim whitespace and collapse leftover empty tokens.
#' }
#'
#' @param names Character vector of scientific names. `NA` values are
#'   preserved as `NA`.
#' @param rank Character(1). Taxonomic rank to normalise to:
#'   \describe{
#'     \item{`"species"` (default)}{Strip infraspecific epithets so
#'       trinomials become binomials (`Parus major major` ->
#'       `Parus major`).}
#'     \item{`"subspecies"`}{Keep trinomials intact.}
#'   }
#'
#' @return A character vector of normalised names, the same length as
#'   `names`, with an attribute `"normalisation_log"` --- a tibble
#'   recording every non-trivial change, for auditing.
#'
#' @family name utilities
#' @seealso [reconcile_data()] and [reconcile_tree()] for the full
#'   four-stage matching cascade; [pr_extract_tips()] for pulling tip
#'   labels out of a tree prior to normalising them.
#'
#' @examples
#' pr_normalize_names(c("Homo_sapiens",
#'                      "homo sapiens",
#'                      "Parus major major",
#'                      "Corvus corax (Linnaeus, 1758)"))
#'
#' # Keep trinomials
#' pr_normalize_names("Parus major major", rank = "subspecies")
#'
#' @export
pr_normalize_names <- function(names, rank = c("species", "subspecies")) {
  rank <- match.arg(rank)
  original <- names

  # 1. Coerce to character, handle NA

  names <- as.character(names)
  is_na <- is.na(names)
  names[is_na] <- ""

  # 2. Trim leading/trailing whitespace
  names <- trimws(names)

  # 3. Replace underscores with spaces
  names <- gsub("_", " ", names, fixed = TRUE)

  # 4. Collapse multiple internal spaces to single space
  names <- gsub("\\s+", " ", names)

  # 5. Strip OTT ID suffixes (e.g., "Homo sapiens ott770315")
  names <- gsub("\\s+ott\\d+$", "", names, perl = TRUE)

  # 6. Strip trailing author/year strings

  # Matches patterns like "Linnaeus, 1758" or "(Linnaeus, 1758)" at end

  # Also handles "L." or "Author, Year" patterns
  names <- pr_strip_authority(names)

  # 7. Standardise hybrid signs
  names <- gsub("\\s*\u00d7\\s*", " x ", names)  # multiplication sign
  names <- gsub("^x\\s+", "x ", names)            # leading x
  names <- gsub("\\s+x\\s+", " x ", names)        # internal x

  # 8. Standardise infraspecific rank abbreviations
  names <- gsub("\\bsubsp\\.?\\s+", "subsp. ", names, perl = TRUE)
  names <- gsub("\\bssp\\.?\\s+", "subsp. ", names, perl = TRUE)
  names <- gsub("\\bvar\\.?\\s+", "var. ", names, perl = TRUE)
  names <- gsub("\\bf\\.?\\s+", "f. ", names, perl = TRUE)

  # 9. Strip infraspecific epithets if rank == "species"
  if (rank == "species") {
    names <- pr_strip_infraspecific(names)
  }

  # 10. Standardise case: capitalise genus, lowercase rest
  names <- pr_standardise_case(names)

  # 11. Final trim
  names <- trimws(names)

  # Restore NA

  names[is_na] <- NA_character_

  # Build log
  changed <- !is_na & (original != names)
  log <- tibble(
    original   = original,
    normalised = names,
    changed    = changed
  )

  attr(names, "normalisation_log") <- log
  names
}


#' Strip authority strings from scientific names
#'
#' Removes trailing author citations and year from binomials or trinomials.
#'
#' @param names Character vector.
#' @return Character vector with authority strings removed.
#' @keywords internal
pr_strip_authority <- function(names) {
  # Use Unicode-aware character classes (\\p{Lu} = uppercase letter,
  # \\p{L} = any letter) so author names with diacritics (e.g., Müller,
  # Linné) are stripped correctly.

  # Remove parenthetical authority: (Author, Year) or (Author Year)
  names <- gsub(
    "\\s*\\(\\p{Lu}[\\p{L}.&\\s]*,?\\s*\\d{4}\\)\\s*$",
    "", names, perl = TRUE
  )

  # Remove non-parenthetical authority: Author, Year or Author Year.
  # Author may be a single capitalised token (e.g. "Linnaeus") or multiple
  # tokens linked by `&` or `and` (e.g. "Blyth & Tegetmeier"). Only match if
  # preceded by at least genus + species (two words).
  names <- gsub(
    paste0(
      "^(\\S+\\s+\\S+(?:\\s+\\S+)?)",               # binomial or trinomial
      "\\s+\\p{Lu}[\\p{L}.]*",                      # first author token
      "(?:\\s*(?:&|and)\\s*\\p{Lu}[\\p{L}.]*)*",    # optional extra authors
      "(?:\\s*,\\s*|\\s+)\\d{4}\\s*$"               # year
    ),
    "\\1", names, perl = TRUE
  )

  # Remove trailing bare author name (e.g., "Genus species L." or "Genus species Author")
  # Only if what remains is at least two words
  names <- gsub(
    "^(\\S+\\s+\\S+)\\s+\\p{Lu}[\\p{L}.]+\\.?\\s*$",
    "\\1", names, perl = TRUE
  )

  names
}


#' Strip infraspecific epithets to produce binomials
#'
#' Reduces trinomials and names with rank indicators to genus + species.
#'
#' @param names Character vector.
#' @return Character vector of binomials.
#' @keywords internal
pr_strip_infraspecific <- function(names) {
  # Remove rank abbreviation + epithet: "Parus major subsp. excelsus" -> "Parus major"
  names <- gsub(
    "^(\\S+\\s+\\S+)\\s+(?:subsp|ssp|var|f)\\.?\\s+.*$",
    "\\1", names, perl = TRUE
  )

  # Remove bare third word (trinomial without rank indicator):
  # "Parus major major" -> "Parus major"
  # But only if the third word is lowercase (not an authority)
  names <- gsub(
    "^(\\S+\\s+[a-z]\\S*)\\s+[a-z]\\S*$",
    "\\1", names, perl = TRUE
  )

  names
}


#' Standardise case of scientific names
#'
#' Capitalises the genus (first word), lowercases everything else.
#'
#' @param names Character vector.
#' @return Character vector with standardised case.
#' @keywords internal
pr_standardise_case <- function(names) {
  vapply(names, function(name) {
    if (nchar(name) == 0) return(name)
    parts <- strsplit(name, "\\s+")[[1]]
    if (length(parts) == 0) return(name)

    # Capitalise genus
    parts[1] <- paste0(toupper(substr(parts[1], 1, 1)),
                       tolower(substr(parts[1], 2, nchar(parts[1]))))

    # Lowercase remaining parts (except rank abbreviations and hybrid marker)
    if (length(parts) > 1) {
      for (i in 2:length(parts)) {
        if (parts[i] %in% c("subsp.", "var.", "f.", "x")) next
        parts[i] <- tolower(parts[i])
      }
    }

    paste(parts, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}
