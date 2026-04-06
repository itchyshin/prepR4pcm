# Name normalisation -------------------------------------------------------

#' Normalise scientific names
#'
#' Applies a sequence of deterministic transformations to scientific names so
#' that trivial formatting differences (underscores, case, whitespace,
#' authority strings, OTT suffixes) are removed.
#'
#' @param names Character vector of scientific names.
#' @param rank Character. `"species"` (default) strips infraspecific epithets
#'   to produce binomials. `"subspecies"` retains trinomials.
#'
#' @return A character vector of normalised names, with an attribute
#'   `"normalisation_log"` — a tibble recording what changed.
#'
#' @examples
#' pr_normalize_names(c("Homo_sapiens", "homo sapiens", "Parus major major"))
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
  # Remove parenthetical authority: (Author, Year) or (Author Year)
  names <- gsub("\\s*\\([A-Z][a-zA-Z.&\\s]*,?\\s*\\d{4}\\)\\s*$", "", names,
                perl = TRUE)

  # Remove non-parenthetical authority: Author, Year or Author Year
  # Only match if preceded by at least genus + species (two words)
  names <- gsub(
    "^(\\S+\\s+\\S+(?:\\s+\\S+)?)\\s+[A-Z][a-zA-Z.&]*(?:\\s*,\\s*|\\s+)\\d{4}\\s*$",
    "\\1", names, perl = TRUE
  )

  # Remove trailing bare author name (e.g., "Genus species L." or "Genus species Author")
  # Only if what remains is at least two words
  names <- gsub(
    "^(\\S+\\s+\\S+)\\s+[A-Z][a-zA-Z.]+\\.?\\s*$",
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
