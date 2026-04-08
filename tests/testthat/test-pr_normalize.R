test_that("underscores are replaced with spaces", {
  expect_equal(
    as.character(pr_normalize_names("Homo_sapiens")),
    "Homo sapiens"
  )
})

test_that("case is standardised", {
  expect_equal(
    as.character(pr_normalize_names("homo sapiens")),
    "Homo sapiens"
  )
  expect_equal(
    as.character(pr_normalize_names("HOMO SAPIENS")),
    "Homo sapiens"
  )
})

test_that("whitespace is normalised", {
  expect_equal(
    as.character(pr_normalize_names("Homo  sapiens")),
    "Homo sapiens"
  )
  expect_equal(
    as.character(pr_normalize_names("  Homo sapiens  ")),
    "Homo sapiens"
  )
})

test_that("OTT suffixes are stripped", {
  expect_equal(
    as.character(pr_normalize_names("Homo_sapiens_ott770315")),
    "Homo sapiens"
  )
})

test_that("infraspecific names are stripped by default", {
  expect_equal(
    as.character(pr_normalize_names("Parus major major")),
    "Parus major"
  )
  expect_equal(
    as.character(pr_normalize_names("Quercus robur subsp. sessiliflora")),
    "Quercus robur"
  )
})

test_that("infraspecific names are kept with rank = 'subspecies'", {
  result <- as.character(
    pr_normalize_names("Parus major major", rank = "subspecies")
  )
  expect_equal(result, "Parus major major")
})

test_that("NA values are preserved", {
  result <- pr_normalize_names(c("Homo sapiens", NA, "Pan troglodytes"))
  expect_true(is.na(result[2]))
  expect_equal(as.character(result[1]), "Homo sapiens")
  expect_equal(as.character(result[3]), "Pan troglodytes")
})

test_that("normalisation log is attached as attribute", {
  result <- pr_normalize_names(c("Homo_sapiens", "Pan troglodytes"))
  log <- attr(result, "normalisation_log")
  expect_s3_class(log, "tbl_df")
  expect_true(log$changed[1])   # Homo_sapiens changed
  expect_false(log$changed[2])  # Pan troglodytes unchanged
})

test_that("authority strings are stripped", {
  expect_equal(
    as.character(pr_normalize_names("Homo sapiens Linnaeus, 1758")),
    "Homo sapiens"
  )
})

test_that("authority strings with diacritics are stripped", {
  expect_equal(
    as.character(pr_normalize_names("Passer domesticus M\u00fcller, 1776")),
    "Passer domesticus"
  )
  expect_equal(
    as.character(pr_normalize_names("Turdus merula Linn\u00e9, 1758")),
    "Turdus merula"
  )
})

test_that("empty character vector returns empty", {
  result <- pr_normalize_names(character(0))
  expect_length(result, 0)
})

test_that("all-NA vector returns all NA", {
  result <- pr_normalize_names(c(NA, NA, NA))
  expect_true(all(is.na(result)))
  expect_length(result, 3)
})

test_that("single-word names are preserved", {
  # Monotypic genus used alone — should not be destroyed
  result <- as.character(pr_normalize_names("Tyrannosaurus"))
  expect_equal(result, "Tyrannosaurus")
})


# --- M12. pr_normalize_names combinatorial grid -----------------------------

test_that("normalization is length-preserving across input varieties", {
  inputs <- list(
    ascii      = c("Parus major", "Corvus corax"),
    underscore = c("Parus_major", "Corvus_corax"),
    whitespace = c("  Parus   major  ", "Parus\tmajor"),
    mixed_case = c("parus MAJOR", "CORVUS corax"),
    diacritics = c("Passer domesticus M\u00fcller, 1776",
                   "Turdus merula Linn\u00e9, 1758"),
    authority  = c("Turdus merula (Linnaeus, 1758)",
                   "Corvus corax Blyth & Tegetmeier 1881"),
    numerals   = c("Aves sp. 1", "Aves sp. 2"),
    empty_str  = c("", ""),
    na_only    = c(NA_character_, NA_character_)
  )

  for (nm in names(inputs)) {
    for (rank in c("species", "subspecies")) {
      res <- pr_normalize_names(inputs[[nm]], rank = rank)
      expect_length(res, length(inputs[[nm]]))
      expect_true(is.character(res),
                  info = sprintf("input=%s rank=%s", nm, rank))
    }
  }
})


test_that("normalization is idempotent (normalize(normalize(x)) == normalize(x))", {
  inputs <- c(
    "Parus_major",
    "  parus MAJOR  ",
    "Turdus merula Linn\u00e9, 1758",
    "Passer domesticus (M\u00fcller, 1776)",
    "Aquila chrysaetos",
    "Quercus robur subsp. sessiliflora"
  )

  once  <- as.character(pr_normalize_names(inputs))
  twice <- as.character(pr_normalize_names(once))

  expect_equal(twice, once)
})


test_that("diacritic authorities resolve to same normalized form as ASCII twins", {
  # The key invariant: a name with a diacritic author must normalize to the
  # same binomial as the same name with an ASCII author. This was the #A4
  # regression.
  diacritic <- pr_normalize_names("Passer domesticus M\u00fcller, 1776")
  ascii     <- pr_normalize_names("Passer domesticus Mueller, 1776")

  expect_equal(as.character(diacritic), "Passer domesticus")
  expect_equal(as.character(ascii),     "Passer domesticus")
})


test_that("empty strings stay empty, NA stays NA, non-NA elements normalize independently", {
  mix <- c("Homo_sapiens", NA_character_, "", "Parus major")
  out <- pr_normalize_names(mix)

  expect_equal(as.character(out[1]), "Homo sapiens")
  expect_true(is.na(out[2]))
  expect_equal(as.character(out[3]), "")
  expect_equal(as.character(out[4]), "Parus major")
})


test_that("authority stripping handles parenthetical and non-parenthetical forms", {
  cases <- list(
    # non-parenthetical: Author, Year
    list(in_ = "Parus major Linnaeus, 1758",           out = "Parus major"),
    # parenthetical
    list(in_ = "Turdus merula (Linnaeus, 1758)",       out = "Turdus merula"),
    # Author Year (no comma)
    list(in_ = "Corvus corax Blyth 1881",              out = "Corvus corax"),
    # bare author (no year)
    list(in_ = "Homo sapiens L.",                      out = "Homo sapiens"),
    # Diacritic author, parenthetical
    list(in_ = "Turdus merula (Linn\u00e9, 1758)",     out = "Turdus merula"),
    # ampersand-linked authors
    list(in_ = "Corvus corax Blyth & Tegetmeier 1881", out = "Corvus corax")
  )

  for (case in cases) {
    expect_equal(
      as.character(pr_normalize_names(case$in_)),
      case$out,
      info = sprintf("input: %s", case$in_)
    )
  }
})


test_that("rank='subspecies' preserves trinomials and rank-marked forms", {
  cases <- list(
    list(in_ = "Parus major major",                   out = "Parus major major"),
    list(in_ = "Quercus robur subsp. sessiliflora",   out = "Quercus robur subsp. sessiliflora"),
    list(in_ = "Corvus corax ssp. varius",            out = "Corvus corax subsp. varius"),
    list(in_ = "Canis lupus var. familiaris",         out = "Canis lupus var. familiaris")
  )

  for (case in cases) {
    expect_equal(
      as.character(pr_normalize_names(case$in_, rank = "subspecies")),
      case$out,
      info = sprintf("subspecies rank input: %s", case$in_)
    )
  }
})


test_that("large vector is processed without error and preserves length", {
  big <- rep(c("Parus_major", "Corvus corax", "Turdus merula"), 3334)[seq_len(10000)]
  out <- pr_normalize_names(big)
  expect_length(out, 10000)
  # Sample a few to verify correctness
  expect_equal(as.character(out[1]), "Parus major")
  expect_equal(as.character(out[2]), "Corvus corax")
})


test_that("rank argument is validated and defaults apply", {
  # Default rank is species (strips trinomials)
  expect_equal(
    as.character(pr_normalize_names("Parus major major")),
    "Parus major"
  )
  # Invalid rank argument errors via match.arg
  expect_error(pr_normalize_names("Parus major", rank = "family"))
})
