latin_identity_rows <- function(inputs, ids, provider = "WCVP",
                                version = "2025", rank = "species") {
  tibble::tibble(
    input = inputs,
    identity_status = ifelse(is.na(ids), "unknown", "known"),
    accepted_id = ids,
    provider = provider,
    version = version,
    rank = rank
  )
}

test_that("gender endings are candidates but not identity evidence", {
  x <- "Plant albus"
  y <- "Plant alba"

  expect_equal(nrow(pr_suggest_gender(x, y)), 1L)
  out <- pr_fuzzy_match(x, y, quiet = TRUE)

  expect_equal(nrow(out), 0L)
  candidates <- attr(out, "identity_candidates")
  expect_equal(nrow(candidates), 1L)
  expect_identical(candidates$reason, "identity_not_established")
  expect_identical(candidates$identity_state, "unknown")
})

test_that("gender-ending candidates require the same authority identity", {
  x <- "Plant albus"
  y <- "Plant alba"
  ix <- latin_identity_rows(x, "WCVP:1")
  iy <- latin_identity_rows(y, "WCVP:1")

  out <- pr_fuzzy_match(x, y, identity_x = ix, identity_y = iy, quiet = TRUE)
  expect_equal(nrow(out), 1L)
  expect_match(out$notes, "Latin-aware")
  expect_match(out$notes, "WCVP@2025")

  conflict <- pr_fuzzy_match(
    x, y, identity_x = ix, identity_y = latin_identity_rows(y, "WCVP:2"),
    quiet = TRUE
  )
  expect_equal(nrow(conflict), 0L)
  expect_identical(attr(conflict, "identity_candidates")$reason,
                   "distinct_accepted_ids")

  incomparable <- pr_fuzzy_match(
    x, y, identity_x = ix,
    identity_y = latin_identity_rows(y, "WCVP:1", version = "2024"),
    quiet = TRUE
  )
  expect_equal(nrow(incomparable), 0L)
  expect_identical(attr(incomparable, "identity_candidates")$reason,
                   "identity_not_established")

  missing_status <- latin_identity_rows(y, NA_character_)
  missing_status$identity_status <- NA_character_
  unknown <- pr_fuzzy_match(
    x, y, identity_x = ix, identity_y = missing_status, quiet = TRUE
  )
  expect_equal(nrow(unknown), 0L)
  expect_identical(attr(unknown, "identity_candidates")$reason,
                   "identity_not_established")

  missing_input_status <- ix
  missing_input_status$identity_status <- NA_character_
  unknown_input <- pr_fuzzy_match(
    x, y, identity_x = missing_input_status,
    identity_y = iy, quiet = TRUE
  )
  expect_equal(nrow(unknown_input), 0L)
  expect_identical(attr(unknown_input, "identity_candidates")$reason,
                   "identity_not_established")

  x_is <- "Plant viridis"
  y_is <- "Plant viride"
  is_match <- pr_fuzzy_match(
    x_is, y_is,
    identity_x = latin_identity_rows(x_is, "WCVP:3"),
    identity_y = latin_identity_rows(y_is, "WCVP:3"), quiet = TRUE
  )
  expect_equal(nrow(is_match), 1L)
  expect_match(is_match$notes, "Latin-aware")
})

test_that("same authority identity supplies a cross-genus fallback candidate", {
  x <- "GenusAlpha albus"
  y <- "DifferentGenus alba"
  out <- pr_fuzzy_match(
    x, y,
    identity_x = latin_identity_rows(x, "WCVP:7"),
    identity_y = latin_identity_rows(y, "WCVP:7"),
    quiet = TRUE
  )

  expect_equal(nrow(out), 1L)
  expect_identical(out$name_y, y)
  expect_match(out$notes, "cross-genus")
  expect_lt(out$score, 0.9)
})

test_that("same-ID cross-genus candidates ignore genus edit distance", {
  x <- "Abies alba"
  close_genus <- "Abias nigra"
  distant_genus <- "Quercus nigra"
  ix <- latin_identity_rows(x, "WCVP:8")

  close <- pr_fuzzy_match(
    x, close_genus, identity_x = ix,
    identity_y = latin_identity_rows(close_genus, "WCVP:8"), quiet = TRUE
  )
  distant <- pr_fuzzy_match(
    x, distant_genus, identity_x = ix,
    identity_y = latin_identity_rows(distant_genus, "WCVP:8"), quiet = TRUE
  )

  expect_equal(nrow(close), 1L)
  expect_equal(nrow(distant), 1L)
  expect_match(close$notes, "cross-genus")
  expect_match(distant$notes, "cross-genus")
})

test_that("stem changes are not relabelled as gender variants", {
  x <- "Plant albus"
  y <- "Plant arbus"
  out <- pr_fuzzy_match(x, y, quiet = TRUE)

  expect_equal(nrow(out), 1L)
  expect_false(grepl("Latin-aware", out$notes, fixed = TRUE))
})

test_that("the cascade keeps unauthorised gender candidates for review", {
  out <- pr_run_cascade(
    "Plant albus", "Plant alba", authority = NULL, fuzzy = TRUE, quiet = TRUE
  )

  expect_false(any(out$match_type %in% c("fuzzy", "flagged")))
  expect_true(any(out$match_type == "unresolved"))
  candidates <- attr(out, "identity_candidates")
  expect_equal(nrow(candidates), 1L)
  expect_identical(candidates$reason, "identity_not_established")
  expect_identical(candidates$stage, "fuzzy")
})

test_that("genus similarity cannot rescue a weak epithet", {
  out <- pr_fuzzy_match("Plant rubrum", "Plant viridis", quiet = TRUE)
  expect_equal(nrow(out), 0L)
})

test_that("Erick's five distinct taxa remain blocked by authority identity", {
  pairs <- list(
    c("Coelogyne candoonensis", "Coelogyne cantonensis"),
    c("Coussarea petiolaris", "Coussarea impetiolaris"),
    c("Croton heterocarpus", "Croton heterochrous"),
    c("Croton leiophyllus", "Croton leucophyllus"),
    c("Croton stenopetalus", "Croton stenosepalus")
  )

  for (pair in pairs) {
    out <- pr_fuzzy_match(
      pair[1], pair[2],
      identity_x = latin_identity_rows(pair[1], paste0("WCVP:x:", pair[1])),
      identity_y = latin_identity_rows(pair[2], paste0("WCVP:y:", pair[2])),
      quiet = TRUE
    )
    expect_equal(nrow(out), 0L, info = paste(pair, collapse = " ~ "))
    expect_identical(
      attr(out, "identity_candidates")$reason,
      "distinct_accepted_ids",
      info = paste(pair, collapse = " ~ ")
    )
  }
})

test_that("subspecies gender candidates preserve the infraspecific epithet", {
  x <- "Plant albus subsp. minor"
  same_subspecies <- "Plant alba subsp. minor"
  different_subspecies <- "Plant alba subsp. major"

  confirmed <- pr_fuzzy_match(
    x, same_subspecies, rank = "subspecies",
    identity_x = latin_identity_rows(x, "WCVP:9", rank = "subspecies"),
    identity_y = latin_identity_rows(
      same_subspecies, "WCVP:9", rank = "subspecies"
    ),
    quiet = TRUE
  )
  expect_equal(nrow(confirmed), 1L)
  expect_match(confirmed$notes, "Latin-aware")

  mismatch <- pr_fuzzy_match(
    x, different_subspecies, rank = "subspecies", threshold = 0.8,
    quiet = TRUE
  )
  expect_equal(nrow(mismatch), 0L)
})
