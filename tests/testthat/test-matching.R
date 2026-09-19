test_that("cites_classify_names works as expected", {
  names_test <- c(
    "Tremarctos ornatus (F. G. Cuvier, 1825)",
    "Swietenia macrophylla King",
    "Phragmipedium boissierianum var. czerwiakowianum",
    "Cedrela cf. odorata",
    "Touit sp.",
    "Ara spp.",
    "X Haagespostoa albisetata",
    NA,
    ""
  )

  cl <- cites_classify_names(names_test)

  expect_equal(nrow(cl), length(names_test))
  expect_equal(cl$orig_genus[1], "Tremarctos")
  expect_equal(cl$orig_species[1], "ornatus")
  expect_equal(cl$canonical_name[1], "Tremarctos ornatus")

  expect_equal(cl$orig_genus[3], "Phragmipedium")
  expect_equal(cl$orig_species[3], "boissierianum")
  expect_equal(cl$infra_rank[3], "var.")
  expect_equal(cl$orig_infraspecies[3], "czerwiakowianum")
  expect_equal(cl$canonical_name[3], "Phragmipedium boissierianum var. czerwiakowianum")

  expect_true(cl$has_cf[4])
  expect_equal(cl$canonical_name[4], "Cedrela odorata")

  expect_true(cl$is_sp[5])
  expect_equal(cl$rank[5], 1)

  expect_true(cl$is_spp[6])
  expect_equal(cl$rank[6], 1)

  expect_true(cl$had_hybrid[7])

  expect_true(is.na(cl$canonical_name[8]))
  expect_true(is.na(cl$canonical_name[9]))
})

test_that("cites_classify_names handles data frame input", {
  df <- data.frame(species = c("Tremarctos ornatus", "Swietenia macrophylla"))
  cl <- cites_classify_names(df)
  expect_equal(nrow(cl), 2)
  expect_equal(cl$canonical_name, c("Tremarctos ornatus", "Swietenia macrophylla"))
})

test_that("cites_match direct exact matching works", {
  res <- cites_match(c("Tremarctos ornatus", "Swietenia macrophylla"))
  expect_equal(nrow(res), 2)
  expect_equal(res$match_type, c("exact", "exact"))
  expect_true(all(res$is_cites))
  expect_equal(res$apendice, c("I", "II"))
  expect_equal(res$taxon, c("fauna", "flora"))
})

test_that("cites_match synonym matching resolves to accepted taxon", {
  # Epipedobates femoralis is synonym of Allobates femoralis
  # Paphiopedilum besseae is synonym of Phragmipedium besseae
  res <- cites_match(c("Epipedobates femoralis", "Paphiopedilum besseae"))
  expect_equal(nrow(res), 2)
  expect_equal(res$match_type, c("synonym", "synonym"))
  expect_true(all(res$is_cites))
  expect_equal(res$accepted_name, c("Allobates femoralis", "Phragmipedium besseae"))
  expect_equal(res$apendice, c("II", "I"))
})

test_that("cites_match suffix matching handles Latin gender inflection", {
  res <- cites_match("Cedrela odoratus")
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "suffix")
  expect_true(res$is_cites)
  expect_equal(res$matched_name, "Cedrela odorata")
})

test_that("cites_match fuzzy matching finds close typographical errors", {
  res <- cites_match("Tremarctos ornatu", max_dist = 1)
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "fuzzy")
  expect_true(res$is_cites)
  expect_equal(res$matched_name, "Tremarctos ornatus")
  expect_equal(res$matched_dist, 1)
})

test_that("cites_match genus matching works for sp. and spp.", {
  res <- cites_match(c("Cedrela sp.", "Touit spp."))
  expect_equal(nrow(res), 2)
  expect_equal(res$match_type, c("genus", "genus"))
  expect_true(all(res$is_cites))
  expect_equal(res$matched_name, c("Cedrela", "Touit"))
})

test_that("cites_match unmatched species stay unmatched unless genus_fallback", {
  res_no_fallback <- cites_match("Canis familiaris", genus_fallback = FALSE)
  expect_equal(res_no_fallback$match_type, "unmatched")
  expect_false(res_no_fallback$is_cites)

  res_with_fallback <- cites_match("Canis familiaris", genus_fallback = TRUE)
  expect_equal(res_with_fallback$match_type, "genus")
  expect_true(res_with_fallback$is_cites)

  res_unrelated <- cites_match("Homo sapiens")
  expect_equal(res_unrelated$match_type, "unmatched")
  expect_false(res_unrelated$is_cites)
})

test_that("cites_match taxon filtering restricts search domain", {
  # Swietenia is flora
  res_fauna <- cites_match("Swietenia macrophylla", taxon = "fauna")
  expect_equal(res_fauna$match_type, "unmatched")
  expect_false(res_fauna$is_cites)

  res_flora <- cites_match("Swietenia macrophylla", taxon = "flora")
  expect_equal(res_flora$match_type, "exact")
  expect_true(res_flora$is_cites)
})

test_that("is_cites returns exact vectorized boolean vector", {
  splist <- c("Tremarctos ornatus", "Homo sapiens", "Swietenia macrophylla", NA)
  b <- is_cites(splist)
  expect_equal(length(b), 4)
  expect_equal(b, c(TRUE, FALSE, TRUE, NA))
})

test_that("aliases match_cites_pe, cites_matching and is_cites_pe work identically", {
  res1 <- cites_match("Tremarctos ornatus")
  res2 <- cites_matching("Tremarctos ornatus")
  res3 <- match_cites_pe("Tremarctos ornatus")
  expect_equal(res1, res2)
  expect_equal(res1, res3)

  expect_equal(is_cites("Tremarctos ornatus"), is_cites_pe("Tremarctos ornatus"))
})
