test_that("citesperu attach message generates expected tidyverse-style output", {
  msg <- citesperu:::citesperu_attach_message()

  expect_type(msg, "character")
  expect_true(grepl("citesperu", msg))
  expect_true(grepl("cites_fauna_peru_2018", msg))
  expect_true(grepl("cites_flora_peru_2018", msg))
  expect_true(grepl("cites_match", msg))
  expect_true(grepl("MINAM", msg))
})

test_that("highlight_version formats regular and dev versions", {
  res_norm <- citesperu:::highlight_version("0.1.0")
  expect_equal(res_norm, "0.1.0")

  res_dev <- citesperu:::highlight_version("0.1.0.9000")
  expect_type(res_dev, "character")
})

test_that("citesperu_conflicts detects homonymous functions or returns empty list", {
  conflicts <- citesperu:::citesperu_conflicts()
  expect_type(conflicts, "list")

  # Conflict formatter test
  mock_conflict <- list("is_cites" = c("fake_pkg"))
  c_msg <- citesperu:::citesperu_conflict_message(mock_conflict)
  expect_true(grepl("masks fake_pkg::is_cites", c_msg))
})

test_that(".onAttach respects quiet option", {
  op <- options(citesperu.quiet = TRUE)
  on.exit(options(op))

  # Should return invisible NULL without error
  expect_null(citesperu:::.onAttach("lib", "citesperu"))
})
