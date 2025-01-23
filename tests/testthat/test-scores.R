test_that("Get score works with unknown writers", {
  df <- test[1:2, ]
  df$writer <- c("unknown1", "unknown2")
  d <- get_distances(df, c("abs", "euc"))
  actual <- get_score(d = d, rforest = random_forest)

  expected <- readRDS(testthat::test_path("fixtures", "scores", "unknown_writers.rds"))

  expect_equal(actual, expected)
})

test_that("Get score works with known writers", {
  df <- test[1:2, ]
  d <- get_distances(df, c("abs", "euc"))
  actual <- get_score(d = d, rforest = random_forest)

  expected <- readRDS(testthat::test_path("fixtures", "scores", "known_writers.rds"))

  expect_equal(actual, expected)
})

test_that("Get reference scores works", {
  actual <- get_ref_scores(
    rforest = random_forest,
    df = validation,
    seed = 100,
    downsample_diff_pairs = TRUE
  )

  expect_equal(actual, ref_scores)
})
