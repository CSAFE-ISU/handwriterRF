testthat::test_that("Get distances works for absolute and Euclidean distances", {
  # Run on a dataframe of cluster fill rates from 3 documents
  df <- validation[1:3, 1:6]
  actual <- get_distances(df, c("abs", "euc"))

  expected <- readRDS(testthat::test_path("fixtures", "distances", "abs_euc.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get distances works for manhattan, Euclidean, maximum, and cosine distances", {
  # Run on a dataframe of cluster fill rates from 3 documents
  df <- validation[1:3, ]
  actual <- get_distances(df, c("man", "euc", "max", "cos"))

  expected <- readRDS(testthat::test_path("fixtures", "distances", "man_euc_max_cos.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Absolute distance for single cluster works when cluster fill rates are zero for both documents and data is in a tibble", {
  df <- data.frame(
    docname = c("w0004_s01_pLND_r01", "w0004_s01_pLND_r02"),
    "cluster1" = rep(0, 2),
    "cluster2" = rep(0, 2),
    "cluster3" = rep(0, 2)
  )
  df <- tibble::as_tibble(df)
  actual <- absolute_dist_for_single_cluster(df, "cluster1")

  expected <- matrix(0, nrow = 2, ncol = 2)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Absolute distance works on dataframe with 2 docs", {
  df <- validation[1:2, 1:6]
  actual <- absolute_dist(df)

  expected <- readRDS(testthat::test_path("fixtures", "distances", "abs_2docs.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Absolute distance works on dataframe with 3 docs", {
  df <- validation[1:3, 1:6]
  actual <- absolute_dist(df)

  expected <- readRDS(testthat::test_path("fixtures", "distances", "abs_3docs.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Manhattan distance works on dataframe with 3 docs", {
  df <- validation[1:3, ]
  actual <- manhattan_dist(df)

  expected <- readRDS(testthat::test_path("fixtures", "distances", "man.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Euclidean distance works on dataframe with 3 docs", {
  df <- validation[1:3, ]
  actual <- euclidean_dist(df)

  expected <- readRDS(testthat::test_path("fixtures", "distances", "euc.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Maximum distance works on dataframe with 3 docs", {
  df <- validation[1:3, ]
  actual <- maximum_dist(df)

  expected <- readRDS(testthat::test_path("fixtures", "distances", "max.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Cosine distance works on dataframe with 3 docs", {
  df <- validation[1:3, ]
  actual <- cosine_dist(df)

  expected <- readRDS(testthat::test_path("fixtures", "distances", "cos.rds"))

  testthat::expect_equal(actual, expected)
})
