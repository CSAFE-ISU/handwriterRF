test_that("Get train set works", {
  actual <- get_train_set(df=cfr, train_prompt_code = "pLND")
  expected <- read.csv(testthat::test_path("fixtures", "train", "train_set.csv"))
  expect_equal(actual, expected)
})


test_that("Train random forest works with randomForest package", {
  actual <- train_rf(df = cfr,
                 ntrees = 200,
                 train_prompt_code = "pLND",
                 distance_measures = "euc",
                 output_dir = tempdir(),
                 run_number = 1,
                 downsample = TRUE,
                 package = "randomForest")

  expected <- readRDS(testthat::test_path("fixtures", "train", "rf_1.rds"))

  # test output directory for actual
  expect_equal(attr(actual$rf$terms, '.Environment')$output_dir, tempdir())

  # set output directories to NULL for expected and actual because they won't match:
  # output dir is tempdir for actual and testthat > fixtures > train for expected.
  attr(actual$rf$terms, '.Environment')$output_dir <- NULL
  attr(expected$rf$terms, '.Environment')$output_dir <- NULL

  expect_identical(actual, expected)
})

test_that("Make densities from random forest works", {
  # load random forest from test fixtures
  random_forest <- readRDS(testthat::test_path("fixtures", "train", "rf_1.rds"))
  actual <- make_densities_from_rf(random_forest, tempdir())

  expected <- readRDS(testthat::test_path("fixtures", "train", "densities.rds"))

  expect_identical(actual, expected)
})
