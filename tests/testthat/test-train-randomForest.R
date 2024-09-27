test_that("Train random forest works with randomForest package", {
  train <- get_csafe_train_set(df = cfr, train_prompt_codes = "pLND")
  actual <- train_randomForest_rf(
    df = train,
    ntrees = 200,
    distance_measures = "euc",
    output_dir = tempdir(),
    run_number = 1,
    downsample = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "train", "rf_randomForest1.rds"))

  # test output directory for actual
  expect_equal(attr(actual$rf$terms, ".Environment")$output_dir, tempdir())

  # set output directories to NULL for expected and actual because they won't match:
  # output dir is tempdir for actual and testthat > fixtures > train for expected.
  attr(actual$rf$terms, ".Environment")$output_dir <- NULL
  attr(expected$rf$terms, ".Environment")$output_dir <- NULL

  expect_identical(actual, expected)
})

test_that("Make densities works with randomForest package", {
  # load random forest from test fixtures
  rforest <- readRDS(testthat::test_path("fixtures", "train", "rf_randomForest1.rds"))
  actual <- make_densities_randomForest_rf(rforest)

  expect_identical(actual, rforest$densities)
})
