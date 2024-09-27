test_that("Get train set works", {
  actual <- get_csafe_train_set(df = cfr, train_prompt_codes = "pLND")
  expected <- read.csv(testthat::test_path("fixtures", "train", "train_set.csv"))
  expect_equal(actual, expected)
})

test_that("Train random forest works with ranger package", {
  train <- get_csafe_train_set(df = cfr, train_prompt_codes = "pLND")
  actual <- train_rf(
    df = train,
    ntrees = 200,
    distance_measures = "euc",
    output_dir = tempdir(),
    run_number = 1,
    downsample = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "train", "rf1.rds"))

  expect_identical(actual, expected)
})

test_that("Make densities works with ranger package", {
  # load random forest from test fixtures
  rforest <- readRDS(testthat::test_path("fixtures", "train", "rf1.rds"))
  actual <- make_densities_from_rf(rforest)

  expect_identical(actual, rforest$densities)
})
