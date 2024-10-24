test_that("Get train set works", {
  actual <- get_csafe_train_set(df = cfr, train_prompt_codes = "pLND")
  expected <- read.csv(testthat::test_path("fixtures", "train", "train_set.csv"))
  expect_equal(actual, expected)
})

test_that("Train random forest works with ranger package", {
  # Despite setting the seed, the random forest created on Windows has
  # reasonable values but is not equal to the fixture random forest created on
  # Stephanie's Mac. I tried using skip_on_os(os = "windows", arch = NULL), but
  # this test still returned an error when CRAN ran automated tests on Debian.
  # As a work around, test that the function runs without error.

  train <- get_csafe_train_set(df = cfr, train_prompt_codes = "pLND")
  expect_error(train_rf(
    df = train,
    ntrees = 200,
    distance_measures = "euc",
    output_dir = tempdir(),
    run_number = 1,
    downsample = TRUE
  ), NA)
})

test_that("Make densities works with ranger package", {
  # load random forest from test fixtures
  rforest <- readRDS(testthat::test_path("fixtures", "train", "rf1.rds"))
  actual <- make_densities_from_rf(rforest)

  expect_equal(actual, rforest$densities)
})
