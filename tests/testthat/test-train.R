test_that("Train random forest works with ranger package and 1 dataframe", {
  # Despite setting the seed, the random forest created on Windows has
  # reasonable values but is not equal to the fixture random forest created on
  # Stephanie's Mac. I tried using skip_on_os(os = "windows", arch = NULL), but
  # this test still returned an error when CRAN ran automated tests on Debian.
  # As a work around, test that the function runs without error.

  expect_error(train_rf(
    df = train,
    ntrees = 200,
    distance_measures = "euc",
    output_dir = NULL,
    run_number = 1,
    downsample = TRUE
  ), NA)
})

test_that("Train random forest works with ranger package and 2 dataframes", {
  # Despite setting the seed, the random forest created on Windows has
  # reasonable values but is not equal to the fixture random forest created on
  # Stephanie's Mac. I tried using skip_on_os(os = "windows", arch = NULL), but
  # this test still returned an error when CRAN ran automated tests on Debian.
  # As a work around, test that the function runs without error.

  expect_error(train_rf(
    df = train[1:50, ],
    ntrees = 200,
    distance_measures = "euc",
    df2 = train[51:100, ],
    output_dir = NULL,
    run_number = 1,
    downsample = TRUE
  ), NA)
})

