testthat::test_that("Calculate SLR works on w0030 samples", {
  # get_writer_profiles() was added in handwriter 3.2.3.9000
  testthat::skip_if_not_installed('handwriter', minimum_version = '3.2.3.9000')

  # make sure tempdir > comparison doesn't exist. If a previous test fails, this
  # folder might still be hanging around.
  delete_tempdir_comparison()

  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
  )

  expected <- readRDS(testthat::test_path("fixtures", "slrs", "w0030_v_w0030.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Calculate SLR works on w0030 versus w0238 samples", {
  # get_writer_profiles() was added in handwriter 3.2.3.9000
  testthat::skip_if_not_installed('handwriter', minimum_version = '3.2.3.9000')

  # make sure tempdir > comparison doesn't exist. If a previous test fails, this
  # folder might still be hanging around.
  delete_tempdir_comparison()

  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples1", "w0238_s01_pWOZ_r02.png"),
  )

  expected <- readRDS(testthat::test_path("fixtures", "slrs", "w0030_v_w0238.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Calculate SLR works on w0030 samples in project directory", {
  # get_writer_profiles() was added in handwriter 3.2.3.9000
  testthat::skip_if_not_installed('handwriter', minimum_version = '3.2.3.9000')

  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030")
  )

  expected <- readRDS(testthat::test_path("fixtures", "slrs", "w0030_v_w0030.rds"))

  testthat::expect_equal(actual, expected)
})

test_that("Calculate SLRs throws error if samples are the same file in the same folder", {
  # handwriter::get_clusters_batch() in 3.2.1.9000 makes writer and doc indices
  # optional. calculate_slr() doesn't provide writer or doc indices so
  # 3.2.1.9000+ is required.
  testthat::skip_if_not_installed('handwriter', minimum_version = '3.2.1.9000')

  # make sure tempdir > comparison doesn't exist. If a previous test fails, this
  # folder might still be hanging around.
  delete_tempdir_comparison()

  expect_error(
    calculate_slr(
      sample1_path = testthat::test_path("fixtures", "samples1", "w0238_s01_pWOZ_r02.png"),
      sample2_path = testthat::test_path("fixtures", "samples1", "w0238_s01_pWOZ_r02.png"),
    ),
    "sample1 and sample2 can't be identical."
  )
})

test_that("Calculate SLRs works if samples in different folders have the same file name", {
  # get_writer_profiles() was added in handwriter 3.2.3.9000
  testthat::skip_if_not_installed('handwriter', minimum_version = '3.2.3.9000')

  # make sure tempdir > comparison doesn't exist. If a previous test fails, this
  # folder might still be hanging around.
  delete_tempdir_comparison()

  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples1", "0.png"),
    sample2_path = testthat::test_path("fixtures", "samples2", "0.png"),
  )

  expected <- readRDS(testthat::test_path("fixtures", "slrs", "same_filename_example.rds"))

  testthat::expect_equal(actual, expected)
})

test_that("Interpret SLR returns the correct message for values greater than 1", {
  df <- data.frame("score" = 0.87, "slr" = 4582274302)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 4,582,274,302 means the likelihood of observing a similarity score of 0.87 if the documents were written by the same person is 4,582,274,302 times greater than the likelihood of observing this score if the documents were written by different writers."

  expect_equal(actual, expected)
})

test_that("Interpret SLR returns the correct message for values greater than 0 and less than 1", {
  df <- data.frame("score" = 0.5, "slr" = 0.75)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 0.75 means the likelihood of observing a similarity score of 0.5 if the documents were written by different people is 1.33 times greater than the likelihood of observing this score if the documents were written by the same writer."

  expect_equal(actual, expected)
})

test_that("Interpret SLR returns the correct message for a value of 1", {
  df <- data.frame("score" = 0.75, "slr" = 1)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 1 means the likelihood of observing a similarity score of 0.75 if the documents were written by different people is equal to the likelihood of observing the score if the documents were written by the same writer."

  expect_equal(actual, expected)
})

test_that("Interpret SLR returns the correct message for a value of 0", {
  df <- data.frame("score" = 0.575, "slr" = 0)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 0 means it is virtually impossible that the documents were written by the same person."

  expect_equal(actual, expected)
})

test_that("Interpret SLR returns an error for non-numeric values", {
  df <- data.frame("score" = 0.575, "slr" = "string")
  expect_error(interpret_slr(df), "The slr value is not numeric.")

  df <- data.frame("score" = 0.575, "slr" = NA)
  expect_error(interpret_slr(df), "The slr value is not numeric.")
})

test_that("Interpret SLR returns an error for infinite values", {
  df <- data.frame("score" = 0.575, "slr" = Inf)
  expect_error(interpret_slr(df), "The slr value cannot be infinite.")
})

test_that("Interpret SLR returns an error for invalid values", {
  df <- data.frame("score" = 0.575, "slr" = -1)
  expect_error(interpret_slr(df), "The slr value must be greater than or equal to zero.")

  df <- data.frame("score" = 0.575, "slr" = Inf)
  expect_error(interpret_slr(df), "The slr value cannot be infinite.")
})

test_that("Make densities works with ranger package", {
  actual <- make_densities(scores = ref_scores)

  expected <- readRDS(testthat::test_path("fixtures", "slrs", "densities.csv"))

  expect_equal(actual, expected)
})

test_that("Evaluate density at point changes NA in denominator to 1e-10", {
  den <- density(ref_scores$diff_writer$score)

  actual <- eval_density_at_point(den, 1.5, type = "denominator")

  expect_equal(actual, 1e-10)
})
