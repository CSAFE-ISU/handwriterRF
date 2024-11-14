test_that("Calculate SLR works on w0030 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030")
  )

  expected <- data.frame(
    "sample1" = "fixtures/samples/w0030_s01_pWOZ_r01.png",
    "sample2" = "fixtures/samples/w0030_s01_pWOZ_r02.png",
    "score" = 0.89500000000000001776,
    "numerator" = 3.8745314426619446024,
    "denominator" = 0.062397042796531188014,
    "slr" = 62.094792782028122247
  )

  expect_equal(actual, expected)
})

test_that("Calculate SLR works on w0030 versus w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030_v_w0238")
  )

  expected <- data.frame(
    "sample1" = "fixtures/samples/w0030_s01_pWOZ_r01.png",
    "sample2" = "fixtures/samples/w0238_s01_pWOZ_r02.png",
    "score" = 0.56499999999999994671,
    "numerator" = 0.22081676083216716933,
    "denominator" = 0.16659517717430183392,
    "slr" = 1.3254691076749207657
  )

  expect_equal(actual, expected)
})

test_that("Calculate SLR works on w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r03.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0238")
  )

  expected <- data.frame(
    "sample1" = "fixtures/samples/w0238_s01_pWOZ_r02.png",
    "sample2" = "fixtures/samples/w0238_s01_pWOZ_r03.png",
    "score" = 0.94499999999999995115,
    "numerator" = 3.9664120614606899196,
    "denominator" = 0.022299799230150865736,
    "slr" = 177.867613090337926
  )

  expect_equal(actual, expected)
})

test_that("Calculate SLR works on w0030 samples in temp directory", {
  # The project_dir = NULL is the default for calculate_slr
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r02.png")
  )

  expected <- data.frame(
    "sample1" = "fixtures/samples/w0030_s01_pWOZ_r01.png",
    "sample2" = "fixtures/samples/w0030_s01_pWOZ_r02.png",
    "score" = 0.89500000000000001776,
    "numerator" = 3.8745314426619446024,
    "denominator" = 0.062397042796531188014,
    "slr" = 62.094792782028122247
  )

  expect_equal(actual, expected)
})

test_that("Calculate SLRs throws error if samples are the same file in the same folder", {
  expect_error(
    calculate_slr(
      sample1_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
      sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
      project_dir = testthat::test_path("fixtures", "slrs_w0238")
    ),
    "sample1 and sample2 can't be identical."
  )
})

test_that("Calculate SLRs works if samples in different folders have the same file name", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "0.png"),
    sample2_path = testthat::test_path("fixtures", "samples2", "0.png"),
    project_dir = testthat::test_path("fixtures", "slrs_same_filename")
  )

  expected <- data.frame(
    "sample1" = "fixtures/samples/0.png",
    "sample2" = "fixtures/samples2/0.png",
    "score" = 0.85999999999999998668,
    "numerator" = 3.1337004320166270688,
    "denominator" = 0.097050174109834685954,
    "slr" = 32.289487996900668065
  )

  expect_equal(actual, expected)
})

test_that("Interpret SLR returns the correct message for values greater than 1", {
  df <- data.frame("score" = 0.87, "slr" = 4582274302)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 4,582,274,302 means the likelihood of observing a similarity score of 0.87 if the documents were written by the same person is 4,582,274,302 times greater than the likelihood of observing this score if the documents were written by different writers."

  expect_identical(actual, expected)
})

test_that("Interpret SLR returns the correct message for values greater than 0 and less than 1", {
  df <- data.frame("score" = 0.5, "slr" = 0.75)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 0.75 means the likelihood of observing a similarity score of 0.5 if the documents were written by different people is 1.33 times greater than the likelihood of observing this score if the documents were written by the same writer."

  expect_identical(actual, expected)
})

test_that("Interpret SLR returns the correct message for a value of 1", {
  df <- data.frame("score" = 0.75, "slr" = 1)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 1 means the likelihood of observing a similarity score of 0.75 if the documents were written by different people is equal to the likelihood of observing the score if the documents were written by the same writer."

  expect_identical(actual, expected)
})

test_that("Interpret SLR returns the correct message for a value of 0", {
  df <- data.frame("score" = 0.575, "slr" = 0)
  actual <- interpret_slr(df)

  expected <- "A score-based likelihood ratio of 0 means it is virtually impossible that the documents were written by the same person."

  expect_identical(actual, expected)
})

test_that("Make densities works with ranger package", {
  actual <- make_densities(scores = ref_scores)

  expected <- readRDS(testthat::test_path("fixtures", "slr", "densities.csv"))

  expect_equal(actual, expected)
})
