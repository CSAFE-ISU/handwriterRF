test_that("Calculate SLR works on w0030 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030")
  )

  expected <- data.frame("sample1_path" = "fixtures/samples/w0030_s01_pWOZ_r01.png",
                         "sample2_path" = "fixtures/samples/w0030_s01_pWOZ_r02.png",
                         "docname1" = "w0030_s01_pWOZ_r01.png",
                         "docname2" = "w0030_s01_pWOZ_r02.png",
                         "score" = 0.87,
                         "numerator" = 0.45822743,
                         "denominator" = 1e-10,
                         "slr" = 4582274302)

  expect_equal(actual, expected)
})

test_that("Calculate SLR works on w0030 versus w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030_v_w0238")
  )

  expected <- data.frame("sample1_path" = "fixtures/samples/w0030_s01_pWOZ_r01.png",
                         "sample2_path" = "fixtures/samples/w0238_s01_pWOZ_r02.png",
                         "docname1" = "w0030_s01_pWOZ_r01.png",
                         "docname2" = "w0238_s01_pWOZ_r02.png",
                         "score" = 0.575,
                         "numerator" = 0.00000000,
                         "denominator" = 1e-10,
                         "slr" = 0)

  expect_equal(actual, expected)
})

test_that("Calculate SLR works on w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r03.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0238")
  )

  expected <- data.frame("sample1_path" = "fixtures/samples/w0238_s01_pWOZ_r02.png",
                         "sample2_path" = "fixtures/samples/w0238_s01_pWOZ_r03.png",
                         "docname1" = "w0238_s01_pWOZ_r02.png",
                         "docname2" = "w0238_s01_pWOZ_r03.png",
                         "score" = 0.97,
                         "numerator" = 9.6831846,
                         "denominator" = 1e-10,
                         "slr" = 96831846345)

  expect_equal(actual, expected)
})

test_that("Calculate SLR works on w0030 samples in temp directory", {
  # The project_dir = NULL is the default for calculate_slr
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r02.png")
  )

  expected <- data.frame("sample1_path" = "fixtures/samples/w0030_s01_pWOZ_r01.png",
                         "sample2_path" = "fixtures/samples/w0030_s01_pWOZ_r02.png",
                         "docname1" = "w0030_s01_pWOZ_r01.png",
                         "docname2" = "w0030_s01_pWOZ_r02.png",
                         "score" = 0.87,
                         "numerator" = 0.45822743,
                         "denominator" = 1e-10,
                         "slr" = 4582274302)

  expect_equal(actual, expected)
})

test_that("Calculate SLRs throws error if samples are the same file in the same folder", {
  expect_error(
    calculate_slr(
      sample1_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
      sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
      project_dir = testthat::test_path("fixtures", "slrs_w0238")
    ),
    "sample1_path and sample2_path cannot be identical."
  )
})

test_that("Calculate SLRs works if samples in different folders have the same file name", {
    actual <- calculate_slr(
      sample1_path = testthat::test_path("fixtures", "samples", "0.png"),
      sample2_path = testthat::test_path("fixtures", "samples2", "0.png"),
      project_dir = testthat::test_path("fixtures", "slrs_same_filename")
    )

    expected <- data.frame("sample1_path" = "fixtures/samples/0.png",
                           "sample2_path" = "fixtures/samples2/0.png",
                           "docname1" = "0.png",
                           "docname2" = "0.png",
                           "score" = 0.97,
                           "numerator" = 9.6831846,
                           "denominator" = 1e-10,
                           "slr" = 96831846345)

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

  expected <- "A score-based likelihood ratio of 0.8 means the likelihood of observing a similarity score of 0.5 if the documents were written by different people is 1.33 times greater than the likelihood of observing this score if the documents were written by the same writer."

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
