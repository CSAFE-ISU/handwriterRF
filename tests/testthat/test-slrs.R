test_that("SLR works on w0030 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030")
  )

  expect_equal(actual, 4582274302)
})

test_that("SLR works on w0030 versus w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030_v_w0238")
  )

  expect_equal(actual, 0)
})

test_that("SLR works on w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r03.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0238")
  )

  expect_equal(actual, 96831846345)
})

test_that("SLR works on w0030 samples in temp directory", {
  # The project_dir = NULL is the default for calculate_slr
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r02.png")
  )

  expect_equal(actual, 4582274302)
})

test_that("SLRs throws error if samples are the same file in the same folder", {
  expect_error(
    calculate_slr(
      sample1_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
      sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
      project_dir = testthat::test_path("fixtures", "slrs_w0238")
    ),
    "sample1_path and sample2_path cannot be identical."
  )
})

test_that("SLRs works if samples in different folders have the same file name", {
    actual <- calculate_slr(
      sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
      sample2_path = testthat::test_path("fixtures", "samples2", "w0030_s01_pWOZ_r01.png"),
      project_dir = testthat::test_path("fixtures", "slrs_sample_name")
    )

    expect_equal(actual, 96831846345)
})
