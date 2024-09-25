test_that("SLRs work on w0030 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030")
  )

  expect_equal(actual, 4582274302)
})

test_that("SLRs work on w0030 versus w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0030_s01_pWOZ_r01.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0030_v_w0238")
  )

  expect_equal(actual, 0)
})

test_that("SLRs work on w0238 samples", {
  actual <- calculate_slr(
    sample1_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r02.png"),
    sample2_path = testthat::test_path("fixtures", "samples", "w0238_s01_pWOZ_r03.png"),
    project_dir = testthat::test_path("fixtures", "slrs_w0238"),
    copy_samples = TRUE
  )

  expect_equal(actual, 96831846345)
})


