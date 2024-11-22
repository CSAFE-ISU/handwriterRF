test_that("Compare documents works when score_only is TRUE", {
  actual <- compare_documents(
    sample1 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
    sample2 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
    score_only = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "w0030_v_w0030_score_only.rds"))

  testthat::expect_identical(actual, expected)
})

test_that("Compare writer profiles works on unknown writers when score_only is TRUE", {
  writer_profiles <- test[1:4, ]
  writer_profiles <- writer_profiles %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_score_only_unknown_writers.rds"))

  testthat::expect_identical(actual, expected)
})

test_that("Compare writer profiles works on known writers when score_only is TRUE", {
  writer_profiles <- test[1:4, ]
  actual <- compare_writer_profiles(
    writer_profiles
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_score_only_known_writers.rds"))

  testthat::expect_identical(actual, expected)
})

test_that("Compare writer profiles works on unknown writers when score_only is TRUE", {
  writer_profiles <- test[1:4, ]
  writer_profiles <- writer_profiles %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_score_only_unknown_writers.rds"))

  testthat::expect_identical(actual, expected)
})

test_that("Compare writer profiles works on unknown writers when score_only is FALSE", {
  writer_profiles <- test[1:4, ]
  writer_profiles <- writer_profiles %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_slr_unknown_writers.rds"))

  testthat::expect_identical(actual, expected)
})

test_that("Compare writer profiles works on known writers when score_only is TRUE", {
  writer_profiles <- test[1:4, ]
  actual <- compare_writer_profiles(
    writer_profiles
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_score_only_known_writers.rds"))

  testthat::expect_identical(actual, expected)
})

test_that("Compare writer profiles works on known writers when score_only is FALSE", {
  writer_profiles <- test[1:4, ]
  actual <- compare_writer_profiles(
    writer_profiles,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_slr_known_writers.rds"))

  testthat::expect_identical(actual, expected)
})
