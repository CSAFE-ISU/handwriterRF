# Compare Documents -------------------------------------------------------

testthat::test_that("Compare documents works when score_only is TRUE", {
  # get_writer_profiles() was added in handwriter 3.2.3.9000
  testthat::skip_if_not_installed("handwriter", minimum_version = "3.2.3.9000")

  actual <- compare_documents(
    sample1 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
    sample2 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
    score_only = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "w0030_v_w0030_score_only.rds"))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Compare documents works when score_only is FALSE", {
  # get_writer_profiles() was added in handwriter 3.2.3.9000
  testthat::skip_if_not_installed("handwriter", minimum_version = "3.2.3.9000")

  actual <- compare_documents(
    sample1 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
    sample2 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "w0030_v_w0030_slr.rds"))

  testthat::expect_equal(actual, expected)
})


# Compare Writer Profiles -------------------------------------------------

testthat::test_that("Compare writer profiles works when score_only is FALSE, writers are known, and there are 2 input dataframes", {
  writer_profiles <- test[1:4, ]
  writer_profiles2 <- test[5:8, ]
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    writer_profiles2 = writer_profiles2,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_FALSE_known_2df.rds"))

  testthat::expect_equal(actual, expected, tolerance = 1e-2)
})

testthat::test_that("Compare writer profiles works when score_only is FALSE, writers are known, and there is 1 input dataframe", {
  writer_profiles <- test[1:4, ]
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_FALSE_known.rds"))

  testthat::expect_equal(actual, expected, tolerance = 1e-2)
})

testthat::test_that("Compare writer profiles works when score_only is FALSE, writers are unknown, and there are 2 input dataframes", {
  writer_profiles <- test[1:4, ] %>% dplyr::select(-writer)
  writer_profiles2 <- test[5:8, ] %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    writer_profiles2 = writer_profiles2,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_FALSE_unknown_2df.rds"))

  testthat::expect_equal(actual, expected, tolerance = 1e-3)
})

testthat::test_that("Compare writer profiles works when score_only is FALSE, writers are unknown, and there is 1 input dataframe, local version", {

  writer_profiles <- test[1:4, ] %>% dplyr::select(-writer)
  writer_profiles2 <- test[5:8, ] %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_FALSE_unknown.rds"))

  testthat::expect_equal(actual, expected, tolerance = 1e-3)
})

testthat::test_that("Compare writer profiles works when score_only is TRUE, writers are known, and there are 2 input dataframes", {
  writer_profiles <- test[1:4, ]
  writer_profiles2 <- test[5:8, ]
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    writer_profiles2 = writer_profiles2,
    score_only = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_TRUE_known_2df.rds"))

  testthat::expect_identical(actual, expected)
})

testthat::test_that("Compare writer profiles works when score_only is TRUE, writers are known, and there is 1 input dataframe", {
  writer_profiles <- test[1:4, ]
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    score_only = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_TRUE_known.rds"))

  testthat::expect_identical(actual, expected)
})

testthat::test_that("Compare writer profiles works when score_only is TRUE, writers are unknown, and there are 2 input dataframes", {
  writer_profiles <- test[1:4, ] %>% dplyr::select(-writer)
  writer_profiles2 <- test[5:8, ] %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    writer_profiles2 = writer_profiles2,
    score_only = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_TRUE_unknown_2df.rds"))

  testthat::expect_identical(actual, expected)
})

testthat::test_that("Compare writer profiles works when score_only is TRUE, writers are unknown, and there is 1 input dataframe", {
  writer_profiles <- test[1:4, ] %>% dplyr::select(-writer)
  writer_profiles2 <- test[5:8, ] %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles = writer_profiles,
    score_only = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "cwp_score_only_TRUE_unknown.rds"))

  testthat::expect_identical(actual, expected)
})


# Check Directory Contents ------------------------------------------------

testthat::test_that("Check dir contents works if directory contains correct samples", {
  params <- list(
    samples = list(
      original_path1 = "test/sample1.png",
      original_path2 = "test/sample2.png",
      path1 = "test/sample1.png",
      path2 = "test/sample2.png",
      name1 = basename("test/sample1.png"),
      name2 = basename("test/sample2.png")
    ),
    writer_profiles = NULL,
    score_only = TRUE,
    rforest = NULL,
    project_dir = testthat::test_path("fixtures", "slrs", "slrs_same_filename_example"),
    reference_scores = NULL,
    score = NULL,
    slr = NULL
  )

  testthat::expect_error(check_dir_contents(params, "clusters"), NA)
  testthat::expect_error(check_dir_contents(params, "docs"), NA)
  testthat::expect_error(check_dir_contents(params, "graphs"), NA)
})

testthat::test_that("Check dir contents returns error if dir contains one wrong sample", {
  params <- list(
    samples = list(
      original_path1 = "test/a1.png",
      original_path2 = "test/sample2.png",
      path1 = "test/a1.png",
      path2 = "test/sample2.png",
      name1 = basename("test/a1.png"),
      name2 = basename("test/sample2.png")
    ),
    writer_profiles = NULL,
    score_only = TRUE,
    rforest = NULL,
    project_dir = testthat::test_path("fixtures", "slrs", "slrs_same_filename_example"),
    reference_scores = NULL,
    score = NULL,
    slr = NULL
  )

  testthat::expect_error(
    check_dir_contents(params, "clusters"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  testthat::expect_error(
    check_dir_contents(params, "docs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  testthat::expect_error(
    check_dir_contents(params, "graphs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )
})

testthat::test_that("Check dir contents returns error if dir contains two wrong samples", {
  params <- list(
    samples = list(
      original_path1 = "test/a1.png",
      original_path2 = "test/b2.png",
      path1 = "test/a1.png",
      path2 = "test/b2.png",
      name1 = basename("test/a1.png"),
      name2 = basename("test/b2.png")
    ),
    writer_profiles = NULL,
    score_only = TRUE,
    rforest = NULL,
    project_dir = testthat::test_path("fixtures", "slrs", "slrs_same_filename_example"),
    reference_scores = NULL,
    score = NULL,
    slr = NULL
  )

  testthat::expect_error(
    check_dir_contents(params, "clusters"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  testthat::expect_error(
    check_dir_contents(params, "docs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  testthat::expect_error(
    check_dir_contents(params, "graphs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )
})


# Handle NULL Values ------------------------------------------------------

testthat::test_that("Handle NULL values returns message if user supplies reference scores and score_only is TRUE", {
  params <- list(
    samples = list(
      original_path1 = "test/a1.png",
      original_path2 = "test/b2.png",
      path1 = "test/a1.png",
      path2 = "test/b2.png",
      name1 = basename("test/a1.png"),
      name2 = basename("test/b2.png")
    ),
    writer_profiles = NULL,
    score_only = TRUE,
    rforest = NULL,
    project_dir = testthat::test_path("fixtures", "slrs", "slrs_same_filename_example"),
    reference_scores = ref_scores,
    score = NULL,
    slr = NULL
  )

  testthat::expect_message(
    handle_null_values(params),
    "Reference scores were supplied so score_only will be changed to FALSE."
  )
})

