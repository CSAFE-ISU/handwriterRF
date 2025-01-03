test_that("Compare documents works when score_only is TRUE", {
  # get_writer_profiles() was added in handwriter 3.2.3.9000
  testthat::skip_if_not_installed('handwriter', minimum_version = '3.2.3.9000')

  actual <- compare_documents(
    sample1 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
    sample2 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
    score_only = TRUE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "w0030_v_w0030_score_only.rds"))

  testthat::expect_equal(actual, expected)
})

test_that("Compare writer profiles works on unknown writers when score_only is TRUE", {
  writer_profiles <- test[1:4, ]
  writer_profiles <- writer_profiles %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_score_only_unknown_writers.rds"))

  testthat::expect_equal(actual, expected)
})

test_that("Compare writer profiles works on known writers when score_only is TRUE", {
  writer_profiles <- test[1:4, ]
  actual <- compare_writer_profiles(
    writer_profiles
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_score_only_known_writers.rds"))

  testthat::expect_equal(actual, expected)
})

test_that("Compare writer profiles works on unknown writers when score_only is FALSE", {
  writer_profiles <- test[1:4, ]
  writer_profiles <- writer_profiles %>% dplyr::select(-writer)
  actual <- compare_writer_profiles(
    writer_profiles,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_slr_unknown_writers.rds"))

  testthat::expect_equal(actual, expected)
})

test_that("Compare writer profiles works on known writers when score_only is FALSE", {
  writer_profiles <- test[1:4, ]
  actual <- compare_writer_profiles(
    writer_profiles,
    score_only = FALSE
  )

  expected <- readRDS(testthat::test_path("fixtures", "compare", "test_4rows_slr_known_writers.rds"))

  testthat::expect_equal(actual, expected)
})

test_that("Check dir contents works if directory contains correct samples", {
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

  expect_error(check_dir_contents(params, "clusters"), NA)
  expect_error(check_dir_contents(params, "docs"), NA)
  expect_error(check_dir_contents(params, "graphs"), NA)
})

test_that("Check dir contents returns error if dir contains one wrong sample", {
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

  expect_error(
    check_dir_contents(params, "clusters"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  expect_error(
    check_dir_contents(params, "docs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  expect_error(
    check_dir_contents(params, "graphs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )
})

test_that("Check dir contents returns error if dir contains two wrong samples", {
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

  expect_error(
    check_dir_contents(params, "clusters"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  expect_error(
    check_dir_contents(params, "docs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )

  expect_error(
    check_dir_contents(params, "graphs"),
    "project_dir contains one or more helper files from documents other than sample1 and sample2."
  )
})

test_that("Setup returns message if user supplies reference scores and score_only is TRUE", {
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
