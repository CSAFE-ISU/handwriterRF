test_that("Create directory works", {
  # make sure directory doesn't already exist
  unlink(file.path(tempdir(), "new_dir"), recursive = TRUE)
  expect_false(file.exists(file.path(tempdir(), "new_dir")))

  create_dir(file.path(tempdir(), "new_dir"))
  expect_true(file.exists(file.path(tempdir(), "new_dir")))
})

test_that("Expand docnames works", {
  # test dataframe already has writer column, so drop it
  df <- test[1:10, ] %>% dplyr::select(-tidyselect::all_of(c("writer")))
  actual <- expand_docnames(df = df)

  expected <- readRDS(testthat::test_path("fixtures", "expand_docname", "expanded.rds"))

  expect_identical(actual, expected)
})
