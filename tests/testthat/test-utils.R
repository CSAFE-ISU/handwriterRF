test_that("Create directory works", {
  # make sure directory doesn't already exist
  unlink(file.path(tempdir(), "new_dir"), recursive = TRUE)
  expect_false(file.exists(file.path(tempdir(), "new_dir")))

  create_dir(file.path(tempdir(), "new_dir"))
  expect_true(file.exists(file.path(tempdir(), "new_dir")))
})
