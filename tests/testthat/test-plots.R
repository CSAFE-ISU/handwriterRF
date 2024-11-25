testthat::test_that("Plot scores runs without error when no observed score is supplied", {
  testthat::expect_error(
    plot_scores(ref_scores),
    NA
  )
})
