test_that("Get score works", {
  df <- cfr[1:2, ]
  df$writer <- c("w0004", "w0004")
  d <- get_distances(df, c("abs", "euc"))
  score <- get_score(d = d, rforest = random_forest)

  expect_equal(score, 0.175)
})
