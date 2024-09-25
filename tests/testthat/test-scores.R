test_that("Get score works", {
  d <- get_distances(cfr[1:2, ], c("abs", "euc"))
  score <- get_score(d = d, random_forest = rf)

  expect_equal(score, 0.175)
})
