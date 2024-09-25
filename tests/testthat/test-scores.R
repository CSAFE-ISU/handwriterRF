test_that("Get score works", {
  d1 <- get_distances(cfr[1:2, ], c("abs", "euc"))
  random_forest <- rf$rf
  score <- get_score(random_forest = rf$rf, d = d1)

  expect_equal(score, 0.11)
})
