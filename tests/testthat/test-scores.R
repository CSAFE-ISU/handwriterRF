test_that("Get score works with ranger package", {
  d1 <- get_distances(cfr[1:2, ], c("abs", "euc"))
  random_forest <- rf_ranger$rf
  score <- get_score(random_forest = random_forest, d = d1, package = "ranger")

  expect_equal(score, 0.175)
})
