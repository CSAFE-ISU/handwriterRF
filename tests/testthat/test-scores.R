test_that("Get score works with unknown writers", {
  df <- test[1:2, ]
  df$writer <- c("unknown1", "unknown2")
  d <- get_distances(df, c("abs", "euc"))
  actual <- get_score(d = d, rforest = random_forest, unknown_writers = TRUE)

  expected <- data.frame("docname1" = "w0001_s01_pWOZ_r03",
                         "docname2" = "w0002_s02_pWOZ_r02",
                         "score" = 0.15)

  expect_identical(actual, expected)
})

test_that("Get score works with known writers", {
  df <- test[1:2, ]
  d <- get_distances(df, c("abs", "euc"))
  actual <- get_score(d = d, rforest = random_forest, unknown_writers = FALSE)

  expected <- data.frame("docname1" = "w0001_s01_pWOZ_r03",
                         "writer1" = "w0001",
                         "docname2" = "w0002_s02_pWOZ_r02",
                         "writer2" = "w0002",
                         "match" = as.factor("different"),
                         "score" = 0.15)

  expect_identical(actual, expected)
})

test_that("Get reference scores works", {
  actual <- get_ref_scores(rforest = random_forest,
                           df = validation)

  expect_identical(actual, ref_scores)
})
