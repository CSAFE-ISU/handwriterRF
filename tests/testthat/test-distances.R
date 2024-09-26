testthat::test_that("Get distances works for absolute and Euclidean distances", {
  # Run on a data frame of cluster fill rates from 3 documents
  df <- cfr[1:3, 1:5]
  actual <- get_distances(df, c("abs", "euc"))

  # Expected
  docname1 <- c("w0004_s01_pLND_r01", "w0004_s01_pLND_r01", "w0004_s01_pLND_r02")
  docname2 <- c("w0004_s01_pLND_r02", "w0004_s01_pLND_r03", "w0004_s01_pLND_r03")
  cluster1 <- c(0.00271603634678445945, 0.00235846414126135787, 0.00507450048804581732)
  cluster2 <- c(0.00236406619385342784, 0.00000560205259206997, 0.00236966824644549781)
  cluster3 <- c(0.01795634396869812133, 0.02135502448096983016, 0.00339868051227170884)
  euc <- c(0.01831381858401399249, 0.02148486572151266094, 0.00655110006227082459)
  expected <- data.frame(docname1, docname2, cluster1, cluster2, cluster3, euc)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get distances works for manhattan, Euclidean, maximum, and cosine distances", {
  # Run on a data frame of cluster fill rates from 3 documents
  df <- cfr[1:3, ]
  actual <- get_distances(df, c("man", "euc", "max", "cos"))

  # Expected
  docname1 <- c("w0004_s01_pLND_r01", "w0004_s01_pLND_r01", "w0004_s01_pLND_r02")
  docname2 <- c("w0004_s01_pLND_r02", "w0004_s01_pLND_r03", "w0004_s01_pLND_r03")
  man <- c(0.49063466084742685114, 0.5087448040962209, 0.3331177307633507)
  euc <- c(0.10685899181624387844, 0.09904812595546802489, 0.07217430037487819472)
  max <- c(0.04773301890666338532, 0.03066563588898972570, 0.03822633565792103727)
  cos <- c(0.22288835246478172492, 0.19042881048462328986, 0.11071174930031003891)
  expected <- data.frame(docname1, docname2, man, euc, max, cos)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Absolute distance for single cluster works when cluster fill rates are zero for both documents and data is in a tibble", {
  df <- data.frame(
    docname = c("w0004_s01_pLND_r01", "w0004_s01_pLND_r02"),
    "cluster1" = rep(0, 2),
    "cluster2" = rep(0, 2),
    "cluster3" = rep(0, 2)
  )
  df <- tibble::as_tibble(df)
  actual <- absolute_dist_for_single_cluster(df, "cluster1")

  expected <- matrix(0, nrow = 2, ncol = 2)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Absolute distance works on data frame with 2 docs", {
  df <- cfr[1:2, 1:5]
  actual <- absolute_dist(df)

  docname1 <- c("w0004_s01_pLND_r01")
  docname2 <- c("w0004_s01_pLND_r02")
  cluster1 <- c(0.00271603634678445945)
  cluster2 <- c(0.00236406619385342784)
  cluster3 <- c(0.01795634396869812133)
  expected <- data.frame(docname1, docname2, cluster1, cluster2, cluster3)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Absolute distance works on data frame with 3 docs", {
  df <- cfr[1:3, 1:5]
  actual <- absolute_dist(df)

  docname1 <- c("w0004_s01_pLND_r01", "w0004_s01_pLND_r01", "w0004_s01_pLND_r02")
  docname2 <- c("w0004_s01_pLND_r02", "w0004_s01_pLND_r03", "w0004_s01_pLND_r03")
  cluster1 <- c(0.00271603634678445945, 0.00235846414126135787, 0.00507450048804581732)
  cluster2 <- c(0.00236406619385342784, 0.00000560205259206997, 0.00236966824644549781)
  cluster3 <- c(0.01795634396869812133, 0.02135502448096983016, 0.00339868051227170884)
  expected <- data.frame(docname1, docname2, cluster1, cluster2, cluster3)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Manhattan distance works on data frame with 3 docs", {
  df <- cfr[1:3, ]
  actual <- manhattan_dist(df)

  docname1 <- c("w0004_s01_pLND_r01", "w0004_s01_pLND_r01", "w0004_s01_pLND_r02")
  docname2 <- c("w0004_s01_pLND_r02", "w0004_s01_pLND_r03", "w0004_s01_pLND_r03")
  man <- c(0.49063466084742685114, 0.5087448040962209, 0.3331177307633507)
  expected <- data.frame(docname1, docname2, man)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Euclidean distance works on data frame with 3 docs", {
  df <- cfr[1:3, ]
  actual <- euclidean_dist(df)

  docname1 <- c("w0004_s01_pLND_r01", "w0004_s01_pLND_r01", "w0004_s01_pLND_r02")
  docname2 <- c("w0004_s01_pLND_r02", "w0004_s01_pLND_r03", "w0004_s01_pLND_r03")
  euc <- c(0.10685899181624387844, 0.09904812595546802489, 0.07217430037487819472)
  expected <- data.frame(docname1, docname2, euc)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Maximum distance works on data frame with 3 docs", {
  df <- cfr[1:3, ]
  actual <- maximum_dist(df)

  docname1 <- c("w0004_s01_pLND_r01", "w0004_s01_pLND_r01", "w0004_s01_pLND_r02")
  docname2 <- c("w0004_s01_pLND_r02", "w0004_s01_pLND_r03", "w0004_s01_pLND_r03")
  max <- c(0.04773301890666338532, 0.03066563588898972570, 0.03822633565792103727)
  expected <- data.frame(docname1, docname2, max)

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Cosine distance works on data frame with 3 docs", {
  df <- cfr[1:3, ]

  actual <- cosine_dist(df)

  docname1 <- c("w0004_s01_pLND_r01", "w0004_s01_pLND_r01", "w0004_s01_pLND_r02")
  docname2 <- c("w0004_s01_pLND_r02", "w0004_s01_pLND_r03", "w0004_s01_pLND_r03")
  cos <- c(0.22288835246478172492, 0.19042881048462328986, 0.11071174930031003891)
  expected <- data.frame(docname1, docname2, cos)

  testthat::expect_equal(actual, expected)
})
