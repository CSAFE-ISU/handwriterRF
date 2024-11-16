# save distances
df <- validation[1:3, 1:6]
d <- get_distances(df, c("abs", "euc"))
saveRDS(d, testthat::test_path("fixtures", "distances", "abs_euc.rds"))

df <- validation[1:3, ]
d <- get_distances(df, c("man", "euc", "max", "cos"))
saveRDS(d, testthat::test_path("fixtures", "distances", "man_euc_max_cos.rds"))

df <- validation[1:2, 1:6]
d <- absolute_dist(df)
saveRDS(d, testthat::test_path("fixtures", "distances", "abs_2docs.rds"))

df <- validation[1:3, 1:6]
d <- absolute_dist(df)
saveRDS(d, testthat::test_path("fixtures", "distances", "abs_3docs.rds"))

df <- validation[1:3, ]
d <- manhattan_dist(df)
saveRDS(d, testthat::test_path("fixtures", "distances", "man.rds"))

df <- validation[1:3, ]
d <- euclidean_dist(df)
saveRDS(d, testthat::test_path("fixtures", "distances", "euc.rds"))

df <- validation[1:3, ]
d <- maximum_dist(df)
saveRDS(d, testthat::test_path("fixtures", "distances", "max.rds"))

df <- validation[1:3, ]
d <- cosine_dist(df)
saveRDS(d, testthat::test_path("fixtures", "distances", "cos.rds"))

# save densities
densities <- make_densities(scores = ref_scores)
saveRDS(densities, testthat::test_path("fixtures", "slr", "densities.csv"))
