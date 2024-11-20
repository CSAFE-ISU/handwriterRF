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

# save scores
df <- test[1:2, ]
df$writer <- c("unknown1", "unknown2")
d <- get_distances(df, c("abs", "euc"))
actual <- get_score(d = d, rforest = random_forest, unknown_writers = TRUE)
saveRDS(actual, testthat::test_path("fixtures", "scores", "unknown_writers.rds"))

df <- test[1:2, ]
d <- get_distances(df, c("abs", "euc"))
actual <- get_score(d = d, rforest = random_forest, unknown_writers = FALSE)
saveRDS(actual, testthat::test_path("fixtures", "scores", "known_writers.rds"))

# save densities
densities <- make_densities(scores = ref_scores)
saveRDS(densities, testthat::test_path("fixtures", "slrs", "densities.csv"))

# save slrs
actual <- calculate_slr(
  sample1_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
  sample2_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
)
saveRDS(actual, testthat::test_path("fixtures", "slrs", "w0030_v_w0030.rds"))

actual <- calculate_slr(
  sample1_path = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
  sample2_path = testthat::test_path("fixtures", "samples1", "w0238_s01_pWOZ_r02.png"),
)
saveRDS(actual, testthat::test_path("fixtures", "slrs", "w0030_v_w0238.rds"))

actual <- calculate_slr(
  sample1_path = testthat::test_path("fixtures", "samples1", "0.png"),
  sample2_path = testthat::test_path("fixtures", "samples2", "0.png"),
  project_dir = testthat::test_path("fixtures", "slrs_same_filename_example")
)
saveRDS(actual, testthat::test_path("fixtures", "slrs", "same_filename_example.rds"))
