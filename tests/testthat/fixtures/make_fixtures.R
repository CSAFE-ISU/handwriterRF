# Distances ----
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

# Scores ----
df <- test[1:2, ]
df$writer <- c("unknown1", "unknown2")
d <- get_distances(df, c("abs", "euc"))
actual <- get_score(d = d, rforest = random_forest)
saveRDS(actual, testthat::test_path("fixtures", "scores", "unknown_writers.rds"))

df <- test[1:2, ]
d <- get_distances(df, c("abs", "euc"))
actual <- get_score(d = d, rforest = random_forest)
saveRDS(actual, testthat::test_path("fixtures", "scores", "known_writers.rds"))

# Densities ----
densities <- make_densities(scores = ref_scores)
saveRDS(densities, testthat::test_path("fixtures", "slrs", "densities.csv"))

# SLRs ----
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

# Compare Documents ----
actual <- compare_documents(
  sample1 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
  sample2 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
  score_only = TRUE
)
saveRDS(actual, testthat::test_path("fixtures", "compare", "w0030_v_w0030_score_only.rds"))

# Compare Writer Profiles ----
writer_profiles <- test[1:4, ]
writer_profiles <- writer_profiles %>% dplyr::select(-writer)
actual <- compare_writer_profiles(
  writer_profiles
)
saveRDS(actual, testthat::test_path("fixtures", "compare", "test_4rows_score_only_unknown_writers.rds"))

actual <- compare_writer_profiles(
  writer_profiles,
  score_only = FALSE
)
saveRDS(actual, testthat::test_path("fixtures", "compare", "test_4rows_slr_unknown_writers.rds"))

writer_profiles <- test[1:4, ]
actual <- compare_writer_profiles(
  writer_profiles
)
saveRDS(actual, testthat::test_path("fixtures", "compare", "test_4rows_score_only_known_writers.rds"))

actual <- compare_writer_profiles(
  writer_profiles,
  score_only = FALSE
)
saveRDS(actual, testthat::test_path("fixtures", "compare", "test_4rows_slr_known_writers.rds"))

# Expand Docnames ----
df <- test[1:10, ] %>% dplyr::select(-tidyselect::all_of(c("writer")))
actual <- expand_docnames(df = df)
saveRDS(actual, testthat::test_path("fixtures", "expand_docname", "expanded.rds"))
