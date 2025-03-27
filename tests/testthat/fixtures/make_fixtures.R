# Get Distances -----------------------------------------------------------

df <- validation[1:3, 1:6]
df2 <- validation[4:6, 1:6]
distances <- list("abs_euc" = c("abs", "euc"),
                  "man_euc_max_cos" = c("man", "euc", "max", "cos"))

for (i in 1:length(distances)) {
  # one dataframe as input
  d <- get_distances(df, distances[[i]])
  outfile <- testthat::test_path("fixtures", "distances", paste0(names(distances)[i], ".rds"))
  saveRDS(d, outfile)

  # two dataframes as input
  d <- get_distances(df, distances[[i]], df2)
  outfile <- testthat::test_path("fixtures", "distances", paste0(names(distances)[i], "_2df.rds"))
  saveRDS(d, outfile)
}


# Single Distances --------------------------------------------------------

df <- validation[1:3, 1:6]
df2 <- validation[4:6, 1:6]
distances <- c("abs", "man", "euc", "max", "cos")
for (distance in distances) {
  if (distance == "abs") {
    d <- absolute_dist(df)
    d2 <- absolute_dist(df, df2)
  } else if (distance == "man") {
    d <- manhattan_dist(df)
    d2 <- manhattan_dist(df, df2)
  } else if (distance == "euc") {
    d <- euclidean_dist(df)
    d2 <- euclidean_dist(df, df2)
  } else if (distance == "max") {
    d <- maximum_dist(df)
    d2 <- maximum_dist(df, df2)
  } else {
    d <- cosine_dist(df)
    d2 <- cosine_dist(df, df2)
  }

  # save output from one dataframe
  outfile <- testthat::test_path("fixtures", "distances", paste0(distance, ".rds"))
  saveRDS(d, outfile)

  # save output from two dataframes
  outfile2 <- testthat::test_path("fixtures", "distances", paste0(distance, "_2df.rds"))
  saveRDS(d2, outfile2)
}


# Scores ----

df <- test[1:2, ]
df2 <- test[3:4, ]
df$writer <- c("unknown1", "unknown2")
# distances for one dataframe
d <- get_distances(df, c("abs", "euc"))
actual <- get_score(d = d, rforest = random_forest)
saveRDS(actual, testthat::test_path("fixtures", "scores", "unknown_writers.rds"))
# distances between two dataframes
d <- get_distances(df, c("abs", "euc"), df2)
actual <- get_score(d = d, rforest = random_forest)
saveRDS(actual, testthat::test_path("fixtures", "scores", "unknown_writers_2df.rds"))

df <- test[1:2, ]
df2 <- test[3:4, ]
# distances for one dataframe
d <- get_distances(df, c("abs", "euc"))
actual <- get_score(d = d, rforest = random_forest)
saveRDS(actual, testthat::test_path("fixtures", "scores", "known_writers.rds"))
# distances between two dataframes
d <- get_distances(df, c("abs", "euc"), df2)
actual <- get_score(d = d, rforest = random_forest)
saveRDS(actual, testthat::test_path("fixtures", "scores", "known_writers_2df.rds"))


# Make Densities ----------------------------------------------------------

densities <- make_densities(scores = ref_scores)
saveRDS(densities, testthat::test_path("fixtures", "slrs", "densities.csv"))


# Calculate SLRs ----------------------------------------------------------

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


# Compare Documents -------------------------------------------------------

actual <- compare_documents(
  sample1 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r01.png"),
  sample2 = testthat::test_path("fixtures", "samples1", "w0030_s01_pWOZ_r02.png"),
  score_only = TRUE
)
saveRDS(actual, testthat::test_path("fixtures", "compare", "w0030_v_w0030_score_only.rds"))


# Compare Writer Profiles -------------------------------------------------

# combinations of known / unknown writers and one / two dataframes
writer_profiles <- test[1:4, ]
writer_profiles2 <- test[5:8, ]
writer_profiles_unknown <- writer_profiles %>% dplyr::select(-writer)
writer_profiles_unknown2 <- writer_profiles2 %>% dplyr::select(-writer)
profiles <- list(
  "known" = list("writer_profiles" = writer_profiles, "writer_profiles2" = NULL),
  "unknown" = list("writer_profiles" = writer_profiles_unknown, "writer_profiles2" = NULL),
  "known_2df" = list("writer_profiles" = writer_profiles, "writer_profiles2" = writer_profiles2),
  "unknown_2df" = list("writer_profiles" = writer_profiles_unknown, "writer_profiles2" = writer_profiles_unknown2)
)

for (s in c(TRUE, FALSE)){  # score_only parameter
  for (i in 1:4) {  # combinations of known / unknown writers and one / two dataframes
    actual <- compare_writer_profiles(
      writer_profiles = profiles[[i]]$writer_profiles,
      writer_profiles2 = profiles[[i]]$writer_profiles2,
      score_only = s)
    outfile <- testthat::test_path("fixtures", "compare", paste0("cwp_score_only_", s, "_", names(profiles)[i], ".rds"))
    saveRDS(actual, outfile)
  }
}


# Expand Docnames ----
df <- test[1:10, ] %>% dplyr::select(-tidyselect::all_of(c("writer")))
actual <- expand_docnames(df = df)
saveRDS(actual, testthat::test_path("fixtures", "expand_docname", "expanded.rds"))
