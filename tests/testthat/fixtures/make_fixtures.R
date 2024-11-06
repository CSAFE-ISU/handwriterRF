# save train set
train_set <- get_csafe_train_set(df = cfr, train_prompt_codes = "pLND")
write.csv(train_set, testthat::test_path("fixtures", "train", "train_set.csv"), row.names = FALSE)

# save ranger random forest
random_forest1 <- train_rf(
  df = train_set,
  ntrees = 200,
  distance_measures = "euc",
  output_dir = testthat::test_path("fixtures", "train"),
  run_number = 1,
  downsample = TRUE
)

# save densities
rforest <- readRDS(testthat::test_path("fixtures", "train", "rf1.rds"))
densities <- make_densities_from_rf(scores = rforest$scores)
saveRDS(densities, testthat::test_path("fixtures", "slr", "densities.csv"))
