# save train set
train_set <- get_train_set(df=cfr, train_prompt_code = "pLND")
write.csv(train_set, testthat::test_path("fixtures", "train", "train_set.csv"), row.names = FALSE)

# save randomForest random forest
rf <- train_rf(df = cfr,
               ntrees = 200,
               train_prompt_code = "pLND",
               distance_measures = "euc",
               output_dir = testthat::test_path("fixtures", "train"),
               run_number = 1,
               downsample = TRUE,
               package = "randomForest")
file.rename(testthat::test_path("fixtures", "train", "rf_1.rds"), testthat::test_path("fixtures", "train", "rf_randomForest.rds"))

# save densities
dens <- make_densities_from_rf(rf, testthat::test_path("fixtures", "train"))

# save ranger random forest
rf <- train_rf(df = cfr,
               ntrees = 200,
               train_prompt_code = "pLND",
               distance_measures = "euc",
               output_dir = testthat::test_path("fixtures", "train"),
               run_number = 1,
               downsample = TRUE,
               package = "ranger")
file.rename(testthat::test_path("fixtures", "train", "rf_1.rds"), testthat::test_path("fixtures", "train", "rf_ranger.rds"))


