devtools::load_all()

main_dir <- "data-raw"

train <- get_csafe_train_set(df = cfr, train_prompt_code = "pCMB")
rforest <- train_rf(
  df = cfr,
  ntrees = 200,
  distance_measures = c("abs", "euc"),
  output_dir = main_dir,
  run_number = 1,
  downsample = TRUE
)
file.rename(file.path(main_dir, "rf1.rds"), file.path(main_dir, "random_forest.rds"))
usethis::use_data(random_forest, overwrite = TRUE)
