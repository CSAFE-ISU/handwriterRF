devtools::load_all()

dataset_name <- "CSAFE"
downsample = TRUE
distances <- c("abs", "euc")
prompts <- c("pCMB")

main_dir <- "data-raw/temp"

# randomForest
rf <- train_rf(df = cfr,
               ntrees = 200,
               train_prompt_code = "pCMB",
               distance_measures = c("abs", "euc"),
               output_dir = main_dir,
               run_number=1,
               downsample = TRUE,
               package = "randomForest")
file.rename(file.path(main_dir, "rf_1.rds"), file.path(main_dir, "rf.rds"))
usethis::use_data(rf, overwrite = TRUE)

# ranger
rf_ranger <- train_rf(df = cfr,
                      ntrees = 200,
                      train_prompt_code = "pCMB",
                      distance_measures = c("abs", "euc"),
                      output_dir = main_dir,
                      run_number = 1,
                      downsample = TRUE,
                      package = "ranger")
file.rename(file.path(main_dir, "rf_1.rds"), file.path(main_dir, "rf_ranger.rds"))
usethis::use_data(rf_ranger, overwrite = TRUE)

# randomForest densities
densities <- make_densities_from_rf(random_forest = rf, output_dir = main_dir)
usethis::use_data(densities, overwrite = TRUE)
