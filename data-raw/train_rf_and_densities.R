devtools::load_all()

dataset_name <- "CSAFE"
downsample = TRUE
distances <- c("abs", "euc")
prompts <- c("pCMB")

main_dir <- "data-raw"

rf <- train_rf(df = cfr,
               ntrees = 200,
               train_prompt_code = "pCMB",
               distance_measures = c("abs", "euc"),
               output_dir = main_dir,
               run_number=1,
               downsample = TRUE)
file.rename(file.path(main_dir, "rf_1.rds"), file.path(main_dir, "rf.rds"))

densities <- make_densities_from_rf(rf = rf, output_dir = main_dir)

usethis::use_data(rf, overwrite = TRUE)
usethis::use_data(densities, overwrite = TRUE)
