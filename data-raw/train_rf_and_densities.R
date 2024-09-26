devtools::load_all()

main_dir <- "data-raw/temp"

rf <- train_rf(
  df = cfr,
  ntrees = 200,
  train_prompt_code = "pCMB",
  distance_measures = c("abs", "euc"),
  output_dir = main_dir,
  run_number = 1,
  downsample = TRUE
)
file.rename(file.path(main_dir, "rf1.rds"), file.path(main_dir, "rf.rds"))
usethis::use_data(rf, overwrite = TRUE)

densities <- make_densities_from_rf(random_forest = rf, output_dir = main_dir)
usethis::use_data(densities, overwrite = TRUE)
