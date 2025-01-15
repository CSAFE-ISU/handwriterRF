devtools::load_all()
source("data-raw/helpers.R")


#  Make Train, Validation, and Test Sets ----------------------------------

# clusters_dir <- "/Users/stephanie/Documents/handwriting_datasets/CSAFE_handwriting_database/clusters"
# csafe_profiles <- make_csafe_profiles_df(clusters_dir)
csafe_profiles <- readRDS("data-raw/csafe_writer_profiles.rds")
csafe_sets <- make_csafe_sets(csafe_profiles, 111817)

# clusters_dir = "/Users/stephanie/Documents/handwriting_datasets/CVL/pages_cropped/clusters"
# cvl_profiles <- make_cvl_profiles_df(clusters_dir)
cvl_profiles <- readRDS("data-raw/cvl_writer_profiles.rds")
cvl_sets <- make_cvl_sets(cvl_profiles, 111817)

csafe_sets$train <- make_doc_column(csafe_sets$train)
csafe_sets$valid <- make_doc_column(csafe_sets$valid)
csafe_sets$test <- make_doc_column(csafe_sets$test)

train <- rbind(csafe_sets$train, cvl_sets$train)
validation <- rbind(csafe_sets$valid, cvl_sets$valid)
test <- rbind(csafe_sets$test, cvl_sets$test)

usethis::use_data(train, overwrite = TRUE)
usethis::use_data(validation, overwrite = TRUE)
usethis::use_data(test, overwrite = TRUE)


# Make Random Forest ------------------------------------------------------

main_dir <- "data-raw"
random_forest <- train_rf(
  df = train,
  ntrees = 200,
  distance_measures = c("abs", "euc"),
  output_dir = "data-raw",
  run_number = 1,
  downsample = TRUE
)
file.rename(file.path(main_dir, "rf1.rds"), file.path(main_dir, "random_forest.rds"))
usethis::use_data(random_forest, overwrite = TRUE)


# Make Reference Scores ---------------------------------------------------

ref_scores <- get_ref_scores(
  rforest = random_forest,
  df = validation,
  seed = 100,
  downsample_diff_pairs = TRUE
)
plot_scores(ref_scores)

usethis::use_data(ref_scores, overwrite = TRUE)
