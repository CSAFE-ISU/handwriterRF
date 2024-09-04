run_experiment <- function(df,
                           train_prompt_code,
                           test_prompt_code,
                           train_n,
                           dataset_name,
                           distance_measures,
                           ntrees,
                           run_number,
                           use_random_forest = TRUE,
                           downsample = TRUE) {

  set.seed(run_number)

  # create output directory if it doesn't already exist
  if (downsample){
    outdir <- file.path("experiments", "tests_downsampled", paste0(dataset_name, "_", paste0(distance_measures, collapse="_")))
  } else {
    outdir <- file.path("experiments", "tests_not_downsampled", paste0(dataset_name, "_", paste0(distance_measures, collapse="_")))
  }
  if (!dir.exists(outdir)){
    dir.create(outdir)
  }

  # Log ---------------------------------------------------------------------
  # start new log entry (data frame with 1 row)
  new_log <- data.frame("dataset" = dataset_name,
                        "train_prompt" = train_prompt_code,
                        "test_prompt" = test_prompt_code,
                        "train_n" = train_n,
                        "distance_measures" = paste(distance_measures, collapse = " + "),
                        "use_random_forest" = paste(use_random_forest),
                        "rf_ntrees" = ntrees,
                        "run_number" = run_number,
                        "downsample_train_set" = "TRUE",
                        "downsample_test_set" = paste(downsample))


  # Get Train and Test Sets -------------------------------------------------
  df <- expand_docnames(df)
  # train: n writers * 3 docs each = 3n docs
  # test: m writers * 3 docs each = 3m docs
  sets <- get_train_test_sets(df, train_prompt_code, test_prompt_code, train_n)
  new_log$test_n <- length(sets$writers$test)


  # Get Distances -----------------------------------------------------------
  dists <- list()
  dists$train <- get_distances(df = sets$train, distance_measures = distance_measures)
  dists$test <- get_distances(df = sets$test, distance_measures = distance_measures)

  if (downsample){
    dists$train <- downsample_diff_pairs(dists$train)
    dists$test <- downsample_diff_pairs(dists$test)
  }

  # Random Forest with Ranger Package ---------------------------------------

  # rf <- ranger::ranger(formula,
  #                      data = train,
  #                      importance = 'permutation',
  #                      scale.permutation.importance = TRUE,
  #                      num.trees = 200)
  # train_votes <- get_proportion_of_votes(rf, train)
  # test_votes <- get_proportion_of_votes(rf, test)
  #
  # get_ranger_proportion_of_votes <- function(rf, df){
  #   pred <- predict(rf, subset(df, select = -match), predict.all=TRUE)
  #   pred <- as.data.frame(pred$predictions)
  #   ntrees <- ncol(pred)
  #   votes <- data.frame(votes = rowSums(pred == 2) / ntrees, match = df$match)
  #   return(votes)
  # }

  # Scores ------------------------------------------------------------------

  scores <- get_scores(dists, distance_measures = distance_measures, run_number = run_number, use_random_forest = use_random_forest, ntrees = ntrees, downsample = downsample)


  # SLRs --------------------------------------------------------------------
  slrs <- get_slrs(scores)


  # Evaluate ----------------------------------------------------------------
  # Errors
  errors <- calculate_error_rates(slrs)
  new_log$fnr <- errors$fnr
  new_log$fpr <- errors$fpr

  # AUC
  roc <- PRROC::roc.curve(scores.class0 = slrs$same_writer, scores.class1 = slrs$diff_writer)
  new_log$auc <- roc$auc

  # Save --------------------------------------------------------------------
  # save / update log
  update_log(new_log, outdir)

  return(list("rf"= rf, "slrs" = slrs, "errors" = errors))
}


update_log <- function(new_log, outdir) {

  # sort log columns
  new_log <- new_log %>% dplyr::select(run_number, dataset, train_prompt, test_prompt, train_n, test_n, distance_measures,
                                       use_random_forest, rf_ntrees, downsample_train_set, downsample_test_set, fnr, fpr, auc)

  # add to existing log or start new log
  if (file.exists(file.path(outdir, "experiments_log.csv"))){
    exlog <- read.csv(file.path(outdir, "experiments_log.csv"))
    exlog <- rbind(exlog, new_log)
  } else {
    exlog <- new_log
  }
  write.csv(exlog, file.path(outdir, "experiments_log.csv"), row.names = FALSE)

  return(exlog)
}


make_auc_table_by_test_prompt <- function(log_path){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::group_by(train_prompt, test_prompt) %>%
    dplyr::summarize(mean_auc = mean(auc)) %>%
    tidyr::pivot_wider(names_from = "test_prompt", values_from = "mean_auc")

  write.csv(stats, file.path(dirname(log_path), "mean_auc_by_test_prompt.csv"), row.names = FALSE)

  return(stats)
}

make_auc_table_with_test_prompts_grouped <- function(log_path, test_prompts){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::filter(test_prompt %in% test_prompts) %>%
    dplyr::group_by(train_prompt) %>%
    dplyr::summarize(mean_auc = mean(auc))

  write.csv(stats, file.path(dirname(log_path), paste0("mean_auc_with_", paste0(test_prompts, collapse = "_"), "_grouped.csv")), row.names = FALSE)

  return(stats)
}

make_error_table_by_test_prompt <- function(log_path){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::mutate(error = 0.5*(fpr + fnr)) %>%
    dplyr::group_by(train_prompt, test_prompt) %>%
    dplyr::summarize(mean_error = mean(error)) %>%
    tidyr::pivot_wider(names_from = "test_prompt", values_from = "mean_error")

  write.csv(stats, file.path(dirname(log_path), "mean_error_by_test_prompt.csv"), row.names = FALSE)

  return(stats)
}

make_error_table_with_test_prompts_grouped <- function(log_path, test_prompts){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::mutate(error = 0.5*(fpr + fnr)) %>%
    dplyr::filter(test_prompt %in% test_prompts) %>%
    dplyr::group_by(train_prompt) %>%
    dplyr::summarize(mean_error = mean(error))

  write.csv(stats, file.path(dirname(log_path), paste0("mean_error_with_", paste0(test_prompts, collapse = "_"), "_grouped.csv")), row.names = FALSE)

  return(stats)
}


make_fpr_table_with_test_prompts_grouped <- function(log_path, test_prompts){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::filter(test_prompt %in% test_prompts) %>%
    dplyr::group_by(train_prompt) %>%
    dplyr::summarize(mean_fpr = mean(fpr))

  write.csv(stats, file.path(dirname(log_path), paste0("mean_fpr_with_", paste0(test_prompts, collapse = "_"), "_grouped.csv")), row.names = FALSE)

  return(stats)
}

make_fnr_table_with_test_prompts_grouped <- function(log_path, test_prompts){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::filter(test_prompt %in% test_prompts) %>%
    dplyr::group_by(train_prompt) %>%
    dplyr::summarize(mean_fnr = mean(fnr))

  write.csv(stats, file.path(dirname(log_path), paste0("mean_fnr_with_", paste0(test_prompts, collapse = "_"), "_grouped.csv")), row.names = FALSE)

  return(stats)
}


powerset <- function(x) {
  sets <- lapply(1:(length(x)), function(i) combn(x, i, simplify = F))
  unlist(sets, recursive = F)
}

