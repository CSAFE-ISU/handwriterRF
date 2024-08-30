run_experiment <- function(df, train_prompt_code, test_prompt_code, train_n,
                           dataset_name, distances, ntrees, run_number, downsample = TRUE) {

  set.seed(run_number)

  # Log ---------------------------------------------------------------------
  # start new log entry (data frame with 1 row)
  new_log <- data.frame("dataset" = dataset_name,
                        "train_prompt" = train_prompt_code,
                        "test_prompt" = test_prompt_code,
                        "train_n" = train_n,
                        "distances" = paste(distances, collapse = " + "),
                        "rf_ntrees" = ntrees,
                        "run_number" = run_number)


  # Get Train and Test Sets -------------------------------------------------
  df <- expand_docnames(df)
  # train: n writers * 3 docs each = 3n docs
  # test: m writers * 3 docs each = 3m docs
  sets <- get_train_test_sets(df, train_prompt_code, test_prompt_code, train_n)
  new_log$test_n <- length(sets$writers$test)


  # Get Distances -----------------------------------------------------------
  dists <- list()
  dists$train <- get_distances(df = sets$train, distances = distances)
  dists$test <- get_distances(df = sets$test, distances = distances)

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

  # Random Forests ----------------------------------------------------------

  rf <- randomForest::randomForest(match ~ ., data = subset(dists$train, select = -c(docname1, docname2)), ntree = ntrees)


  # SLRs --------------------------------------------------------------------
  scores <- get_scores(rf, dists)
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
  # create output directory if it doesn't already exist
  if (downsample){
    outdir <- file.path("experiments", "tests_downsampled", paste0(dataset_name, "_", paste0(distances, collapse="_")))
  } else {
    outdir <- file.path("experiments", "tests_not_downsampled", paste0(dataset_name, "_", paste0(distances, collapse="_")))
  }
  if (!dir.exists(outdir)){
    dir.create(outdir)
  }

  # save random forest
  saveRDS(rf, file.path(outdir, paste0("rf_", run_number, ".rds")))

  # save / update log
  update_log(new_log, outdir)

  return(list("rf"= rf, "slrs" = slrs, "errors" = errors))
}


update_log <- function(new_log, outdir) {
  # sort log columns
  new_log <- new_log %>% dplyr::select(run_number, dataset, train_prompt, test_prompt, train_n, test_n, distances,
                                       rf_ntrees, fnr, fpr, auc)

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

  write.csv(stats, file.path(dirname(log_path), "mean_auc_by_test_prompt.csv"))

  return(stats)
}

make_auc_table_test_prompts_grouped_together <- function(log_path, test_prompts){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::filter(test_prompt %in% test_prompts) %>%
    dplyr::group_by(train_prompt) %>%
    dplyr::summarize(mean_auc = mean(auc))

  write.csv(stats, file.path(dirname(log_path), paste0("mean_auc_", paste0(test_prompts, collapse = "_"), "_grouped_together.csv")))

  return(stats)
}

powerset <- function(x) {
  sets <- lapply(1:(length(x)), function(i) combn(x, i, simplify = F))
  unlist(sets, recursive = F)
}

