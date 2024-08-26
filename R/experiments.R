run_experiment <- function(df, train_prompt_code, test_prompt_code, train_n,
                           dataset_name, distances, ntrees, run_number, downsample = TRUE) {
  set.seed(run_number)
  browser()
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
  saveRDS(rf, "rf.rds")
  update_log(new_log)


  return(list("rf"= rf, "slrs" = slrs, "errors" = errors))
}


update_log <- function(new_log) {
  # sort log columns
  new_log <- new_log %>% dplyr::select(run_number, dataset, train_prompt, test_prompt, train_n, test_n, distances,
                                       rf_ntrees, fnr, fpr, auc)

  # add to existing log or start new log
  if (file.exists(file.path("experiments", "experiments_log.csv"))){
    exlog <- read.csv(file.path("experiments", "experiments_log.csv"))
    exlog <- rbind(exlog, new_log)
  } else {
    exlog <- new_log
  }
  write.csv(exlog, file.path("experiments", "experiments_log.csv"), row.names = FALSE)
}


make_auc_table <- function(log_path){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::group_by(train_prompt, test_prompt) %>%
    dplyr::summarize(mean_auc = mean(auc)) %>%
    tidyr::pivot_wider(names_from = "test_prompt", values_from = "mean_auc")

  write.csv(stats, file.path(dirname(log_path), "mean_auc_table.csv"))

  return(stats)
}


make_error_table <- function(log_path){
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::group_by(train_prompt, test_prompt) %>%
    dplyr::summarize(mean_error = 0.5*(mean(fpr) + mean(fnr))) %>%
    tidyr::pivot_wider(names_from = "test_prompt", values_from = "mean_error")

  write.csv(stats, file.path(dirname(log_path), "mean_error_table.csv"))

  return(stats)
}


plot_mean_errors_barchart <- function(log_path) {
  exlog <- read.csv(log_path)

  stats <- exlog %>%
    dplyr::group_by(train_prompt, test_prompt) %>%
    dplyr::summarize(min_fpr = min(fpr),
                     max_fpr = max(fpr),
                     mean_fpr = mean(fpr),
                     min_fnr = min(fnr),
                     max_fnr = max(fnr),
                     mean_fnr = mean(fnr))

  plots <- list()
  plots[[1]] <- stats %>% ggplot(aes(x=test_prompt, y=mean_fpr)) +
    geom_bar(stat="identity", color="black",
             position=position_dodge()) +
    geom_errorbar(aes(ymin=min_fpr, ymax=max_fpr), width=.2,
                  position=position_dodge(.9)) +
    facet_wrap(~train_prompt) +
    ylim(0,1) +
    theme_bw()

  plots[[2]] <- stats %>% ggplot(aes(x=test_prompt, y=mean_fnr)) +
    geom_bar(stat="identity", color="black",
             position=position_dodge()) +
    geom_errorbar(aes(ymin=min_fnr, ymax=max_fnr), width=.2,
                  position=position_dodge(.9)) +
    facet_wrap(~train_prompt) +
    ylim(0,1) +
    theme_bw()

  return(plots)
}
