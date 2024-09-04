get_votes <- function(rf, dists){
  train_votes <- as.data.frame(rf$votes)['same']
  # add labels from train data frame
  train_votes$match <- dists$train$match
  colnames(train_votes) <- c("score", "match")

  test_votes <- as.data.frame(predict(rf, subset(dists$test, select = -c(docname1, docname2)), type="prob"))['same']
  # add labels from test data frame
  test_votes$match <- dists$test$match
  colnames(test_votes) <- c("score", "match")

  return(list("train" = train_votes, "test" = test_votes))
}

get_scores <- function(dists, distance_measures, run_number, use_random_forest, ntrees, downsample) {
  if (use_random_forest){
    rf <- randomForest::randomForest(match ~ ., data = subset(dists$train, select = -c(docname1, docname2)), ntree = ntrees)
    scores <- get_votes(rf, dists)

    # save random forest
    if (downsample){
      outdir <- file.path("experiments", "tests_downsampled", paste0(dataset_name, "_", paste0(distance_measures, collapse="_")))
    } else {
      outdir <- file.path("experiments", "tests_not_downsampled", paste0(dataset_name, "_", paste0(distance_measures, collapse="_")))
    }
    saveRDS(rf, file.path(outdir, paste0("rf_", run_number, ".rds")))
  } else {
    if (ncol(dists$train) != 4){
      warning("More than one distance measure requires a random forest. Using a random forest.")
    }

    scores <- list()
    scores$train <- dists$train %>% dplyr::select(-docname1, -docname2)
    colnames(scores$train) <- c("score", "match")

    scores$test <- dists$train %>% dplyr::select(-docname1, -docname2)
    colnames(scores$test) <- c("score", "match")
  }

  # split the train and test sets into same and different writers to make it
  # easier to calculate errors on the next step
  scores$train_same_writer <- scores$train %>% dplyr::filter(match == "same") %>% dplyr::pull(score)
  scores$train_diff_writer <- scores$train %>% dplyr::filter(match == "different") %>% dplyr::pull(score)

  scores$test_same_writer <- scores$test %>% dplyr::filter(match == "same") %>% dplyr::pull(score)
  scores$test_diff_writer <- scores$test %>% dplyr::filter(match == "different") %>% dplyr::pull(score)

  # drop the original train and test sets
  scores$train <- NULL
  scores$test <- NULL

  return(scores)
}

# plot_votes <- function(votes, type = "train"){
#   p <- votes[[type]] %>%
#     ggplot(aes(x=votes, color=match, fill=match)) +
#     geom_density(alpha = 0.2) +
#     theme_bw()
#   return(p)
# }
