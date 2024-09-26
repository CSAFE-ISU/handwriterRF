# Internal Functions ------------------------------------------------------

#' Train a Random Forest with randomForest
#'
#' Train a random forest from a data frame of cluster fill rates. The package
#' randomForest is used.
#'
#' @param df A data frame of cluster fill rates created with
#'   'get_cluster_fill_rates'
#' @param ntrees An integer number of decision trees to use
#' @param distance_measures A vector of distance measures. Any combination of
#'   "abs", "euc", "man", "max", and "cos" may be used.
#' @param output_dir A path to a directory where the random forest will be
#'   saved.
#' @param run_number An integer used for both the set.seed function and to
#'   distinguish between different runs on the same input data frame.
#' @param downsample Whether to downsample the number of "different writer"
#'   distances before training the random forest. If TRUE, the different writer
#'   distances will be randomly sampled, resulting in the same number of
#'   different writer and same writer pairs.
#'
#' @return A random forest
#'
#' @noRd
train_randomForest_rf <- function(df,
                                  ntrees,
                                  distance_measures,
                                  output_dir,
                                  run_number = 1,
                                  downsample = TRUE) {
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  set.seed(run_number)

  # create output directory if it doesn't already exist
  create_dir(output_dir)

  # get distances between all pairs of documents
  dists <- get_distances(df = df, distance_measures = distance_measures)

  dists <- label_same_different_writer(dists)

  if (downsample) {
    dists <- downsample_diff_pairs(dists)
  }

  # train and save random forest
  rforest <- list()
  rforest$rf <- randomForest::randomForest(match ~ ., data = subset(dists, select = -c(docname1, docname2)), ntree = ntrees)

  # add distances to list
  rforest$dists <- dists

  # make densities from training data
  rforest$densities <- make_densities_randomForest_rf(rforest = rforest)

  saveRDS(rforest, file.path(output_dir, paste0("rf_randomForest", run_number, ".rds")))

  return(rforest)
}

#' Make Densities from a Trained randomForest Random Forest
#'
#' Create densities of "same writer" and "different writer" scores produced by a
#' trained random forest.
#'
#' @param rforest A random forest created with 'train_rf'.
#'
#' @return A list of densities
#'
#' @noRd
make_densities_randomForest_rf <- function(rforest) {
  # Prevent note "no visible binding for global variable"
  score <- session <- prompt <- rep <- total_graphs <- NULL

  scores_df <- as.data.frame(rforest$rf$votes)["same"]
  # add labels from train data frame
  scores_df$match <- rforest$dists$match
  colnames(scores_df) <- c("score", "match")

  # split the train and test sets into same and different writers to make it
  # easier on the next step
  scores <- list()
  scores$same_writer <- scores_df %>%
    dplyr::filter(match == "same") %>%
    dplyr::pull(score)
  scores$diff_writer <- scores_df %>%
    dplyr::filter(match == "different") %>%
    dplyr::pull(score)

  pdfs <- list()
  pdfs$same_writer <- stats::density(scores$same_writer, kernel = "gaussian", n = 10000)
  pdfs$diff_writer <- stats::density(scores$diff_writer, kernel = "gaussian", n = 10000)

  return(pdfs)
}

#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples. WARNING: `library(randomForest)` needs to be
#' called before running this function. Otherwise, `stats::predict` will throw
#' an error.
#'
#' @param d A data frame of distance(s) between two handwriting samples,
#'   calculated with 'get_distances'. The distance(s) needs to be the
#'   distance(s) used to train the random forest.
#' @param rforest A random forest created with `train_rf`.
#'
#' @return A number
#'
#' @noRd
get_randomForest_score <- function(d, rforest) {
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  d <- d %>% dplyr::select(-tidyselect::any_of(c("docname1", "docname2")))

  score <- stats::predict(rforest, d, type = "prob")[, 2]

  return(score)
}
