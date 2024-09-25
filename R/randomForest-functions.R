#' Train a Random Forest with randomForest
#'
#' Train a random forest from a data frame of cluster fill rates. The package
#' randomForest is used.
#'
#' @param df A data frame of cluster fill rates created with
#'   'get_cluster_fill_rates'
#' @param ntrees An integer number of decision trees to use
#' @param train_prompt_code Which prompt to use in the training set: "pLND",
#'   "pPHR", "pWOZ", or "pCMB"
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
                                  train_prompt_code,
                                  distance_measures,
                                  output_dir,
                                  run_number = 1,
                                  downsample = TRUE){
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  set.seed(run_number)

  # create output directory if it doesn't already exist
  create_dir(output_dir)

  # get train set
  train <- get_train_set(df = df, train_prompt_code = train_prompt_code)

  # get distances between all pairs of documents
  dists <- get_distances(df = train, distance_measures = distance_measures)

  dists <- label_same_different_writer(dists)

  if (downsample){
    dists <- downsample_diff_pairs(dists)
  }

  # train and save random forest
  random_forest <- list()
  random_forest$rf <- randomForest::randomForest(match ~ ., data = subset(dists, select = -c(docname1, docname2)), ntree = ntrees)

  # add distances to list
  random_forest$dists <- dists
  saveRDS(random_forest, file.path(output_dir, paste0("rf_randomForest", run_number, ".rds")))

  return(random_forest)
}

#' Make Densities from a Trained randomForest Random Forest
#'
#' Create densities of "same writer" and "different writer" scores produced by a
#' trained random forest.
#'
#' @param random_forest A random forest created with 'train_rf'.
#' @param output_dir A path to a directory where the random forest will be
#'   saved.
#'
#' @return A list of densities
#'
#' @noRd
make_densities_randomForest_rf <- function(random_forest, output_dir) {
  # Prevent note "no visible binding for global variable"
  score <- session <- prompt <- rep <- total_graphs <- NULL

  scores_df <- as.data.frame(random_forest$rf$votes)['same']
  # add labels from train data frame
  scores_df$match <- random_forest$dists$match
  colnames(scores_df) <- c("score", "match")

  # split the train and test sets into same and different writers to make it
  # easier on the next step
  scores <- list()
  scores$same_writer <- scores_df %>% dplyr::filter(match == "same") %>% dplyr::pull(score)
  scores$diff_writer <- scores_df %>% dplyr::filter(match == "different") %>% dplyr::pull(score)

  pdfs <- list()
  pdfs$same_writer <- stats::density(scores$same_writer, kernel = "gaussian", n=10000)
  pdfs$diff_writer <- stats::density(scores$diff_writer, kernel = "gaussian", n=10000)

  saveRDS(pdfs, file.path(output_dir, "densities_randomForest.rds"))

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
#' @param random_forest A random forest created with `train_rf`.
#'
#' @return A number
#'
#' @noRd
get_randomForest_score <- function(d, random_forest){
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  d <- d %>% dplyr::select(-tidyselect::any_of(c("docname1", "docname2")))

  score <- stats::predict(random_forest, d, type="prob")[,2]

  return(score)
}
