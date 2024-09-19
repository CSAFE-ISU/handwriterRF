#' Get Training Set
#'
#' Create a training set from a data frame of cluster fill rates created with
#' 'get_cluster_fill_rates'.
#'
#' @param df A data frame of cluster fill rates created with
#' 'get_cluster_fill_rates'
#' @param train_prompt_code Which prompt to use in the training set: "pLND", "pPHR", "pWOZ", or "pCMB"
#'
#' @return A data frame
#'
#' @noRd
get_train_set <- function(df, train_prompt_code) {

  # build train set
  train <- df %>%
    dplyr::filter(prompt == train_prompt_code) %>%
    dplyr::select(-writer, -session, -prompt, -rep, -total_graphs)

  return(train)
}

train_rf <- function(df, ntrees, train_prompt_code, distance_measures, output_dir, run_number=1, downsample = TRUE){
  set.seed(run_number)

  # create output directory if it doesn't already exist
  create_dir(output_dir, recursive = TRUE)

  # get train set
  df <- expand_docnames(df)
  train <- get_train_set(df = df, train_prompt_code = train_prompt_code)

  # get distances between all pairs of documents
  dists <- get_distances(df = train, distance_measures = distance_measures)

  if (downsample){
    dists <- downsample_diff_pairs(dists)
  }

  # train and save random forest
  rf <- list()
  rf$rf <- randomForest::randomForest(match ~ ., data = subset(dists, select = -c(docname1, docname2)), ntree = ntrees)

  # add distances to list
  rf$dists <- dists
  saveRDS(rf, file.path(output_dir, paste0("rf_", run_number, ".rds")))

  return(rf)
}

make_densities_from_rf <- function(rf, output_dir) {
  scores_df <- as.data.frame(rf$rf$votes)['same']
  # add labels from train data frame
  scores_df$match <- rf$dists$match
  colnames(scores_df) <- c("score", "match")

  # split the train and test sets into same and different writers to make it
  # easier on the next step
  scores <- list()
  scores$same_writer <- scores_df %>% dplyr::filter(match == "same") %>% dplyr::pull(score)
  scores$diff_writer <- scores_df %>% dplyr::filter(match == "different") %>% dplyr::pull(score)

  pdfs <- list()
  pdfs$same_writer <- density(scores$same_writer, kernel = "gaussian", n=10000)
  pdfs$diff_writer <- density(scores$diff_writer, kernel = "gaussian", n=10000)

  saveRDS(pdfs, file.path(output_dir, "densities.rds"))

  return(pdfs)
}



#' Downsample Pairs of Different Writer Distances
#'
#' @param df A data frame of distances
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dists <- get_distances(df = LND, distances = c("euclidean"))
#'
downsample_diff_pairs <- function(df){
  n <- sum(df$match == "same")
  df <- df %>%
    dplyr::group_by(match) %>%
    dplyr::slice_sample(n=n)
  return(df)
}
