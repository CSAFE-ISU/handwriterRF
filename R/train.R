# External Functions ------------------------------------------------------

#' Train a Random Forest
#'
#' Train a random forest with 'ranger' from a data frame of cluster fill rates.
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
#' @export
#'
#' @examples
#' train <- get_csafe_train_set(df = cfr, train_prompt_code = "pCMB")
#' rforest <- train_rf(
#'   df = train,
#'   ntrees = 200,
#'   distance_measures = c("euc"),
#'   run_number = 1,
#'   downsample = TRUE
#' )
train_rf <- function(df,
                     ntrees,
                     distance_measures,
                     output_dir = NULL,
                     run_number = 1,
                     downsample = TRUE) {
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  set.seed(run_number)

  # set output directory to a new folder in the temp directory
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), "comparison")
  }

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
  rforest$rf <- ranger::ranger(match ~ .,
    data = subset(dists, select = -c(docname1, docname2)),
    importance = "permutation",
    scale.permutation.importance = TRUE,
    num.trees = 200
  )

  # add distances to list
  rforest$dists <- dists

  # get densities from training data
  rforest$densities <- make_densities_from_rf(rforest = rforest)

  saveRDS(rforest, file.path(output_dir, paste0("rf", run_number, ".rds")))

  return(rforest)
}


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
#' @export
#'
#' @examples
#' train <- get_csafe_train_set(df = cfr, train_prompt_code = "pCMB")
#'
get_csafe_train_set <- function(df, train_prompt_code) {
  # Prevent note "no visible binding for global variable"
  writer <- session <- prompt <- rep <- total_graphs <- NULL

  df <- expand_docnames(df)

  # build train set
  train <- df %>%
    dplyr::filter(prompt == train_prompt_code) %>%
    dplyr::select(-writer, -session, -prompt, -rep, -total_graphs)

  # return data frame instead of tibble
  train <- as.data.frame(train)

  return(train)
}


# Internal Functions ------------------------------------------------------

#' Make Densities from a Trained Random Forest
#'
#' Create densities of "same writer" and "different writer" scores produced by a
#' trained random forest.
#'
#' @param rforest A 'ranger' random forest created with 'train_rf'.
#'
#' @return A list of densities
#'
#' @noRd
make_densities_from_rf <- function(rforest) {
  # Prevent note "no visible binding for global variable"
  score <- session <- prompt <- rep <- total_graphs <- NULL

  scores_df <- data.frame("score" = get_score(rforest$dists, rforest = rforest))

  # add labels from train data frame
  scores_df$match <- rforest$dists$match

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


#' Downsample Pairs of Different Writer Distances
#'
#' @param df A data frame of distances
#'
#' @return A data frame
#'
#' @noRd
downsample_diff_pairs <- function(df) {
  n <- sum(df$match == "same")
  df <- df %>%
    dplyr::group_by(match) %>%
    dplyr::slice_sample(n = n)
  return(df)
}


#' Label Same and Different Writer Pairs
#'
#' Labels distances as belonging to same or different writers.
#'
#' @param dists A data frame of distances
#'
#' @return A data frame
#' @noRd
label_same_different_writer <- function(dists) {
  # prevent note "no visible binding for global variable"
  writer1 <- writer2 <- session1 <- prompt1 <- rep1 <- session2 <- prompt2 <- rep2 <- NULL

  dists <- expand_docnames(dists, "docname1", "1")
  dists <- expand_docnames(dists, "docname2", "2")

  dists <- dists %>% dplyr::mutate(match = ifelse(writer1 == writer2, "same", "different"))

  # make match a factor
  dists$match <- as.factor(dists$match)

  # drop columns in prep for rf
  dists <- dists %>% dplyr::select(-writer1, -session1, -prompt1, -rep1, -writer2, -session2, -prompt2, -rep2)

  return(dists)
}
