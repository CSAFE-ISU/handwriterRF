#' Get Training and Testing Sets
#'
#' Create training and testing sets from a data frame of distances created with
#' 'get_distances'.
#'
#' @param dists A data frame of distances created with 'get_distances'
#' @param train_n The number of writers to assign to the training set
#' @param downsample True or False. True downsamples the different writer
#'   distances in the training set to equal the number of same writer distances.
#'   The test set is not changed.
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dists <- get_distances(df = cfr, distances = c("euclidean"))
#' sets <- get_train_test_sets(dists, 70)
get_train_test_sets <- function(dists, train_n, downsample = TRUE) {
  writers <- get_train_test_writers(dists = dists, train_n = train_n)

  # build train set
  train_w_doc_info <- dists %>%
    dplyr::filter(writer1 %in% writers$train, writer2 %in% writers$train)

  if (downsample){
    train_w_doc_info <- downsample_diff_pairs(train_w_doc_info)
  }

  # drop columns with doc info in prep for the random forest
  train <- train_w_doc_info %>%
    dplyr::select(-writer1, -session1, -prompt1, -rep1, -docname1, -writer2, -session2, -prompt2, -rep2, -docname2)

  # build test set
  test_w_doc_info <- dists %>%
    dplyr::filter(writer1 %in% writers$test, writer2 %in% writers$test)

  test <- test_w_doc_info %>%
    dplyr::select(-writer1, -session1, -prompt1, -rep1, -docname1, -writer2, -session2, -prompt2, -rep2, -docname2)

  return(list("train" = train, "test" = test, "train_w_doc_info" = train_w_doc_info, "test_w_doc_info" = test_w_doc_info))
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

#' Get Writers for Train and Test Sets
#'
#' Randomly assign writers to train and test sets.
#'
#' @param dists A data frame of distances created with `get_distances`
#' @param train_n The number of writers in 'dists' to assign to to the train set. The
#'   remaining writers are assigned to the test set.
#'
#' @return A list
#' @export
#'
#' @examples
#' dists <- get_distances(df = cfr, distances = c("euclidean"))
#' writers <- get_train_test_writers(dists, 70)
#'
get_train_test_writers <- function(dists, train_n) {
  writers <- unique(dists$writer1)
  train <- sample(writers, train_n)
  test <- setdiff(writers, train)
  return(list("train" = train, "test" = test))
}
