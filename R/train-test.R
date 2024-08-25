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
get_train_test_sets <- function(df, train_prompt_code, test_prompt_code, train_n) {
  writers <- get_train_test_writers(df = df, train_n = train_n)

  # build train set
  train <- df %>%
    dplyr::filter(writer %in% writers$train, prompt == train_prompt_code) %>%
    dplyr::select(-writer, -session, -prompt, -rep, -total_graphs)

  # build test set
  test <- df %>%
    dplyr::filter(writer %in% writers$test, prompt == test_prompt_code) %>%
    dplyr::select(-writer, -session, -prompt, -rep, -total_graphs)

  return(list("train" = train, "test" = test, "writers" = writers))
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
get_train_test_writers <- function(df, train_n) {
  writers <- unique(df$writer)
  train <- sample(writers, train_n)
  test <- setdiff(writers, train)
  return(list("train" = train, "test" = test))
}
