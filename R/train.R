# The handwriterRF R package performs writership analysis of handwritten
# documents. Copyright (C) 2024 Iowa State University of Science and Technology
# on behalf of its Center for Statistics and Applications in Forensic Evidence
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.


# External Functions ------------------------------------------------------

#' Train a Random Forest
#'
#' Train a random forest with \pkg{ranger} from a data frame of cluster fill rates.
#'
#' @param df A data frame of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}}
#' @param ntrees An integer number of decision trees to use
#' @param distance_measures A vector of distance measures. Any combination of
#'   'abs', 'euc', 'man', 'max', and 'cos' may be used.
#' @param output_dir A path to a directory where the random forest will be
#'   saved.
#' @param run_number An integer used for both the set.seed function and to
#'   distinguish between different runs on the same input data frame.
#' @param downsample Whether to downsample the number of different writer
#'   distances before training the random forest. If TRUE, the different writer
#'   distances will be randomly sampled, resulting in the same number of
#'   different writer and same writer pairs.
#'
#' @return A random forest
#'
#' @export
#'
#' @examples
#' train <- get_csafe_train_set(df = cfr, train_prompt_code = 'pCMB')
#' rforest <- train_rf(
#'   df = train,
#'   ntrees = 200,
#'   distance_measures = c('euc'),
#'   run_number = 1,
#'   downsample = TRUE
#' )
train_rf <- function(df,
                     ntrees,
                     distance_measures,
                     output_dir = NULL,
                     run_number = 1,
                     downsample = TRUE) {
  # Prevent note 'no visible binding for global variable'
  docname1 <- docname2 <- NULL

  set.seed(run_number)

  # set output directory to a new folder in the temp directory
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), 'comparison')
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
    importance = 'permutation',
    scale.permutation.importance = TRUE,
    num.trees = 200
  )

  # add distances to list
  rforest$dists <- dists

  # get densities from training data
  rforest$densities <- make_densities_from_rf(rforest = rforest)

  saveRDS(rforest, file.path(output_dir, paste0('rf', run_number, '.rds')))

  return(rforest)
}


#' Get Training Set
#'
#' Create a training set from a data frame of cluster fill rates from the CSAFE
#' Handwriting Database.
#'
#' @param df A data frame of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}}
#' @param train_prompt_codes A character vector of which prompt(s) to use in the
#'   training set. Available prompts are 'pLND', 'pPHR', 'pWOZ', and 'pCMB'.
#'
#' @return A data frame
#'
#' @export
#'
#' @examples
#' train <- get_csafe_train_set(df = cfr, train_prompt_codes = 'pCMB')
#'
get_csafe_train_set <- function(df, train_prompt_codes) {
  # Prevent note 'no visible binding for global variable'
  writer <- session <- prompt <- rep <- total_graphs <- NULL

  df <- expand_docnames(df)

  # build train set
  train <- df %>%
    dplyr::filter(prompt %in% train_prompt_codes) %>%
    dplyr::select(-writer, -session, -prompt, -rep, -total_graphs)

  # return data frame instead of tibble
  train <- as.data.frame(train)

  return(train)
}


# Internal Functions ------------------------------------------------------

#' Make Densities from a Trained Random Forest
#'
#' Create densities of same writer and different writer scores produced by a
#' trained random forest.
#'
#' @param rforest A \pkg{ranger} random forest created with \code{\link{train_rf}}.
#'
#' @return A list of densities
#'
#' @noRd
make_densities_from_rf <- function(rforest) {
  # Prevent note 'no visible binding for global variable'
  score <- session <- prompt <- rep <- total_graphs <- NULL

  scores_df <- data.frame('score' = get_score(rforest$dists, rforest = rforest))

  # add labels from train data frame
  scores_df$match <- rforest$dists$match

  # split the train and test sets into same and different writers to make it
  # easier on the next step
  scores <- list()
  scores$same_writer <- scores_df %>%
    dplyr::filter(match == 'same') %>%
    dplyr::pull(score)
  scores$diff_writer <- scores_df %>%
    dplyr::filter(match == 'different') %>%
    dplyr::pull(score)

  pdfs <- list()
  pdfs$same_writer <- stats::density(scores$same_writer, kernel = 'gaussian', n = 10000)
  pdfs$diff_writer <- stats::density(scores$diff_writer, kernel = 'gaussian', n = 10000)

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
  n <- sum(df$match == 'same')
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
  # prevent note 'no visible binding for global variable'
  writer1 <- writer2 <- session1 <- prompt1 <- rep1 <- session2 <- prompt2 <- rep2 <- NULL

  dists <- expand_docnames(dists, 'docname1', '1')
  dists <- expand_docnames(dists, 'docname2', '2')

  dists <- dists %>% dplyr::mutate(match = ifelse(writer1 == writer2, 'same', 'different'))

  # make match a factor
  dists$match <- as.factor(dists$match)

  # drop columns in prep for rf
  dists <- dists %>% dplyr::select(-writer1, -session1, -prompt1, -rep1, -writer2, -session2, -prompt2, -rep2)

  return(dists)
}


#' Which Distances were Used in a Random Forest
#'
#' @param rforest A \pkg{ranger} random forest created with \code{\link{train_rf}}.
#'
#' @return A character vector of distance measures
#'
#' @noRd
which_dists <- function(rforest) {
  # get the distance measures from the column names of rforest$dist
  df <- rforest$dists %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::starts_with('cluster'), dplyr::any_of(c('man', 'euc', 'max', 'cos')))
  distance_measures <- colnames(df)

  # add 'abs' and delete 'cluster<#>'
  if (any(startsWith(distance_measures, 'cluster'))){
    distance_measures <- c('abs', distance_measures)
    distance_measures <- distance_measures[!startsWith(distance_measures, 'cluster')]
  }

  return(distance_measures)
}
