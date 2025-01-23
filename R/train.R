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
#' Train a random forest with \pkg{ranger} from a dataframe of writer profiles
#' estimated with \code{\link{get_cluster_fill_rates}}. `train_rf` calculates
#' the distance between all pairs of writer profiles using one or more distance
#' measures. Currently, the available distance measures are absolute, Manhattan,
#' Euclidean, maximum, and cosine.
#'
#' The absolute distance between two n-length vectors of cluster fill rates, a
#' and b, is a vector of the same length as a and b. It can be calculated as
#' abs(a-b) where subtraction is performed element-wise, then the absolute
#' value of each element is returned. More specifically, element i of the vector is \eqn{|a_i
#' - b_i|} for \eqn{i=1,2,...,n}.
#'
#' The Manhattan distance between two  n-length vectors of cluster fill rates, a and b, is
#' \eqn{\sum_{i=1}^n |a_i - b_i|}. In other words, it is the sum of the absolute
#' distance vector.
#'
#' The Euclidean distance between two  n-length vectors of cluster fill rates, a and b, is
#' \eqn{\sqrt{\sum_{i=1}^n (a_i - b_i)^2}}. In other words, it is the sum of the elements of the
#' absolute distance vector.
#'
#' The maximum distance between two n-length vectors of cluster fill rates, a and b, is
#' \eqn{\max_{1 \leq i \leq n}{\{|a_i - b_i|\}}}. In other words, it is the sum of the elements of the
#' absolute distance vector.
#'
#' The cosine distance between two n-length vectors of cluster fill rates, a and b, is
#' \eqn{\sum_{i=1}^n (a_i - b_i)^2 / (\sqrt{\sum_{i=1}^n a_i^2}\sqrt{\sum_{i=1}^n b_i^2})}.
#'
#' @param df A dataframe of writer profiles created with
#'   \code{\link{get_cluster_fill_rates}}
#' @param ntrees An integer number of decision trees to use
#' @param distance_measures A vector of distance measures. Any combination of
#'   'abs', 'euc', 'man', 'max', and 'cos' may be used.
#' @param output_dir A path to a directory where the random forest will be
#'   saved.
#' @param run_number An integer used for both the set.seed function and to
#'   distinguish between different runs on the same input dataframe.
#' @param downsample_diff_pairs Whether to downsample the number of different writer
#'   distances before training the random forest. If TRUE, the different writer
#'   distances will be randomly sampled, resulting in the same number of
#'   different writer and same writer pairs.
#'
#' @return A random forest
#'
#' @export
#'
#' @examples
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
                     downsample_diff_pairs = TRUE) {

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

  if (downsample_diff_pairs) {
    dists <- downsample(dists)
  }

  # train and save random forest
  rforest <- list()
  train_df <- dists %>% dplyr::select(-tidyselect::any_of(c("docname1", "docname2")))

  rforest$rf <- ranger::ranger(match ~ .,
    data = train_df,
    importance = "permutation",
    scale.permutation.importance = TRUE,
    num.trees = 200
  )

  # add distances to list
  rforest$distance_measures <- distance_measures

  saveRDS(rforest, file.path(output_dir, paste0("rf", run_number, ".rds")))

  return(rforest)
}


# Internal Functions ------------------------------------------------------

#' Downsample Pairs of Different Writers
#'
#' @param df A dataframe
#'
#' @return A dataframe
#'
#' @noRd
downsample <- function(df) {
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
#' @param dists A dataframe of distances
#'
#' @return A dataframe
#' @noRd
label_same_different_writer <- function(dists) {

  dists$match <- ifelse(dists$writer1 == dists$writer2, "same", "different")

  # make match a factor
  dists$match <- as.factor(dists$match)

  # drop columns in prep for rf
  dists <- dists %>% dplyr::select(-tidyselect::all_of(c("writer1", "writer2")))

  return(dists)
}
