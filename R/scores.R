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

#' Get Reference Scores
#'
#' Create reference scores of same writer and different writer scores from a
#' data frame of cluster fill rates.
#'
#' @param rforest A \pkg{ranger} random forest created with
#'   \code{\link{train_rf}}.
#' @param df A data frame of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}} with an added writer ID column.
#'
#' @return A list of scores
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_ref_scores(rforest = random_forest, df = validation)
#' }
#'
get_ref_scores <- function(rforest, df) {
  # Prevent note 'no visible binding for global variable'
  score <- session <- prompt <- rep <- total_graphs <- NULL

  dist_measures <- which_dists(rforest = rforest)
  d <- get_distances(df = df, distance_measures = dist_measures)

  scores_df <- get_score(d = d, rforest = rforest)

  # split into same and different writers to make it easier on the next step
  scores <- list()
  scores$same_writer <- scores_df %>%
    dplyr::filter(match == "same")
  scores$diff_writer <- scores_df %>%
    dplyr::filter(match == "different")

  return(scores)
}
