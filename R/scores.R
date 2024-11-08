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


# Internal Functions ------------------------------------------------------

#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples as described in Madeline Johnson and Danica
#' Ommen (2021) <doi:10.1002/sam.11566>.
#'
#' @param d A data frame of distance(s) between two handwriting samples,
#'   calculated with \code{\link{get_distances}}. The distance(s) needs to be the
#'   distance(s) used to train the random forest.
#' @param rforest A \pkg{ranger} random forest created with \code{\link{train_rf}}.
#'
#' @return A number
#'
#' @noRd
get_score <- function(d, rforest) {
  get_prop_same_votes <- function(preds) {
    # Get the proportion of decision trees in the trained random forest that
    # predict (vote) same writer.
    preds <- as.data.frame(preds)
    ntrees <- ncol(preds)
    prop <- rowSums(preds == 2) / ntrees
    return(prop)
  }

  # Prevent note 'no visible binding for global variable'
  docname1 <- docname2 <- NULL

  # Get only the distance columns
  d <- d %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("docname1", "writer1", "docname2", "writer2", "match")))

  # Get predictions: a matrix with a row for each doc and a column for each
  # decision tree. 1 = 'different', 2 = 'same'
  preds <- ranger::predictions(stats::predict(rforest$rf, d, predict.all = TRUE))
  score <- get_prop_same_votes(preds = preds)

  return(score)
}

#' Get Validation Scores
#'
#' Create reference scores of same writer and different writer scores from a
#' validation set.
#'
#' @param rforest A \pkg{ranger} random forest created with
#'   \code{\link{train_rf}}.
#' @param df A data frame of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}} with an added writer ID column.
#'
#' @return A list scores
#'
#' @noRd
get_validation_scores <- function(rforest, df) {
  # Prevent note 'no visible binding for global variable'
  score <- session <- prompt <- rep <- total_graphs <- NULL

  dist_measures <- which_dists(rforest = rforest)
  d <- get_distances(df = df, distance_measures = dist_measures)

  scores_df <- get_score(d = d, rforest = rforest)
  scores_df <- data.frame("score" = scores_df)
  scores_df$match <- label_same_different_writer(dists = d)$match

  # split into same and different writers to make it easier on the next step
  scores <- list()
  scores$same_writer <- scores_df %>%
    dplyr::filter(match == "same") %>%
    dplyr::pull(score)
  scores$diff_writer <- scores_df %>%
    dplyr::filter(match == "different") %>%
    dplyr::pull(score)

  return(scores)
}
