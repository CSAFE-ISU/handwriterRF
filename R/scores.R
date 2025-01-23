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
#' dataframe of cluster fill rates.
#'
#' @param rforest A \pkg{ranger} random forest created with
#'   \code{\link{train_rf}}.
#' @param df A dataframe of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}} with an added writer ID column.
#' @param seed Optional. An integer to set the seed for the random number
#'   generator to make the results reproducible.
#' @param downsample_diff_pairs If TRUE, the different writer pairs are
#'   down-sampled to equal the number of same writer pairs. If FALSE, all
#'   different writer pairs are used.
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
get_ref_scores <- function(rforest, df, seed = NULL, downsample_diff_pairs = FALSE) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  d <- get_distances(df = df, distance_measures = rforest$distance_measures)

  scores_df <- get_score(d = d, rforest = rforest)

  if (downsample_diff_pairs) {
    scores_df <- downsample(scores_df)
  }

  # split into same and different writers to make it easier on the next step
  scores <- list()
  scores$same_writer <- scores_df %>%
    dplyr::filter(match == "same")
  scores$diff_writer <- scores_df %>%
    dplyr::filter(match == "different")

  return(scores)
}


# Internal Functions ------------------------------------------------------

#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples as described in Madeline Johnson and Danica
#' Ommen (2021) <doi:10.1002/sam.11566>.
#'
#' @param d A dataframe of distance(s) between two handwriting samples,
#'   calculated with \code{\link{get_distances}}. The distance(s) needs to be
#'   the distance(s) used to train the random forest.
#' @param rforest A \pkg{ranger} random forest created with
#'   \code{\link{train_rf}}.
#'
#' @return A dataframe
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

  make_scores_df <- function(score, d) {
    scores_df <- data.frame("score" = score)
    scores_df$docname1 <- d$docname1
    scores_df$docname2 <- d$docname2

    # Add writer1, writer2, and match columns for known writing samples only
    if (!all(startsWith(d$writer1, "unknown")) && !all(startsWith(d$writer2, "unknown"))) {
      scores_df$match <- label_same_different_writer(dists = d)$match
      scores_df$writer1 <- d$writer1
      scores_df$writer2 <- d$writer2
    }

    # Sort columns
    scores_df <- scores_df %>%
      dplyr::select(tidyselect::any_of(c("docname1", "writer1", "docname2", "writer2", "match", "score")))
  }

  # Get only the distance columns
  dists_only <- d %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("docname1", "writer1", "docname2", "writer2", "match")))

  # Get predictions: a matrix with a row for each doc and a column for each
  # decision tree. 1 = 'different', 2 = 'same'
  preds <- ranger::predictions(stats::predict(rforest$rf, dists_only, predict.all = TRUE))
  score <- get_prop_same_votes(preds = preds)

  scores_df <- make_scores_df(score = score, d = d)

  return(scores_df)
}
