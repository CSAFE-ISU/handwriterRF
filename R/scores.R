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

  d <- d %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c('docname1', 'docname2', 'match')))

  preds <- ranger::predictions(stats::predict(rforest$rf, d, predict.all = TRUE))
  score <- get_prop_same_votes(preds = preds)

  return(score)
}
