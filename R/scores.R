# Internal Functions ------------------------------------------------------

#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples as described in Madeline Johnson and Danica
#' Ommen (2021) <doi:10.1002/sam.11566>.
#'
#' @param d A data frame of distance(s) between two handwriting samples,
#'   calculated with 'get_distances'. The distance(s) needs to be the
#'   distance(s) used to train the random forest.
#' @param rforest A 'ranger' random forest created with `train_rf`.
#'
#' @return A number
#'
#' @noRd
get_score <- function(d, rforest) {
  get_prop_same_votes <- function(preds) {
    # Get the proportion of decision trees in the trained random forest that
    # predict, or vote, 'same writer'.
    preds <- as.data.frame(preds)
    ntrees <- ncol(preds)
    prop <- rowSums(preds == 2) / ntrees
    return(prop)
  }

  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  d <- d %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("docname1", "docname2", "match")))

  preds <- ranger::predictions(stats::predict(rforest$rf, d, predict.all = TRUE))
  score <- get_prop_same_votes(preds = preds)

  return(score)
}
