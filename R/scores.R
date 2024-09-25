#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples.
#'
#' @param d A data frame of distance(s) between two handwriting samples,
#'   calculated with 'get_distances'. The distance(s) needs to be the
#'   distance(s) used to train the random forest.
#' @param random_forest A random forest created with `train_rf`.
#' @param package The name of the package used to create the random forest:
#'   "randomForest" or "ranger".
#'
#' @return A number
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Select the cluster fill rates for two handwriting samples
#' # Calculate the Euclidean distance between the cluster fill rates
#' d <- get_distances(cfr[1:2, ], c("abs", "euc"))
#' random_forest <- rf$rf
#' score <- get_score(random_forest, d)
#' }
#'
get_score <- function(d, random_forest, package = "ranger"){
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  d <- d %>% dplyr::select(-tidyselect::any_of(c("docname1", "docname2")))

  if (package == "randomForest"){
    score <- stats::predict(random_forest, d, type="prob")[,2]
  } else if (package == "ranger"){
    pred <- ranger::predictions(stats::predict(random_forest, d, predict.all = TRUE))
    pred <- as.data.frame(pred)
    ntrees <- ncol(pred)
    score <- rowSums(pred == 2) / ntrees
  } else {
    stop("That package has not been implemented yet.")
  }

  return(score)
}
