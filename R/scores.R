#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples.
#'
#' @param d A data frame of distance(s) between two handwriting samples,
#'   calculated with 'get_distances'. The distance(s) needs to be the
#'   distance(s) used to train the random forest.
#' @param random_forest A random forest created with `train_rf`.
#'
#' @return A number
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Select the cluster fill rates for two handwriting samples
#' # Calculate the Euclidean distance between the cluster fill rates
#' d1 <- get_distances(cfr[1:2, ], c("abs", "euc"))
#' random_forest <- rf$rf
#' score <- get_score(random_forest, d1)
#' }
#'
get_score <- function(d, random_forest){
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  score <- stats::predict(random_forest, subset(d, select = -c(docname1, docname2)), type="prob")[,"same"]
  return(score)
}
