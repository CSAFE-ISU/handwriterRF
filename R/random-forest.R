
#' Get Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples.
#'
#' @param rf A random forest created with 'randomForest'
#' @param d A distance between two handwriting samples, calculated with
#'   'get_distances'
#'
#' @return A number
#'
#' @export
#'
#' @examples
#' # Select the cluster fill rates for two handwriting samples
#' # Calculate the Euclidean distance between the cluster fill rates
#' d1 <- get_distances(cfr[1:2, ], c("abs", "euc"))
#' score <- get_score(rf$rf, d1)
#'
#' d2 <- get_distances(cfr[1:3, ], c("abs", "euc"))
#' score <- get_score(rf$rf, d2)
get_score <- function(rf, d){
  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  score <- stats::predict(rf, subset(d, select = -c(docname1, docname2)), type="prob")[,"same"]
  return(score)
}
