#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples.
#'
#' @param d A data frame of distance(s) between two handwriting samples,
#'   calculated with 'get_distances'. The distance(s) needs to be the
#'   distance(s) used to train the random forest.
#' @param random_forest A 'ranger' random forest created with `train_rf`.
#'
#' @return A number
#'
#' @export
#'
#' @examples
#' # Select the cluster fill rates for two handwriting samples
#' # Calculate the Euclidean distance between the cluster fill rates
#' d <- get_distances(cfr[1:2, ], c("abs", "euc"))
#' score <- get_score(d = d, random_forest = rf)
#'
get_score <- function(d, random_forest){
  get_prop_same_votes <- function(preds) {
    preds <- as.data.frame(preds)
    ntrees <- ncol(preds)
    prop <- rowSums(preds == 2) / ntrees
    return(prop)
  }

  # Prevent note "no visible binding for global variable"
  docname1 <- docname2 <- NULL

  d <- d %>% dplyr::ungroup() %>% dplyr::select(-tidyselect::any_of(c("docname1", "docname2", "match")))

  preds <- ranger::predictions(stats::predict(random_forest$rf, d, predict.all = TRUE))
  score <- get_prop_same_votes(preds = preds)

  return(score)
}
