
# External Functions ------------------------------------------------------

#' Get Distances
#'
#' Calculate distances using 'stats::dist' between all pairs of cluster fill
#' rates.
#'
#' @param cfr A data frame of cluster fill rates created with
#'   `get_cluster_fill_rates`
#' @param distance A distance method used by 'stats::dist'
#'
#' @return A data frame of distances
#'
#' @export
#'
#' @examples
#' distances <- get_distances(cfr, "euclidean")
#'
get_distances <- function(cfr, distance = "euclidean"){
  # prevent note "no visible binding for global variable"
  docname <- docname1 <- docname2 <- writer1 <- writer2 <- NULL

  # calculate distances between all pairs of docs
  dists <- as.data.frame(as.matrix(stats::dist(x = cfr[-seq(1,6)], method = distance)))

  # set column names
  colnames(dists) <- cfr$docname

  # make row with docnames the first column
  dists$docname <- cfr$docname
  dists <- dists %>% dplyr::select(docname, tidyselect::everything())

  colnames(dists)[colnames(dists) == "docname"] <- "docname1"
  dists <- reshape2::melt(dists, id.vars = "docname1", variable.name = "docname2", value.name = "dist")

  # delete distances between same doc
  dists <- dists %>% dplyr::filter(docname1 != docname2)

  # expand docnames
  dists <- expand_docnames(dists, "docname1", "1")
  dists <- expand_docnames(dists, "docname2", "2")

  dists <- label_same_different_writer(dists)

  return(dists)
}


# Internal Functions ------------------------------------------------------
#' Label Same and Different Writer Pairs
#'
#' Labels distances as belonging to same or different writers.
#'
#' @param dists A data frame of distances
#'
#' @return A data frame
#' @noRd
label_same_different_writer <- function(dists){
  # prevent note "no visible binding for global variable"
  writer1 <- writer2 <- NULL

  same <- dists %>% dplyr::filter(writer1 == writer2)
  same$match <- "same"

  diff <- dists %>% dplyr::filter(writer1 != writer2)
  diff$match <- "different"

  dists <- rbind(same, diff)

  return(dists)
}
