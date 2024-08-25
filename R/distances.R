
# External Functions ------------------------------------------------------

#' Get Distances
#'
#' Calculate distances using 'stats::dist' between all pairs of cluster fill
#' rates using one or more distance measures
#'
#' @param df A data frame of cluster fill rates created with
#'   `get_cluster_fill_rates`
#' @param distances A vector of distance measures. Use "absolute" to calculate
#'   the absolute difference between cluster fill rates of two documents. The
#'   output distance will be a vector the length of the number of clusters. Use
#'   distance measures accepted by 'stats::dist': "euclidean", "maximum",
#'   "manhattan", "canberra", "binary", or "minkowski".
#'
#' @return A data frame of distances
#'
#' @export
#'
#' @examples
#' distances <- get_distances(df = cfr, distances = c("manhattan", "euclidean")
#'
get_distances <- function(df, distances) {
  dists <- list()
  if ("absolute" %in% distances){
    abs <- get_abs_dists(df)
    dists[["abs"]] <- abs

    # delete absolute from distances list
    distances <- distances[which(distances != "absolute")]
  }

  for (method in distances){
    new_dists <- get_single_method_distances(df, distance = method)
    colnames(new_dists)[colnames(new_dists) == "dist"] <- method
    dists[[method]] <- new_dists
  }

  # combine data frames
  dists <- purrr::reduce(dists, dplyr::left_join, by = c("docname1"="docname1", "docname2"="docname2"))

  dists <- label_same_different_writer(dists)

  return(dists)
}

#' Get Distances with a Single Distance Measure
#'
#' Calculate distances using 'stats::dist' between all pairs of cluster fill
#' rates for a single distance measure.
#'
#' @param df A data frame of cluster fill rates created with
#'   `get_cluster_fill_rates`
#' @param distance A distance measure used by 'stats::dist'
#' @param dist_col_label A name for the output distance column
#'
#' @return A data frame of distances
#'
#' @export
#'
#' @examples
#' distances <- get_single_method_distances(df = cfr, distance = "euclidean", dist_col_label = "dist")
#'
get_single_method_distances <- function(df, distance = "euclidean", dist_col_label = "dist"){
  # prevent note "no visible binding for global variable"
  docname <- docname1 <- docname2 <- writer1 <- writer2 <- NULL

  # drop all columns except clusters
  clusters <- df %>% dplyr::select(-docname)

  # calculate distances between all pairs of docs
  dists <- as.data.frame(as.matrix(stats::dist(x = clusters, method = distance)))

  # set column names
  colnames(dists) <- df$docname

  # make docname the first column
  dists$docname <- df$docname
  dists <- dists %>% dplyr::select(docname, tidyselect::everything())

  colnames(dists)[colnames(dists) == "docname"] <- "docname1"
  dists <- reshape2::melt(dists, id.vars = "docname1", variable.name = "docname2", value.name = dist_col_label)

  # delete distances between same doc
  dists <- dists %>% dplyr::filter(docname1 != docname2)

  return(dists)
}

get_abs_dists <- function(df) {
  abs_dist_for_single_cluster <- function(df, k){
    df <- df %>% dplyr::select(docname, paste0("cluster", k))
    dists <- get_single_method_distances(df = df, distance = "manhattan", dist_col_label = paste0("cluster", k))
    return(dists)
  }

  abs_dists <- lapply(1:40, function(k) {abs_dist_for_single_cluster(df, k)})
  dists <- purrr::reduce(abs_dists, dplyr::left_join, by = c("docname1"="docname1", "docname2"="docname2"))
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

  dists <- expand_docnames(dists, "docname1", "1")
  dists <- expand_docnames(dists, "docname2", "2")

  dists <- dists %>% dplyr::mutate(match = ifelse(writer1 == writer2, "same", "different"))

  # make match a factor
  dists$match <- as.factor(dists$match)

  # drop columns in prep for rf
  dists <- dists %>% dplyr::select(-writer1, -session1, -prompt1, -rep1, -writer2, -session2, -prompt2, -rep2)

  return(dists)
}
