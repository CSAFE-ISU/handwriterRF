
# External Functions ------------------------------------------------------

#' Get Distances
#'
#' Calculate distances using 'stats::dist' between all pairs of cluster fill
#' rates using one or more distance measures
#'
#' @param df A data frame of cluster fill rates created with
#'   `get_cluster_fill_rates`
#' @param distance_measures A vector of distance measures. Use "absolute" to calculate
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
#' distances <- get_distances(df = cfr, distance_measures = c("man", "euc")
#'
get_distances <- function(df, distance_measures) {
  dists <- list()
  if ("abs" %in% distance_measures){
    abs <- get_abs_dists(df)
    dists[["abs"]] <- abs

    # delete absolute from distance measures list
    distance_measures <- distance_measures[which(distance_measures != "abs")]
  }

  for (method in distance_measures){
    new_dists <- get_single_method_distances(df, distance_measure = method, dist_col_label = method)
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
#' @param distance_measure A distance measure used by 'stats::dist'
#' @param dist_col_label A name for the output distance column
#'
#' @return A data frame of distances
#'
#' @export
#'
#' @examples
#' distances <- get_single_method_distances(df = cfr, distance_measure = "euclidean", dist_col_label = "dist")
#'
get_single_method_distances <- function(df, distance_measure = "euc", dist_col_label = "dist"){
  # prevent note "no visible binding for global variable"
  docname <- docname1 <- docname2 <- writer1 <- writer2 <- NULL

  # drop all columns except clusters
  clusters <- df %>% dplyr::ungroup() %>% dplyr::select(-docname)

  # calculate distances between all pairs of docs
  if (distance_measure == "man"){
    dists <- manhattan_dist(clusters)
  } else if (distance_measure == "euc"){
    dists <- euclidean_dist(clusters)
  } else if (distance_measure == "max"){
    dists <- maximum_dist(clusters)
  } else if (distance_measure == "cos"){
    dists <- cosine_dist(clusters)
  } else {
    stop("That distance measure has not been defined. Use 'man', 'euc', 'max', or 'cos'.")
  }

  # set lower triangle as NA because they are duplicates of upper triangle
  dists[lower.tri(dists)] <- NA
  # set diagonal entries to NA because each is the distance between a document
  # and itself. We don't want to use these distances in our distributions.
  diag(dists) <- NA

  # format data frame
  dists <- as.data.frame(dists)
  colnames(dists) <- df$docname
  dists$docname <- df$docname
  dists <- dists %>% dplyr::select(docname, tidyselect::everything())

  # reshape matrix to three columns (docname1, docname2, distance name) and drop
  # NAs
  colnames(dists)[colnames(dists) == "docname"] <- "docname1"
  dists <- reshape2::melt(dists, id.vars = "docname1", variable.name = "docname2", value.name = dist_col_label, na.rm = TRUE)

  # check number of rows
  if (nrow(dists) != choose(nrow(df), 2)){
    stop(paste("There should be", choose(nrow(df), 2), "distances, but there are", nrow(dists)))
  }

  return(dists)
}

get_abs_dists <- function(df) {
  abs_dist_for_single_cluster <- function(df, k){
    df <- df %>% dplyr::select(docname, k)
    dists <- get_single_method_distances(df = df, distance_measure = "man", dist_col_label = k)
    return(dists)
  }

  non_empty_clusters <- colnames(df)[!(colnames(df) %in% c("docname", "total_graphs"))]

  abs_dists <- lapply(non_empty_clusters, function(k) {abs_dist_for_single_cluster(df, k)})
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

manhattan_dist <- function(df){
  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {rowSums(abs(df[i,] - df[j,]))})
  return(d)
}

euclidean_dist <- function(df){
  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {sqrt(rowSums((df[i,] - df[j,])^2))})
  return(d)
}

maximum_dist <- function(df){
  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {apply(abs(df[i,] - df[j,]), 1, max)})
  return(d)
}

cosine_dist <- function(df){
  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {(rowSums((df[i,] - df[j,])^2)) / (sqrt(rowSums((df[i,])^2)) * sqrt(rowSums((df[j,])^2)))})
  return(d)
}
