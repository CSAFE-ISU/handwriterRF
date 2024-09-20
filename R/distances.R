
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
#' distances <- get_distances(df = cfr, distance_measures = c("abs", "euc"))
#'
get_distances <- function(df, distance_measures) {
  dists <- list()

  for (method in distance_measures){
    if (method == "abs"){
      dists[["abs"]] <- absolute_dist(df)
    } else if (method == "man"){
      dists[["man"]] <- manhattan_dist(df)
    } else if (method == "euc"){
      dists[["euc"]] <- euclidean_dist(df)
    } else if (method == "max"){
      dists[["max"]] <- maximum_dist(df)
    } else if (method == "cos"){
      dists[["cos"]] <- cosine_dist(df)
    }
    # remove method from list
    distance_measures <- distance_measures[which(distance_measures != method)]
  }

  # combine data frames
  dists <- purrr::reduce(dists, dplyr::left_join, by = c("docname1"="docname1", "docname2"="docname2"))

  return(dists)
}


# Internal Functions ------------------------------------------------------

#' Calculate the Absolute Distances for a Single Cluster
#'
#' Calculate the absolute distances for a single cluster between all pairs of
#' documents in a data frame.
#'
#' @param df A data frame of cluster will rates created with
#'   `get_cluster_fill_rates`.
#' @param k The name of a cluster. E.g., "cluster1"
#'
#' @return A matrix
#'
#' @noRd
absolute_dist_for_single_cluster <- function(df, k){
  # select cluster k
  df <- df %>% dplyr::select(dplyr::all_of(k))
  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {abs(df[i,] - df[j,])})
  return(d)
}

#' Calculate the Absolute Distances
#'
#' Calculate the absolute distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   `get_cluster_fill_rates`.
#'
#' @return A data frame
#'
#' @noRd
absolute_dist <- function(df){
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  dists <- lapply(colnames(df), function(k) {absolute_dist_for_single_cluster(df, k)})
  dists <- lapply(1:length(dists), function(i) {dist_matrix2df(dists[[i]], docnames, paste0("cluster", i))})

  # combine data frames
  dists <- purrr::reduce(dists, dplyr::left_join, by = c("docname1"="docname1", "docname2"="docname2"))

  return(dists)
}

#' Calculate the Manhattan Distances
#'
#' Calculate the manhattan distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   `get_cluster_fill_rates`.
#'
#' @return A data frame
#'
#' @noRd
manhattan_dist <- function(df){
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {rowSums(abs(df[i,] - df[j,]))})

  df <- dist_matrix2df(d, docnames, "man")

  return(df)
}

#' Calculate the Euclidean Distances
#'
#' Calculate the Euclidean distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   `get_cluster_fill_rates`.
#'
#' @return A data frame
#'
#' @noRd
euclidean_dist <- function(df){
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {sqrt(rowSums((df[i,] - df[j,])^2))})

  df <- dist_matrix2df(d, docnames, "euc")

  return(df)
}

#' Calculate the Maximum Distances
#'
#' Calculate the maximum distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   `get_cluster_fill_rates`.
#'
#' @return A data frame
#'
#' @noRd
maximum_dist <- function(df){
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {apply(abs(df[i,] - df[j,]), 1, max)})

  df <- dist_matrix2df(d, docnames, "max")

  return(df)
}

#' Calculate the Cosine Distances
#'
#' Calculate the cosine distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   `get_cluster_fill_rates`.
#'
#' @return A data frame
#'
#' @noRd
cosine_dist <- function(df){
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(seq_len(nrow(df)), seq_len(nrow(df)),
             function(i, j) {(rowSums((df[i,] - df[j,])^2)) / (sqrt(rowSums((df[i,])^2)) * sqrt(rowSums((df[j,])^2)))})

  df <- dist_matrix2df(d, docnames, "cos")

  return(df)
}

#' Select the Cluster Columns
#'
#' For a data frame created with `get_cluster_fill_rates`, create
#' a data frame that consists solely of the cluster columns.
#'
#' @param df A data frame of cluster will rates created with
#'   `get_cluster_fill_rates`.
#'
#' @return A data frame
#'
#' @noRd
get_cluster_cols <- function(df){
  # drop all columns except clusters
  df <- df %>% dplyr::ungroup() %>% dplyr::select(dplyr::starts_with("cluster"))
  return(df)
}

#' Convert a Matrix of Distances to a Data Frame
#'
#' Convert a matrix of distances to a data frame with three columns: docname1,
#' docname2, and <dist_col_label>.
#'
#' @param m A matrix of distances
#' @param docnames A vector of document names corresponding to rows and columns
#'   of the distance matrix.
#' @param dist_col_label A character string to name the distance column in the
#'   returned data frame.
#'
#' @return A data frame
#'
#' @noRd
dist_matrix2df <- function(m, docnames, dist_col_label){
  # Prevent note "no visible binding for global variable"
  docname <- docname2 <- NULL

  # set lower triangle as NA because they are duplicates of upper triangle. Set
  m[lower.tri(m)] <- NA
  # diagonal entries to NA because each is the distance between a document and
  # itself. We don't want to use these distances in our distributions.
  diag(m) <- NA

  # format data frame
  df <- as.data.frame(m)
  colnames(df) <- docnames
  df$docname <- docnames
  df <- df %>% dplyr::select(docname, tidyselect::everything())

  # reshape matrix to three columns (docname1, docname2, distance name) and drop
  # NAs
  colnames(df)[colnames(df) == "docname"] <- "docname1"
  df <- reshape2::melt(df, id.vars = "docname1", variable.name = "docname2", value.name = dist_col_label, na.rm = TRUE)

  # change docname2 column from factor to character
  df <- df %>% dplyr::mutate(docname2 = as.character(docname2))

  # reset row names
  row.names(df) <- NULL

  return(df)
}
