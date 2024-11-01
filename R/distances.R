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


# External Functions ------------------------------------------------------

#' Get Distances
#'
#' Calculate distances using between all pairs of cluster fill rates in a data
#' frame using one or more distance measures. The available distance measures
#' absolute distance, Manhattan distance, Euclidean distance, maximum distance,
#' and cosine distance.
#'
#' The absolute distance between two n-length vectors of cluster fill rates, a
#' and b, is a vector of the same length as a and b. It can be calculated as
#' abs(a-b) where subtraction is performed element-wise, then the absolute
#' value of each element is returned. More specifically, element i of the vector is \eqn{|a_i
#' - b_i|} for \eqn{i=1,2,...,n}.
#'
#' The Manhattan distance between two  n-length vectors of cluster fill rates, a and b, is
#' \eqn{\sum_{i=1}^n |a_i - b_i|}. In other words, it is the sum of the absolute
#' distance vector.
#'
#' The Euclidean distance between two  n-length vectors of cluster fill rates, a and b, is
#' \eqn{\sqrt{\sum_{i=1}^n (a_i - b_i)^2}}. In other words, it is the sum of the elements of the
#' absolute distance vector.
#'
#' The maximum distance between two n-length vectors of cluster fill rates, a and b, is
#' \eqn{\max_{1 \leq i \leq n}{\{|a_i - b_i|\}}}. In other words, it is the sum of the elements of the
#' absolute distance vector.
#'
#' The cosine distance between two n-length vectors of cluster fill rates, a and b, is
#' \eqn{\sum_{i=1}^n (a_i - b_i)^2 / (\sqrt{\sum_{i=1}^n a_i^2}\sqrt{\sum_{i=1}^n b_i^2})}.
#'
#' @param df A data frame of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}}
#' @param distance_measures A vector of distance measures. Use 'abs' to
#'   calculate the absolute difference, 'man' for the Manhattan distance, 'euc'
#'   for the Euclidean distance, 'max' for the maximum absolute distance, and
#'   'cos' for the cosine distance. The vector can be a single distance, or any
#'   combination of these five distance measures.
#'
#' @return A data frame of distances
#'
#' @export
#'
#' @examples
#' # calculate maximum and Euclidean distances between the first 3 documents in cfr.
#' distances <- get_distances(df = cfr[1:3, ], distance_measures = c('max', 'euc'))
#'
#' distances <- get_distances(df = cfr, distance_measures = c('man'))
#'
get_distances <- function(df, distance_measures) {
  dists <- list()

  for (method in distance_measures) {
    if (method == 'abs') {
      dists[['abs']] <- absolute_dist(df)
    } else if (method == 'man') {
      dists[['man']] <- manhattan_dist(df)
    } else if (method == 'euc') {
      dists[['euc']] <- euclidean_dist(df)
    } else if (method == 'max') {
      dists[['max']] <- maximum_dist(df)
    } else if (method == 'cos') {
      dists[['cos']] <- cosine_dist(df)
    }
    # remove method from list
    distance_measures <- distance_measures[which(distance_measures != method)]
  }

  # combine data frames
  dists <- purrr::reduce(dists, dplyr::left_join, by = c('docname1' = 'docname1', 'docname2' = 'docname2'))

  return(dists)
}


# Internal Functions ------------------------------------------------------

#' Calculate the Absolute Distances for a Single Cluster
#'
#' Calculate the absolute distances for a single cluster between all pairs of
#' documents in a data frame.
#'
#' @param df A data frame of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param k The name of a cluster. E.g., 'cluster1'
#'
#' @return A matrix
#'
#' @noRd
absolute_dist_for_single_cluster <- function(df, k) {
  # outer throws error if df is a tibble and the cluster k is a zero vector,
  # so convert df to a data frame.
  df <- as.data.frame(df)

  # select cluster k
  df <- df %>% dplyr::select(tidyselect::all_of(k))
  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      abs(df[i, ] - df[j, ])
    }
  )
  return(d)
}

#' Calculate the Absolute Distances
#'
#' Calculate the absolute distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A data frame
#'
#' @noRd
absolute_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  dists <- lapply(colnames(df), function(k) {
    absolute_dist_for_single_cluster(df, k)
  })
  dists <- lapply(1:length(dists), function(i) {
    dist_matrix2df(dists[[i]], docnames, paste0('cluster', i))
  })

  # combine data frames
  dists <- purrr::reduce(dists, dplyr::left_join, by = c('docname1' = 'docname1', 'docname2' = 'docname2'))

  return(dists)
}

#' Calculate the Manhattan Distances
#'
#' Calculate the manhattan distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A data frame
#'
#' @noRd
manhattan_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      rowSums(abs(df[i, ] - df[j, ]))
    }
  )

  df <- dist_matrix2df(d, docnames, 'man')

  return(df)
}

#' Calculate the Euclidean Distances
#'
#' Calculate the Euclidean distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A data frame
#'
#' @noRd
euclidean_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      sqrt(rowSums((df[i, ] - df[j, ])^2))
    }
  )

  df <- dist_matrix2df(d, docnames, 'euc')

  return(df)
}

#' Calculate the Maximum Distances
#'
#' Calculate the maximum distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A data frame
#'
#' @noRd
maximum_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      apply(abs(df[i, ] - df[j, ]), 1, max)
    }
  )

  df <- dist_matrix2df(d, docnames, 'max')

  return(df)
}

#' Calculate the Cosine Distances
#'
#' Calculate the cosine distances between all pairs of documents in a data
#' frame.
#'
#' @param df A data frame of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A data frame
#'
#' @noRd
cosine_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      (rowSums((df[i, ] - df[j, ])^2)) / (sqrt(rowSums((df[i, ])^2)) * sqrt(rowSums((df[j, ])^2)))
    }
  )

  df <- dist_matrix2df(d, docnames, 'cos')

  return(df)
}

#' Select the Cluster Columns
#'
#' For a data frame created with \code{\link{get_cluster_fill_rates}}, create
#' a data frame that consists solely of the cluster columns.
#'
#' @param df A data frame of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A data frame
#'
#' @noRd
get_cluster_cols <- function(df) {
  # drop all columns except clusters
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::starts_with('cluster'))
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
dist_matrix2df <- function(m, docnames, dist_col_label) {
  # Prevent note 'no visible binding for global variable'
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
  df <- df %>% dplyr::select(tidyselect::all_of(c('docname')), tidyselect::everything())

  # reshape matrix to three columns (docname1, docname2, distance name) and drop
  # NAs
  colnames(df)[colnames(df) == 'docname'] <- 'docname1'
  df <- reshape2::melt(df, id.vars = 'docname1', variable.name = 'docname2', value.name = dist_col_label, na.rm = TRUE)

  # change docname2 column from factor to character
  df <- df %>% dplyr::mutate(docname2 = as.character(docname2))

  # reset row names
  row.names(df) <- NULL

  return(df)
}
