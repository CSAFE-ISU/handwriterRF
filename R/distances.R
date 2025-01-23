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
#' @param df A dataframe of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}} and an added column that contains a writer ID.
#' @param distance_measures A vector of distance measures. Use 'abs' to
#'   calculate the absolute difference, 'man' for the Manhattan distance, 'euc'
#'   for the Euclidean distance, 'max' for the maximum absolute distance, and
#'   'cos' for the cosine distance. The vector can be a single distance, or any
#'   combination of these five distance measures.
#'
#' @return A dataframe of distances
#'
#' @export
#'
#' @examples
#'
#' rates <- test[1:3, ]
#' # calculate maximum and Euclidean distances between the first 3 documents in test.
#' distances <- get_distances(df = rates, distance_measures = c("max", "euc"))
#'
#' # calculate maximum and distances between all documents in test.
#' distances <- get_distances(df = test, distance_measures = c("man"))
#'
get_distances <- function(df, distance_measures) {
  dists <- list()

  # If no writer column, label writers as unknown1, unknown2, etc.
  if (!("writer" %in% colnames(df))) {
    df$writer <- paste0("unknown", 1:nrow(df))
  }

  for (method in distance_measures) {
    if (method == "abs") {
      dists[["abs"]] <- absolute_dist(df)
    } else if (method == "man") {
      dists[["man"]] <- manhattan_dist(df)
    } else if (method == "euc") {
      dists[["euc"]] <- euclidean_dist(df)
    } else if (method == "max") {
      dists[["max"]] <- maximum_dist(df)
    } else if (method == "cos") {
      dists[["cos"]] <- cosine_dist(df)
    }
    # remove method from list
    distance_measures <- distance_measures[which(distance_measures != method)]
  }

  # combine dataframes
  dists <- purrr::reduce(dists, dplyr::left_join, by = c(
    "docname1" = "docname1",
    "writer1" = "writer1",
    "docname2" = "docname2",
    "writer2" = "writer2"
  ))

  return(dists)
}


# Internal Functions ------------------------------------------------------

#' Calculate the Absolute Distances for a Single Cluster
#'
#' Calculate the absolute distances for a single cluster between all pairs of
#' documents in a dataframe.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param k The name of a cluster. E.g., 'cluster1'
#'
#' @return A matrix
#'
#' @noRd
absolute_dist_for_single_cluster <- function(df, k) {
  # outer throws error if df is a tibble and the cluster k is a zero vector,
  # so convert df to a dataframe.
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
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A dataframe
#'
#' @noRd
absolute_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  writers <- df$writer
  df <- get_cluster_cols(df)

  dists <- lapply(colnames(df), function(k) {
    absolute_dist_for_single_cluster(df, k)
  })
  dists <- lapply(1:length(dists), function(i) {
    dist_matrix2df(dists[[i]], docnames, writers, paste0("cluster", i))
  })

  # combine dataframes
  dists <- purrr::reduce(dists, dplyr::left_join, by = c(
    "docname1" = "docname1",
    "writer1" = "writer1",
    "docname2" = "docname2",
    "writer2" = "writer2"
  ))

  return(dists)
}

#' Calculate the Manhattan Distances
#'
#' Calculate the manhattan distances between all pairs of documents in a data
#' frame.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A dataframe
#'
#' @noRd
manhattan_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  writers <- df$writer
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      rowSums(abs(df[i, ] - df[j, ]))
    }
  )

  df <- dist_matrix2df(d, docnames, writers, "man")

  return(df)
}

#' Calculate the Euclidean Distances
#'
#' Calculate the Euclidean distances between all pairs of documents in a data
#' frame.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A dataframe
#'
#' @noRd
euclidean_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  writers <- df$writer
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      sqrt(rowSums((df[i, ] - df[j, ])^2))
    }
  )

  df <- dist_matrix2df(d, docnames, writers, "euc")

  return(df)
}

#' Calculate the Maximum Distances
#'
#' Calculate the maximum distances between all pairs of documents in a data
#' frame.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A dataframe
#'
#' @noRd
maximum_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  writers <- df$writer
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      apply(abs(df[i, ] - df[j, ]), 1, max)
    }
  )

  df <- dist_matrix2df(d, docnames, writers, "max")

  return(df)
}

#' Calculate the Cosine Distances
#'
#' Calculate the cosine distances between all pairs of documents in a data
#' frame.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A dataframe
#'
#' @noRd
cosine_dist <- function(df) {
  # split docnames and clusters
  docnames <- df$docname
  writers <- df$writer
  df <- get_cluster_cols(df)

  d <- outer(
    seq_len(nrow(df)), seq_len(nrow(df)),
    function(i, j) {
      (rowSums((df[i, ] - df[j, ])^2)) / (sqrt(rowSums((df[i, ])^2)) * sqrt(rowSums((df[j, ])^2)))
    }
  )

  df <- dist_matrix2df(d, docnames, writers, "cos")

  return(df)
}

#' Select the Cluster Columns
#'
#' For a dataframe created with \code{\link{get_cluster_fill_rates}}, create
#' a dataframe that consists solely of the cluster columns.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#'
#' @return A dataframe
#'
#' @noRd
get_cluster_cols <- function(df) {
  # drop all columns except clusters
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::starts_with("cluster"))
  return(df)
}

#' Convert a Matrix of Distances to a Data Frame
#'
#' Convert a matrix of distances to a dataframe with five columns: docname1, writer1,
#' docname2, writer2, and <dist_col_label>.
#'
#' @param m A matrix of distances
#' @param docnames A vector of document names corresponding to rows and columns
#'   of the distance matrix.
#' @param writers A vector of writer IDs.
#' @param dist_col_label A character string to name the distance column in the
#'   returned dataframe.
#'
#' @return A dataframe
#'
#' @noRd
dist_matrix2df <- function(m, docnames, writers, dist_col_label) {
  # set lower triangle as NA because they are duplicates of upper triangle. Set
  m[lower.tri(m)] <- NA
  # diagonal entries to NA because each is the distance between a document and
  # itself. We don't want to use these distances in our distributions.
  diag(m) <- NA

  # format dataframe
  df <- as.data.frame(m)
  colnames(df) <- docnames
  df$docname <- docnames
  df <- df %>% dplyr::select(tidyselect::all_of(c("docname")), tidyselect::everything())

  # reshape matrix to five columns (docname1, writer1, docname2, writer2 distance name) and drop
  # NAs
  colnames(df)[colnames(df) == "docname"] <- "docname1"
  df <- reshape2::melt(df, id.vars = "docname1", variable.name = "docname2", value.name = dist_col_label, na.rm = TRUE)

  # change docname2 column from factor to character
  df$docname2 <- as.character(df$docname2)

  # add writer columns
  lookup <- data.frame(docnames, writers)
  df <- df %>% dplyr::left_join(lookup, by = dplyr::join_by("docname1" == "docnames"))
  colnames(df)[colnames(df) == "writers"] <- "writer1"
  df <- df %>% dplyr::left_join(lookup, by = dplyr::join_by("docname2" == "docnames"))
  colnames(df)[colnames(df) == "writers"] <- "writer2"
  df <- df %>% dplyr::select(tidyselect::all_of(c("docname1", "writer1", "docname2", "writer2")), tidyselect::everything())

  # reset row names
  row.names(df) <- NULL

  return(df)
}
