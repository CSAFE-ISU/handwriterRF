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
#' Calculate distances between (1) all pairs of cluster fill rates in a data
#' frame, or (2) all pairs of cluster fill rates between two data frames, using
#' one or more distance measures. The available distance measures absolute
#' distance, Manhattan distance, Euclidean distance, maximum distance, and
#' cosine distance.
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
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the distance will be calculated between every pair of rows in
#'   df1. If df2 is supplied, the distance will be calculated between every row
#'   of df1 and every row of df2. Distances will not be calculated between pairs
#'   of df1 rows or between pairs of df2 rows.
#'
#' @return A dataframe of distances
#'
#' @export
#'
#' @examples
#'
#' # calculate maximum and Euclidean distances between the first 3 documents in test.
#' rates <- test[1:3, ]
#' distances <- get_distances(df = rates, distance_measures = c("max", "euc"))
#'
#' # calculate maximum and Euclidean distances between the the first 3 documents in test and
#' # the next 3 documents in test
#' rates2 <- test[4:6, ]
#' distances <- get_distances(df = rates, distance_measures = c("max", "euc"), df2 = rates2)
#'
#' # calculate cosine distances between all documents in test.
#' distances <- get_distances(df = test, distance_measures = c("cos"))
#'
get_distances <- function(df, distance_measures, df2 = NULL) {
  dists <- list()

  # If no writer column, label writers as unknown1, unknown2, etc.
  if (!("writer" %in% colnames(df))) {
    df$writer <- paste0("unknown", 1:nrow(df))
  }

  # If df2 is NULL set it equal to df. If df2 is not NULL and does not have a
  # writer column, label writers as df2_unknown1, df2_unknown2, etc.
  if (is.null(df2)) {
    df2 <- df
  } else if (!("writer" %in% colnames(df2))) {
    df$writer <- paste0("df2_unknown", 1:nrow(df))
  }

  for (method in distance_measures) {
    if (method == "abs") {
      dists[["abs"]] <- absolute_dist(df = df, df2 = df2)
    } else if (method == "man") {
      dists[["man"]] <- manhattan_dist(df = df, df2 = df2)
    } else if (method == "euc") {
      dists[["euc"]] <- euclidean_dist(df = df, df2 = df2)
    } else if (method == "max") {
      dists[["max"]] <- maximum_dist(df = df, df2 = df2)
    } else if (method == "cos") {
      dists[["cos"]] <- cosine_dist(df = df, df2 = df2)
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
#' Calculate the absolute distances for a single cluster between (1) all pairs
#' of documents in a dataframe, or (2) all pairs where one document is from the
#' first dataframe and the other document is from a second dataframe.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param k The name of a cluster. E.g., 'cluster1'
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the distance will be calculated between every pair of rows in
#'   df1. If df2 is supplied, the distance will be calculated between every row
#'   of df1 and every row of df2. Distances will not be calculated between pairs
#'   of df1 rows or between pairs of df2 rows.
#'
#' @return A matrix
#'
#' @noRd
absolute_dist_for_single_cluster <- function(df, k, df2 = NULL) {
  # outer throws error if df is a tibble and the cluster k is a zero vector,
  # so convert to a dataframe.
  df1 <- as.data.frame(df)
  if (!is.null(df2)) {
    df2 <- as.data.frame(df2)
  } else {
    df2 <- df1
  }

  # select cluster k
  df1 <- df1 %>% dplyr::select(tidyselect::all_of(k))
  df2 <- df2 %>% dplyr::select(tidyselect::all_of(k))
  d <- outer(
    seq_len(nrow(df1)), seq_len(nrow(df2)),
    function(i, j) {
      abs(df1[i, ] - df2[j, ])
    }
  )
  return(d)
}

#' Calculate the Absolute Distances
#'
#' Calculate the absolute distances between (1) all pairs of documents in a
#' dataframe, or (2) all pairs where one document is from the first dataframe
#' and the other document is from a second dataframe.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the distance will be calculated between every pair of rows in
#'   df1. If df2 is supplied, the distance will be calculated between every row
#'   of df1 and every row of df2. Distances will not be calculated between pairs
#'   of df1 rows or between pairs of df2 rows.
#'
#' @return A dataframe
#'
#' @noRd
absolute_dist <- function(df, df2 = NULL) {
  data <- split_clusters_and_labels(df = df, df2 = df2)

  if (!identical(colnames(data$df1), colnames(data$df2))) {
    stop("Both dataframes must have the same column names.")
  }

  dists <- lapply(colnames(data$df1), function(k) {
    absolute_dist_for_single_cluster(df = data$df1, k = k, df2 = data$df2)
  })
  dists <- lapply(1:length(dists), function(i) {
    dist_matrix2df(m = dists[[i]], docnames = data$docnames1, writers = data$writers1,
                   dist_col_label = paste0("cluster", i), docnames2 = data$docnames2,
                   writers2 = data$writers2)
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

#' Calculate a Single Distance Measure
#'
#' Calculate a single distance measure between (1) all pairs of documents in a
#' dataframe, or (2) all pairs where one document is from the first dataframe
#' and the other document is from a second dataframe.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the distance will be calculated between every pair of rows in
#'   df1. If df2 is supplied, the distance will be calculated between every row
#'   of df1 and every row of df2. Distances will not be calculated between pairs
#'   of df1 rows or between pairs of df2 rows.
#'
#' @return A dataframe
#'
#' @noRd
get_single_dist <- function(df, df2 = NULL, distance_measure = "man") {
  data <- split_clusters_and_labels(df = df, df2 = df2)

  d <- switch(distance_measure,
              "man" = outer(
                seq_len(nrow(data$df1)), seq_len(nrow(data$df2)),
                function(i, j) {
                  rowSums(abs(data$df1[i, ] - data$df2[j, ]))
                }),
              "euc" = outer(
                seq_len(nrow(data$df1)), seq_len(nrow(data$df2)),
                function(i, j) {
                  sqrt(rowSums((data$df1[i, ] - data$df2[j, ])^2))
                }),
              "max" = outer(
                seq_len(nrow(data$df1)), seq_len(nrow(data$df2)),
                function(i, j) {
                  apply(abs(data$df1[i, ] - data$df2[j, ]), 1, max)
                }),
              "cos" = outer(
                seq_len(nrow(data$df1)), seq_len(nrow(data$df2)),
                function(i, j) {
                  (rowSums((data$df1[i, ] - data$df2[j, ])^2)) / (sqrt(rowSums((data$df2[i, ])^2)) * sqrt(rowSums((data$df2[j, ])^2)))
                })
  )

  df <- dist_matrix2df(m = d, docnames = data$docnames1, writers = data$writers1,
                       docnames2 = data$docnames2, writers2 = data$writers2,
                       dist_col_label = distance_measure)

  return(df)
}

#' Calculate the Manhattan Distances
#'
#' Calculate the Manhattan distances between (1) all pairs of documents in a
#' dataframe, or (2) all pairs where one document is from the first dataframe
#' and the other document is from a second dataframe.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the distance will be calculated between every pair of rows in
#'   df1. If df2 is supplied, the distance will be calculated between every row
#'   of df1 and every row of df2. Distances will not be calculated between pairs
#'   of df1 rows or between pairs of df2 rows.
#'
#' @return A dataframe
#'
#' @noRd
manhattan_dist <- function(df, df2 = NULL) {
  df <- get_single_dist(df = df, df2 = df2, distance_measure = "man")
  return(df)
}

#' Calculate the Euclidean Distances
#'
#' Calculate the Euclidean distances between (1) all pairs of documents in a
#' dataframe, or (2) all pairs where one document is from the first dataframe
#' and the other document is from a second dataframe.
#'
#'
#' @param df1 A dataframe of cluster fill rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the Euclidean distance will be calculated between every pair of
#'   rows in df1. If df2 is supplied, the Euclidean distance will be calculated
#'   between every row of df1 and every row of df2. Distances will not be
#'   calculated between pairs of df1 rows or between pairs of df2 rows.
#'
#'
#' @return A dataframe
#'
#' @noRd
euclidean_dist <- function(df, df2 = NULL) {
  df <- get_single_dist(df = df, df2 = df2, distance_measure = "euc")
  return(df)
}

#' Calculate the Maximum Distances
#'
#' Calculate the maximum distances between (1) all pairs of documents in a
#' dataframe, or (2) all pairs where one document is from the first dataframe
#' and the other document is from a second dataframe.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the distance will be calculated between every pair of rows in
#'   df1. If df2 is supplied, the distance will be calculated between every row
#'   of df1 and every row of df2. Distances will not be calculated between pairs
#'   of df1 rows or between pairs of df2 rows.
#'
#' @return A dataframe
#'
#' @noRd
maximum_dist <- function(df, df2 = NULL) {
  df <- get_single_dist(df = df, df2 = df2, distance_measure = "max")
  return(df)
}

#' Calculate the Cosine Distances
#'
#' Calculate the cosine distances between (1) all pairs of documents in a
#' dataframe, or (2) all pairs where one document is from the first dataframe
#' and the other document is from a second dataframe.
#'
#' @param df A dataframe of cluster will rates created with
#'   \code{\link{get_cluster_fill_rates}}.
#' @param df2 Optional. A second dataframe of cluster fill rates. If df2 is not
#'   supplied, the distance will be calculated between every pair of
#'   rows in df1. If df2 is supplied, the distance will be calculated
#'   between every row of df1 and every row of df2. Distances will not be
#'   calculated between pairs of df1 rows or between pairs of df2 rows.
#'
#' @return A dataframe
#'
#' @noRd
cosine_dist <- function(df, df2 = NULL) {
  df <- get_single_dist(df = df, df2 = df2, distance_measure = "cos")
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

#' Convert a Matrix of Distances to a Dataframe
#'
#' Convert a matrix of distances to a dataframe with five columns: docname1,
#' writer1, docname2, writer2, and <dist_col_label>. docnames1 and writers1
#' correpsond to the rows of the distance matrix and docnames2 and writers2
#' correspond to the columns.
#'
#' If the distance matrix records the distances between pairs of rows in single
#' dataframe, the diagonal entries of the distance matrix will be ignored to
#' avoid using distances between a row and itself. Also, for example, the
#' distance between the 3rd row and the 5th row of the dataframe is recorded in
#' the distance matrix twice: m[3, 5] and m[5, 3]. To fix this problem, the
#' lower triangle of the distance matrix is ignored.
#'
#' @param m A matrix of distances
#' @param docnames1 A vector of document names corresponding to rows of the
#'   distance matrix.
#' @param writers1 A vector of writer IDs.
#' @param dist_col_label A character string to name the distance column in the
#'   returned dataframe.
#' @param docnames2 Optinal. A vector of document names corresponding to columns of the
#'   distance matrix.
#' @param writers2 Optional. A vector of writer IDs.
#'
#' @return A dataframe
#'
#' @noRd
dist_matrix2df <- function(m, docnames, writers, dist_col_label, docnames2 = NULL, writers2 = NULL) {
  if (identical(docnames, docnames2) || is.null(docnames2)) {
    docnames2 <- docnames
    writers2 <- writers
    # set lower triangle as NA because they are duplicates of upper triangle. Set
    m[lower.tri(m)] <- NA
    # diagonal entries to NA because each is the distance between a document and
    # itself. We don't want to use these distances in our distributions.
    diag(m) <- NA
  }

  # format dataframe
  df <- as.data.frame(m)
  colnames(df) <- docnames2
  df$docname1 <- docnames
  df <- df %>% dplyr::select(tidyselect::all_of(c("docname1")), tidyselect::everything())

  # reshape matrix to five columns (docname1, writer1, docname2, writer2
  # distance name) and drop NAs
  df <- reshape2::melt(df, id.vars = "docname1", variable.name = "docname2", value.name = dist_col_label, na.rm = TRUE)

  # change docname2 column from factor to character
  df$docname2 <- as.character(df$docname2)

  # add writer columns
  lookup1 <- data.frame(docname1 = docnames, writer1 = writers)
  df <- df %>% dplyr::left_join(lookup1, by = dplyr::join_by("docname1" == "docname1"))

  lookup2 <- data.frame(docname2 = docnames2, writer2 = writers2)
  df <- df %>% dplyr::left_join(lookup2, by = dplyr::join_by("docname2" == "docname2"))

  df <- df %>% dplyr::select(tidyselect::all_of(c("docname1", "writer1", "docname2", "writer2")), tidyselect::everything())

  # reset row names
  row.names(df) <- NULL

  return(df)
}

split_clusters_and_labels <- function(df, df2) {
  # split docnames and clusters
  docnames1 <- df$docname
  writers1 <- df$writer
  df1 <- get_cluster_cols(df)

  # if df2 is null, set it equal to df1 to calculate the distances between all
  # pairs of rows of df1. Also set docnames2 and writers2 equal to docnames1 and
  # writers1, respectively.
  if (is.null(df2)) {
    df2 = df1
    docnames2 = docnames1
    writers2 = writers1
  } else {
    docnames2 = df2$docname
    writers2 = df2$writer
    df2 <- get_cluster_cols(df2)
  }

  return(list("docnames1" = docnames1, "writers1" = writers1, "df1" = df1,
              "docnames2" = docnames2, "writers2" = writers2, "df2" = df2))
}
