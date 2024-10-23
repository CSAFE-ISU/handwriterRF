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

#' Get Cluster Fill Rates
#'
#' Calculate cluster fill rates from a data frame of cluster fill counts created
#' with \code{\link[handwriter]{get_cluster_fill_counts}}.
#'
#' @param df A data frame of cluster fill rates created with
#'   \code{\link[handwriter]{get_cluster_fill_counts}}.
#'
#' @return A data frame of cluster fill rates.
#'
#' @export
#'
#' @examples
#' rates <- get_cluster_fill_rates(df = cfc)
#'
get_cluster_fill_rates <- function(df) {
  # Prevent note "no visible binding for global variable"
  docname <- writer <- doc <- NULL

  # drop label columns and calculate cluster fill rates: each row sums to 1.
  df_clusters_only <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(-docname, -writer, -doc)
  df_clusters_only <- as.matrix(df_clusters_only)
  total_graphs <- rowSums(df_clusters_only)
  cfr <- diag(1 / total_graphs) %*% df_clusters_only

  # add missing clusters
  missing_labels <- setdiff(1:40, colnames(cfr))
  if (length(missing_labels) > 0) {
    missing <- lapply(missing_labels, function(k) data.frame(k = rep(0, nrow(cfr))))
    missing <- do.call(cbind, missing)
    colnames(missing) <- missing_labels
    cfr <- cbind(cfr, missing)
    # sort columns numerically
    cfr <- cfr[as.character(sort(as.numeric(colnames(cfr))))]
  }

  # add "cluster" to column names
  colnames(cfr) <- paste0("cluster", colnames(cfr))

  # check all rows sum to 1 (within machine precision)
  if (!all.equal(rep(1, nrow(cfr)), rowSums(cfr), tolerance = sqrt(.Machine$double.eps))) {
    stop("One or more rows does not sum to 1 (within machine precision).")
  }

  # add label columns and total_graphs column
  cfr <- cbind(df[, 1], data.frame(total_graphs = total_graphs), cfr)

  return(cfr)
}
