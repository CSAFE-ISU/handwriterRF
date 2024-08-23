#' Get Cluster Fill Rates
#'
#' Calculate cluster fill rates from a data frame of cluster fill counts created
#' with 'handwriter::get_cluster_fill_counts'.
#'
#' @param cfc A data frame of cluster fill rates created with
#'   'handwriter::get_cluster_fill_counts'
#'
#' @return A data frame of cluster fill rates
#' @noRd
get_cluster_fill_rates <- function(cfc){
  # drop label columns and calculate cluster fill rates: each row sums to 1.
  cfc_clusters_only <- as.matrix(cfc[-1])
  total_graphs <- rowSums(cfc_clusters_only)
  cfr <- diag(1/total_graphs) %*% cfc_clusters_only

  # add "cluster" to column names
  colnames(cfr) <- paste0("cluster", colnames(cfr))

  # check all rows sum to 1 (within machine precision)
  if (!all.equal(rep(1, nrow(cfr)), rowSums(cfr), tolerance = sqrt(.Machine$double.eps))){
    stop("One or more rows does not sum to 1 (within machine precision).")
  }

  # add label columns and total_graphs column
  cfr <- cbind(cfc[,1], data.frame(total_graphs = total_graphs), cfr)

  return(cfr)
}
