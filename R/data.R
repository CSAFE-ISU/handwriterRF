#' Cluster Fill Counts for 1200 CSAFE Handwriting Database Samples
#'
#' A dataset containing cluster fill counts for for 1,200 handwriting samples
#' from the CSAFE Handwriting Database. The documents were split into graphs
#' with 'handwriter::process_batch_dir'. The graphs were grouped into clusters
#' with 'handwriter::get_clusterassignment'. The cluster fill counts were
#' calculated with 'handwriter::get_cluster_fill_counts'.
#'
#' @format A data frame with 1200 rows and 41 variables:
#' \describe{
#'   \item{docname}{file name of the handwriting sample}
#'   \item{1}{number of graphs in cluster 1}
#'   \item{2}{number of graphs in cluster 2}
#'   \item{3}{number of graphs in cluster 3}
#'   \item{4}{number of graphs in cluster 4}
#'   \item{5}{number of graphs in cluster 5}
#'   \item{6}{number of graphs in cluster 6}
#'   \item{7}{number of graphs in cluster 7}
#'   \item{8}{number of graphs in cluster 8}
#'   \item{9}{number of graphs in cluster 9}
#'   \item{10}{number of graphs in cluster 10}
#'   \item{11}{number of graphs in cluster 11}
#'   \item{12}{number of graphs in cluster 12}
#'   \item{13}{number of graphs in cluster 13}
#'   \item{14}{number of graphs in cluster 14}
#'   \item{15}{number of graphs in cluster 15}
#'   \item{16}{number of graphs in cluster 16}
#'   \item{17}{number of graphs in cluster 17}
#'   \item{18}{number of graphs in cluster 18}
#'   \item{19}{number of graphs in cluster 19}
#'   \item{20}{number of graphs in cluster 20}
#'   \item{21}{number of graphs in cluster 21}
#'   \item{22}{number of graphs in cluster 22}
#'   \item{23}{number of graphs in cluster 23}
#'   \item{24}{number of graphs in cluster 24}
#'   \item{25}{number of graphs in cluster 25}
#'   \item{26}{number of graphs in cluster 26}
#'   \item{27}{number of graphs in cluster 27}
#'   \item{28}{number of graphs in cluster 28}
#'   \item{29}{number of graphs in cluster 29}
#'   \item{30}{number of graphs in cluster 30}
#'   \item{31}{number of graphs in cluster 31}
#'   \item{32}{number of graphs in cluster 32}
#'   \item{33}{number of graphs in cluster 33}
#'   \item{34}{number of graphs in cluster 34}
#'   \item{35}{number of graphs in cluster 35}
#'   \item{36}{number of graphs in cluster 36}
#'   \item{37}{number of graphs in cluster 37}
#'   \item{38}{number of graphs in cluster 38}
#'   \item{39}{number of graphs in cluster 39}
#'   \item{40}{number of graphs in cluster 40}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>
"cfc"

#'Cluster Fill Rates for 1200 CSAFE Handwriting Database Samples
#'
#' A dataset containing cluster fill rates for for 1,200 handwriting samples from
#' the CSAFE Handwriting Database. The dataset was created by running
#' 'get_cluster_fill_rates' on the cluster fill counts data frame 'cfc'. Cluster
#' fill rates are calculated by calculating the proportion of total graphs assigned
#' to each cluster.
#'
#' @format A data frame with 1200 rows and 42 variables:
#' \describe{
#'   \item{docname}{file name of the handwriting sample}
#'   \item{total_graphs}{the total number of graphs in the handwriting sample}
#'   \item{cluster1}{number of graphs in cluster 1}
#'   \item{cluster2}{number of graphs in cluster 2}
#'   \item{cluster3}{number of graphs in cluster 3}
#'   \item{cluster4}{number of graphs in cluster 4}
#'   \item{cluster5}{number of graphs in cluster 5}
#'   \item{cluster6}{number of graphs in cluster 6}
#'   \item{cluster7}{number of graphs in cluster 7}
#'   \item{cluster8}{number of graphs in cluster 8}
#'   \item{cluster9}{number of graphs in cluster 9}
#'   \item{cluster10}{number of graphs in cluster 10}
#'   \item{cluster11}{number of graphs in cluster 11}
#'   \item{cluster12}{number of graphs in cluster 12}
#'   \item{cluster13}{number of graphs in cluster 13}
#'   \item{cluster14}{number of graphs in cluster 14}
#'   \item{cluster15}{number of graphs in cluster 15}
#'   \item{cluster16}{number of graphs in cluster 16}
#'   \item{cluster17}{number of graphs in cluster 17}
#'   \item{cluster18}{number of graphs in cluster 18}
#'   \item{cluster19}{number of graphs in cluster 19}
#'   \item{cluster20}{number of graphs in cluster 20}
#'   \item{cluster21}{number of graphs in cluster 21}
#'   \item{cluster22}{number of graphs in cluster 22}
#'   \item{cluster23}{number of graphs in cluster 23}
#'   \item{cluster24}{number of graphs in cluster 24}
#'   \item{cluster25}{number of graphs in cluster 25}
#'   \item{cluster26}{number of graphs in cluster 26}
#'   \item{cluster27}{number of graphs in cluster 27}
#'   \item{cluster28}{number of graphs in cluster 28}
#'   \item{cluster29}{number of graphs in cluster 29}
#'   \item{cluster30}{number of graphs in cluster 30}
#'   \item{cluster31}{number of graphs in cluster 31}
#'   \item{cluster32}{number of graphs in cluster 32}
#'   \item{cluster33}{number of graphs in cluster 33}
#'   \item{cluster34}{number of graphs in cluster 34}
#'   \item{cluster35}{number of graphs in cluster 35}
#'   \item{cluster36}{number of graphs in cluster 36}
#'   \item{cluster37}{number of graphs in cluster 37}
#'   \item{cluster38}{number of graphs in cluster 38}
#'   \item{cluster39}{number of graphs in cluster 39}
#'   \item{cluster40}{number of graphs in cluster 40}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>
"cfr"

