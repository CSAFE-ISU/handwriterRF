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
#'   \item{docname}{The file name of the handwriting sample. The file
#'   name includes the writer ID, the writing session, prompt, and
#'   repetition number of the handwriting sample. There are 1,200
#'   handwriting samples.}
#'   \item{writer}{Writer ID. There are 100 distinct writer ID's. Each
#'   writer has 12 documents.}
#'   \item{doc}{A document code that records the writing session, prompt,
#'   and repetition number of the handwriting sample. There are 12 distinct
#'   document codes. Each writer has a writing sample for each of the 12 document
#'   codes.}
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

#' Cluster Fill Rates for 1200 CSAFE Handwriting Database Samples
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

#' Cluster Template with 40 Clusters
#'
#' A cluster template created by 'handwriter' with K=40
#' clusters. This template was created from 100 handwriting samples from the
#' CSAFE Handwriting Database. This template is suitable for casework.
#'
#' 'handwriter' splits handwriting samples into component shapes
#' called *graphs*. The graphs are sorted into 40 clusters with a K-Means
#' algorithm. See 'handwriter' for more details.
#'
#' @format A list containing the contents of the cluster template.
#' \describe{
#' \item{centers_seed}{An integer for the random number generator use to select the
#' starting cluster centers for the K-Means algorithm.}
#' \item{cluster}{A vector of cluster assignments
#'   for each graph used to create the cluster template. The clusters are numbered sequentially 1, 2,...,K.}
#' \item{centers}{The final cluster centers produced by the K-Means algorithm.}
#' \item{K}{The number of clusters in the template.}
#' \item{n}{The number of training graphs to used to create the template.}
#' \item{docnames}{A vector that lists the training document from which each graph originated.}
#' \item{writers}{A vector that lists the writer of each graph.}
#' \item{iters}{The maximum number of iterations for the K-means
#'   algorithm.}
#' \item{changes}{A vector of the number of graphs that
#'   changed clusters on each iteration of the K-means algorithm.}
#' \item{outlierCutoff}{A vector of the outlier cutoff values calculated on
#'   each iteration of the K-means algorithm.}
#' \item{stop_reason}{The reason the
#'   K-means algorithm terminated.}
#' \item{wcd}{The within cluster
#'   distances on the final iteration of the K-means algorithm. More specifically,
#'   the distance between each graph and the center of the cluster to which it
#'   was assigned  on each iteration. The output of 'handwriter::make_clustering_template' stores
#'   the within cluster distances on each iteration, but the previous iterations were removed here to reduce the file size.}
#' \item{wcss}{A vector of the
#'   within-cluster sum of squares on each iteration of the K-means algorithm.}}
#' @examples
#' # view number of clusters
#' templateK40$K
#'
#' # view number of iterations
#' templateK40$iters
#'
#' # view cluster centers
#' templateK40$centers
#'
#' @keywords cluster
#' @md
"templateK40"


#' Distances and a 'ranger' Random Forest
#'
#' A list that contains a data frame of Euclidean distances between the cluster
#' fill rates of pairs of documents and a random forest trained on those
#' distances. The random forest was created with 'ranger'.
#'
#' @format A list with the following components:
#' \describe{
#' \item{rf}{A random forest created with 'randomForest'}
#' \item{dists}{A data frame of Euclidean distances between pairs of cluster fill
#' rates of documents used to train the random forest.}}
#'
#' @examples
#' # view the random forest
#' rf$rf
#'
#' # view the distances data frame
#' rf$dists
#'
#' @md
"rf"

#' Same Writer and Different Writer Densities
#'
#' A list of 'same writer' and 'different writer' densities created with
#' 'make_densities_from_rf' and the random forest 'rf'. A similarity score was
#' calculated for each distance value used to train 'rf'. The similarity score
#' is the proportion of decision trees that predicted 'same writer' for the
#' distance value. The 'same writer' density was created by applying the
#' 'density' function to the 'same writer' similarity scores. Similarly, the
#' 'different writer' density was created by applying the 'density' function to
#' the 'different writer' similarity scores.
#'
#' @format A list with the following components:
#' \describe{
#' \item{same_writer}{A density of same writer scores created with the 'density' function.}
#' \item{diff_writer}{A density of different writer scores created with the 'density' function.}}
#'
#' @examples
#' \dontrun{
#' plot(densities$same_writer)
#' plot(densities$diff_writer)
#' }
#'
#' @md
"densities"
