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


#' Cluster Fill Counts for 1200 CSAFE Handwriting Database Samples
#'
#' A dataset containing cluster fill counts for for 1,200 handwriting samples
#' from the CSAFE Handwriting Database. The documents were split into graphs
#' with \code{\link[handwriter]{process_batch_dir}}. The graphs were grouped into clusters
#' with \code{\link[handwriter]{get_clusters_batch}}. The cluster fill counts were
#' calculated with \code{\link[handwriter]{get_cluster_fill_counts}}.
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
#'   \item{1}{The number of graphs in cluster 1}
#'   \item{2}{The number of graphs in cluster 2}
#'   \item{3}{The number of graphs in cluster 3}
#'   \item{4}{The number of graphs in cluster 4}
#'   \item{5}{The number of graphs in cluster 5}
#'   \item{6}{The number of graphs in cluster 6}
#'   \item{7}{The number of graphs in cluster 7}
#'   \item{8}{The number of graphs in cluster 8}
#'   \item{9}{The number of graphs in cluster 9}
#'   \item{10}{The number of graphs in cluster 10}
#'   \item{11}{The number of graphs in cluster 11}
#'   \item{12}{The number of graphs in cluster 12}
#'   \item{13}{The number of graphs in cluster 13}
#'   \item{14}{The number of graphs in cluster 14}
#'   \item{15}{The number of graphs in cluster 15}
#'   \item{16}{The number of graphs in cluster 16}
#'   \item{17}{The number of graphs in cluster 17}
#'   \item{18}{The number of graphs in cluster 18}
#'   \item{19}{The number of graphs in cluster 19}
#'   \item{20}{The number of graphs in cluster 20}
#'   \item{21}{The number of graphs in cluster 21}
#'   \item{22}{The number of graphs in cluster 22}
#'   \item{23}{The number of graphs in cluster 23}
#'   \item{24}{The number of graphs in cluster 24}
#'   \item{25}{The number of graphs in cluster 25}
#'   \item{26}{The number of graphs in cluster 26}
#'   \item{27}{The number of graphs in cluster 27}
#'   \item{28}{The number of graphs in cluster 28}
#'   \item{29}{The number of graphs in cluster 29}
#'   \item{30}{The number of graphs in cluster 30}
#'   \item{31}{The number of graphs in cluster 31}
#'   \item{32}{The number of graphs in cluster 32}
#'   \item{33}{The number of graphs in cluster 33}
#'   \item{34}{The number of graphs in cluster 34}
#'   \item{35}{The number of graphs in cluster 35}
#'   \item{36}{The number of graphs in cluster 36}
#'   \item{37}{The number of graphs in cluster 37}
#'   \item{38}{The number of graphs in cluster 38}
#'   \item{39}{The number of graphs in cluster 39}
#'   \item{40}{The number of graphs in cluster 40}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>
"cfc"

#' Cluster Fill Rates for 1200 CSAFE Handwriting Database Samples
#'
#' A dataset containing cluster fill rates for for 1,200 handwriting samples
#' from the CSAFE Handwriting Database. The dataset was created by running
#' \code{\link{get_cluster_fill_rates}} on the cluster
#' fill counts data frame cfc. Cluster fill rates are the proportion of total
#' graphs assigned to each cluster.
#'
#' @format A data frame with 1200 rows and 42 variables:
#' \describe{
#'   \item{docname}{file name of the handwriting sample}
#'   \item{total_graphs}{The total number of graphs in the handwriting sample}
#'   \item{cluster1}{The number of graphs in cluster 1}
#'   \item{cluster2}{The number of graphs in cluster 2}
#'   \item{cluster3}{The number of graphs in cluster 3}
#'   \item{cluster4}{The number of graphs in cluster 4}
#'   \item{cluster5}{The number of graphs in cluster 5}
#'   \item{cluster6}{The number of graphs in cluster 6}
#'   \item{cluster7}{The number of graphs in cluster 7}
#'   \item{cluster8}{The number of graphs in cluster 8}
#'   \item{cluster9}{The number of graphs in cluster 9}
#'   \item{cluster10}{The number of graphs in cluster 10}
#'   \item{cluster11}{The number of graphs in cluster 11}
#'   \item{cluster12}{The number of graphs in cluster 12}
#'   \item{cluster13}{The number of graphs in cluster 13}
#'   \item{cluster14}{The number of graphs in cluster 14}
#'   \item{cluster15}{The number of graphs in cluster 15}
#'   \item{cluster16}{The number of graphs in cluster 16}
#'   \item{cluster17}{The number of graphs in cluster 17}
#'   \item{cluster18}{The number of graphs in cluster 18}
#'   \item{cluster19}{The number of graphs in cluster 19}
#'   \item{cluster20}{The number of graphs in cluster 20}
#'   \item{cluster21}{The number of graphs in cluster 21}
#'   \item{cluster22}{The number of graphs in cluster 22}
#'   \item{cluster23}{The number of graphs in cluster 23}
#'   \item{cluster24}{The number of graphs in cluster 24}
#'   \item{cluster25}{The number of graphs in cluster 25}
#'   \item{cluster26}{The number of graphs in cluster 26}
#'   \item{cluster27}{The number of graphs in cluster 27}
#'   \item{cluster28}{The number of graphs in cluster 28}
#'   \item{cluster29}{The number of graphs in cluster 29}
#'   \item{cluster30}{The number of graphs in cluster 30}
#'   \item{cluster31}{The number of graphs in cluster 31}
#'   \item{cluster32}{The number of graphs in cluster 32}
#'   \item{cluster33}{The number of graphs in cluster 33}
#'   \item{cluster34}{The number of graphs in cluster 34}
#'   \item{cluster35}{The number of graphs in cluster 35}
#'   \item{cluster36}{The number of graphs in cluster 36}
#'   \item{cluster37}{The number of graphs in cluster 37}
#'   \item{cluster38}{The number of graphs in cluster 38}
#'   \item{cluster39}{The number of graphs in cluster 39}
#'   \item{cluster40}{The number of graphs in cluster 40}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>
"cfr"

#' Cluster Template with 40 Clusters
#'
#' A cluster template created by \pkg{handwriter} with 40 clusters. This template
#' was created from 120 handwriting samples from the CSAFE Handwriting Database.
#'
#' \pkg{handwriter} splits handwriting samples into component shapes called
#' graphs. The graphs are sorted into 40 clusters with a K-Means algorithm.
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
#'   was assigned  on each iteration. The output of \code{\link[handwriter]{make_clustering_template}}' stores
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


#' A \pkg{ranger} Random Forest, Distances, and Densities
#'
#' A list that contains a trained random forest created with \pkg{ranger}, the data
#' frame of distances used to train the random forest, and two densities
#' obtained from the random forest.
#'
#' @format A list with the following components:
#' \describe{
#' \item{dists}{The data frame used to train the random forest. The data frame has
#' 600 rows. Each row contains the absolute and Euclidean distances between the
#' cluster fill rates of two handwriting samples. If both handwriting samples are
#' from the same writer, the class is 'same'. If the handwriting samples are from
#' different writers, the class is 'different'. There are 300 'same' distances and
#' 300 'different' distances in the data frame.}
#' \item{rf}{A random forest created with \pkg{ranger} with settings:
#' importance = 'permutation', scale.permutation.importance = TRUE, and num.trees = 200.}
#' \item{densities}{A similarity score was obtained for each pair of handwriting samples in the
#' training data frame, dists, by calculating the proportion of decision trees that voted 'same'
#' class for the pair. The 'same_writer' density was created by applying \code{\link[stats]{density}}
#' to the similarity scores for the 300 same writer pairs in dists. Similarly, the 'diff_writer'
#' density was created by applying the \code{\link[stats]{density}} function to the similarity scores for the 300
#' different writer pairs in dists. The default settings were used with \code{\link[stats]{density}}.}
#' }
#'
#' @examples
#' # view the random forest
#' random_forest$rf
#'
#' # view the distances data frame
#' random_forest$dists
#'
#' # plot the same writer density
#' plot(random_forest$densities$same_writer)
#'
#' # plot the different writer density
#' plot(random_forest$densities$diff_writer)
#'
#' @md
"random_forest"
