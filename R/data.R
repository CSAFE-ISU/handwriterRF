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

#' A Dataframe of Cluster Fill Counts
#'
#' The cfc dataframe contains cluster fill counts for two documents from the
#' CSAFE Handwriting Database: w0238_s01_pWOZ_r02.rds and
#' w0238_s01_pWOZ_r03.rds.
#'
#' The documents were split into graphs with
#' \code{\link[handwriter]{process_batch_dir}}. The graphs were grouped into
#' clusters with \code{\link[handwriter]{get_clusters_batch}} and the cluster
#' template \code{\link[handwriter]{templateK40}}. The number of graphs in each
#' cluster, the cluster fill counts, were counted with
#' \code{\link[handwriter]{get_cluster_fill_counts}}. The dataframe cfc has a
#' column for each cluster in \code{\link[handwriter]{templateK40}} that has at
#' least one graph from w0238_s01_pWOZ_r02.rds or w0238_s01_pWOZ_r03.rds
#' assigned to it. Empty clusters do not have columns in cfc, so cfc only has 12
#' cluster columns instead of 40.
#'
#' @format A dataframe with 2 rows and 15 variables:
#' \describe{
#'   \item{docname}{The file name of the handwriting sample.}
#'   \item{writer}{Writer ID.}
#'   \item{doc}{The name of the handwriting prompt.}
#'   \item{3}{The number of graphs in cluster 3.}
#'   \item{10}{The number of graphs in cluster 10.}
#'   \item{12}{The number of graphs in cluster 12.}
#'   \item{15}{The number of graphs in cluster 15.}
#'   \item{16}{The number of graphs in cluster 16.}
#'   \item{17}{The number of graphs in cluster 17.}
#'   \item{19}{The number of graphs in cluster 19.}
#'   \item{20}{The number of graphs in cluster 20.}
#'   \item{23}{The number of graphs in cluster 23.}
#'   \item{25}{The number of graphs in cluster 25.}
#'   \item{27}{The number of graphs in cluster 27.}
#'   \item{29}{The number of graphs in cluster 29.}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>
#'
#' @md
"cfc"

#' A \pkg{ranger} Random Forest and Data Frame of Distances
#'
#' A list that contains a trained random forest created with \pkg{ranger} and
#' the dataframe of distances used to train the random forest.
#'
#' @format A list with the following components:
#' \describe{
#' \item{rf}{A random forest created with \pkg{ranger} with settings:
#' importance = 'permutation', scale.permutation.importance = TRUE, and num.trees = 200.}
#' \item{distance_measures}{A vector of the distance measures used to train the random forest: c('abs', 'euc')}
#' }
#'
#' @examples
#' # view the random forest
#' random_forest$rf
#'
#' # view the distance measures used to train the random forest
#' random_forest$distance_measures
#'
#' @md
"random_forest"


#' Reference Similarity Scores
#'
#' A list containing two dataframes. The same_writer dataframe contains
#' similarity scores from same writer pairs. The diff_writer dataframe contains
#' similarity scores from different writer pairs. The similarity scores are
#' calculated from the validation dataframe with the following steps:
#' \enumerate{
#'     \item The absolute and Euclidean distances are calculated between pairs of writer profiles.
#'     \item `random_forest` uses the distances between the pair to predict the class of the pair
#'     as same writer or different writer.
#'     \item The proportion of decision trees that predict same writer is used as the similarity
#'     score.
#' }
#'
#' @format A list with the following components:
#' \describe{
#' \item{same_writer}{A dataframe of 1,800 same writer similarity scores. The columns docname1
#' and writer1 record the file name and the writer ID of the first handwriting sample. The columns
#' docname2 and writer2 record the file name and writer ID of the second handwriting sample. The match
#' column records the class, which is same, of the pairs of handwriting samples. The similarity scores
#' between the pairs of handwriting samples are in the score column.}
#' \item{diff_writer}{A dataframe of 717,600 different writer similarity scores. The columns docname1
#' and writer1 record the file name and the writer ID of the first handwriting sample. The columns
#' docname2 and writer2 record the file name and writer ID of the second handwriting sample. The match
#' column records the class, which is different, of the pairs of handwriting samples. The similarity scores
#' between the pairs of handwriting samples are in the score column.}
#' }
#'
#' @examples
#' summary(ref_scores$same_writer)
#'
#' summary(ref_scores$diff_writer)
#'
#' plot_scores(ref_scores)
#'
#' @md
"ref_scores"


#' Cluster Template with 40 Clusters
#'
#' A cluster template created by \pkg{handwriter} with 40 clusters. This
#' template was created from 100 handwriting samples from the CSAFE Handwriting
#' Database, the CVL Handwriting Database, and the IAM Handwriting Database.
#'
#' \pkg{handwriter} splits handwriting samples into component shapes called
#' graphs. The graphs are sorted into 40 clusters with a K-Means algorithm.
#'
#' @format A list containing the contents of the cluster template.
#' \describe{
#' \item{cluster}{A vector of cluster assignments
#'   for each graph used to create the cluster template. The clusters are numbered sequentially 1, 2,...,40.}
#' \item{centers}{The final cluster centers produced by the K-Means algorithm.}
#' \item{K}{The number of clusters in the template (40).}
#' \item{n}{The number of training graphs to used to create the template (32,708).}
#' \item{wcd}{The within cluster
#'   distances, the distance between each graph and the nearest cluster center, on the final iteration of the K-means algorithm.}
#' }
#'
#' @examples
#' handwriter::plot_cluster_centers(templateK40)
#'
#' @keywords cluster
#' @md
"templateK40"


#' A Test Set of Cluster Fill Rates
#'
#' Writers from the CSAFE Handwriting Database and the CVL Handwriting Database
#' were randomly assigned to train, validation, and test sets.
#'
#' The test dataframe contains cluster fill rates for 332 handwritten documents
#' from the CSAFE Handwriting Database and the CVL Handwriting Database. The
#' documents are from 83 writers. The CSAFE Handwriting Database has nine
#' repetitions of each prompt. Two London Letter prompts and two Wizard of Oz
#' prompts were randomly selected from each writer. The CVL Handwriting Database
#' does not contain multiple repetitions of prompts and four Engligh language
#' prompts were randomly selected from each writer.
#'
#' The documents were split into graphs with
#' \code{\link[handwriter]{process_batch_dir}}. The graphs were grouped into
#' clusters with \code{\link[handwriter]{get_clusters_batch}}. The cluster fill
#' counts were calculated with
#' \code{\link[handwriter]{get_cluster_fill_counts}}. Finally,
#' \code{\link{get_cluster_fill_rates}} calculated the cluster fill rates.
#'
#' @format A dataframe with 332 rows and 43 variables:
#' \describe{
#'   \item{docname}{The file name of the handwriting sample.}
#'   \item{writer}{Writer ID. There are 83 distinct writer ID's. Each
#'   writer has four documents in the dataframe.}
#'   \item{doc}{The name of the handwriting prompt.}
#'   \item{total_graphs}{The total number of graphs in the document.}
#'   \item{cluster1}{The proportion of graphs in cluster 1}
#'   \item{cluster2}{The proportion of graphs in cluster 2}
#'   \item{cluster3}{The proportion of graphs in cluster 3}
#'   \item{cluster4}{The proportion of graphs in cluster 4}
#'   \item{cluster5}{The proportion of graphs in cluster 5}
#'   \item{cluster6}{The proportion of graphs in cluster 6}
#'   \item{cluster7}{The proportion of graphs in cluster 7}
#'   \item{cluster8}{The proportion of graphs in cluster 8}
#'   \item{cluster9}{The proportion of graphs in cluster 9}
#'   \item{cluster10}{The proportion of graphs in cluster 10}
#'   \item{cluster11}{The proportion of graphs in cluster 11}
#'   \item{cluster12}{The proportion of graphs in cluster 12}
#'   \item{cluster13}{The proportion of graphs in cluster 13}
#'   \item{cluster14}{The proportion of graphs in cluster 14}
#'   \item{cluster15}{The proportion of graphs in cluster 15}
#'   \item{cluster16}{The proportion of graphs in cluster 16}
#'   \item{cluster17}{The proportion of graphs in cluster 17}
#'   \item{cluster18}{The proportion of graphs in cluster 18}
#'   \item{cluster19}{The proportion of graphs in cluster 19}
#'   \item{cluster20}{The proportion of graphs in cluster 20}
#'   \item{cluster21}{The proportion of graphs in cluster 21}
#'   \item{cluster22}{The proportion of graphs in cluster 22}
#'   \item{cluster23}{The proportion of graphs in cluster 23}
#'   \item{cluster24}{The proportion of graphs in cluster 24}
#'   \item{cluster25}{The proportion of graphs in cluster 25}
#'   \item{cluster26}{The proportion of graphs in cluster 26}
#'   \item{cluster27}{The proportion of graphs in cluster 27}
#'   \item{cluster28}{The proportion of graphs in cluster 28}
#'   \item{cluster29}{The proportion of graphs in cluster 29}
#'   \item{cluster30}{The proportion of graphs in cluster 30}
#'   \item{cluster31}{The proportion of graphs in cluster 31}
#'   \item{cluster32}{The proportion of graphs in cluster 32}
#'   \item{cluster33}{The proportion of graphs in cluster 33}
#'   \item{cluster34}{The proportion of graphs in cluster 34}
#'   \item{cluster35}{The proportion of graphs in cluster 35}
#'   \item{cluster36}{The proportion of graphs in cluster 36}
#'   \item{cluster37}{The proportion of graphs in cluster 37}
#'   \item{cluster38}{The proportion of graphs in cluster 38}
#'   \item{cluster39}{The proportion of graphs in cluster 39}
#'   \item{cluster40}{The proportion of graphs in cluster 40}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>,
#'   <https://cvl.tuwien.ac.at/research/cvl-databases/an-off-line-database-for-writer-retrieval-writer-identification-and-word-spotting/>
#'
#' @md
"test"


#' A Training Set of Cluster Fill Rates
#'
#' Writers from the CSAFE Handwriting Database and the CVL Handwriting Database
#' were randomly assigned to train, validation, and test sets.
#'
#' The train dataframe contains cluster fill rates for 800 handwritten
#' documents from the CSAFE Handwriting Database and the CVL Handwriting
#' Database. The documents are from 200 writers. The CSAFE Handwriting Database
#' has nine repetitions of each prompt. Two London Letter prompts and two Wizard
#' of Oz prompts were randomly selected from each writer. The CVL Handwriting
#' Database does not contain multiple repetitions of prompts and four English
#' language prompts were randomly selected from each writer.
#'
#' The documents were split into graphs with
#' \code{\link[handwriter]{process_batch_dir}}. The graphs were grouped into
#' clusters with \code{\link[handwriter]{get_clusters_batch}}. The cluster fill
#' counts were calculated with
#' \code{\link[handwriter]{get_cluster_fill_counts}}. Finally,
#' \code{\link{get_cluster_fill_rates}} calculated the cluster fill rates.
#'
#' @format A dataframe with 800 rows and 43 variables:
#' \describe{
#'   \item{docname}{The file name of the handwriting sample.}
#'   \item{writer}{Writer ID. There are 200 distinct writer ID's. Each
#'   writer has 4 documents in the dataframe.}
#'   \item{doc}{The name of the handwriting prompt.}
#'   \item{total_graphs}{The total number of graphs in the document.}
#'   \item{cluster1}{The proportion of graphs in cluster 1}
#'   \item{cluster2}{The proportion of graphs in cluster 2}
#'   \item{cluster3}{The proportion of graphs in cluster 3}
#'   \item{cluster4}{The proportion of graphs in cluster 4}
#'   \item{cluster5}{The proportion of graphs in cluster 5}
#'   \item{cluster6}{The proportion of graphs in cluster 6}
#'   \item{cluster7}{The proportion of graphs in cluster 7}
#'   \item{cluster8}{The proportion of graphs in cluster 8}
#'   \item{cluster9}{The proportion of graphs in cluster 9}
#'   \item{cluster10}{The proportion of graphs in cluster 10}
#'   \item{cluster11}{The proportion of graphs in cluster 11}
#'   \item{cluster12}{The proportion of graphs in cluster 12}
#'   \item{cluster13}{The proportion of graphs in cluster 13}
#'   \item{cluster14}{The proportion of graphs in cluster 14}
#'   \item{cluster15}{The proportion of graphs in cluster 15}
#'   \item{cluster16}{The proportion of graphs in cluster 16}
#'   \item{cluster17}{The proportion of graphs in cluster 17}
#'   \item{cluster18}{The proportion of graphs in cluster 18}
#'   \item{cluster19}{The proportion of graphs in cluster 19}
#'   \item{cluster20}{The proportion of graphs in cluster 20}
#'   \item{cluster21}{The proportion of graphs in cluster 21}
#'   \item{cluster22}{The proportion of graphs in cluster 22}
#'   \item{cluster23}{The proportion of graphs in cluster 23}
#'   \item{cluster24}{The proportion of graphs in cluster 24}
#'   \item{cluster25}{The proportion of graphs in cluster 25}
#'   \item{cluster26}{The proportion of graphs in cluster 26}
#'   \item{cluster27}{The proportion of graphs in cluster 27}
#'   \item{cluster28}{The proportion of graphs in cluster 28}
#'   \item{cluster29}{The proportion of graphs in cluster 29}
#'   \item{cluster30}{The proportion of graphs in cluster 30}
#'   \item{cluster31}{The proportion of graphs in cluster 31}
#'   \item{cluster32}{The proportion of graphs in cluster 32}
#'   \item{cluster33}{The proportion of graphs in cluster 33}
#'   \item{cluster34}{The proportion of graphs in cluster 34}
#'   \item{cluster35}{The proportion of graphs in cluster 35}
#'   \item{cluster36}{The proportion of graphs in cluster 36}
#'   \item{cluster37}{The proportion of graphs in cluster 37}
#'   \item{cluster38}{The proportion of graphs in cluster 38}
#'   \item{cluster39}{The proportion of graphs in cluster 39}
#'   \item{cluster40}{The proportion of graphs in cluster 40}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>,
#'   <https://cvl.tuwien.ac.at/research/cvl-databases/an-off-line-database-for-writer-retrieval-writer-identification-and-word-spotting/>
#'
#' @md
"train"


#' A Validation Set of Cluster Fill Rates
#'
#' Writers from the CSAFE Handwriting Database and the CVL Handwriting Database
#' were randomly assigned to train, validation, and test sets.
#'
#' The validation dataframe contains cluster fill rates for 1,200 handwritten
#' documents from the CSAFE Handwriting Database and the CVL Handwriting
#' Database. The documents are from 300 writers. The CSAFE Handwriting Database
#' has nine repetitions of each prompt. Two London Letter prompts and two Wizard
#' of Oz prompts were randomly selected from each writer. The CVL Handwriting
#' Database does not contain multiple repetitions of prompts and four English
#' language prompts were randomly selected from each writer.
#'
#' The documents were split into graphs with
#' \code{\link[handwriter]{process_batch_dir}}. The graphs were grouped into
#' clusters with \code{\link[handwriter]{get_clusters_batch}}. The cluster fill
#' counts were calculated with
#' \code{\link[handwriter]{get_cluster_fill_counts}}. Finally,
#' \code{\link{get_cluster_fill_rates}} calculated the cluster fill rates.
#'
#' @format A dataframe with 1,200 rows and 43 variables:
#' \describe{
#'   \item{docname}{The file name of the handwriting sample.}
#'   \item{writer}{Writer ID. There are 300 distinct writer ID's. Each
#'   writer has 4 documents in the dataframe.}
#'   \item{doc}{The name of the handwriting prompt.}
#'   \item{total_graphs}{The total number of graphs in the document.}
#'   \item{cluster1}{The proportion of graphs in cluster 1}
#'   \item{cluster2}{The proportion of graphs in cluster 2}
#'   \item{cluster3}{The proportion of graphs in cluster 3}
#'   \item{cluster4}{The proportion of graphs in cluster 4}
#'   \item{cluster5}{The proportion of graphs in cluster 5}
#'   \item{cluster6}{The proportion of graphs in cluster 6}
#'   \item{cluster7}{The proportion of graphs in cluster 7}
#'   \item{cluster8}{The proportion of graphs in cluster 8}
#'   \item{cluster9}{The proportion of graphs in cluster 9}
#'   \item{cluster10}{The proportion of graphs in cluster 10}
#'   \item{cluster11}{The proportion of graphs in cluster 11}
#'   \item{cluster12}{The proportion of graphs in cluster 12}
#'   \item{cluster13}{The proportion of graphs in cluster 13}
#'   \item{cluster14}{The proportion of graphs in cluster 14}
#'   \item{cluster15}{The proportion of graphs in cluster 15}
#'   \item{cluster16}{The proportion of graphs in cluster 16}
#'   \item{cluster17}{The proportion of graphs in cluster 17}
#'   \item{cluster18}{The proportion of graphs in cluster 18}
#'   \item{cluster19}{The proportion of graphs in cluster 19}
#'   \item{cluster20}{The proportion of graphs in cluster 20}
#'   \item{cluster21}{The proportion of graphs in cluster 21}
#'   \item{cluster22}{The proportion of graphs in cluster 22}
#'   \item{cluster23}{The proportion of graphs in cluster 23}
#'   \item{cluster24}{The proportion of graphs in cluster 24}
#'   \item{cluster25}{The proportion of graphs in cluster 25}
#'   \item{cluster26}{The proportion of graphs in cluster 26}
#'   \item{cluster27}{The proportion of graphs in cluster 27}
#'   \item{cluster28}{The proportion of graphs in cluster 28}
#'   \item{cluster29}{The proportion of graphs in cluster 29}
#'   \item{cluster30}{The proportion of graphs in cluster 30}
#'   \item{cluster31}{The proportion of graphs in cluster 31}
#'   \item{cluster32}{The proportion of graphs in cluster 32}
#'   \item{cluster33}{The proportion of graphs in cluster 33}
#'   \item{cluster34}{The proportion of graphs in cluster 34}
#'   \item{cluster35}{The proportion of graphs in cluster 35}
#'   \item{cluster36}{The proportion of graphs in cluster 36}
#'   \item{cluster37}{The proportion of graphs in cluster 37}
#'   \item{cluster38}{The proportion of graphs in cluster 38}
#'   \item{cluster39}{The proportion of graphs in cluster 39}
#'   \item{cluster40}{The proportion of graphs in cluster 40}
#' }
#' @source <https://forensicstats.org/handwritingdatabase/>,
#'   <https://cvl.tuwien.ac.at/research/cvl-databases/an-off-line-database-for-writer-retrieval-writer-identification-and-word-spotting/>
#'
#' @md
"validation"
