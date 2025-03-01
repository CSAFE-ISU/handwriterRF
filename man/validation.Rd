% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{validation}
\alias{validation}
\title{A Validation Set of Cluster Fill Rates}
\format{
A dataframe with 1,200 rows and 43 variables:
\describe{
\item{docname}{The file name of the handwriting sample.}
\item{writer}{Writer ID. There are 300 distinct writer ID's. Each
writer has 4 documents in the dataframe.}
\item{doc}{The name of the handwriting prompt.}
\item{total_graphs}{The total number of graphs in the document.}
\item{cluster1}{The proportion of graphs in cluster 1}
\item{cluster2}{The proportion of graphs in cluster 2}
\item{cluster3}{The proportion of graphs in cluster 3}
\item{cluster4}{The proportion of graphs in cluster 4}
\item{cluster5}{The proportion of graphs in cluster 5}
\item{cluster6}{The proportion of graphs in cluster 6}
\item{cluster7}{The proportion of graphs in cluster 7}
\item{cluster8}{The proportion of graphs in cluster 8}
\item{cluster9}{The proportion of graphs in cluster 9}
\item{cluster10}{The proportion of graphs in cluster 10}
\item{cluster11}{The proportion of graphs in cluster 11}
\item{cluster12}{The proportion of graphs in cluster 12}
\item{cluster13}{The proportion of graphs in cluster 13}
\item{cluster14}{The proportion of graphs in cluster 14}
\item{cluster15}{The proportion of graphs in cluster 15}
\item{cluster16}{The proportion of graphs in cluster 16}
\item{cluster17}{The proportion of graphs in cluster 17}
\item{cluster18}{The proportion of graphs in cluster 18}
\item{cluster19}{The proportion of graphs in cluster 19}
\item{cluster20}{The proportion of graphs in cluster 20}
\item{cluster21}{The proportion of graphs in cluster 21}
\item{cluster22}{The proportion of graphs in cluster 22}
\item{cluster23}{The proportion of graphs in cluster 23}
\item{cluster24}{The proportion of graphs in cluster 24}
\item{cluster25}{The proportion of graphs in cluster 25}
\item{cluster26}{The proportion of graphs in cluster 26}
\item{cluster27}{The proportion of graphs in cluster 27}
\item{cluster28}{The proportion of graphs in cluster 28}
\item{cluster29}{The proportion of graphs in cluster 29}
\item{cluster30}{The proportion of graphs in cluster 30}
\item{cluster31}{The proportion of graphs in cluster 31}
\item{cluster32}{The proportion of graphs in cluster 32}
\item{cluster33}{The proportion of graphs in cluster 33}
\item{cluster34}{The proportion of graphs in cluster 34}
\item{cluster35}{The proportion of graphs in cluster 35}
\item{cluster36}{The proportion of graphs in cluster 36}
\item{cluster37}{The proportion of graphs in cluster 37}
\item{cluster38}{The proportion of graphs in cluster 38}
\item{cluster39}{The proportion of graphs in cluster 39}
\item{cluster40}{The proportion of graphs in cluster 40}
}
}
\source{
\url{https://forensicstats.org/handwritingdatabase/},
\url{https://cvl.tuwien.ac.at/research/cvl-databases/an-off-line-database-for-writer-retrieval-writer-identification-and-word-spotting/}
}
\usage{
validation
}
\description{
Writers from the CSAFE Handwriting Database and the CVL Handwriting Database
were randomly assigned to train, validation, and test sets.
}
\details{
The validation dataframe contains cluster fill rates for 1,200 handwritten
documents from the CSAFE Handwriting Database and the CVL Handwriting
Database. The documents are from 300 writers. The CSAFE Handwriting Database
has nine repetitions of each prompt. Two London Letter prompts and two Wizard
of Oz prompts were randomly selected from each writer. The CVL Handwriting
Database does not contain multiple repetitions of prompts and four English
language prompts were randomly selected from each writer.

The documents were split into graphs with
\code{\link[handwriter]{process_batch_dir}}. The graphs were grouped into
clusters with \code{\link[handwriter]{get_clusters_batch}}. The cluster fill
counts were calculated with
\code{\link[handwriter]{get_cluster_fill_counts}}. Finally,
\code{\link{get_cluster_fill_rates}} calculated the cluster fill rates.
}
\keyword{datasets}
