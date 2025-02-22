% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ref_scores}
\alias{ref_scores}
\title{Reference Similarity Scores}
\format{
A list with the following components:
\describe{
\item{same_writer}{A dataframe of 1,800 same writer similarity scores. The columns docname1
and writer1 record the file name and the writer ID of the first handwriting sample. The columns
docname2 and writer2 record the file name and writer ID of the second handwriting sample. The match
column records the class, which is same, of the pairs of handwriting samples. The similarity scores
between the pairs of handwriting samples are in the score column.}
\item{diff_writer}{A dataframe of 717,600 different writer similarity scores. The columns docname1
and writer1 record the file name and the writer ID of the first handwriting sample. The columns
docname2 and writer2 record the file name and writer ID of the second handwriting sample. The match
column records the class, which is different, of the pairs of handwriting samples. The similarity scores
between the pairs of handwriting samples are in the score column.}
}
}
\usage{
ref_scores
}
\description{
A list containing two dataframes. The same_writer dataframe contains
similarity scores from same writer pairs. The diff_writer dataframe contains
similarity scores from different writer pairs. The similarity scores are
calculated from the validation dataframe with the following steps:
\enumerate{
\item The absolute and Euclidean distances are calculated between pairs of writer profiles.
\item \code{random_forest} uses the distances between the pair to predict the class of the pair
as same writer or different writer.
\item The proportion of decision trees that predict same writer is used as the similarity
score.
}
}
\examples{
summary(ref_scores$same_writer)

summary(ref_scores$diff_writer)

plot_scores(ref_scores)

}
\keyword{datasets}
