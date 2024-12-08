% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{get_writer_profiles}
\alias{get_writer_profiles}
\title{Estimate Writer Profiles}
\usage{
get_writer_profiles(
  input_dir,
  num_cores = 1,
  template = templateK40,
  output_dir = NULL
)
}
\arguments{
\item{input_dir}{A filepath to a folder containing one or more handwritten
documents, scanned and saved as PNG file(s).}

\item{num_cores}{An integer number greater than or equal to 1 of cores to use
for parallel processing.}

\item{template}{Optional. A cluster template created with
[`handwriter::make_clustering_template`]. The default is the cluster
template `templateK40` included with 'handwriterRF'.}

\item{output_dir}{Optional. A filepath to a folder to save the RDS files
created by [`handwriter::process_batch_dir`] and
[`handwriter::get_clusters_batch`]. If no folder is supplied, the RDS files
will be saved to the temporary directory and then deleted before the
function terminates.}
}
\value{
A data frame
}
\description{
Estimate writer profiles from handwritten documents scanned and saved as PNG
files. Each file in `input_dir` is split into component shapes called graphs
with [`handwriter::process_batch_dir`]. Then the graphs are sorted into
clusters with similar shapes using the cluster `template` and
[`handwriter::get_clusters_batch`]. An estimate of the writer profile for a
document is the proportion of graphs from that document assigned to each of
the clusters in `template`. The writer profiles are estimated by running
[`handwriter::get_cluster_fill_counts`] and then [`get_cluster_fill_rates`].
}
\details{
The functions [`handwriter::process_batch_dir`] and
[`handwriter::get_clusters_batch`] take upwards of 30 seconds per document
and the results are saved to RDS files in `project_dir` > graphs and
`project_dir` > clusters, respectively.
}
\examples{
\donttest{
docs <- system.file(file.path("extdata", "docs"), package = "handwriterRF")
profiles <- get_writer_profiles(docs)

plot_writer_profiles(profiles)
}

}
