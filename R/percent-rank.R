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

#' Calculate Percent Ranks
#'
#' Compares two handwriting samples scanned and saved a PNG images with the
#' following steps:
#' \enumerate{
#'     \item \code{\link[handwriter]{processDocument}} splits the writing in both samples into component shapes, or graphs.
#'     \item \code{\link[handwriter]{get_clusters_batch}} groups the graphs into clusters of similar shapes.
#'     \item \code{\link[handwriter]{get_cluster_fill_counts}} counts the number of graphs assigned to each cluster.
#'     \item \code{\link{get_cluster_fill_rates}} calculates the proportion of graphs assigned to each cluster. The cluster fill rates serve as a writer profile.
#'     \item A similarity score is calculated between the cluster fill rates of the two documents using a random forest trained with \pkg{ranger}.
#'     \item The similarity score is compared to reference samples of same writer and different
#'     writer similarity scores. The percent rank of the observed similarity score is returned for each sample. The percent rank for score x is
#'     calculated as the number of scores in the sample less than or equal to x divided by the total number of scores.
#' }
#'
#' @param sample1_path A file path to a handwriting sample saved in PNG file
#'   format.
#' @param sample2_path A file path to a second handwriting sample saved in PNG
#'   file format.
#' @param rforest Optional. A random forest trained with \pkg{ranger}. If
#'   rforest is not given, the data object random_forest is used.
#' @param project_dir Optional. A path to a directory where helper files will be
#'   saved. If no project directory is specified, the helper files will be saved
#'   to tempdir() and deleted before the function terminates.
#'
#' @return A list of two numbers
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Compare two samples from the same writer
#' s1 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r01.png"),
#'                   package = "handwriterRF")
#' s2 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r02.png"),
#'                   package = "handwriterRF")
#' calculate_slr(s1, s2)
#'
#' # Compare samples from two writers
#' s1 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r01.png"),
#'                   package = "handwriterRF")
#' s2 <- system.file(file.path("extdata", "docs", "w0238_s01_pWOZ_r02.png"),
#'                   package = "handwriterRF")
#' calculate_slr(s1, s2)
#' }
#'
calculate_percent_rank <- function(sample1_path, sample2_path, rforest = random_forest, project_dir = NULL) {
  copy_samples_to_project_dir <- function(sample1_path, sample2_path, project_dir) {
    # Copy samples to project_dir > docs
    message("Copying samples to output directory > docs...\n")
    create_dir(file.path(project_dir, "docs"))

    # rename samples if file paths are different but file names are the same
    if (identical(basename(sample1_path), basename(sample2_path))){
      file.copy(sample1_path, file.path(project_dir, "docs", "sample1.png"))
      file.copy(sample2_path, file.path(project_dir, "docs", "sample2.png"))
    } else {
      file.copy(sample1_path, file.path(project_dir, "docs", basename(sample1_path)))
      file.copy(sample2_path, file.path(project_dir, "docs", basename(sample2_path)))
    }

    # get the sample paths in the project directory
    sample_paths <- list.files(file.path(project_dir, "docs"), full.names = TRUE)
    return(sample_paths)
  }

  skip_if_processed <- function(sample_path, project_dir) {
    # process file if it hasn't already been processed and saved in project_dir
    # > graph
    outfile <- gsub(".png", "_proclist.rds", basename(sample_path))
    outfile_path <- file.path(project_dir, "graphs", outfile)
    if (!file.exists(outfile_path)) {
      doc <- handwriter::processDocument(sample_path)
      saveRDS(doc, outfile_path)
    }
    return()
  }

  process_and_save_samples <- function(sample1_path, sample2_path, project_dir) {
    # Process samples and save in project_dir > graphs
    message("Processing samples...")

    create_dir(file.path(project_dir, "graphs"))

    skip_if_processed(sample_path = sample1_path, project_dir = project_dir)
    skip_if_processed(sample_path = sample2_path, project_dir = project_dir)

    return()
  }

  get_percent_rank <- function(score, ref_scores) {
    # Calculate the percent rank of score for a sample of reference scores
    pr <- 100 * sum(ref_scores <= score) / length(ref_scores)
    return(pr)
  }

  # error if sample1_path == sample2_path
  if (identical(sample1_path, sample2_path)) {
    stop("sample1_path and sample2_path cannot be identical.")
  }

  # set output directory as temp directory if NULL
  if (is.null(project_dir)) {
    project_dir <- file.path(tempdir(), "comparison")
  }

  # keep original sample paths so they can be recorded in the data frame at the
  # end
  sample1_path_org <- sample1_path
  sample2_path_org <- sample2_path

  # copy samples
  sample_paths <- copy_samples_to_project_dir(
    sample1_path = sample1_path,
    sample2_path = sample2_path,
    project_dir = project_dir
  )
  sample1_path <- sample_paths[1]
  sample2_path <- sample_paths[2]

  # process
  process_and_save_samples(
    sample1_path = sample1_path,
    sample2_path = sample2_path,
    project_dir = project_dir
  )

  # cluster
  clusters <- handwriter::get_clusters_batch(
    template = templateK40,
    input_dir = file.path(project_dir, "graphs"),
    output_dir = file.path(project_dir, "clusters"),
    writer_indices = c(2, 5),
    doc_indices = c(7, 18),
    save_master_file = TRUE
  )
  counts <- handwriter::get_cluster_fill_counts(clusters)
  rates <- get_cluster_fill_rates(counts)

  # distance
  message("Calculating distance between samples...\n")
  dist_measures <- which_dists(rforest = rforest)
  d <- get_distances(df = rates, distance_measures = dist_measures)

  # score
  message("Calculating similarity score between samples...\n")
  score <- get_score(rforest = rforest, d = d)

  # percent rank
  message("Calculating percent ranks for samples...\n")
  percent_rank_same_writer <- get_percent_rank(score = score, ref_scores = random_forest$scores$same_writer)
  percent_rank_diff_writer <- get_percent_rank(score = score, ref_scores = random_forest$scores$diff_writer)

  # make data frame of results
  df <- data.frame("sample1_path" = sample1_path_org, "sample2_path" = sample2_path_org,
                   "docname1" = basename(sample1_path_org), "docname2" = basename(sample2_path_org),
                   "score" = score, "percent_rank_same_writer" = percent_rank_same_writer,
                   "percent_rank_diff_writer" = percent_rank_diff_writer)

  # delete project folder from temp directory or save results to project folder
  if (project_dir == file.path(tempdir(), "comparison")) {
    unlink(project_dir, recursive = TRUE)
  } else {
    saveRDS(df, file.path(project_dir, "percent_rank.rds"))
  }

  return(df)
}
