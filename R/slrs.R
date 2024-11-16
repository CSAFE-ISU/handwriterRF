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

#' Calculate a Score-Based Likelihood Ratio
#'
#' Compares two handwriting samples scanned and saved a PNG images with the
#' following steps:
#' \enumerate{
#'     \item \code{\link[handwriter]{processDocument}} splits the writing in both samples into component shapes, or graphs.
#'     \item \code{\link[handwriter]{get_clusters_batch}} groups the graphs into clusters of similar shapes.
#'     \item \code{\link[handwriter]{get_cluster_fill_counts}} counts the number of graphs assigned to each cluster.
#'     \item \code{\link{get_cluster_fill_rates}} calculates the proportion of graphs assigned to each cluster. The cluster fill rates serve as a writer profile.
#'     \item A similarity score is calculated between the cluster fill rates of the two documents using a random forest trained with \pkg{ranger}.
#'     \item The similarity score is compared to reference distributions of same writer and different
#'     writer similarity scores. The result is a score-based likelihood ratio that conveys the strength
#'     of the evidence in favor of same writer or different writer. For more details, see Madeline
#'     Johnson and Danica Ommen (2021) <doi:10.1002/sam.11566>.
#' }
#'
#' @param sample1_path A file path to a handwriting sample saved in PNG file
#'   format.
#' @param sample2_path A file path to a second handwriting sample saved in PNG
#'   file format.
#' @param unknown_writers TRUE if the true writer is known for both writing
#'   samples. Otherwise, FALSE. If TRUE, the writer IDs and a column that
#'   records the ground truth of whether the same person or different people
#'   wrote the samples will be added to the output data frame.
#' @param rforest Optional. A random forest trained with \pkg{ranger}. If no
#'   random forest is specified, `random_forest` will be used.
#' @param reference_scores Optional. A data frame of reference similarity
#'   scores. If reference scores is not specified, `ref_scores` will be used.
#' @param project_dir A path to a directory where helper files will be saved. If
#'   no project directory is specified, the helper files will be saved to
#'   tempdir() and deleted before the function terminates.
#'
#' @return A data frame
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Compare two samples from the same writer
#' s1 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r01.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' calculate_slr(s1, s2)
#'
#' # Compare samples from two writers
#' s1 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r01.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0238_s01_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' calculate_slr(s1, s2)
#' }
#'
#' @md
calculate_slr <- function(sample1_path, sample2_path, unknown_writers = TRUE,
                          rforest = NULL, reference_scores = NULL,
                          project_dir = NULL) {
  df <- compare_documents(
    sample1 = sample1_path,
    sample2 = sample2_path,
    score_only = FALSE,
    rforest = rforest,
    reference_scores = reference_scores,
    project_dir = project_dir
  )
  return(df)
}

#' Interpret an SLR Value
#'
#' Verbally interprent an SLR value.
#'
#' @param df A data frame created by \code{\link{calculate_slr}}.
#'
#' @return A string
#'
#' @export
#'
#' @examples
#' df <- data.frame("score" = 5, "slr" = 20)
#' interpret_slr(df)
#'
#' df <- data.frame("score" = 0.12, "slr" = 0.5)
#' interpret_slr(df)
#'
#' df <- data.frame("score" = 1, "slr" = 1)
#' interpret_slr(df)
#'
#' df <- data.frame("score" = 0, "slr" = 0)
#' interpret_slr(df)
#'
interpret_slr <- function(df) {
  if (df$slr > 1) {
    x <- paste("A score-based likelihood ratio of", format(round(df$slr, 1), big.mark = ","), "means the likelihood of observing a similarity score of", df$score, "if the documents were written by the same person is", format(round(df$slr, 1), big.mark = ","), "times greater than the likelihood of observing this score if the documents were written by different writers.")
  } else if (df$slr > 0 && df$slr < 1) {
    x <- paste("A score-based likelihood ratio of", format(round(df$slr, 2), big.mark = ","), "means the likelihood of observing a similarity score of", df$score, "if the documents were written by different people is", format(round((1 / df$slr), 2), nsmall = 2, big.mark = ","), "times greater than the likelihood of observing this score if the documents were written by the same writer.")
  } else if (df$slr == 1) {
    x <- paste("A score-based likelihood ratio of", format(round(df$slr, 1), big.mark = ","), "means the likelihood of observing a similarity score of", df$score, "if the documents were written by different people is equal to the likelihood of observing the score if the documents were written by the same writer.")
  } else if (df$slr == 0) {
    x <- paste("A score-based likelihood ratio of 0 means it is virtually impossible that the documents were written by the same person.")
  } else {
    stop("The slr value is invalid.")
  }
  return(x)
}
