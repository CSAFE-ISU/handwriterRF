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
#' `r lifecycle::badge("superseded")` `calculate_slr` has been superseded in
#' favor of `compare_documents()` which offers more functionality.
#'
#' Compares two handwriting
#' samples scanned and saved a PNG images with the following steps:
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
#' @param rforest Optional. A random forest trained with \pkg{ranger}. If no
#'   random forest is specified, `random_forest` will be used.
#' @param reference_scores Optional. A dataframe of reference similarity
#'   scores. If reference scores is not specified, `ref_scores` will be used.
#' @param project_dir A path to a directory where helper files will be saved. If
#'   no project directory is specified, the helper files will be saved to
#'   tempdir() and deleted before the function terminates.
#'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Compare two samples from the same writer
#' s1 <- system.file(file.path("extdata", "docs", "w0005_s01_pLND_r03.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0005_s02_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' calculate_slr(s1, s2)
#'
#' # Compare samples from two writers
#' s1 <- system.file(file.path("extdata", "docs", "w0005_s02_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0238_s01_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' calculate_slr(s1, s2)
#' }
#'
#' @md
calculate_slr <- function(sample1_path, sample2_path,
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
#' @param df A dataframe created by \code{\link{calculate_slr}}.
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
  # check for non-numeric values
  if (!is.numeric(df$slr)) {
    stop("The slr value is not numeric.")
  }

  # check for infinite values
  if (is.infinite(df$slr)) {
    stop("The slr value cannot be infinite.")
  }

  # create appropriate message if slr >= 0, return error if slr < 0
  if (df$slr > 1) {
    x <- paste("A score-based likelihood ratio of", format(round(df$slr, 1), big.mark = ","), "means the likelihood of observing a similarity score of", df$score, "if the documents were written by the same person is", format(round(df$slr, 1), big.mark = ","), "times greater than the likelihood of observing this score if the documents were written by different writers.")
  } else if (df$slr > 0 && df$slr < 1) {
    x <- paste("A score-based likelihood ratio of", format(round(df$slr, 2), big.mark = ","), "means the likelihood of observing a similarity score of", df$score, "if the documents were written by different people is", format(round((1 / df$slr), 2), nsmall = 2, big.mark = ","), "times greater than the likelihood of observing this score if the documents were written by the same writer.")
  } else if (df$slr == 1) {
    x <- paste("A score-based likelihood ratio of", format(round(df$slr, 1), big.mark = ","), "means the likelihood of observing a similarity score of", df$score, "if the documents were written by different people is equal to the likelihood of observing the score if the documents were written by the same writer.")
  } else if (df$slr == 0) {
    x <- paste("A score-based likelihood ratio of 0 means it is virtually impossible that the documents were written by the same person.")
  } else {
    stop("The slr value must be greater than or equal to zero.")
  }
  return(x)
}

#' Get Rates of Misleading Evidence for SLRs
#'
#' Calculate the rates of misleading evidence for score-based likelihood ratios
#' (SLRs) when the ground truth is known.
#'
#' @param df A dataframe of SLRs from [`compare_writer_profiles`] with
#'   `score_only = FALSE`.
#' @param threshold A number greater than zero that serves as a decision
#'   threshold. If the ground truth for two documents is that they came from the
#'   same writer and the SLR is less than the decision threshold, this is
#'   misleading evidence that incorrectly supports the defense (false negative).
#'   If the ground truth for two documents is that they came from different
#'   writers and the SLR is greater than the decision threshold, this is
#'   misleading evidence that incorrectly supports the prosecution (false
#'   positive).
#'
#' @return A list
#' @export
#'
#' @examples
#' \donttest{
#' comparisons <- compare_writer_profiles(test, score_only = FALSE)
#' get_rates_of_misleading_slrs(comparisons)
#' }
#'
get_rates_of_misleading_slrs <- function(df, threshold = 1) {
  # Use across to prevent "no visible binding for global variable"
  known_matches <- df %>%
    dplyr::filter(dplyr::across(c("ground_truth")) == "same writer")
  known_non_matches <- df %>%
    dplyr::filter(dplyr::across(c("ground_truth")) == "different writer")

  # Use across to prevent "no visible binding for global variable"
  fn <- known_matches %>%
    dplyr::filter(dplyr::across(c("slr")) < threshold)
  fp <- known_non_matches %>%
    dplyr::filter(dplyr::across(c("slr")) > threshold)

  defense <- nrow(fn) / nrow(known_matches)
  prosecution <- nrow(fp) / nrow(known_non_matches)

  return(list("incorrectly_favors_defense" = defense, "incorrectly_favors_prosecution" = prosecution))

}


# Internal Functions ------------------------------------------------------

#' Make Densities
#'
#' Create densities of same writer and different writer scores from reference
#' scores created with get_validation scores().
#'
#' @param scores A list of reference scores created with
#'   \code{\link{get_validation_scores}}.
#'
#' @return A list of densities
#'
#' @noRd
make_densities <- function(scores) {
  pdfs <- list()
  pdfs$same_writer <- stats::density(scores$same_writer$score, kernel = "gaussian", n = 10000)
  pdfs$diff_writer <- stats::density(scores$diff_writer$score, kernel = "gaussian", n = 10000)

  return(pdfs)
}

#' Evaluate Density at a Point
#'
#' @param den A density created with \code{\link[stats]{density}}
#' @param x A number at which to evaluate the density. I.e., calculate the
#'   height of the density at the point.
#' @param type Use 'numerator' or 'denominator' to specify whether the density
#'   is for the numerator or denominator of the score-based likelihood ratio.
#'   This is used to determine how to handle NAs or zeros. If the density is for
#'   the numerator and the density evaluated at the point is NA, the output
#'   value is 0. If the density is for the denominator and the density evaluated
#'   at the point is NA or zero, the output is the value input for zero
#'   correction, to avoid dividing by zero when the score-based likelihood is
#'   calculated. If the density
#' @param zero_correction A small number to be used in place of zero in the
#'   denominator of the score-based likelihood ratio.
#'
#' @return A number
#'
#' @noRd
eval_density_at_point <- function(den, x, type, zero_correction = 1e-10) {
  y <- stats::approx(den$x, den$y, xout = x, n = 10000)$y

  # correct NAs
  if (is.na(y) && (type == "numerator")) {
    y <- 0
  }
  if (is.na(y) && (type == "denominator")) {
    y <- zero_correction
  }

  # correct zero in denominator
  if ((y == 0) && (type == "denominator")) {
    y <- zero_correction
  }

  return(y)
}
