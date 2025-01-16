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

#' Compare Documents
#'
#' Compare two handwritten documents to predict whether they were written by the
#' same person. Use either a similarity score or a score-based likelihood ratio
#' as a comparison method.
#'
#' @param sample1 A filepath to a handwritten document scanned and saved as a
#'   PNG file.
#' @param sample2 A filepath to a handwritten document scanned and saved as a
#'   PNG file.
#' @param score_only TRUE returns only the similarity score. FALSE returns the
#'   similarity score and a score-based likelihood ratio for that score,
#'   calculated using `reference_scores`.
#' @param rforest Optional. A random forest created with [`ranger::ranger()`].
#'   If a random forest is not supplied, `random_forest` will be used.
#' @param project_dir Optional. A folder in which to save helper files and a CSV
#'   file with the results. If no project directory is supplied. Helper files
#'   will be saved to tempdir() > comparison but deleted before the function
#'   terminates. A CSV file with the results will not be saved, but a dataframe
#'   of the results will be returned.
#' @param reference_scores Optional. A list of same writer and different writer
#'   similarity scores used for reference to calculate a score-based likelihood
#'   ratio. If reference scores are not supplied, `ref_scores` will be used only
#'   if `score_only` is FALSE. If score only is true, reference scores are
#'   unnecessary because a score-based likelihood ratio will not be calculated.
#'   If reference scores are supplied, `score_only` will automatically be set to
#'   FALSE.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' \donttest{
#' # Compare two documents from the same writer with a similarity score
#' s1 <- system.file(file.path("extdata", "docs", "w0005_s01_pLND_r03.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0005_s02_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' compare_documents(s1, s2, score_only = TRUE)
#'
#' # Compare two documents from the same writer with a score-based
#' # likelihood ratio
#' s1 <- system.file(file.path("extdata", "docs", "w0005_s01_pLND_r03.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0005_s02_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' compare_documents(s1, s2, score_only = FALSE)
#' }
#'
#' @md
compare_documents <- function(sample1,
                              sample2,
                              score_only = TRUE,
                              rforest = NULL,
                              project_dir = NULL,
                              reference_scores = NULL) {
  params <- list(
    samples = list(
      original_path1 = sample1,
      original_path2 = sample2,
      path1 = sample1,
      path2 = sample2,
      name1 = basename(sample1),
      name2 = basename(sample2)
    ),
    writer_profiles = NULL,
    score_only = score_only,
    rforest = rforest,
    project_dir = project_dir,
    reference_scores = reference_scores,
    score = NULL,
    slr = NULL
  )

  params <- handle_null_values(params)

  params <- handle_samples_w_same_name(params)

  create_dirs(params = params)

  params <- run_checks(params)

  params <- copy_samples_to_project_dir(params)

  message("Estimating writer profiles...")
  profiles <- handwriter::get_writer_profiles(
    input_dir = file.path(params$project_dir, "docs"),
    measure = "rates",
    template = templateK40,
    num_cores = 1,
    output_dir = params$project_dir)

  message("Calculating distance between samples...")
  params$dist <- get_distances(df = profiles, distance_measures = params$rforest$distance_measures)

  message("Calculating similarity score...")
  params$score <- get_score(d = params$dist, rforest = params$rforest)$score

  # Optional. Calculate SLR
  if (!score_only) {
    message("Calculating SLR...")
    params <- get_slr(params)
  }

  df <- make_results_df(params)

  clean_up(params)

  return(df)
}


#' Compare Writer Profiles
#'
#' Compare the writer profiles from two handwritten documents to predict whether
#' they were written by the same person. Use either a similarity score or a
#' score-based likelihood ratio as a comparison method.
#'
#' @param writer_profiles A dataframe of writer profiles or cluster fill rates
#'   calculated with [get_cluster_fill_rates]
#' @param score_only TRUE returns only the similarity score. FALSE returns the
#'   similarity score and a score-based likelihood ratio for that score,
#'   calculated using `reference_scores`.
#' @param rforest Optional. A random forest created with [`ranger::ranger()`].
#'   If a random forest is not supplied, `random_forest` will be used.
#' @param reference_scores Optional. A list of same writer and different writer
#'   similarity scores used for reference to calculate a score-based likelihood
#'   ratio. If reference scores are not supplied, `ref_scores` will be used only
#'   if `score_only` is FALSE. If score only is true, reference scores are
#'   unnecessary because a score-based likelihood ratio will not be calculated.
#'   If reference scores are supplied, `score_only` will automatically be set to
#'   FALSE.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' compare_writer_profiles(test[1:2, ], score_only = TRUE)
#'
#' compare_writer_profiles(test[1:2, ], score_only = FALSE)
#'
#' @md
compare_writer_profiles <- function(
    writer_profiles,
    score_only = TRUE,
    rforest = NULL,
    reference_scores = NULL) {
  params <- list(
    samples = NULL,
    writer_profiles = writer_profiles,
    score_only = score_only,
    rforest = rforest,
    project_dir = NULL,
    reference_scores = reference_scores,
    score = NULL,
    slr = NULL
  )

  if (!is_rates_df(params$writer_profiles)) {
    stop("Writer profiles must be a rates dataframe created by get_writer_profiles() with measure = 'rates'.")
  }

  params <- handle_null_values(params)

  message("Calculating distance between samples...")
  params$dist <- get_distances(df = writer_profiles, distance_measures = params$rforest$distance_measures)

  message("Calculating similarity score...")
  params$score <- get_score(d = params$dist, rforest = params$rforest)$score

  # Optional. Calculate SLR
  if (!score_only) {
    message("Calculating SLR...")
    params <- get_slr(params)
  }

  df <- make_results_df(params)

  return(df)
}

# Internal Functions ------------------------------------------------------

handle_null_values <- function(params) {
  if (!is.null(params$reference_scores) && params$score_only) {
    message("Reference scores were supplied so score_only will be changed to FALSE.")
    params$score_only <- FALSE
  }

  if (is.null(params$project_dir)) {
    params$project_dir <- file.path(tempdir(), "comparison")
  }

  if (is.null(params$rforest)) {
    params$rforest <- random_forest
  }

  if (is.null(params$reference_scores)) {
    params$reference_scores <- ref_scores
  }

  return(params)
}

create_dirs <- function(params, subdirs = NULL) {
  create_dir(params$project_dir)
  create_dir(file.path(params$project_dir, "clusters"))
  create_dir(file.path(params$project_dir, "docs"))
  create_dir(file.path(params$project_dir, "graphs"))
}

handle_samples_w_same_name <- function(params) {

  # samples in two different directories CAN have the same filename
  if (!is.null(params$samples) &&
      (params$samples$original_path1 != params$samples$original_path2) &&
      (params$samples$name1 == params$samples$name2)) {
    message("Samples have the same file name so they will be renamed 'sample1.png' and 'sample2.png'.")
    params$samples$name1 <- "sample1.png"
    params$samples$name2 <- "sample2.png"
  }

  # samples' paths CANNOT be identical
  if (!is.null(params$samples) &&
      (params$samples$original_path1 == params$samples$original_path2)) {
    stop("sample1 and sample2 can't be identical.")
  }

  return(params)
}

check_dir_contents <- function(params, dir_name) {
  if (!is.null(params$project_dir) && dir.exists(file.path(params$project_dir, dir_name))) {
    actual_files <- list.files(file.path(params$project_dir, dir_name))

    expected_files <- switch(dir_name,
                             "docs" = c(params$samples$name1, params$samples$name2),
                             "graphs" = c(
                               "problems.txt",
                               stringr::str_replace(params$samples$name1, ".png", "_proclist.rds"),
                               stringr::str_replace(params$samples$name2, ".png", "_proclist.rds")
                             ),
                             "clusters" = c(
                               stringr::str_replace(params$samples$name1, ".png", ".rds"),
                               stringr::str_replace(params$samples$name2, ".png", ".rds")
                             )
    )

    if (length(setdiff(actual_files, expected_files)) > 0) {
      stop("project_dir contains one or more helper files from documents other than sample1 and sample2.")
    }
  }
}

#' Is a Data Frame a Rates Data Frame
#'
#' Check if a dataframe is a rates dataframe.
#'
#' @param df A dataframe
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_rates_df <- function(df) {
  clusters <- get_cluster_cols(df)
  # Use all.equal to test for "near equality" of row sums. Because of rounding
  # errors, row sums might not be equal to one.
  if (ncol(clusters) > 0 && all.equal(unname(rowSums(clusters)), rep(1, nrow(clusters)))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

run_checks <- function(params) {

  check_dir_contents(params, "clusters")
  check_dir_contents(params, "docs")
  check_dir_contents(params, "graphs")

  return(params)
}

copy_samples_to_project_dir <- function(params) {
  # Copy samples to project_dir > docs
  message("Copying samples to project directory > docs...\n")

  # New file paths for samples in project directory
  params$samples$path1 <- file.path(params$project_dir, "docs", params$samples$name1)
  params$samples$path2 <- file.path(params$project_dir, "docs", params$samples$name2)

  file.copy(params$samples$original_path1, params$samples$path1)
  file.copy(params$samples$original_path2, params$samples$path2)

  return(params)
}


get_slr <- function(params) {
  get_slr_for_single_score <- function(score, densities) {
    numerator <- eval_density_at_point(den = densities$same_writer, x = score, type = "numerator")
    denominator <- eval_density_at_point(den = densities$diff_writer, x = score, type = "denominator")
    slr <- numerator / denominator
    return(slr)
  }

  densities <- make_densities(scores = params$reference_scores)

  params$slr <- sapply(params$score, function(x) get_slr_for_single_score(score = x, densities = densities))

  return(params)
}

make_results_df <- function(params) {
  df <- params$dist

  # drop distance measures
  df <- df %>% dplyr::select(tidyselect::any_of(c("docname1", "writer1", "docname2", "writer2")))

  df$score <- params$score

  if (!params$score_only) {
    df$slr <- params$slr
  }

  if (!all(startsWith(df$writer1, "unknown")) && !all(startsWith(df$writer2, "unknown"))) {
    df$ground_truth <- ifelse(df$writer1 == df$writer2, "same writer", "different writer")
  }

  df <- df %>%
    dplyr::select(tidyselect::any_of(c("docname1", "writer1", "docname2", "writer2", "ground_truth", "score", "slr")))
  return(df)
}

clean_up <- function(params) {
  # Optional. Delete comparison folder and contents in tempdir()
  if (params$project_dir == file.path(tempdir(), "comparison")) {
    delete_tempdir_comparison()
  }
}
