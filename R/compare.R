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
#'   terminates. A CSV file with the results will not be saved, but a data frame
#'   of the results will be returned.
#' @param reference_scores Optional. A list of same writer and different writer
#'   similarity scores used for reference to calculate a score-based likelihood
#'   ratio. If reference scores are not supplied, `ref_scores` will be used only
#'   if `score_only` is FALSE. If score only is true, reference scores are
#'   unnecessary because a score-based likelihood ratio will not be calculated.
#'   If reference scores are supplied, `score_only` will automatically be set to
#'   FALSE.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # Compare two documents from the same writer with a similarity score
#' s1 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r01.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' compare_documents(s1, s2, score_only = TRUE)
#'
#' # Compare two documents from the same writer with a score-based
#' # likelihood ratio
#' s1 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r01.png"),
#'   package = "handwriterRF"
#' )
#' s2 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r02.png"),
#'   package = "handwriterRF"
#' )
#' compare_documents(s1, s2, score_only = FALSE)
compare_documents <- function(sample1,
                              sample2,
                              score_only = TRUE,
                              rforest = NULL,
                              project_dir = NULL,
                              reference_scores = NULL) {
  params <- list(
    sample1_path_original = sample1,
    sample2_path_original = sample2,
    sample1_path = sample1,
    sample2_path = sample2,
    sample1_name = basename(sample1),
    sample2_name = basename(sample2),
    score_only = score_only,
    rforest = rforest,
    project_dir = project_dir,
    reference_scores = reference_scores,
    score = NULL,
    slr = NULL,
    unknown_writers = TRUE
  )

  params <- setup(params)

  params <- run_checks(params)

  params <- copy_samples_to_project_dir(params)

  handwriter::process_batch_dir(
    input_dir = file.path(params$project_dir, "docs"),
    output_dir = file.path(params$project_dir, "graphs")
  )

  clusters <- handwriter::get_clusters_batch(
    template = templateK40,
    input_dir = file.path(params$project_dir, "graphs"),
    output_dir = file.path(params$project_dir, "clusters"),
    num_cores = 1,
    save_master_file = FALSE
  )

  message("Estimating writer profiles...")
  profiles <- get_writer_profiles(clusters = clusters, unknown_writers = params$unknown_writers)

  message("Calculating distance between samples...")
  dist_measures <- which_dists(rforest = params$rforest)
  d <- get_distances(df = profiles, distance_measures = dist_measures)

  message("Calculating similarity score...")
  params$score <- get_score(d = d, rforest = params$rforest, unknown_writers = params$unknown_writers)$score

  # Optional. Calculate SLR
  if (!score_only) {
    message("Calculating SLR...")
    params <- get_slr(params)
  }

  df <- make_results_df(params)

  clean_up(params)

  return(df)
}


# Internal Functions ------------------------------------------------------

run_checks <- function(params) {
  # samples can't be identical
  if (params$sample1_path_original == params$sample2_path_original) {
    stop("sample1 and sample2 can't be identical.")
  }

  check_dir_contents(params, "clusters")
  check_dir_contents(params, "docs")
  check_dir_contents(params, "graphs")

  return(params)
}

check_dir_contents <- function(params, dir_name) {
  if (!is.null(params$project_dir) && dir.exists(file.path(params$project_dir, dir_name))) {
    actual_files <- list.files(file.path(params$project_dir, dir_name))

    expected_files <- switch(dir_name,
      "docs" = c(params$sample1_name, params$sample2_name),
      "graphs" = c(
        "problems.txt",
        stringr::str_replace(params$sample1_name, ".png", "_proclist.rds"),
        stringr::str_replace(params$sample2_name, ".png", "_proclist.rds")
      ),
      "clusters" = c(
        stringr::str_replace(params$sample1_name, ".png", ".rds"),
        stringr::str_replace(params$sample2_name, ".png", ".rds")
      )
    )

    if (length(setdiff(actual_files, expected_files)) > 0) {
      stop("project_dir contains one or more helper files from documents other than sample1 and sample2.")
    }
  }
}

setup <- function(params) {
  handle_null_values <- function(params) {
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

  create_dirs <- function(params) {
    create_dir(params$project_dir)
    create_dir(file.path(params$project_dir, "clusters"))
    create_dir(file.path(params$project_dir, "docs"))
    create_dir(file.path(params$project_dir, "graphs"))
  }

  params <- handle_null_values(params)

  create_dirs(params)

  if (!is.null(params$reference_scores) && params$score_only) {
    message("Reference scores were supplied so score_only will be changed to FALSE.")
    params$score_only <- FALSE
  }

  if ((params$sample1_path_original != params$sample2_path_original) && (params$sample1_name == params$sample2_name)) {
    message("Samples have the same file name so they will be renamed 'sample1.png' and 'sample2.png'.")
    params$sample1_name <- "sample1.png"
    params$sample2_name <- "sample2.png"
  }

  return(params)
}

copy_samples_to_project_dir <- function(params) {
  # Copy samples to project_dir > docs
  message("Copying samples to project directory > docs...\n")

  # New file paths for samples in project directory
  params$sample1_path <- file.path(params$project_dir, "docs", params$sample1_name)
  params$sample2_path <- file.path(params$project_dir, "docs", params$sample2_name)

  file.copy(params$sample1_path_original, params$sample1_path)
  file.copy(params$sample2_path_original, params$sample2_path)

  return(params)
}

get_writer_profiles <- function(clusters, unknown_writers = TRUE) {
  counts <- handwriter::get_cluster_fill_counts(clusters)
  profiles <- get_cluster_fill_rates(counts)

  # get_distances requires a writer ID column
  if (unknown_writers) {
    profiles$writer <- c("unknown1", "unknown2")
  }

  return(profiles)
}

#' Calculate a Similarity Score
#'
#' Use a trained random forest to produce a similarity score for the distance
#' between two handwriting samples as described in Madeline Johnson and Danica
#' Ommen (2021) <doi:10.1002/sam.11566>.
#'
#' @param d A data frame of distance(s) between two handwriting samples,
#'   calculated with \code{\link{get_distances}}. The distance(s) needs to be the
#'   distance(s) used to train the random forest.
#' @param rforest A \pkg{ranger} random forest created with \code{\link{train_rf}}.
#'
#' @return A data frame
#'
#' @noRd
get_score <- function(d, rforest, unknown_writers = TRUE) {
  get_prop_same_votes <- function(preds) {
    # Get the proportion of decision trees in the trained random forest that
    # predict (vote) same writer.
    preds <- as.data.frame(preds)
    ntrees <- ncol(preds)
    prop <- rowSums(preds == 2) / ntrees
    return(prop)
  }

  make_scores_df <- function(score, d, unknown_writers) {
    scores_df <- data.frame("score" = score)
    scores_df$docname1 <- d$docname1
    scores_df$docname2 <- d$docname2

    # Add writer1, writer2, and match columns for known writing samples only
    if (!unknown_writers) {
      scores_df$match <- label_same_different_writer(dists = d)$match
      scores_df$writer1 <- d$writer1
      scores_df$writer2 <- d$writer2
    }

    # Sort columns
    scores_df <- scores_df %>%
      dplyr::select(tidyselect::any_of(c("docname1", "writer1", "docname2", "writer2", "match", "score")))
  }

  # Prevent note 'no visible binding for global variable'
  docname1 <- docname2 <- NULL

  # Get only the distance columns
  dists_only <- d %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("docname1", "writer1", "docname2", "writer2", "match")))

  # Get predictions: a matrix with a row for each doc and a column for each
  # decision tree. 1 = 'different', 2 = 'same'
  preds <- ranger::predictions(stats::predict(rforest$rf, dists_only, predict.all = TRUE))
  score <- get_prop_same_votes(preds = preds)

  scores_df <- make_scores_df(score = score, d = d, unknown_writers = unknown_writers)

  return(scores_df)
}

get_slr <- function(params) {
  densities <- make_densities(scores = params$reference_scores)
  params$numerator <- eval_density_at_point(den = densities$same_writer, x = params$score, type = "numerator")
  params$denominator <- eval_density_at_point(den = densities$diff_writer, x = params$score, type = "denominator")
  params$slr <- params$numerator / params$denominator

  return(params)
}

#' Make Densities
#'
#' Create densities of same writer and different writer scores from reference scores created with
#' get_validation scores().
#'
#' @param scores A list of reference scores created with \code{\link{get_validation_scores}}.
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

  # correct NA
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


make_results_df <- function(params) {
  df <- data.frame(
    "sample1" = params$sample1_path_original,
    "sample2" = params$sample2_path_original,
    "score" = params$score
  )

  if (!params$score_only) {
    df$numerator <- params$numerator
    df$denominator <- params$denominator
    df$slr <- params$slr
  }

  if (!params$unknown_writers) {
    df$writer1 <- params$writer1
    df$writer2 <- params$writer2
    df$ground_truth <- ifelse(df$writer1 == df$writer2, "same writer", "different writer")
  }

  df <- df %>% dplyr::select(tidyselect::any_of(c(
    "sample1", "writer1", "sample2", "writer2",
    "ground_truth", "score", "numerator", "denominator",
    "slr"
  )))
  return(df)
}

clean_up <- function(params) {
  # Optional. Delete comparison folder and contents in tempdir()
  if (params$project_dir == file.path(tempdir(), "comparison")) {
    unlink(file.path(tempdir(), "comparison"), recursive = TRUE)
  }
}
