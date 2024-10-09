# External Functions ------------------------------------------------------

#' Calculate a Score-Based Likelihood Ratio
#'
#' Compares two handwriting samples scanned and saved a PNG images. The writing
#' in both samples is split into component shapes, or graphs, with
#' 'handwriter::processDocument'. The graphs are grouped into clusters of
#' similar shapes with 'handwriter::get_clusterassignment'. The proportion of
#' graphs assigned to each cluster, called the cluster fill rates, are used as
#' writer profiles. The cluster fill rates are calculated with
#' 'get_cluster_fill_rates'. A similarity score is calculated between the two
#' samples using a random forest trained with 'ranger'. The similarity score is
#' compared to reference distributions of 'same writer' and 'different writer'
#' similarity scores. The result is a score-based likelihood ratio that conveys
#' the strength of the evidence in favor of 'same writer' or 'different writer'.
#' For more details, see Madeline Johnson and Danica Ommen (2021) <doi:10.1002/sam.11566>.
#'
#' @param sample1_path A file path to a handwriting sample saved in PNG file
#'   format.
#' @param sample2_path A file path to a second handwriting sample saved in PNG
#'   file format.
#' @param rforest Optional. A random forest trained with 'ranger'. If rforest is
#'   not given, the data object random_forest is used.
#' @param project_dir Optional. A path to a directory where helper files will be
#'   saved. If no project directory is specified, the helper files will be saved
#'   to 'tempdir()' and deleted before the function terminates.
#' @param copy_samples TRUE or FALSE. If TRUE, the PNG files will be copied to
#'   the project directory. If a project directory is not given, the samples
#'   will not be copied even if copy_samples is TRUE.
#'
#' @return A number
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare two samples from the same writer
#' sample1 <- system.file(file.path("extdata", "w0030_s01_pWOZ_r01.png"), package = "handwriterRF")
#' sample2 <- system.file(file.path("extdata", "w0030_s01_pWOZ_r02.png"), package = "handwriterRF")
#' calculate_slr(sample1, sample2)
#'
#' # Compare samples from two writers
#' sample1 <- system.file(file.path("extdata", "w0030_s01_pWOZ_r01.png"), package = "handwriterRF")
#' sample2 <- system.file(file.path("extdata", "w0238_s01_pWOZ_r02.png"), package = "handwriterRF")
#' calculate_slr(sample1, sample2)
#' }
#'
calculate_slr <- function(sample1_path, sample2_path, rforest = random_forest, project_dir = NULL, copy_samples = FALSE) {
  copy_samples_to_project_dir <- function(sample1_path, sample2_path, project_dir) {
    # Copy samples to project_dir > docs
    message("Copying samples to output directory > docs...\n")
    create_dir(file.path(project_dir, "docs"))
    file.copy(sample1_path, file.path(project_dir, "docs", basename(sample1_path)))
    file.copy(sample2_path, file.path(project_dir, "docs", basename(sample2_path)))
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

  # error if sample1_path == sample2_path
  if (identical(sample1_path, sample2_path)) {
    stop("sample1_path and sample2_path cannot be identical.")
  }

  # set output directory as temp directory if NULL
  if (is.null(project_dir)) {
    project_dir <- file.path(tempdir(), "comparison")
    # the project directory will be deleted from the temp directory so copying
    # samples to the project directory is useless.
    copy_samples <- FALSE
  }

  if (copy_samples) {
    copy_samples_to_project_dir(
      sample1_path = sample1_path,
      sample2_path = sample2_path,
      project_dir = project_dir
    )
  }

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

  # SLR
  message("Calculating SLR for samples...\n")
  numerator <- eval_density_at_point(den = rforest$densities$same_writer, x = score, type = "numerator")
  denominator <- eval_density_at_point(den = rforest$densities$diff_writer, x = score, type = "denominator")
  slr <- numerator / denominator
  df <- data.frame("sample1_path" = sample1_path, "sample2_path" = sample2_path,
                   "docname1" = basename(sample1_path), "docname2" = basename(sample2_path),
                   "slr" = slr)

  # delete project folder from temp directory or save SLR to project folder
  if (project_dir == file.path(tempdir(), "comparison")) {
    unlink(project_dir, recursive = TRUE)
  } else {
    saveRDS(df, file.path(project_dir, "slr.rds"))
  }

  return(slr)
}


# Internal Functions ------------------------------------------------------

#' Evaluate Density at a Point
#'
#' @param den A density created with 'density'
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
