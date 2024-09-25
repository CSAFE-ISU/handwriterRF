calculate_slr <- function(sample1_path, sample2_path, project_dir = NULL, copy_samples = FALSE){

  copy_samples_to_project_dir <- function(sample1_path, sample2_path, project_dir){
    # Copy samples to project_dir > docs
    message("Copying samples to output directory > docs...\n")
    create_dir(file.path(project_dir, "docs"))
    file.copy(sample1_path, file.path(project_dir, "docs", basename(sample1_path)))
    file.copy(sample2_path, file.path(project_dir, "docs", basename(sample2_path)))
  }

  process_and_save_samples <- function(sample1_path, sample2_path, project_dir) {
    # Process samples and save in project_dir > graphs
    message("Processing samples...")

    create_dir(file.path(project_dir, "graphs"))

    # Skip if processed doc1 already exists in project_dir > graphs
    outfile1 <- file.path(project_dir, "graphs", stringr::str_replace(basename(sample1_path), ".png", "_proclist.rds"))
    if (!file.exists(outfile1)){
      doc1 <- handwriter::processDocument(sample1_path)
      saveRDS(doc1, outfile1)
    }

    # Skip if processed doc2 already exists in project_dir > graphs
    outfile2 <- file.path(project_dir, "graphs", stringr::str_replace(basename(sample2_path), ".png", "_proclist.rds"))
    if (!file.exists(outfile2)){
      doc2 <- handwriter::processDocument(sample2_path)
      saveRDS(doc2, outfile2)
    }

    return()
  }

  # error if sample1_path == sample2_path
  if (identical(sample1_path, sample2_path)){
    stop("sample1_path and sample2_path cannot be identical.")
  }

  # set output directory as temp directory if NULL
  if (is.null(project_dir)){
    project_dir <- file.path(tempdir(), "comparison")
    # the project directory will be deleted from the temp directory so copying
    # samples to the project directory is useless.
    copy_samples <- FALSE
  }

  if (copy_samples){
    copy_samples_to_project_dir(sample1_path = sample1_path,
                                sample2_path = sample2_path,
                                project_dir = project_dir)
  }

  process_and_save_samples(sample1_path = sample1_path,
                           sample2_path = sample2_path,
                           project_dir = project_dir)

  clusters <- handwriter::get_clusters_batch(template = templateK40,
                                             input_dir = file.path(project_dir, "graphs"),
                                             output_dir = file.path(project_dir, "clusters"),
                                             writer_indices = c(2,5),
                                             doc_indices = c(7, 18),
                                             save_master_file = TRUE)
  counts <- handwriter::get_cluster_fill_counts(clusters)
  rates <- get_cluster_fill_rates(counts)

  # Distance
  message("Calculating distance between samples...\n")
  d <- get_distances(rates, c("abs", "euc"))

  # Score
  message("Calculating similarity score between samples...\n")
  score <- get_score(random_forest = rf, d = d)

  # SLR
  message("Calculating SLR for samples...\n")
  numerator <- eval_density_at_point(den = densities$same_writer, x = score, type = "numerator")
  denominator <- eval_density_at_point(den = densities$diff_writer, x = score, type = "denominator")

  # Delete project folder from temp directory
  if (project_dir == file.path(tempdir(), "comparison")){
    unlink(project_dir, recursive = TRUE)
  }

  return(numerator / denominator)
}

eval_density_at_point <- function(den, x, type, zero_correction = 1e-10){
  y <- stats::approx(den$x, den$y, xout = x, n=10000)$y

  # correct NA
  if (is.na(y) && (type == "numerator")){
    y <- 0
  }
  if (is.na(y) && (type == "denominator")){
    y <- zero_correction
  }

  # correct zero in denominator
  if ((y == 0) && (type == "denominator")){
    y <- zero_correction
  }

  return(y)
}

correct_NAs <- function(y, type, zero_correction){
  if (type == "numerator"){
    y <- 0
  } else if (type == "denominator"){
    y <- zero_correction
  } else {
    stop("Type not defined. Use 'numerator' or 'denominator'.")
  }

  return(y)
}
