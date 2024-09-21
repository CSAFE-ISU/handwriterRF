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
    doc1 <- handwriter::processDocument(sample1_path)
    doc2 <- handwriter::processDocument(sample2_path)
    saveRDS(doc1, file.path(project_dir, "graphs", paste0(doc1$docname, "_proclist.rds")))
    saveRDS(doc2, file.path(project_dir, "graphs", paste0(doc2$docname, "_proclist.rds")))
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
  score <- get_score(random_forest = rf$rf, d = d)

  # SLR
  message("Calculating SLR for samples...\n")
  numerator <- eval_density_at_point(den = densities$same_writer, x = score)
  denominator <- eval_density_at_point(den = densities$diff_writer, x = score)

  # Delete project folder from temp directory
  if (project_dir == file.path(tempdir(), "comparison")){
    unlink(project_dir, recursive = TRUE)
  }

  return(numerator / denominator)
}

eval_density_at_point <- function(den, x){
  y <- stats::approx(den$x, den$y, xout = x, n=10000)$y
  return(y)
}

# correct_NAs <- function(evals, zero_correction = 1e-10){
#   evals$numerators[is.na(evals$numerators)] <- 0
#   evals$denominators[is.na(evals$denominators)] <- zero_correction
#   return(evals)
# }
#
# correct_zeros <- function(evals, zero_correction = 1e-10){
#   evals$denominators[which(evals$denominators == 0)] <- zero_correction
#   return(evals)
# }
