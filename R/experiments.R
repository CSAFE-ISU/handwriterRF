get_test_samples <- function(train, sessions, prompts, seed = 100) {
  # Prevent note "no visible binding for global variable"
  session <- prompt <- path <- writer <- docname <- NULL

  set.seed(seed)

  # csafe database
  db <- "/Users/stephanie/Documents/handwriting_datasets/CSAFE_Handwriting_Database/docs"

  # get test writers ----
  train_writers <- expand_docnames(train, "docname")
  train_writers <- unique(train_writers$writer)
  test_writers <- setdiff(list.files(db), train_writers)

  # get all possible test samples
  samples <- unlist(sapply(file.path(db, test_writers), function(x) list.files(x, pattern = ".png", full.names = TRUE), USE.NAMES = FALSE))
  samples <- data.frame("path" = samples)
  samples$docname <- basename(samples$path)
  samples <- expand_docnames(samples, "docname")

  # select session 2, WOZ and LND
  samples <- samples %>%
    dplyr::filter(session %in% sessions, prompt %in% prompts)

  # make data frame same writer pairs
  same <- samples %>%
    dplyr::select(-path, -session, -rep) %>%
    dplyr::group_by(writer, prompt) %>%
    dplyr::slice_sample(n=1) %>%
    tidyr::pivot_wider(names_from = prompt,
                       values_from = docname) %>%
    dplyr::ungroup() %>%
    dplyr::select(-writer)
  colnames(same) <- c("docname1", "docname2")
  same$match <- "same"

  # make data frame of writer pairs
  diff <- same
  diff$docname2 <- sample(diff$docname2, size = nrow(diff))
  diff$match <- "different"

  # get paths
  samples <- rbind(same, diff)
  samples <- expand_docnames(samples, "docname1", "1")
  samples <- expand_docnames(samples, "docname2", "2")
  samples$sample1_path <- file.path(db, samples$writer1, samples$docname1)
  samples$sample2_path <- file.path(db, samples$writer2, samples$docname2)

  return(samples)
}

get_slrs <- function(samples, rforest, experiment_dir) {
  for (i in 1:nrow(samples)) {
    sample1_path <- samples$sample1_path[i]
    sample2_path <- samples$sample2_path[i]
    project_dir <- file.path(experiment_dir, paste0(basename(sample1_path), "_v_", basename(sample2_path)))
    slr <- calculate_slr(sample1_path = sample1_path,
                         sample2_path = sample2_path,
                         rforest = rforest,
                         project_dir = project_dir,
                         copy_samples = FALSE)
  }
}

load_slrs <- function(experiment_dir) {
  slrs <- list.files(experiment_dir, pattern = ".png", full.names = TRUE)
  results <- list()
  for (i in 1:length(slrs)) {
    current_folder <- slrs[i]
    if (file.exists(file.path(current_folder, "slr.rds"))){
      results[[i]] <- readRDS(file.path(current_folder, "slr.rds"))
    }
  }
  df <- do.call(rbind, results)

  df <- label_same_different_writer(df)

  return(df)
}

calculate_errors <- function(df) {
  # Prevent note "no visible binding for global variable"
  total <- slr <- NULL

  # false positive rate
  total_diff <- df %>%
    dplyr::filter(match == "different") %>%
    dplyr::summarize(total = dplyr::n()) %>%
    dplyr::pull(total)
  fpr <- df %>%
    dplyr::filter(match == "different") %>%
    dplyr::filter(slr > 1) %>%
    dplyr::summarize(fpr = dplyr::n() / total_diff) %>%
    dplyr::pull()

  # false negative rate
  total_same <- df %>%
    dplyr::filter(match == "same") %>%
    dplyr::summarize(total = dplyr::n()) %>%
    dplyr::pull()
  fnr <- df %>%
    dplyr::filter(match == "same") %>%
    dplyr::filter(slr < 1) %>%
    dplyr::summarize(fnr = dplyr::n() / total_same) %>%
    dplyr::pull()

  return(list("fpr" = fpr, "fnr" = fnr))
}

