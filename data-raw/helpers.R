make_csafe_profiles_df <- function(clusters_dir) {
  # Make master file for CSAFE writer profiles
  writers <- list.files(clusters_dir)
  dfs <- lapply(writers, function(w) readRDS(file.path(clusters_dir, w, "all_clusters.rds")))
  all_clusters <- do.call(rbind, dfs)
  profiles <- handwriter::get_cluster_fill_rates(all_clusters)
  saveRDS(profiles, "data-raw/csafe_writer_profiles.rds")
  return(profiles)
}

make_csafe_sets <- function(all_profiles, seed = 111817) {

  set.seed(seed)

  # Count remaining writers
  rw <- unique(all_profiles$writer)
  message(paste("Number of writers remaining:", length(rw)))

  # Get session, prompt, and rep
  all_profiles <- expand_docnames(all_profiles)

  # Select two LND, two WOZ, and two PHR from each writer
  rp <- all_profiles %>%
    dplyr::group_by(writer, prompt) %>%
    dplyr::slice_sample(n=2) %>%
    dplyr::ungroup()

  # Assign writers to sets
  rw <- unique(rp$writer)
  train_writers <- sample(rw, size=100)
  rw <- rw[!(rw %in% train_writers)]
  message(paste("Number of writers remaining:", length(rw)))
  valid_writers <- sample(rw, size=150)
  rw <- rw[!(rw %in% valid_writers)]
  message(paste("Number of writers remaining:", length(rw)))
  test_writers <- rw
  rw <- rw[!(rw %in% test_writers)]
  message(paste("Number of writers remaining:", length(rw)))

  csafe_sets <- list()
  csafe_sets$train <- rp %>%
    dplyr::filter(writer %in% train_writers) %>%
    dplyr::filter(prompt != "pPHR")
  csafe_sets$valid <- rp %>%
    dplyr::filter(writer %in% valid_writers) %>%
    dplyr::filter(prompt != "pPHR")
  csafe_sets$test <- rp %>%
    dplyr::filter(writer %in% test_writers)

  saveRDS(csafe_sets, "data-raw/csafe_sets.rds")

  return(csafe_sets)
}

make_cvl_profiles_df <- function(clusters_dir) {
  writers <- list.files(clusters_dir)
  clusters_dfs <- lapply(writers, function(writer) readRDS(file.path(clusters_dir, writer, "all_clusters.rds")))
  clusters_df <- do.call(rbind, clusters_dfs)
  profiles <- handwriter::get_cluster_fill_rates(clusters_df)
  saveRDS(profiles, "data-raw/cvl_writer_profiles.rds")
  return(profiles)
}


make_cvl_sets <- function(all_profiles, seed) {
  set.seed(seed)

  message(paste("Number of prompts: ", nrow(all_profiles)))

  # Drop writer with only 3 prompts
  message("Drop writer with only three prompts")
  missing_prompts <- all_profiles %>%
    dplyr::group_by(writer) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::filter(n == 3) %>%
    dplyr::pull(writer)
  rp <- all_profiles %>% dplyr::filter(writer != missing_prompts)
  message(paste("Number of prompts remaining: ", nrow(rp)))
  message(paste("Number of writers remaining: ", length(unique(rp$writer))))

  # Split by prompt
  dfs <- split(rp, f = rp$doc)

  # Writers
  rw <- unique(dfs[[1]]$writer)

  # Assign writers who wrote prompts 7 and 8 to test set
  message("Assign writers who wrote prompts 7 and 8 to test set")
  test_writers <- unique(dfs[[7]]$writer)
  rw <- rw[!(rw %in% test_writers)]
  message("Number of writers in test set: ", length(test_writers))
  message("Number of writers remaining: ", length(rw))

  # Train set
  message("Randomly select 100 writers for train set")
  train_writers <- sample(rw, size = 100)
  rw <- rw[!(rw %in% train_writers)]
  message("Number of writers in train set: ", length(train_writers))
  message("Number of writers remaining: ", length(rw))

  # Validation set
  message("Randomly select 150 writers for validation set")
  valid_writers <- sample(rw, size = 150)
  rw <- rw[!(rw %in% valid_writers)]
  message("Number of writers in validation set: ", length(valid_writers))
  message("Number of writers remaining: ", length(rw))

  # Assign remaining writers to test set
  message("Assign remaining writers to test set")
  test_writers <- sort(c(test_writers, rw))
  rw <- rw[!(rw %in% test_writers)]
  message("Number of writers in test set: ", length(test_writers))
  message("Number of writers remaining: ", length(rw))

  # Get train prompts - Use prompts 1-4 from train writers
  message("Get prompts from train writers")
  train <- rp %>%
    dplyr::filter(writer %in% train_writers, doc <= 4)
  rp <- rp %>% dplyr::filter(!(docname %in% train$docname))
  message("Number of prompts in train set: ", nrow(train))
  message("Number of prompts remaining: ", nrow(rp))

  # Get validation prompts - Use prompts 1-4 from validation writers
  message("Get prompts from validation writers")
  valid <- rp %>%
    dplyr::filter(writer %in% valid_writers, doc <= 4)
  rp <- rp %>% dplyr::filter(!(docname %in% valid$docname))
  message("Number of prompts in valid set: ", nrow(valid))
  message("Number of prompts remaining: ", nrow(rp))

  # Assign remaining prompts to test set
  message("Assign remaining prompts to test set")
  test <- rp %>%
    dplyr::filter(writer %in% test_writers)
  rp <- rp %>% dplyr::filter(!(docname %in% test$docname))
  message("Number of prompts in test set: ", nrow(test))
  message("Number of prompts remaining: ", nrow(rp))

  if ((length(unique(rp$doc)) == 1) && (unique(rp$doc) == 6)) {
    message("The remaining prompts are German.")
  } else {
    stop("There are unaccounted for prompts remaining.")
  }

  # combine sets
  cvl_sets <- list()
  cvl_sets$train <- train
  cvl_sets$valid <- valid
  cvl_sets$test <- test

  saveRDS(cvl_sets, "data-raw/cvl_sets.rds")

  return(cvl_sets)
}

make_doc_column <- function(df) {
  if ("session" %in% colnames(df)) {
    df$doc <- df$prompt
    df <- df %>% dplyr::select(-tidyselect::any_of(c("session", "prompt", "rep")))
    df <- df %>% dplyr::select(tidyselect::any_of(c("docname", "writer", "doc")),
                               tidyselect::everything())
  }
  return(df)
}
