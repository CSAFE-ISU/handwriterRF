devtools::load_all()


# Helper Functions --------------------------------------------------------

load_cluster_fill_rates <- function(clusters_dir) {
  files <- list.files(clusters_dir, full.names = TRUE)
  dfs <- lapply(files, readRDS)
  clusters <- do.call(rbind, dfs)
  counts <- handwriter::get_cluster_fill_counts(clusters)
  rates <- get_cluster_fill_rates(counts)
  return(rates)
}

make_csafe_sets <- function(rates) {
  find_writers_with_27_docs <- function(df) {
    # Filter csafe data frame for writers with 27 docs
    df <- expand_docnames(df)
    writers <- df %>%
      dplyr::group_by(writer) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::filter(n == 27) %>%
      dplyr::pull(writer)
    return(writers)
  }

  sample_prompts <- function(df, writers) {
    df <- df %>% dplyr::filter(writer %in% writers)

    # select one WOZ, LND, and PHR from each writer in df
    woz <- df %>%
      dplyr::filter(prompt == "pWOZ") %>%
      dplyr::group_by(writer) %>%
      dplyr::slice_sample(n=1)
    lnd <- df %>%
      dplyr::filter(prompt == "pLND") %>%
      dplyr::group_by(writer) %>%
      dplyr::slice_sample(n=1)
    # phr <- df %>%
    #   dplyr::filter(prompt == "pPHR") %>%
    #   dplyr::group_by(writer) %>%
    #   dplyr::slice_sample(n=1)
    # df <- rbind(woz, lnd, phr)
    df <- rbind(woz, lnd)
    return(df)
  }

  drop_columns <- function(df) {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-session, -prompt, -rep)
  }

  # split writers train, validation, and test sets
  all_writers <- find_writers_with_27_docs(df = rates)
  writers <- split_writers(all_writers = all_writers)

  rates_exp <- expand_docnames(df = rates)
  prompts <- lapply(writers, function(x) sample_prompts(df = rates_exp, writers = x))
  prompts <- lapply(prompts, drop_columns)

  return(prompts)
}

make_cvl_sets <- function(rates){
  expand_cvl_docnames <- function(df) {
    df <- df %>%
      tidyr::separate(docname, into = c("writer", "prompt"), extra = "merge", remove = FALSE)
    return(df)
  }

  sample_prompts <- function(df, writers) {
    # select 2 prompts from each writer

    prompts <- list()

    prompts$train <- df %>%
      dplyr::filter(writer %in% writers$train) %>%
      dplyr::group_by(writer) %>%
      dplyr::slice_sample(n=2)

    prompts$validation <- df %>%
      dplyr::filter(writer %in% writers$validation) %>%
      dplyr::group_by(writer) %>%
      dplyr::slice_sample(n=2)

    prompts$test <- df %>%
      dplyr::filter(writer %in% writers$test) %>%
      dplyr::group_by(writer) %>%
      dplyr::slice_sample(n=2)

    return(prompts)
  }

  drop_prompt_column <- function(df) {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-prompt)
    return(df)
  }

  rates_exp <- expand_cvl_docnames(df = rates)
  writers <- split_writers(all_writers = unique(rates_exp$writer))
  prompts <- sample_prompts(df = rates_exp, writers = writers)
  prompts <- lapply(prompts, drop_prompt_column)

  return(prompts)
}

split_writers <- function(all_writers) {
  # split writers into train, validation, and test sets

  writers <- list()

  # sample writers for random forest and remove from list
  writers$train <- sample(all_writers, size=100)
  all_writers <- setdiff(all_writers, writers$train)

  # sample writers for validation and remove from list
  writers$validation <- sample(all_writers, size = 100)
  all_writers <- setdiff(all_writers, writers$validation)

  # put remaining writers in test set
  writers$test <- all_writers

  return(writers)
}


# Run Code ----------------------------------------------------------------

# Load data and split into train, validation, and test sets
csafe <- load_cluster_fill_rates(clusters_dir = "/Users/stephanie/Documents/handwriting_datasets/CSAFE_Handwriting_Database/clusters")
csafe <- make_csafe_sets(rates = csafe)
saveRDS(csafe, "data-raw/csafe_sets.rds")

cvl <- load_cluster_fill_rates("/Users/stephanie/Documents/handwriting_datasets/CVL/clusters")
cvl <- make_cvl_sets(rates = cvl)
saveRDS(cvl, "data-raw/cvl_sets.rds")

# Train random forest
train <- rbind(csafe$train, cvl$train)
saveRDS(train, "data-raw/train.rds")
usethis::use_data(train, overwrite = TRUE)

random_forest <- train_rf(train, ntrees = 200, distance_measures = c("abs", "euc"))
saveRDS(random_forest, "data-raw/random_forest.rds")
usethis::use_data(random_forest, overwrite = TRUE)

# Get similarity scores on validation set
validation <- rbind(csafe$validation, cvl$validation)
saveRDS(validation, "data-raw/validation.rds")
usethis::use_data(validation, overwrite = TRUE)

ref_scores <- get_validation_scores(rforest = random_forest, df = ref_scores)
saveRDS(ref_scores, "data-raw/ref_scores.rds")
usethis::use_data(ref_scores, overwrite = TRUE)

# Test set
test <- rbind(csafe$test, cvl$test)
saveRDS(test, "data-raw/test.rds")
usethis::use_data(test, overwrite = TRUE)

plot_histograms(scores = ref_scores, obs_score = 0.3, downsample_size = NULL)
