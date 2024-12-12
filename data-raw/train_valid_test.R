install.packages("handwriter")
devtools::install_github("CSAFE-ISU/handwriterRF")

library(handwriter)
library(handwriterRF)

# Helper Functions --------------------------------------------------------

load_cluster_fill_rates <- function(clusters_dir) {
  files <- list.files(clusters_dir, full.names = TRUE)
  dfs <- lapply(files, readRDS)
  clusters <- do.call(rbind, dfs)
  rates <- handwriter::get_cluster_fill_rates(clusters)
  return(rates)
}

make_csafe_sets <- function(rates, prompts = c("pWOZ", "pLND"), num_per_prompt = 3,
                            num_train_writers = 100, num_validation_writers = 200) {
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

  sample_prompts <- function(df, writers, prompts, num_per_prompt) {
    df <- df %>% dplyr::filter(writer %in% writers)

    # randomly select prompts from each writer
    dfs <- lapply(prompts, function(p) {
      df %>%
        dplyr::filter(prompt == p) %>%
        dplyr::group_by(writer) %>%
        dplyr::slice_sample(n = num_per_prompt)
    })
    df <- do.call(rbind, dfs)

    return(df)
  }

  drop_columns <- function(df) {
    df$doc <- paste(df$session, df$prompt, df$rep, sep = "_")
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-tidyselect::any_of(c("session", "prompt", "rep")))
  }

  # drop writer and doc column to prevent error with expand_docnames
  rates <- rates %>% dplyr::select(-tidyselect::any_of(c("writer", "doc")))

  # split writers train, validation, and test sets
  all_writers <- find_writers_with_27_docs(df = rates)
  writers <- split_writers(
    all_writers = all_writers,
    num_train_writers = num_train_writers,
    num_validation_writers = num_validation_writers
  )

  rates_exp <- expand_docnames(df = rates)
  docs <- lapply(writers, function(x) sample_prompts(df = rates_exp, writers = x, prompts = prompts, num_per_prompt = num_per_prompt))
  docs <- lapply(docs, drop_columns)

  return(docs)
}

make_cvl_sets <- function(rates, num_per_writer = 4, use_German_prompt = FALSE,
                          num_train_writers, num_validation_writers) {
  expand_cvl_docnames <- function(df) {
    df <- df %>%
      tidyr::separate(docname, into = c("writer", "prompt"), extra = "merge", remove = FALSE)
    return(df)
  }

  find_writers_with_5plus_docs <- function(df) {
    # Filter cvl data frame for writers with 5 or more docs
    writers <- df %>%
      dplyr::group_by(writer) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::filter(n >= 5) %>%
      dplyr::pull(writer)
    return(writers)
  }

  sample_cvl_prompts <- function(df, set_writers, num_per_writer, use_German_prompt) {
    if (!use_German_prompt) {
      df <- df %>% dplyr::filter(prompt != "6-cropped")
    }

    df <- df %>%
      dplyr::filter(writer %in% set_writers) %>%
      dplyr::group_by(writer) %>%
      dplyr::slice_sample(n = num_per_writer)

    return(df)
  }

  drop_cvl_prompt_column <- function(df) {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-prompt)
    return(df)
  }

  rates <- expand_cvl_docnames(df = rates)
  rates$writer <- paste0("c", rates$writer)

  all_writers <- find_writers_with_5plus_docs(df = rates)
  writers <- split_writers(
    all_writers = all_writers,
    num_train_writers = num_train_writers,
    num_validation_writers = num_validation_writers
  )
  docs <- lapply(writers, function(w) {
    sample_cvl_prompts(
      df = rates,
      set_writers = w,
      num_per_writer = num_per_writer,
      use_German_prompt = use_German_prompt
    )
  })
  docs <- lapply(docs, drop_cvl_prompt_column)

  return(docs)
}

split_writers <- function(all_writers, num_train_writers, num_validation_writers) {
  # split writers into train, validation, and test sets

  writers <- list()

  # sample writers for random forest and remove from list
  writers$train <- sample(all_writers, size = num_train_writers)
  all_writers <- setdiff(all_writers, writers$train)

  # sample writers for validation and remove from list
  writers$validation <- sample(all_writers, size = num_validation_writers)
  all_writers <- setdiff(all_writers, writers$validation)

  # put remaining writers in test set
  writers$test <- all_writers

  return(writers)
}


# Run Code ----------------------------------------------------------------

set.seed(100)

# If you need cluster assignments CVL data
handwriter::get_clusters_batch(input_dir = "path/to/cvl/graphs/dir",
                               output_dir = "path/to/cvl/clusters/dir",
                               template = "path/to/template.rds",
                               writer_indices = c(1,4),
                               doc_indices = c(6,6),
                               num_cores = 4)

# Create data frames of csafe and cvl cluster fill rates
csafe <- load_cluster_fill_rates(clusters_dir = "/Users/stephanie/Documents/handwriting_datasets/CSAFE_Handwriting_Database/300dpi/clusters")
cvl <- load_cluster_fill_rates("/Users/stephanie/Documents/handwriting_datasets/CVL/300dpi/clusters")

# Make sets. Feel free to change num_train_writers and num_validation_writers.
# Writers not assigned to either of these sets will be placed in the test set.
csafe <- make_csafe_sets(
  rates = csafe,
  prompts = c("pWOZ", "pLND"),
  num_per_prompt = 2,
  num_train_writers = 100,
  num_validation_writers = 150
)
cvl <- make_cvl_sets(
  rates = cvl,
  num_per_writer = 4,
  use_German_prompt = FALSE,
  num_train_writers = 100,
  num_validation_writers = 150
)

# Train random forest
train <- rbind(csafe$train, cvl$train)

# Choose distance measures
rf <- train_rf(train,
               ntrees = 200,
               distance_measures = c("abs", "euc"))


# Get similarity scores on validation set. Note: there will be many times more
# 'different writer' scores compared to 'same writer' scores.
validation <- rbind(csafe$validation, cvl$validation)
rscores <- get_ref_scores(rforest = rf, df = validation)

# Test set
test <- rbind(csafe$test, cvl$test)

results <- compare_writer_profiles(writer_profiles = test,
                                   score_only = FALSE,
                                   rforest = rf,
                                   reference_scores = rscores)
