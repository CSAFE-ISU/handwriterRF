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
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-session, -prompt, -rep)
  }

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
    df <- expand_cvl_docnames(df)
    writers <- df %>%
      dplyr::group_by(writer) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::filter(n >= 5) %>%
      dplyr::pull(writer)
    return(writers)
  }

  sample_prompts <- function(df, writers, num_per_writer, use_German_prompt) {
    df <- expand_cvl_docnames(df = df)

    if (!use_German_prompt) {
      df <- df %>% dplyr::filter(prompt != "6-cropped")
    }

    df <- df %>%
      dplyr::filter(writer %in% writers) %>%
      dplyr::group_by(writer) %>%
      dplyr::slice_sample(n = num_per_writer)

    return(df)
  }

  drop_prompt_column <- function(df) {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-prompt)
    return(df)
  }

  all_writers <- find_writers_with_5plus_docs(df = rates)
  writers <- split_writers(
    all_writers = all_writers,
    num_train_writers = num_train_writers,
    num_validation_writers = num_validation_writers
  )
  docs <- lapply(writers, function(w) {
    sample_prompts(
      df = rates,
      writers = w,
      num_per_writer = num_per_writer,
      use_German_prompt = use_German_prompt
    )
  })
  docs <- lapply(docs, drop_prompt_column)

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

# Create data frames of csafe and cvl cluster fill rates
# csafe <- load_cluster_fill_rates(clusters_dir = "/Users/stephanie/Documents/handwriting_datasets/CSAFE_Handwriting_Database/clusters")
# saveRDS(csafe, "data-raw/csafe_cfr.rds")
#
# cvl <- load_cluster_fill_rates("/Users/stephanie/Documents/handwriting_datasets/CVL/clusters")
# saveRDS(cvl, "data-raw/cvl_cfr.rds")

# Load cluster fill rates
csafe <- readRDS("data-raw/csafe_cfr.rds")
cvl <- readRDS("data-raw/cvl_cfr.rds")

# Make sets
csafe <- make_csafe_sets(
  rates = csafe, prompts = c("pWOZ", "pLND"), num_per_prompt = 2,
  num_train_writers = 100, num_validation_writers = 150
)
saveRDS(csafe, "data-raw/csafe_sets.rds")

cvl <- make_cvl_sets(
  rates = cvl, num_per_writer = 4, use_German_prompt = FALSE,
  num_train_writers = 100, num_validation_writers = 150
)
saveRDS(cvl, "data-raw/cvl_sets.rds")

# csafe <- readRDS("data-raw/csafe_sets.rds")
# cvl <- readRDS("data-raw/cvl_sets.rds")

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

ref_scores <- get_ref_scores(rforest = random_forest, df = validation)
saveRDS(ref_scores, "data-raw/ref_scores.rds")
usethis::use_data(ref_scores, overwrite = TRUE)

# Test set
test <- rbind(csafe$test, cvl$test)
saveRDS(test, "data-raw/test.rds")
usethis::use_data(test, overwrite = TRUE)

plot_scores(scores = ref_scores)
