sample_and_copy_docs <- function(main_dir, n){
  # sample n writers
  writers <- list.files(main_dir)
  set.seed(100)
  writers <- sort(sample(writers, size=n))

  # get docs
  docs <- lapply(writers, function(x) data.frame(doc = list.files(file.path(main_dir, x), full.names = TRUE)))
  docs <- do.call(rbind, docs)

  # delete "Thumbs.db" files
  docs <- docs %>% filter(basename(doc) != "Thumbs.db")

  # copy to data-raw folder
  dir.create("data-raw/CSAFE_handwriting_db/docs", showWarnings = FALSE, recursive = TRUE)
  file.copy(docs$doc, file.path("data-raw/CSAFE_handwriting_db/docs", basename(docs$doc)))
}

sort_by_prompt <- function(path = "data-raw/CSAFE_handwriting_db/docs"){
  files <- list.files(path, pattern = ".png|.rds")
  prompts <- c("LND", "PHR", "WOZ")
  for (prompt in prompts){
    temp <- grep(prompt, files, value=TRUE)
    dir.create(file.path(path, prompt), showWarnings = FALSE, recursive = TRUE)
    file.rename(file.path(path, temp), file.path(path, prompt, temp))
  }
}

sort_by_session <- function(path){
  files <- list.files(path, pattern = ".png|.rds")
  sessions <- c("s01", "s02", "s03")
  for (session in sessions){
    temp <- grep(session, files, value=TRUE)
    dir.create(file.path(path, session), showWarnings = FALSE, recursive = TRUE)
    file.rename(file.path(path, temp), file.path(path, session, temp))
  }
}

expand_docnames <- function(df){
  df <- df %>% tidyr::separate_wider_delim(docname,
                                           delim = "_",
                                           names = c("writer", "session", "prompt", "rep"),
                                           cols_remove = FALSE)
  return(df)
}

get_combined_cfc <- function(all_clusters){
  # get cluster fill counts
  df <- handwriter::get_cluster_fill_counts(all_clusters)

  # get combined doc cluster fill counts
  df <- df %>% ungroup() %>% select(-writer, -doc)
  df <- expand_docnames(df)
  CMB <- df %>%
    group_by(writer, rep) %>%
    summarise(across(where(is.numeric), list(sum = sum)))
  colnames(CMB) <- c("writer", "rep", seq(1,40))
  CMB$session <- "s01"
  CMB$prompt <- "pCMB"
  CMB <- CMB %>% dplyr::mutate(docname = paste(writer, session, prompt, rep, sep = "_"))

  # add to master df
  df <- rbind(df, CMB)

  return(df)
}

get_cluster_fill_rates <- function(cfc){
  # drop label columns and calculate cluster fill rates: each row sums to 1.
  cfc_clusters_only <- as.matrix(cfc[-seq(1,5)])
  total_graphs <- rowSums(cfc_clusters_only)
  cfr <- diag(1/total_graphs) %*% cfc_clusters_only

  # add "cluster" to column names
  colnames(cfr) <- paste0("cluster", colnames(cfr))

  # check all rows sum to 1 (within machine precision)
  if (!all.equal(rep(1, nrow(cfr)), rowSums(cfr), tolerance = sqrt(.Machine$double.eps))){
    stop("One or more rows does not sum to 1 (within machine precision).")
  }

  # add label columns and total_graphs column
  cfr <- cbind(cfc[seq(1,5)], data.frame(total_graphs = total_graphs), cfr)

  return(cfr)
}


# Docs --------------------------------------------------------------------

# sample_and_copy_docs(main_dir = "/Users/stephanie/Documents/handwriting_datasets CSAFE_Handwriting_Database", n = 100)
# sort_by_prompt(path = "data-raw/CSAFE_handwriting_db/docs")
# sort_by_session(path = "data-raw/CSAFE_handwriting_db/docs/LND")
# sort_by_session(path = "data-raw/CSAFE_handwriting_db/docs/PHR")
# sort_by_session(path = "data-raw/CSAFE_handwriting_db/docs/WOZ")


# Graphs ------------------------------------------------------------------

handwriter::process_batch_dir(input_dir = "data-raw/CSAFE_handwriting_db/docs/PHR/s01",
                              output_dir = "data-raw/CSAFE_handwriting_db/graphs/PHR/s01")


# Clusters ----------------------------------------------------------------
template <- readRDS("data-raw/template.rds")
handwriter::get_clusters_batch(template = template,
                               input_dir = "data-raw/CSAFE_handwriting_db/graphs/PHR/s01",
                               output_dir = "data-raw/CSAFE_handwriting_db/clusters/PHR/s01",
                               writer_indices = c(2, 5),
                               doc_indices = c(7, 18),
                               num_cores = 4,
                               save_master_file = TRUE)


# Cluster Fill Rates ------------------------------------------------------

# load clusters
LND <- readRDS("data-raw/CSAFE_handwriting_db/clusters/LND/s01/all_clusters.rds")
WOZ <- readRDS("data-raw/CSAFE_handwriting_db/clusters/WOZ/s01/all_clusters.rds")
PHR <- readRDS("data-raw/CSAFE_handwriting_db/clusters/PHR/s01/all_clusters.rds")
all_clusters <- rbind(LND, WOZ, PHR)

cfc <- get_combined_cfc(all_clusters)

cfr <- get_cluster_fill_rates(cfc = cfc)

usethis::use_data(cfr, overwrite = TRUE)

