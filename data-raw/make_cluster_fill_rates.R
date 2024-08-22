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

get_cluster_fill_rates <- function(all_clusters_path){
  clusters <- readRDS(all_clusters_path)
  cfc <- handwriter::get_cluster_fill_counts(clusters)

  # drop label columns and calculate cluster fill rates: each row sums to 1.
  cfc_clusters_only <- as.matrix(cfc[-c(1,2,3)])
  total_graphs <- rowSums(cfc_clusters_only)
  cfr <- diag(1/total_graphs) %*% cfc_clusters_only

  # add "cluster" to column names
  colnames(cfr) <- paste0("cluster", colnames(cfr))

  # add label columns and total_graphs column
  cfr <- cbind(cfc[c(1,2,3)], data.frame(total_graphs = total_graphs), cfr)

  return(cfr)
}


# Docs --------------------------------------------------------------------

# sample_and_copy_docs(main_dir = "/Users/stephanie/Documents/handwriting_datasets CSAFE_Handwriting_Database", n = 100)
# sort_by_prompt(path = "data-raw/CSAFE_handwriting_db/docs")
# sort_by_session(path = "data-raw/CSAFE_handwriting_db/docs/LND")
# sort_by_session(path = "data-raw/CSAFE_handwriting_db/docs/PHR")
# sort_by_session(path = "data-raw/CSAFE_handwriting_db/docs/WOZ")


# Graphs ------------------------------------------------------------------

# handwriter::process_batch_dir(input_dir = "data-raw/CSAFE_handwriting_db/docs/LND/s01",
#                               output_dir = "data-raw/CSAFE_handwriting_db/graphs/LND/s01")


# Clusters ----------------------------------------------------------------
template <- readRDS("data-raw/template.rds")
# handwriter::get_clusters_batch(template = template,
#                                input_dir = "data-raw/CSAFE_handwriting_db/graphs/LND/s01",
#                                output_dir = "data-raw/CSAFE_handwriting_db/clusters/LND/s01",
#                                writer_indices = c(2, 5),
#                                doc_indices = c(7, 18),
#                                num_cores = 4)

all_clusters_path <- "data-raw/CSAFE_handwriting_db/clusters/LND/s01/all_clusters.rds"

cfr <- get_cluster_fill_rates(all_clusters_path = all_clusters_path)

usethis::use_data(cfr)

