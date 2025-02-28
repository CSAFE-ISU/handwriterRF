devtools::load_all()

load_cluster_assignments <- function(main_dir) {
  writers <- list.files(main_dir)
  dfs <- list()
  for (i in 1:length(writers)) {
    writer <- writers[i]
    files <- list.files(file.path(main_dir, writer), pattern = ".rds", full.names = TRUE)
    dfs[[i]] <-  files %>%
      purrr::map_dfr(readRDS)
  }
  clusters <- do.call(rbind, dfs)
  return(clusters)
}

# CSAFE ----
# Get writer profiles from all CSAFE docs
csafe_dir <- "/Users/stephanie/Documents/non_version_control/handwriting_datasets/CSAFE_Handwriting_Database/clusters"
csafe_writers <- unique(train$writer[startsWith(train$writer, "w")])
csafe_all <- load_cluster_assignments(csafe_dir)
csafe_all <- handwriter::get_cluster_fill_rates(csafe_all)
csafe_all <- expand_docnames(df = csafe_all)

# Randomly select two common phrases from each CSAFE writer from session 1
set.seed(100)
csafe_short <- csafe_all %>%
  dplyr::filter(writer %in% csafe_writers,
                prompt == "pPHR",
                session == "s01") %>%
  dplyr::group_by(writer) %>%
  dplyr::slice_sample(n=2)

# Randomly select one WOZ and one LND from each CSAFE writer from session 1
csafe_long <- csafe_all %>%
  dplyr::filter(writer %in% csafe_writers,
                prompt != "pPHR",
                session == "s01") %>%
  dplyr::group_by(writer, prompt) %>%
  dplyr::slice_sample(n=1)

