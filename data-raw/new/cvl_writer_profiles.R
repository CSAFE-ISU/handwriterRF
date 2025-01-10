devtools::load_all()

# Pages cropped ----

graphs_dir <- "/Users/stephanie/Documents/handwriting_datasets/CVL/pages_cropped/graphs"
clusters_dir <- "/Users/stephanie/Documents/handwriting_datasets/CVL/pages_cropped/clusters"
writers <- list.files(graphs_dir)

# Make all_clusters.rds for each writer
for (writer in writers) {
  clusters <- handwriter::get_clusters_batch(
    template = templateK40,
    input_dir = file.path(graphs_dir, writer),
    output_dir = file.path(clusters_dir, writer),
    writer_indices = c(1,4),
    doc_indices = c(6, 6),
    num_cores = 4,
    save_master_file = TRUE)
  print(paste("Checked writer", writer))
}

# Make dataframe of all writer profiles
clusters_dfs <- lapply(writers, function(writer) readRDS(file.path(clusters_dir, writer, "all_clusters.rds")))
clusters_df <- do.call(rbind, clusters_dfs)
profiles <- handwriter::get_cluster_fill_rates(clusters_df)
saveRDS(profiles, "data-raw/new/cvl_writer_profiles.rds")

