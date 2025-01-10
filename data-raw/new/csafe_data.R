devtools::load_all()

docs_dir <- "/Users/stephanie/Documents/handwriting_datasets/CSAFE_handwriting_database/docs"

graphs_dir <- "/Users/stephanie/Documents/handwriting_datasets/CSAFE_handwriting_database/graphs"

clusters_dir <- "/Users/stephanie/Documents/handwriting_datasets/CSAFE_handwriting_database/clusters"

writers <- list.files(docs_dir)

# Make master file for CSAFE writer profiles
dfs <- lapply(writers, function(w) readRDS(file.path(clusters_dir, w, "all_clusters.rds")))
all_clusters <- do.call(rbind, dfs)
profiles <- handwriter::get_cluster_fill_rates(all_clusters)
saveRDS(profiles, "data-raw/new/csafe_writer_profiles.rds")



# # Get clusters ----
# for (writer in writers) {
#   clusters <- handwriter::get_clusters_batch(
#     template = template,
#     input_dir = file.path(graphs_dir, writer),
#     output_dir = file.path(clusters_dir, writer),
#     writer_indices = c(1,5),
#     doc_indices = c(7, 18),
#     num_cores = 4,
#     save_master_file = TRUE)
#   print(paste("Checked writer", writer))
# }

# # Find cluster folders with too many files
# cluster_writers <- lapply(file.path(clusters_dir, writers), function(f) {list.files(f, full.names = TRUE)})
# too_many <- sapply(cluster_writers, function(w) length(w) != 28)
# too_many <- cluster_writers[too_many]
# wrong_format <- lapply(too_many, function(filepath) as.data.frame(filepath))
# wrong_format <- do.call(rbind, wrong_format)
# wrong_format$docname <- basename(wrong_format$filepath)
#
# # Fix graphs ----
# fix_graphs <- wrong_format %>% dplyr::filter(docname != "all_clusters.rds")
# fix_graphs <- fix_graphs %>% dplyr::filter(stringr::str_detect(docname, "r[123]"))
# # Get current graph filepath
# fix_graphs <- fix_graphs %>%
#   dplyr::mutate(filepath = stringr::str_replace_all(filepath, "r([123])", "r0\\1"),
#                 filepath = stringr::str_replace(filepath, ".rds", "_proclist.rds"),
#                 filepath = stringr::str_replace(filepath, "clusters", "graphs"))
# # Get corrected docname
# fix_graphs <- fix_graphs %>%
#   dplyr::mutate(docname = stringr::str_replace(docname, "r([123])", "r0\\1"),
#                 docname = stringr::str_replace(docname, ".rds", ""))
#
# for (i in 1:nrow(fix_graphs)){
#   fix_file <- fix_graphs$filepath[i]
#   fix <- readRDS(fix_file)
#   fix$docname <- fix_graphs$docname[i]
#   saveRDS(fix, fix_file)
# }
#
# # Delete cluster files ----
# delete_clusters <- wrong_format %>% dplyr::filter(docname == "all_clusters.rds" | stringr::str_detect(docname, "r[123]"))
# file.remove(delete_clusters$filepath)
