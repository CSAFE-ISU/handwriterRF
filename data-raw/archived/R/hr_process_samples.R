#------------------------------------------------------------------------------
# title: "Random Forests for Handwriter ntrees"
# author: "Maureen Gallagher"
# purpose: To find the optimal number of trees for a random forest
#------------------------------------------------------------------------------

library(handwriter)
library(dplyr)

# Template used: template_k40_3source_100docs.rds
template <- readRDS("template.rds")


# process_batch_dir(input_dir = 'cvl_docs_for_hr',
#                  output_dir = 'cvl_graphs',
#                  )

# get_clusters_batch(template,
#                   "iam_graphs2",
#                   'iam_clusters2',
#                   c(1,7),
#                   c(8,8),
#                   num_cores = 4)

# get_cluster_fill_counts

# cl <- readRDS("cvl_clusters/0001-1-cropped.rds")
# show(cl)

files <- list.files("cvl_clusters", full.names = FALSE)
files2 <- list.files("cvl_graphs", full.names = FALSE)
files <- gsub(".{4}$", "", files)
files2 <- gsub(".{13}$", "", files2)
setdiff(files2, files)


files <- list.files("cvl_clusters", full.names = TRUE)
clusters <- lapply(files, readRDS)
clusters <- do.call(rbind, clusters)
clusters <- clusters[, c("docname", "writer", "doc", "cluster")]

cluster_fill_counts <- get_cluster_fill_counts(clusters)


cluster_fill_counts <- cluster_fill_counts %>%
  mutate(total = rowSums(across(1:40))) %>%
  mutate(across(1:40, ~ . / total))


cluster_fill_counts <- cluster_fill_counts[, c(1, 4:43, 44, 2, 3)]

save(cluster_fill_counts, file = "iam_cluster_fill_counts.RData")
