# create temp folder

graphs_dir <- file.path(tempdir(), "make_clusters", "graphs")
clusters_dir <- file.path(tempdir(), "make_clusters", "clusters")
create_dir(graphs_dir)
create_dir(clusters_dir)

# get graphs
handwriter::process_batch_dir(system.file("extdata", package = "handwriterRF"),
                              graphs_dir)

# get clusters
handwriter::get_clusters_batch(template = templateK40,
                               input_dir = graphs_dir,
                               output_dir = clusters_dir,
                               writer_indices = c(2, 5),
                               doc_indices = c(7, 18))

# save
w0030_s01_pWOZ_r01_clusters <- readRDS(file.path(clusters_dir, "w0030_s01_pWOZ_r01.rds"))
w0030_s01_pWOZ_r02_clusters <- readRDS(file.path(clusters_dir, "w0030_s01_pWOZ_r02.rds"))
w0238_s01_pWOZ_r02_clusters <- readRDS(file.path(clusters_dir, "w0238_s01_pWOZ_r02.rds"))
w0238_s01_pWOZ_r03_clusters <- readRDS(file.path(clusters_dir, "w0238_s01_pWOZ_r03.rds"))
usethis::use_data(w0030_s01_pWOZ_r01_clusters)
usethis::use_data(w0030_s01_pWOZ_r02_clusters)
usethis::use_data(w0238_s01_pWOZ_r02_clusters)
usethis::use_data(w0238_s01_pWOZ_r03_clusters)
