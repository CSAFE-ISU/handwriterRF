templateK40 <- readRDS(file.path("data-raw", "templateK40.rds"))
# delete graphs to reduce file size
templateK40$template_graphs <- NULL
# delete all wcd except for last iteration to reduce file size
templateK40$wcd <- templateK40$wcd[templateK40$iters,]
usethis::use_data(templateK40, overwrite = TRUE)
