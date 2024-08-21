# save Mattie's data in the data folder
cfr <- readRDS("data-raw/RData/d1.Rdata")
usethis::use_data(cfr, overwrite = TRUE)
