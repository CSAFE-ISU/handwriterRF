main_dir <- "/Users/stephanie/Documents/handwriting_datasets/CSAFE_Handwriting_Database"

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

sort_by_prompt <- function(path ="data-raw/CSAFE_handwriting_db/docs"){
  files <- list.files(path, pattern = ".png")
  prompts <- c("LND", "PHR", "WOZ")
  for (prompt in prompts){
    temp <- grep(prompt, files, value=TRUE)
    dir.create(file.path(path, prompt), showWarnings = FALSE, recursive = TRUE)
    file.rename(file.path(path, temp), file.path(path, prompt, temp))
  }
}

# sample_and_copy_docs(main_dir, 100)
# sort_by_prompt()

handwriter::process_batch_dir(input_dir = "data-raw/CSAFE_handwriting_db/docs/LND",
                              output_dir = "data-raw/CSAFE_handwriting_db/graphs/LND")

