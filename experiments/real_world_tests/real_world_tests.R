devtools::load_all()
library(ggplot2)


false_positive_rate <- function(df) {
  n <- sum(df$ground_truth == "different writer")
  fp <- df %>%
    dplyr::filter(ground_truth == "different writer",
                  slr > 1)
  fpr <- nrow(fp) / n

  message(paste("False positive rate", fpr))

  return(fpr)
}

false_negative_rate <- function(df) {
  n <- sum(df$ground_truth == "same writer")
  fn <- df %>%
    dplyr::filter(ground_truth == "same writer",
                  slr < 1)
  fnr <- nrow(fn) / n

  message(paste("False negative rate", fnr))

  return(fnr)
}

calculate_errors <- function(project_dir, writer, test_df) {
  profiles <- suppressMessages(get_writer_profiles(input_dir = file.path(project_dir, "docs"),
                                                   measure = "rates",
                                                   num_cores = 4,
                                                   output_dir = project_dir))
  profiles$writer <- writer
  profiles$doc <- seq(1:nrow(profiles))

  results <- suppressMessages(compare_writer_profiles(
    rbind(profiles, test_df),
    score_only = FALSE
  ))

  # filter for comparisons that use one or more documents from the real-world writer
  results <- results %>%
    dplyr::filter(writer1 == writer | writer2 == writer)

  fpr <- false_positive_rate(df = results)
  fnr <- false_negative_rate(df = results)

  return(list("fpr" = fpr, "fnr" = fnr, "results" = results))

}

histogram_of_fp_writers <- function(errors) {

  fp <- errors$results %>%
    dplyr::filter(ground_truth == "different writer",
                  slr > 1)
  p <- fp %>% ggplot2::ggplot(ggplot2::aes(x=writer2)) +
    ggplot2::geom_histogram(stat="count") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  return(p)
}


## Baseline with Test Set

# The test data frame included with handwriterRF contains 332 paragraph length
# writing samples from 83 writers. The samples are from the CSAFE Handwriting
# Database and the CVL Handwriting Database. The samples were scanned at 300 DPI
# and saved as PNG files. I also created 72 DPI versions of these samples. The
# table below shows the number of words in each writing prompt.

database <- c(rep("csafe", 2), rep("cvl", 7))
prompt <- c("London Letter", "Wizard of Oz", "1", "2", "3", "4", "5", "6", "7")
words <- c(86, 67, 92, 49, 73, 52, 50, 66, 78)
data.frame(database, prompt, words)


### 300 DPI Resolution Version of Test Set

baseline300 <- compare_writer_profiles(
  writer_profiles = test,
  score_only = FALSE)
fpr <- false_positive_rate(df = baseline300)
fnr <- false_negative_rate(df = baseline300)


### 72 DPI Resolution Version of Test Set

# I downsampled the 332 documents in the `test` data frame using Photoshop. Then
# I ran `get_writer_profiles()` and `compare_documents()` on these samples using
# the `random_forest` and `ref_scores` included with handwriterRF. The random
# forest and reference scores were both created from 300 dpi training samples.
# This gives us a baseline for false positive and false negative rates on the
# test set before we look at results on the real-world samples.

csafe_dir <- "~/Documents/handwriting_datasets/CSAFE_Handwriting_Database/72dpi"
csafe72 <- get_writer_profiles(input_dir = file.path(csafe_dir, "docs"),
                               measure = "rates",
                               num_cores = 4,
                               output_dir = csafe_dir)

cvl_dir <- "~/Documents/handwriting_datasets/CVL/72dpi"

cvl72_counts <- get_writer_profiles(input_dir = file.path(cvl_dir, "docs"),
                             measure = "counts",
                             num_cores = 4,
                             output_dir = cvl_dir)
cvl72 <- get_writer_profiles(input_dir = file.path(cvl_dir, "docs"),
                             measure = "rates",
                             num_cores = 4,
                             output_dir = cvl_dir)
test72 <- rbind(csafe72, cvl72)
test72$doc <- rep(1:4, nrow(test72)/4)

baseline72 <- compare_writer_profiles(
  writer_profiles = test72,
  score_only = FALSE)

fpr72 <- false_positive_rate(df = baseline72)
fnr72 <- false_negative_rate(df = baseline72)

## Real-world Samples from Writer 1

### 300 DPI Samples - 2 Black and 1 Blue

project_dir <- "/Users/stephanie/Documents/handwriting_datasets/real_world_datasets/rw001/2black_1blue/300dpi"
writer <- "rw001"
test_df <- test
test_df <- test_df %>%
  dplyr::group_by(writer) %>%
  dplyr::mutate(doc = dplyr::row_number())
errors <- calculate_errors(project_dir = project_dir, writer = writer, test_df = test_df)

histogram_of_fp_writers(errors)


### 300 DPI Samples - 2 Black and 1 Blue Adjusted to Black

project_dir <- "/Users/stephanie/Documents/handwriting_datasets/real_world_datasets/rw001/2black_1blue_adjusted/300dpi"
writer <- "rw001"
test_df <- test
test_df <- test_df %>%
  dplyr::group_by(writer) %>%
  dplyr::mutate(doc = dplyr::row_number())
errors <- calculate_errors(project_dir = project_dir, writer = writer, test_df = test_df)

histogram_of_fp_writers(errors)



## Real-world Samples from Writer 2

# Handwriting samples from this writer are not in the training, validation, or
# test sets used by handwriterRF.

### 300 DPI samples

#### 2 Short Paragraphs and 1 Long Paragraph

# The samples have 36, 40, and 121.

project_dir <- "/Users/stephanie/Documents/handwriting_datasets/real_world_datasets/rw002/short_v_long/300dpi"
writer <- "rw002"
test_df <- test
test_df <- test_df %>%
  dplyr::group_by(writer) %>%
  dplyr::mutate(doc = dplyr::row_number())
errors <- calculate_errors(project_dir = project_dir, writer = writer, test_df = test_df)

histogram_of_fp_writers(errors)


#### 2 Short Paragraphs and 1 Long Paragraph Split into 2 Documents

# The samples have 36, 40, \~60, and \~62

project_dir <- "/Users/stephanie/Documents/handwriting_datasets/real_world_datasets/rw002/short_v_long_in_halves/300dpi"
writer <- "rw002"
test_df <- test
test_df <- test_df %>%
  dplyr::group_by(writer) %>%
  dplyr::mutate(doc = dplyr::row_number())
errors <- calculate_errors(project_dir = project_dir, writer = writer, test_df = test_df)

histogram_of_fp_writers(errors)

#### 2 Short Paragraphs and 1 Long Paragraph Split into 2 Documents and Cross-Out Removed

# The samples have 36, 40, \~60, and \~62

project_dir <- "/Users/stephanie/Documents/handwriting_datasets/real_world_datasets/rw002/short_v_long_in_halves_no_xout/300dpi"
writer <- "rw002"
test_df <- test
test_df <- test_df %>%
  dplyr::group_by(writer) %>%
  dplyr::mutate(doc = dplyr::row_number())
errors <- calculate_errors(project_dir = project_dir, writer = writer, test_df = test_df)

histogram_of_fp_writers(errors)


## Real-world Samples from Writer 3

### 72 DPI samples

# Writer 3 wrote three paragraph-length handwriting samples on white, unlined
# paper. The samples were photographed with an iPhone 13 Pro, saved to a Mac
# computer, and converted from JPEG to PNG file format. The resolution of the
# files is 72 DPI. Handwriting samples from this writer are not in the training,
# validation, or test sets used by handwriterRF.

project_dir <- "/Users/stephanie/Documents/handwriting_datasets/real_world_datasets/rw003"
writer <- "rw003"
errors <- calculate_errors(project_dir = project_dir, writer = writer, test_df = test72)
