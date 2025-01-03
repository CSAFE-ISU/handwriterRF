# Experiments with handwriting samples from real-world writer rw001

devtools::load_all()
library(handwriter)

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
  profiles <- handwriter::get_writer_profiles(input_dir = file.path(project_dir, "docs"),
                                              num_cores = 4,
                                              measure = "rates",
                                              output_dir = project_dir)
  profiles$writer <- writer
  profiles$doc <- seq(1:nrow(profiles))

  results <- compare_writer_profiles(
    rbind(profiles, test_df),
    score_only = FALSE
  )

  # filter for comparisons that use one or more documents from the real-world writer
  results <- results %>%
    dplyr::filter(writer1 == writer | writer2 == writer)

  fpr <- false_positive_rate(df = results)
  fnr <- false_negative_rate(df = results)

  return(list("fpr" = fpr, "fnr" = fnr, "results" = results))

}

project_dir <- "/Users/stephanie/Documents/handwriting_datasets/real_world_datasets/rw001/300dpi"
writer <- "rw001"
test_df <- test
test_df <- test_df %>%
  dplyr::group_by(writer) %>%
  dplyr::mutate(doc = dplyr::row_number())
errors <- calculate_errors(project_dir = project_dir, writer = writer, test_df = test_df)

results <- errors$results
results <- results |> dplyr::filter(writer1 == "rw001", writer2 == "rw001")
docname <- c("rw001_1", "rw001_2", "rw001_3", "rw001_3_grayscale", "rw001_4", "rw001_5")
doc <- c("doc1_short", "doc2_short", "doc3_short_blue_ink", "doc3_short_grayscale", "doc4_long", "doc5_long")
lookup <- data.frame(docname, doc)
results <- results |> dplyr::left_join(lookup, by = dplyr::join_by(docname1 == docname), suffix = c("1","2"))
results <- results |> dplyr::left_join(lookup, by = dplyr::join_by(docname2 == docname), suffix = c("1","2"))
results <- results |> dplyr::select(doc1, doc2, slr)
results <- results |> dplyr::mutate(log_slr = log10(slr))

results_no_blue_ink <- results |> dplyr::filter(doc1 != "doc3_short_blue_ink", doc2 != "doc3_short_blue_ink")

results |>
  ggplot(aes(x=doc1, y=doc2)) +
  geom_tile(aes(fill=log_slr)) +
  geom_text(aes(label=round(log_slr,2))) +
  scale_fill_gradient2(low = "red", midpoint = 0, high = "steelblue") +
  theme_bw()

results_no_blue_ink |>
  ggplot(aes(x=doc1, y=doc2)) +
  geom_tile(aes(fill=log_slr)) +
  geom_text(aes(label=round(log_slr,2))) +
  scale_fill_gradient2(low = "red", midpoint = 0, high = "steelblue") +
  theme_bw()
