devtools::load_all()
library(ggplot2)


add_factor <- function(df) {
  add_factor_database <- function(df) {
    df$database1 <- ifelse(startsWith(df$writer1, "w"), "csafe", "cvl")
    df$database2 <- ifelse(startsWith(df$writer2, "w"), "csafe", "cvl")
    df$factor_database <- paste0(df$database1, "_", df$database2)
    df$factor_database[df$factor_database == "cvl_csafe"] <- "csafe_cvl"
    df$factor_database <- factor(df$factor_database)
    return(df)
  }

  add_factor_language <- function(df) {
    df$language1 <- ifelse(stringr::str_detect(df$docname1, "-6-"), "German", "English")
    df$language2 <- ifelse(stringr::str_detect(df$docname2, "-6-"), "German", "English")
    df$factor_language <- paste0(df$language1, "_", df$language2)
    df$factor_language[df$factor_language == "German_English"] <- "English_German"
    df$factor_language <- factor(df$factor_language)
    return(df)
  }

  add_factor_length <- function(df) {
    df$length1 <- ifelse(stringr::str_detect(df$docname1, "pPHR"), "short", "long")
    df$length2 <- ifelse(stringr::str_detect(df$docname2, "pPHR"), "short", "long")
    df$factor_length <- paste0(df$length1, "_", df$length2)
    df$factor_length[df$factor_length == "short_long"] <- "long_short"
    df$factor_length <- factor(df$factor_length)
    return(df)
  }

  add_factor_prompt_in_train <- function(df) {
    df$prompt_in_train1 <- ifelse(stringr::str_detect(df$docname1, 'pLND|pWOZ|-1-|-2-|-3-|-4-'), "yes", "no")
    df$prompt_in_train2 <- ifelse(stringr::str_detect(df$docname2, 'pLND|pWOZ|-1-|-2-|-3-|-4-'), "yes", "no")
    df$factor_prompt_in_train <- paste0(df$prompt_in_train1, "_", df$prompt_in_train2)
    df$factor_prompt_in_train[df$factor_prompt_in_train == "yes_no"] <- "no_yes"
    df$factor_prompt_in_train <- factor(df$factor_prompt_in_train)
    return(df)
  }

  add_factor_prompt <- function(df) {
    df$prompt1 <- NA
    df$prompt1[stringr::str_detect(df$docname1, 'pLND')] <- "pLND"
    df$prompt1[stringr::str_detect(df$docname1, 'pWOZ')] <- "pWOZ"
    df$prompt1[stringr::str_detect(df$docname1, 'pPHR')] <- "pPHR"
    df$prompt1[stringr::str_detect(df$docname1, '-1-')] <- "1"
    df$prompt1[stringr::str_detect(df$docname1, '-2-')] <- "2"
    df$prompt1[stringr::str_detect(df$docname1, '-3-')] <- "3"
    df$prompt1[stringr::str_detect(df$docname1, '-4-')] <- "4"
    df$prompt1[stringr::str_detect(df$docname1, '-6-')] <- "6"
    df$prompt1[stringr::str_detect(df$docname1, '-7-')] <- "7"
    df$prompt1[stringr::str_detect(df$docname1, '-8-')] <- "8"

    df$prompt2 <- NA
    df$prompt2[stringr::str_detect(df$docname2, 'pLND')] <- "pLND"
    df$prompt2[stringr::str_detect(df$docname2, 'pWOZ')] <- "pWOZ"
    df$prompt2[stringr::str_detect(df$docname2, 'pPHR')] <- "pPHR"
    df$prompt2[stringr::str_detect(df$docname2, '-1-')] <- "1"
    df$prompt2[stringr::str_detect(df$docname2, '-2-')] <- "2"
    df$prompt2[stringr::str_detect(df$docname2, '-3-')] <- "3"
    df$prompt2[stringr::str_detect(df$docname2, '-4-')] <- "4"
    df$prompt2[stringr::str_detect(df$docname2, '-6-')] <- "6"
    df$prompt2[stringr::str_detect(df$docname2, '-7-')] <- "7"
    df$prompt2[stringr::str_detect(df$docname2, '-8-')] <- "8"

    df$factor_prompt <- sapply(1:nrow(slrs), function(i) {
      pair <- sort(c(slrs$prompt1[i], slrs$prompt2[i]))
      return(paste0(pair[1], "_", pair[2]))})

    df$factor_prompt <- factor(df$factor_prompt)

    return(df)
  }

  add_factor_prompt_match <- function(df) {
    df$factor_prompt_match <- factor(ifelse(df$prompt1 == df$prompt2, "yes", "no"))
    return(df)
  }

  # Add Factors for a pair of prompts:
  # 1. Database: same database csafe, same database cvl, different databases
  # 2. Language: same language English, same language German (no same writer pairs), different languages
  # 3. Length: both long, both short, one long and one short
  # 4. Prompts used in training: both yes, both no, one yes and one no

  df <- add_factor_database(df)
  df <- add_factor_language(df)
  df <- add_factor_length(df)
  df <- add_factor_prompt_in_train(df)
  df <- add_factor_prompt(df)
  df <- add_factor_prompt_match(df)

  df <- df %>% dplyr::select(tidyselect::any_of(c("docname1", "writer1", "database1", "language1", "length1", "prompt_in_train1", "prompt1",
                                                  "docname2", "writer2", "database2", "language2", "length2", "prompt_in_train2", "prompt2",
                                                  "factor_database", "factor_language", "factor_length", "factor_prompt_in_train", "factor_prompt", "factor_prompt_match",
                                                  "ground_truth", "score", "slr")))

  return(df)
}

add_prediction_label <- function(df) {
  df$prediction_label <- ifelse(df$slr > 1, "same writer", "different writer")
  return(df)
}

get_known_matches <- function(df) {
  df <- df %>%
    dplyr::filter(ground_truth == "same writer")
  return(df)
}

get_known_nonmatches <- function(df) {
  df <- df %>%
    dplyr::filter(ground_truth == "different writer")
  return(df)
}

get_errors <- function(df, type = "slr") {
  if (type == "slr") {
    errors <- list()

    KM_df <- get_known_matches(df)
    KNM_df <- get_known_nonmatches(df)
    errors$KM <- nrow(km_df)
    errors$KNM <- nrow(knm_df)

    errors$FN_df <- KM_df %>%
      dplyr::filter(slr < 1)
    errors$FP_df <- KNM_df %>%
      dplyr::filter(slr > 1)
    errors$FN <- nrow(errors$FN_df)
    errors$FP <- nrow(errors$FP_df)

    errors$FNR <- errors$FN / errors$KM
    errors$FPR <- errors$FP / errors$KNM

    errors$error <- (errors$FN + errors$FP) / (errors$KM + errors$KNM)

  }
  return(errors)
}

errors_by_factor <- function(slrs_df, factor_name = "database", pivot = "longer") {
  dfs <- slrs_df %>%
    dplyr::group_by(across(paste0("factor_", factor_name))) %>%
    dplyr::group_map(~ get_errors(.x), .keep = TRUE)
  names(dfs) <- levels(slrs_df[,paste0("factor_", factor_name)])

  df <- data.frame(factors = names(dfs),
                   "KM" = sapply(dfs, function(x) x$KM),
                   "KNM" = sapply(dfs, function(x) x$KNM),
                   "FN" = sapply(dfs, function(x) x$FN),
                   "FP" = sapply(dfs, function(x) x$FP),
                   "FNR" = sapply(dfs, function(x) round(x$FNR, 4)),
                   "FPR" = sapply(dfs, function(x) round(x$FPR, 4)))

  if (!is.null(pivot) && pivot == "wider") {
    df <- df %>% tidyr::separate_wider_delim(tidyselect::all_of(c("factors")),
                                             delim = "_",
                                             names = c(paste0(factor_name, "1"), paste0(factor_name, "2")),
                                             cols_remove = FALSE)
  } else if (!is.null(pivot) && pivot == "longer") {
    df <- df %>%
      tidyr::pivot_longer(cols = FPR:FNR, names_to = "type", values_to = "rate") %>%
      dplyr::mutate(type = factor(type, levels = c("FNR", "FPR")))
  }

  return(df)
}

plot_fp_total_obs <- function(df) {
  p <- df %>%
    dplyr::filter(type == "FPR") %>%
    ggplot(aes(x = factors, y = round(100*rate,2), fill = type)) +
    geom_col(position = "dodge", fill = "#C8102E") +
    coord_flip() +
    geom_text(aes(label = paste0(scales::percent(rate), " (", scales::comma(FP), " out of ", scales::comma(KNM), " KNM pairs.)")),
              hjust = 0,
              position = position_nudge(y = 5),
              colour = "black") +
    labs(fill = "Error", y = "Rate") +
    ylim(0,100) +
    theme_bw() +
    theme(axis.title.y = element_blank(), legend.position="none")
  return(p)
}

plot_fn_total_obs <- function(df) {
  p <- df %>%
    dplyr::filter(type == "FNR") %>%
    ggplot(aes(x = factors, y = round(100*rate,2), fill = type)) +
    geom_col(position = "dodge", fill = "#00843D") +
    coord_flip() +
    geom_text(aes(label = paste0(scales::percent(rate), " (", scales::comma(FN), " out of ", scales::comma(KM), " KM pairs.)")),
              hjust = 0,
              position = position_nudge(y = 5),
              colour = "black") +
    geom_text(aes(label = ifelse(is.na(rate), "No ground truth matches available", ""), y = 0.25),
              hjust = 0,
              col = "black",
              position = position_dodge(.9)) +
    labs(fill = "Error", y = "Rate") +
    ylim(0,100) +
    theme_bw() +
    theme(axis.title.y = element_blank(), legend.position="none")
  return(p)
}



# SLRs for Test -----------------------------------------------------------

slrs <- compare_writer_profiles(test, score_only = FALSE)
slrs <- add_factor(slrs)
saveRDS(slrs, "experiments/package_test_data/slrs_for_test.rds")


# All Samples -------------------------------------------------------------

slrs <- readRDS("experiments/package_test_data/slrs_for_test.rds")
length <- errors_by_factor(slrs, "length")


# Long Samples ------------------------------------------------------------

slrs_long <- slrs %>%
  dplyr::filter(factor_length == "long_long") %>%
  dplyr::mutate(factor_prompt = factor(factor_prompt, levels = unique(factor_prompt)))

db <- errors_by_factor(slrs_long, "database")
language <- errors_by_factor(slrs_long, "language")
prompt_in_train <- errors_by_factor(slrs_long, "prompt_in_train")
prompt <- errors_by_factor(slrs_long, "prompt")
prompt_match <- errors_by_factor(slrs_long, "prompt_match")


# Plots -------------------------------------------------------------------

# False Positive Rates
plot_fp_total_obs(length)
ggsave("experiments/package_test_data/plots/fpr_by_factors/factor_length_fpr_horiz.png", width = 6, height = 3)

plot_fp_total_obs(db)
ggsave("experiments/package_test_data/plots/fpr_by_factors/factor_database_fpr_horiz.png", width = 6, height = 3)

plot_fp_total_obs(language)
ggsave("experiments/package_test_data/plots/fpr_by_factors/factor_language_fpr_horiz.png", width = 6, height = 3)

plot_fp_total_obs(prompt_in_train)
ggsave("experiments/package_test_data/plots/fpr_by_factors/factor_prompt_in_train_fpr_horiz.png", width = 6, height = 3)

plot_fp_total_obs(prompt_match)
ggsave("experiments/package_test_data/plots/fpr_by_factors/factor_prompt_match_fpr_horiz.png", width = 6, height = 3)

# False Negative Rates
plot_fn_total_obs(length)
ggsave("experiments/package_test_data/plots/fnr_by_factors/factor_length_fnr_horiz.png", width = 6, height = 3)

plot_fn_total_obs(db)
ggsave("experiments/package_test_data/plots/fnr_by_factors/factor_database_fnr_horiz.png", width = 6, height = 3)

plot_fn_total_obs(language)
ggsave("experiments/package_test_data/plots/fnr_by_factors/factor_language_fnr_horiz.png", width = 6, height = 3)

plot_fn_total_obs(prompt_in_train)
ggsave("experiments/package_test_data/plots/fnr_by_factors/factor_prompt_in_train_fnr_horiz.png", width = 6, height = 3)

plot_fn_total_obs(prompt_match)
ggsave("experiments/package_test_data/plots/fnr_by_factors/factor_prompt_match_fnr_horiz.png", width = 6, height = 3)
