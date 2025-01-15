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

    errors$factor_prompt <- as.character(unique(df$factor_prompt))

    km <- get_known_matches(df)
    knm <- get_known_nonmatches(df)
    errors$num_km <- nrow(km)
    errors$num_knm <- nrow(knm)

    errors$fn <- km %>%
      dplyr::filter(slr < 1)
    errors$fp <- knm %>%
      dplyr::filter(slr > 1)
    errors$num_fn <- nrow(errors$fn)
    errors$num_fp <- nrow(errors$fp)

    errors$FNR <- errors$num_fn / errors$num_km
    errors$FPR <- errors$num_fp / errors$num_knm

    errors$error <- (errors$num_fn + errors$num_fp) / (errors$num_km + errors$num_knm)

  }
  return(errors)
}

errors_by_factor <- function(slrs_df, factor_name = "database", pivot = "longer") {
  dfs <- slrs_df %>%
    dplyr::group_by(across(paste0("factor_", factor_name))) %>%
    dplyr::group_map(~ get_errors(.x), .keep = TRUE)
  names(dfs) <- levels(slrs_df[,paste0("factor_", factor_name)])

  df <- data.frame(factors = names(dfs),
                   "num_KM" = sapply(dfs, function(x) x$num_km),
                   "num_KNM" = sapply(dfs, function(x) x$num_knm),
                   "num_FN" = sapply(dfs, function(x) x$num_fn),
                   "num_FP" = sapply(dfs, function(x) x$num_fp),
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

plot_fpr_fnr <- function(df) {
  p <- df %>% ggplot(aes(x = factors, y = rate, fill = type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = rate),
              vjust = -1,
              colour = "black",
              position = position_dodge(.9)) +
    geom_text(aes(label = ifelse(is.na(rate), "No data", ""), y = 0.25),
              vjust = -1,
              col = "black",
              position = position_dodge(.9)) +
    labs(fill = "Error", y = "Rate") +
    ylim(0,100) +
    theme_bw() +
    theme(axis.title.x = element_blank())
  return(p)
}

plot_fp_total_obs <- function(df) {
  p <- df %>%
    dplyr::filter(type == "FPR") %>%
    ggplot(aes(x = factors, y = round(100*rate,2), fill = type)) +
    geom_col(position = "dodge", fill = "#C8102E") +
    coord_flip() +
    geom_text(aes(label = paste0(scales::percent(rate), " (", scales::comma(num_FP), " out of ", scales::comma(num_KNM), " KNM pairs.)")),
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
    geom_text(aes(label = paste0(scales::percent(rate), " (", scales::comma(num_FN), " out of ", scales::comma(num_KM), " KM pairs.)")),
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


# SLRS for All Test Sets --------------------------------------------------

# all_tests <- rbind(test$csafe_lnd, test$csafe_phr, test$csafe_woz, test$cvl_1_2_3_4, test$cvl_6, test$cvl_7_8)
# all_tests <- all_tests %>% dplyr::distinct()
# slrs_all_test_sets <- compare_writer_profiles(all_tests, score_only = FALSE)
# slrs_all_test_sets <- add_factor(slrs_all_test_sets)
# slrs_all_test_sets <- add_factor(slrs_all_test_sets)
# slrs_all_test_sets <- add_prediction_label(slrs_all_test_sets)
# saveRDS(slrs_all_test_sets, "experiments/package_test_data/slrs_all_test_sets.rds")


# All Samples -------------------------------------------------------------

slrs <- readRDS("experiments/package_test_data/slrs_all_test_sets.rds")
# slrs <- add_factor(slrs)
# saveRDS(slrs, "experiments/package_test_data/slrs_all_test_sets.rds")
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

plot_fpr_fnr(length)
ggsave("experiments/package_test_data/plots/factor_length.png", width = 6, height = 4)

plot_fpr_fnr(db)
ggsave("experiments/package_test_data/plots/factor_database.png", width = 6, height = 4)

plot_fpr_fnr(language)
ggsave("experiments/package_test_data/plots/factor_language.png", width = 6, height = 4)

plot_fpr_fnr(prompt_in_train)
ggsave("experiments/package_test_data/plots/factor_prompt_in_train.png", width = 6, height = 4)

plot_fpr_fnr(prompt_match)
ggsave("experiments/package_test_data/plots/factor_prompt_match.png", width = 6, height = 4)


# fpr
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

# fnr
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

# # Split FNRs into groups
# FNR_cvl_1234 <- df %>%
#   dplyr::filter(prompt1 %in% c("1", "2", "3", "4"), prompt2 %in% c("1", "2", "3", "4"))
#
# FNR_cvl_678 <- df %>%
#   dplyr::filter(prompt1 %in% c("6", "7", "8"), prompt2 %in% c("6", "7", "8"))
#
# FNR_csafe <- df %>%
#   dplyr::filter(prompt1 %in% c("pLND", "pPHR", "pWOZ"), prompt2 %in% c("pLND", "pPHR", "pWOZ"))
#
# p <- df %>%
#   ggplot2::ggplot(aes(x = prompt2, y = prompt1, fill = FPR)) +
#   geom_tile() +
#   geom_text(aes(label = FPR), size = 3) +
#   scale_fill_gradient2("Probability ", low = "grey90", midpoint = 0, high = "steelblue") +
#   ylab("Prompt 1") +
#   xlab("Prompt 2") +
#   theme_bw() +
#   labs(title = "False Positive Rates")
# ggsave("experiments/package_test_data/plots/FPR_by_prompt_pairs.png", width = 5.25)
#
# FNR_cvl_1234 %>%
#   ggplot2::ggplot(aes(x = prompt2, y = prompt1, fill = FNR)) +
#   geom_tile() +
#   geom_text(aes(label = FNR), size = 3) +
#   scale_fill_gradient2("Probability ", low = "grey90", midpoint = 0, high = "steelblue", na.value = "gray90") +
#   ylab("Prompt 1") +
#   xlab("Prompt 2") +
#   theme_bw() +
#   labs(title = "False Negative Rates")
# ggsave("experiments/package_test_data/plots/FNR_cvl_1234_by_prompt_pairs.png", width = 5.25)
#
# FNR_cvl_678 %>%
#   ggplot2::ggplot(aes(x = prompt2, y = prompt1, fill = FNR)) +
#   geom_tile() +
#   geom_text(aes(label = FNR), size = 3) +
#   scale_fill_gradient2("Probability ", low = "grey90", midpoint = 0, high = "steelblue", na.value = "gray90") +
#   ylab("Prompt 1") +
#   xlab("Prompt 2") +
#   theme_bw() +
#   labs(title = "False Negative Rates")
# ggsave("experiments/package_test_data/plots/FNR_cvl_678_by_prompt_pairs.png", width = 5.25)
#
# FNR_csafe %>%
#   ggplot2::ggplot(aes(x = prompt2, y = prompt1, fill = FNR)) +
#   geom_tile() +
#   geom_text(aes(label = FNR), size = 3) +
#   scale_fill_gradient2("Probability ", low = "grey90", midpoint = 0, high = "steelblue", na.value = "gray90") +
#   ylab("Prompt 1") +
#   xlab("Prompt 2") +
#   theme_bw() +
#   labs(title = "False Negative Rates")
# ggsave("experiments/package_test_data/plots/FNR_csafe_by_prompt_pairs.png", width = 5.25)
