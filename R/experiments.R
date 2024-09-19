# run_experiment <- function(df,
#                            train_prompt_code,
#                            test_prompt_code,
#                            train_n,
#                            dataset_name,
#                            distance_measures,
#                            ntrees,
#                            run_number,
#                            use_random_forest = TRUE,
#                            downsample = TRUE) {
#
#   set.seed(run_number)
#
#   # create output directory if it doesn't already exist
#   outdir <- get_experiment_dir(dataset_name = dataset_name, distances_measures = distance_measures, downsample = downsample)
#
#   # Log ---------------------------------------------------------------------
#   # start new log entry (data frame with 1 row)
#   new_log <- data.frame("dataset" = dataset_name,
#                         "train_prompt" = train_prompt_code,
#                         "test_prompt" = test_prompt_code,
#                         "train_n" = train_n,
#                         "distance_measures" = paste(distance_measures, collapse = " + "),
#                         "use_random_forest" = paste(use_random_forest),
#                         "rf_ntrees" = ntrees,
#                         "run_number" = run_number,
#                         "downsample_train_set" = "TRUE",
#                         "downsample_test_set" = paste(downsample))
#
#
#   # Get Train and Test Sets -------------------------------------------------
#   df <- expand_docnames(df)
#   # train: n writers * 3 docs each = 3n docs
#   # test: m writers * 3 docs each = 3m docs
#   sets <- get_train_test_sets(df, train_prompt_code, test_prompt_code, train_n)
#   new_log$test_n <- length(sets$writers$test)
#
#
#   # Get Distances -----------------------------------------------------------
#   dists <- list()
#   dists$train <- get_distances(df = sets$train, distance_measures = distance_measures)
#   dists$test <- get_distances(df = sets$test, distance_measures = distance_measures)
#
#   if (downsample){
#     dists$train <- downsample_diff_pairs(dists$train)
#     dists$test <- downsample_diff_pairs(dists$test)
#   }
#
#   # Random Forest with Ranger Package ---------------------------------------
#
#   # rf <- ranger::ranger(formula,
#   #                      data = train,
#   #                      importance = 'permutation',
#   #                      scale.permutation.importance = TRUE,
#   #                      num.trees = 200)
#   # train_votes <- get_proportion_of_votes(rf, train)
#   # test_votes <- get_proportion_of_votes(rf, test)
#   #
#   # get_ranger_proportion_of_votes <- function(rf, df){
#   #   pred <- predict(rf, subset(df, select = -match), predict.all=TRUE)
#   #   pred <- as.data.frame(pred$predictions)
#   #   ntrees <- ncol(pred)
#   #   votes <- data.frame(votes = rowSums(pred == 2) / ntrees, match = df$match)
#   #   return(votes)
#   # }
#
#   # Scores ------------------------------------------------------------------
#
#   scores <- get_scores(dists, distance_measures = distance_measures, run_number = run_number, use_random_forest = use_random_forest, ntrees = ntrees, downsample = downsample)
#
#
#   # SLRs --------------------------------------------------------------------
#   slrs <- get_slrs(scores)
#
#
#   # Evaluate ----------------------------------------------------------------
#   # Errors
#   errors <- calculate_error_rates(slrs)
#   new_log$fnr <- errors$fnr
#   new_log$fpr <- errors$fpr
#
#   # AUC
#   roc <- PRROC::roc.curve(scores.class0 = slrs$same_writer, scores.class1 = slrs$diff_writer)
#   new_log$auc <- roc$auc
#
#   # Save --------------------------------------------------------------------
#   # save / update log
#   update_log(new_log, outdir)
#
#   return(list("rf"= rf, "slrs" = slrs, "errors" = errors))
# }
#
# get_experiment_dir <- function(dataset_name, distance_measures, downsample) {
#   if (downsample){
#     outdir <- file.path("experiments", "tests_downsampled", paste0(dataset_name, "_", paste0(distance_measures, collapse="_")))
#   } else {
#     outdir <- file.path("experiments", "tests_not_downsampled", paste0(dataset_name, "_", paste0(distance_measures, collapse="_")))
#   }
#   if (!dir.exists(outdir)){
#     dir.create(outdir)
#   }
#   return(outdir)
# }
#
# get_experiment_log <- function(dataset_name, distance_measures, downsample){
#   experiment_dir <- get_experiment_dir(dataset_name, distance_measures, downsample = TRUE)
#   return(file.path(experiment_dir, "experiments_log.csv"))
# }
#
# update_log <- function(new_log, outdir) {
#
#   # sort log columns
#   new_log <- new_log %>% dplyr::select(run_number, dataset, train_prompt, test_prompt, train_n, test_n, distance_measures,
#                                        use_random_forest, rf_ntrees, downsample_train_set, downsample_test_set, fnr, fpr, auc)
#
#   # add to existing log or start new log
#   if (file.exists(file.path(outdir, "experiments_log.csv"))){
#     exlog <- read.csv(file.path(outdir, "experiments_log.csv"))
#     exlog <- rbind(exlog, new_log)
#   } else {
#     exlog <- new_log
#   }
#   write.csv(exlog, file.path(outdir, "experiments_log.csv"), row.names = FALSE)
#
#   return(exlog)
# }
#
#
# make_auc_grid <- function(log_path){
#   exlog <- read.csv(log_path)
#
#   stats <- exlog %>%
#     dplyr::group_by(train_prompt, test_prompt) %>%
#     dplyr::summarize(mean_auc = mean(auc)) %>%
#     tidyr::pivot_wider(names_from = "test_prompt", values_from = "mean_auc")
#
#   write.csv(stats, file.path(dirname(log_path), "mean_auc_by_test_prompt.csv"), row.names = FALSE)
#
#   return(stats)
# }
#
#
# make_master_table <- function(dataset_name, stat, distance_measures, downsample, test_prompts){
#   make_table <- function(log_path, stat, test_prompts){
#
#     df <- read.csv(log_path)
#
#     if (stat == "error"){
#       df <- df %>%
#         dplyr::mutate(error = 0.5*(fpr + fnr))
#     }
#
#     df <- df %>%
#       dplyr::filter(test_prompt %in% test_prompts) %>%
#       dplyr::group_by(train_prompt, distance_measures, use_random_forest) %>%
#       dplyr::summarize(mean_stat = mean(!!sym(stat)))
#     colnames(df)[colnames(df) == "mean_stat"] <- paste0("mean_", stat)
#
#     write.csv(df, file.path(dirname(log_path), paste0("mean_", stat, paste0(test_prompts, collapse = "_"), "_grouped.csv")), row.names = FALSE)
#
#     return(df)
#   }
#
#   distances <- powerset(x=distance_measures)
#   df <- list()
#   for (i in 1:length(distances)){
#     distance_measures <- distances[[i]]
#     log_path <- get_experiment_log(dataset_name = dataset_name, distance_measures = distance_measures, downsample = downsample)
#     stats <- make_table(log_path, stat, test_prompts = c("pLND", "pCMB", "pWOZ"))
#     df[[i]] <- stats
#   }
#   df <- do.call(rbind, df)
#   return(df)
# }
#
# powerset <- function(x) {
#   sets <- lapply(1:(length(x)), function(i) combn(x, i, simplify = F))
#   unlist(sets, recursive = F)
# }
#

# get_scores <- function(dists, distance_measures, run_number, use_random_forest, ntrees, downsample, type, output_dir) {
#   if (use_random_forest){
#     rf <- randomForest::randomForest(match ~ ., data = subset(dists, select = -c(docname1, docname2)), ntree = ntrees)
#     scores_df <- get_votes(rf = rf, dists = dists, type = type)
#
#     # save random forest
#     saveRDS(rf, file.path(output_dir, paste0("rf_", run_number, ".rds")))
#   } else {
#     # if (ncol(dists$train) != 4){
#     #   warning("More than one distance measure requires a random forest. Using a random forest.")
#     # }
#     #
#     # scores <- list()
#     # scores$train <- dists$train %>% dplyr::select(-docname1, -docname2)
#     # colnames(scores$train) <- c("score", "match")
#     #
#     # scores$test <- dists$train %>% dplyr::select(-docname1, -docname2)
#     # colnames(scores$test) <- c("score", "match")
#   }
#
#   # split the train and test sets into same and different writers to make it
#   # easier on the next step
#   scores <- list()
#   scores$same_writer <- scores_df %>% dplyr::filter(match == "same") %>% dplyr::pull(score)
#   scores$diff_writer <- scores_df %>% dplyr::filter(match == "different") %>% dplyr::pull(score)
#
#   return(scores)
# }

# calculate_error_rates <- function(slrs) {
#   error_rates <- list()
#   error_rates$fnr <- sum(slrs$same_writer < 1) / length(slrs$same_writer)
#   error_rates$fpr <- sum(slrs$diff_writer > 1) / length(slrs$diff_writer)
#   return(error_rates)
# }

# get_densities <- function(scores){
#   pdfs <- list()
#   pdfs$same_writer <- density(scores$same_writer, kernel = "gaussian", n=10000)
#   pdfs$diff_writer <- density(scores$diff_writer, kernel = "gaussian", n=10000)
#   return(pdfs)
# }

# get_slrs <- function(scores, ){
#   browser()
#   pdfs <- get_densities(scores)
#
#   test_same_writer_evals <- eval_density_at_point(density = pdfs, x = scores$test_same_writer)
#   test_same_writer_evals <- correct_NAs(evals = test_same_writer_evals, zero_correction = zero_correction)
#   test_same_writer_evals <- correct_zeros(evals = test_same_writer_evals, zero_correction = zero_correction)
#
#   test_diff_writer_evals <- eval_density_at_point(density = pdfs, x = scores$test_diff_writer)
#   test_diff_writer_evals <- correct_NAs(evals = test_diff_writer_evals, zero_correction = zero_correction)
#   test_diff_writer_evals <- correct_zeros(evals = test_diff_writer_evals, zero_correction = zero_correction)
#
#   slrs <- list()
#   slrs$same_writer <- test_same_writer_evals$numerators / test_same_writer_evals$denominators
#   slrs$diff_writer <- test_diff_writer_evals$numerators / test_diff_writer_evals$denominators
#
#   return(slrs)
# }

#' #' Get Training and Testing Sets
#' #'
#' #' Create training and testing sets from a data frame of distances created with
#' #' 'get_distances'.
#' #'
#' #' @param dists A data frame of distances created with 'get_distances'
#' #' @param train_n The number of writers to assign to the training set
#' #' @param downsample True or False. True downsamples the different writer
#' #'   distances in the training set to equal the number of same writer distances.
#' #'   The test set is not changed.
#' #'
#' #' @return A list of data frames
#' #' @noRd
#' get_train_test_sets <- function(df, train_prompt_code, test_prompt_code, train_n) {
#'   writers <- get_train_test_writers(df = df, train_n = train_n)
#'
#'   # build train set
#'   train <- df %>%
#'     dplyr::filter(writer %in% writers$train, prompt == train_prompt_code) %>%
#'     dplyr::select(-writer, -session, -prompt, -rep, -total_graphs)
#'
#'   # build test set
#'   test <- df %>%
#'     dplyr::filter(writer %in% writers$test, prompt == test_prompt_code) %>%
#'     dplyr::select(-writer, -session, -prompt, -rep, -total_graphs)
#'
#'   return(list("train" = train, "test" = test, "writers" = writers))
#' }

#' #' Get Writers for Train and Test Sets
#' #'
#' #' Randomly assign writers to train and test sets.
#' #'
#' #' @param dists A data frame of distances created with `get_distances`
#' #' @param train_n The number of writers in 'dists' to assign to to the train set. The
#' #'   remaining writers are assigned to the test set.
#' #'
#' #' @return A list
#' #' @noRd
#' get_train_test_writers <- function(df, train_n) {
#'   writers <- unique(df$writer)
#'   train <- sample(writers, train_n)
#'   test <- setdiff(writers, train)
#'   return(list("train" = train, "test" = test))
#' }
