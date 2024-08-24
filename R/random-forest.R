get_votes <- function(rf, sets){
  train_votes <- as.data.frame(rf$votes)['same']
  # add labels from train data frame
  train_votes$match <- sets$train$match
  colnames(train_votes) <- c("votes", "match")

  test_votes <- as.data.frame(predict(rf, sets$test, type="prob"))['same']
  # add labels from test data frame
  test_votes$match <- sets$test$match
  colnames(test_votes) <- c("votes", "match")

  return(list("train" = train_votes, "test" = test_votes))
}

get_scores <- function(rf, sets) {
  votes <- get_votes(rf, sets)

  scores <- list()
  scores$train_same_writer <- votes$train %>% dplyr::filter(match == "same") %>% dplyr::pull(votes)
  scores$train_diff_writer <- votes$train %>% dplyr::filter(match == "different") %>% dplyr::pull(votes)

  scores$test_same_writer <- votes$test %>% dplyr::filter(match == "same") %>% dplyr::pull(votes)
  scores$test_diff_writer <- votes$test %>% dplyr::filter(match == "different") %>% dplyr::pull(votes)

  return(scores)
}

# plot_votes <- function(votes, type = "train"){
#   p <- votes[[type]] %>%
#     ggplot(aes(x=votes, color=match, fill=match)) +
#     geom_density(alpha = 0.2) +
#     theme_bw()
#   return(p)
# }
