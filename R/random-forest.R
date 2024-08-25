get_votes <- function(rf, dists){
  train_votes <- as.data.frame(rf$votes)['same']
  # add labels from train data frame
  train_votes$match <- dists$train$match
  colnames(train_votes) <- c("votes", "match")

  test_votes <- as.data.frame(predict(rf, subset(dists$test, select = -c(docname1, docname2)), type="prob"))['same']
  # add labels from test data frame
  test_votes$match <- dists$test$match
  colnames(test_votes) <- c("votes", "match")

  return(list("train" = train_votes, "test" = test_votes))
}

get_scores <- function(rf, dists) {
  votes <- get_votes(rf, dists)

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
