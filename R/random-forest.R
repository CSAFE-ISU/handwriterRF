get_score <- function(rf, d){
  score <- predict(rf, subset(d, select = -c(docname1, docname2)), type="prob")[,"same"]
  return(score)
}

# plot_votes <- function(votes, type = "train"){
#   p <- votes[[type]] %>%
#     ggplot(aes(x=votes, color=match, fill=match)) +
#     geom_density(alpha = 0.2) +
#     theme_bw()
#   return(p)
# }
