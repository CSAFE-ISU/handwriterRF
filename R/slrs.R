get_densities <- function(scores){
  pdfs <- list()
  pdfs$same_writer <- density(scores$train_same_writer, kernel = "gaussian", n=10000)
  pdfs$diff_writer <- density(scores$train_diff_writer, kernel = "gaussian", n=10000)
  return(pdfs)
}

get_slrs <- function(scores){
  pdfs <- get_densities(scores)

  slrs <- list()

  # slrs for same writer scores
  numerators_same_writer <- approx(pdfs$same_writer$x, pdfs$same_writer$y, xout = scores$test_same_writer, n=10000)$y
  denominators_same_writer <- approx(pdfs$diff_writer$x, pdfs$diff_writer$y, xout = scores$test_same_writer, n=10000)$y
  # replace NA's with 0 in numers
  numerators_same_writer[is.na(numerators_same_writer)] <- 0
  # replace NA's with Inf in denoms
  denominators_same_writer[is.na(denominators_same_writer)] <- 0.01
  slrs$same_writer <- numerators_same_writer / denominators_same_writer

  # slrs for different writer scores
  numerators_diff_writer <- approx(pdfs$same_writer$x, pdfs$same_writer$y, xout = scores$test_diff_writer, n=10000)$y
  denominators_diff_writer <- approx(pdfs$diff_writer$x, pdfs$diff_writer$y, xout = scores$test_diff_writer, n=10000)$y
  # replace NA's with 0 in numers
  numerators_diff_writer[is.na(numerators_diff_writer)] <- 0
  # replace NA's with Inf in denoms
  denominators_diff_writer[is.na(denominators_diff_writer)] <- 0.01
  slrs$diff_writer <- numerators_diff_writer / denominators_diff_writer

  return(slrs)
}

calculate_error_rates <- function(slrs) {
  error_rates <- list()
  error_rates$fnr <- sum(slrs$same_writer < 1) / length(slrs$same_writer)
  error_rates$fpr <- sum(slrs$diff_writer > 1) / length(slrs$diff_writer)
  return(error_rates)
}
