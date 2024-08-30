get_slrs <- function(scores, zero_correction = 1e-10){

  pdfs <- get_densities(scores)

  test_same_writer_evals <- eval_density_at_point(density = pdfs, x = scores$test_same_writer)
  test_same_writer_evals <- correct_NAs(evals = test_same_writer_evals, zero_correction = zero_correction)
  test_same_writer_evals <- correct_zeros(evals = test_same_writer_evals, zero_correction = zero_correction)

  test_diff_writer_evals <- eval_density_at_point(density = pdfs, x = scores$test_diff_writer)
  test_diff_writer_evals <- correct_NAs(evals = test_diff_writer_evals, zero_correction = zero_correction)
  test_diff_writer_evals <- correct_zeros(evals = test_diff_writer_evals, zero_correction = zero_correction)

  slrs <- list()
  slrs$same_writer <- test_same_writer_evals$numerators / test_same_writer_evals$denominators
  slrs$diff_writer <- test_diff_writer_evals$numerators / test_diff_writer_evals$denominators

  return(slrs)
}

get_densities <- function(scores){
  pdfs <- list()
  pdfs$same_writer <- density(scores$train_same_writer, kernel = "gaussian", n=10000)
  pdfs$diff_writer <- density(scores$train_diff_writer, kernel = "gaussian", n=10000)
  return(pdfs)
}

eval_density_at_point <- function(density, x){
  evals <- list()
  evals$numerators <- approx(density$same_writer$x, density$same_writer$y, xout = x, n=10000)$y
  evals$denominators <- approx(density$diff_writer$x, density$diff_writer$y, xout = x, n=10000)$y
  return(evals)
}

correct_NAs <- function(evals, zero_correction){
  evals$numerators[is.na(evals$numerators)] <- 0
  evals$denominators[is.na(evals$denominators)] <- zero_correction
  return(evals)
}

correct_zeros <- function(evals, zero_correction){
  evals$denominators[which(evals$denominators == 0)] <- zero_correction
  return(evals)
}

calculate_error_rates <- function(slrs) {
  error_rates <- list()
  error_rates$fnr <- sum(slrs$same_writer < 1) / length(slrs$same_writer)
  error_rates$fpr <- sum(slrs$diff_writer > 1) / length(slrs$diff_writer)
  return(error_rates)
}
