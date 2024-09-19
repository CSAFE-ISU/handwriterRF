eval_density_at_point <- function(density, x){
  evals <- list()
  evals$numerators <- approx(density$same_writer$x, density$same_writer$y, xout = x, n=10000)$y
  evals$denominators <- approx(density$diff_writer$x, density$diff_writer$y, xout = x, n=10000)$y
  return(evals)
}

correct_NAs <- function(evals, zero_correction = 1e-10){
  evals$numerators[is.na(evals$numerators)] <- 0
  evals$denominators[is.na(evals$denominators)] <- zero_correction
  return(evals)
}

correct_zeros <- function(evals, zero_correction = 1e-10){
  evals$denominators[which(evals$denominators == 0)] <- zero_correction
  return(evals)
}
