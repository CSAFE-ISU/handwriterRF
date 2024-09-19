calculate_slr <- function(score, pdfs = densities){
  numerator <- eval_density_at_point(den = pdfs$same_writer, x = score)
  denominator <- eval_density_at_point(den = pdfs$diff_writer, x = score)
  return(numerator / denominator)
}

eval_density_at_point <- function(den, x){
  y <- approx(den$x, den$y, xout = x, n=10000)$y
  return(y)
}

# correct_NAs <- function(evals, zero_correction = 1e-10){
#   evals$numerators[is.na(evals$numerators)] <- 0
#   evals$denominators[is.na(evals$denominators)] <- zero_correction
#   return(evals)
# }
#
# correct_zeros <- function(evals, zero_correction = 1e-10){
#   evals$denominators[which(evals$denominators == 0)] <- zero_correction
#   return(evals)
# }
