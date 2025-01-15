devtools::load_all()

ref_scores <- get_ref_scores(
  rforest = random_forest,
  df = validation,
  seed = 100,
  downsample_diff_pairs = TRUE
)
plot_scores(ref_scores)

usethis::use_data(ref_scores, overwrite = TRUE)
