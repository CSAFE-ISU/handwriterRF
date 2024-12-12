# handwriterRF (development version)

## Major changes

* Created `compare_documents()` to compare two handwritten documents using either a similarity score or a score-based likelihood ratio as a comparison method.

* Created function `compare_writer_profiles()` to make experiments faster on large numbers of documents compared to `compare_documents()`. `compare_writer_profiles()` calculates either a similarity score or score-based likelihood ratio for every pair of documents.    

* Created new data frames of writer profiles `train`, `validation`, and `test`. Created a new `random_forest` from `train`. Created `ref_scores`, a list of same writer and different writer similarity scores, from `validation`. 

## Minor improvements and bug fixes

* `calculate_slr()` has been superseded in favor of `compare_documents()`, which includes the functionality of `caclulate_slr()` and offers additional functionality.

* Created `plot_scores()` to plot histograms of the reference same writer and different writer similarity scores in `random_forest$scores`.

* Deprecated `get_cluster_fill_rates()` in favor of `handwriter::get_cluster_fill_rates()`.

# handwriterRF 1.0.2

* Removed quotes around "same writer" and "different writer" in documentation.

* Removed dontrun{} from the examples for random_forest. Changed example for get_distances() to something that runs in less than 5 seconds and removed dontrun{} from this example. The examples for calculate_slr() take longer than 5 seconds to run so dontrun{} was changed to donttest{} for these examples.

# handwriterRF 1.0.1

# handwriterRF 1.0.0

* Initial CRAN submission.
