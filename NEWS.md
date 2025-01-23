# handwriterRF (development version)

## Minor improvements

* Introduced `compare_documents()` to compare two handwritten documents using either a similarity score or a score-based likelihood ratio as a comparison method. This new function incorporates the features of `calculate_slr()` and provides additional flexibility: users can now opt to return either just the similarity score or both the similarity score and the score-based likelihood ratio.

* Introduced `compare_writer_profiles()` to speed up experiments involving large numbers of documents, offering a more efficient alternative to `compare_documents()`. In large-scale experiments where the same document is used in multiple comparisons, `compare_documents()` can be slow because it processes each document every time it is used. `compare_writer_profiles()` addresses this issue by allowing users to process each document only once, even when it is involved in multiple comparisons.

* Introduced new dataframes of writer profiles `train`, `validation`, and `test`. Created a new `random_forest` from `train`. Created `ref_scores`, a list of same writer and different writer similarity scores, from `validation`. 

* Introduced `plot_scores()` to plot histograms of same writer and different writer similarity scores created with `get_ref_scores()` and a dataframe of writer profiles.

* The handwriter package now has a function `handwriter::get_cluster_fill_rates()` to calculate cluster fill rates for one or more handwriting samples, `get_cluster_fill_rates()` has been superseded in favor of `handwriter::get_cluster_fill_rates()`.

# handwriterRF 1.0.2

* Removed quotes around "same writer" and "different writer" in documentation.

* Removed dontrun{} from the examples for random_forest. Changed example for get_distances() to something that runs in less than 5 seconds and removed dontrun{} from this example. The examples for calculate_slr() take longer than 5 seconds to run so dontrun{} was changed to donttest{} for these examples.

# handwriterRF 1.0.1

# handwriterRF 1.0.0

* Initial CRAN submission.
