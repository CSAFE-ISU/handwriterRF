# handwriterRF 1.1.1

## Fixes 
* Fixed the vignette, training-slr-model.Rmd, to save files to the temporary directory instead of the Desktop.

# handwriterRF 1.1.0

## New Features and Enhancements

* **Two New Vignettes**:
  
  1. **An Introduction to the SLR Model**: This vignette provides an overview of the score-based likelihood ratio (SLR) method used by handwriterRF to compare handwriting samples. It explains how the package uses a pre-trained random forest and a set of reference similarity scores to calculate SLRs.
  2. **SLR Model Tutorial**: This tutorial demonstrates how to train your own random forest model and generate your own set of reference similarity scores for SLR calculations.

* **New Function**: `compare_documents()`. The `compare_documents()` function allows users to compare two handwritten documents, either by a similarity score or using the score-based likelihood ratio (SLR) method. This function consolidates the functionality of `calculate_slr()` and offers additional flexibility, allowing users to choose whether to return just the similarity score or both the similarity score and the SLR.

* **New Function**: `compare_writer_profiles()`. To optimize large-scale experiments, `compare_writer_profiles()` provides a more efficient alternative to `compare_documents()`. In experiments involving many comparisons with the same document, `compare_documents()` can be slow, as it reprocesses the same document multiple times. `compare_writer_profiles()` solves this by allowing each document to be processed only once, even if it is used in several comparisons.

* **New Function**: `plot_scores()`. The `plot_scores()` function allows users to visualize histograms of same-writer and different-writer similarity scores created with get_ref_scores(), providing an easy way to analyze and compare similarity scores from a dataframe of writer profiles.

* **New Dataframes**: `train`, `validation`, and `test`. These new dataframes store writer profiles for training, validation, and testing purposes. The package creates `random_forest` from the `train` dataframe, and the reference similarity scores, `ref_scores`, from the validation dataframe.

* **Function Update:** `get_cluster_fill_rates()`. The function `get_cluster_fill_rates()` has been introduced in the handwriter R package to calculate cluster fill rates for one or more handwriting samples. This supersedes the previous version of `get_cluster_fill_rates()` included in handwriterRF.

# handwriterRF 1.0.2

* Removed quotes around "same writer" and "different writer" in documentation.

* Removed dontrun{} from the examples for random_forest. Changed example for get_distances() to something that runs in less than 5 seconds and removed dontrun{} from this example. The examples for calculate_slr() take longer than 5 seconds to run so dontrun{} was changed to donttest{} for these examples.

# handwriterRF 1.0.1

# handwriterRF 1.0.0

* Initial CRAN submission.
