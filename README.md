# handwriterRF

<!-- badges: start -->
[![R-CMD-check MacOS](https://github.com/CSAFE-ISU/handwriterRF/actions/workflows/R-CMD-check-macos.yaml/badge.svg)](https://github.com/CSAFE-ISU/handwriterRF/actions/workflows/R-CMD-check-macos.yaml)
[![R-CMD-check Ubuntu](https://github.com/CSAFE-ISU/handwriterRF/actions/workflows/R-CMD-check-ubuntu.yaml/badge.svg)](https://github.com/CSAFE-ISU/handwriterRF/actions/workflows/R-CMD-check-ubuntu.yaml)
[![R-CMD-check Windows](https://github.com/CSAFE-ISU/handwriterRF/actions/workflows/R-CMD-check-windows.yaml/badge.svg)](https://github.com/CSAFE-ISU/handwriterRF/actions/workflows/R-CMD-check-windows.yaml)
[![Codecov test coverage](https://codecov.io/gh/CSAFE-ISU/handwriterRF/graph/badge.svg)](https://app.codecov.io/gh/CSAFE-ISU/handwriterRF)
<!-- badges: end -->

HandwriterRF is designed to assist forensic document examiners by
performing a statistical analysis on two handwriting samples. One or
both of the samples could be from unknown writers. Two hypotheses are
considered:

$H_p: \text{The two documents were written by the same writer.}$
$H_d: \text{The two documents were written by different writers.}$

The statistical analysis produces a *score-based likelihood ratio
(SLR)*. An SLR greater than one, indicates that the evidence supports
$H_p$ over $H_d$, and the larger the SLR, the stronger the support. An
SLR less than one, indicates that the evidence supports $H_d$ over
$H_p$, and the closer the SLR is to zero, the stronger the support.

# Quick Start

## Installation

HandwriterRF requires R and RStudio IDE.

- Install R from [POSIT](https://posit.co/download/rstudio-desktop/)
- Install RStudio IDE from
  [POSIT](https://posit.co/download/rstudio-desktop/)

Install the handwriterRF R package. Open RStudio, navigate to the
console window, and type

``` r
install.packages("handwriterRF")
```

## Compare Two Handwriting Samples

### Calculate a Score-base Likelihood Ratio

Open RStudio, navigate to the console window, and load handwriterRF.

``` r
library(handwriterRF)
```

The package includes 4 example handwriting samples from the [CSAFE
Handwriting Database](https://forensicstats.org/handwritingdatabase/).
Compare 2 of these samples. In this case, both samples are from writer
30.

``` r
sample1 <- system.file(file.path("extdata", "docs", "w0005_s01_pLND_r03.png"), package = "handwriterRF")
sample2 <- system.file(file.path("extdata", "docs", "w0005_s02_pWOZ_r02.png"), package = "handwriterRF")
slr <- calculate_slr(sample1, sample2)
```

If you would like to use your own handwriting samples, scan and save
them as PNG images.

``` r
sample1 <- "path/to/your_sample1.png"
sample2 <- "path/to/your_sample2.png"
slr <- calculate_slr(sample1, sample2)
```

The result is a dataframe:

- *docname1* is the file name of the first sample.
- *writer1* is “unknown1”.
- *docname2* is the file name of the second sample.
- *writer2* is “unknown2”.
- *score* is the similarity score between the two samples.
- *slr* is a score-based likelihood ratio that quantifies the strength
  of evidence in favor of same writer or different writer.

Display the slr dataframe. We hide the file path columns here so that
the dataframe fits on this page.

``` r
slr
```

                docname1  writer1           docname2  writer2 score      slr
    1 w0005_s01_pLND_r03 unknown1 w0005_s02_pWOZ_r02 unknown2 0.635 1.482318

### Interpret the Score-base Likelihood Ratio

View a verbal interpretation of the score-based likelihood ratio.

``` r
interpret_slr(slr)
```

    [1] "A score-based likelihood ratio of 1.5 means the likelihood of observing a similarity score of 0.635 if the documents were written by the same person is 1.5 times greater than the likelihood of observing this score if the documents were written by different writers."
