# handwriterRF


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
sample1 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r01.png"), package = "handwriterRF")
sample2 <- system.file(file.path("extdata", "docs", "w0030_s01_pWOZ_r02.png"), package = "handwriterRF")
slr <- calculate_slr(sample1, sample2)
```

If you would like to use your own handwriting samples, scan and save
them as PNG images.

``` r
sample1 <- "path/to/your_sample1.png"
sample2 <- "path/to/your_sample2.png"
slr <- calculate_slr(sample1, sample2)
```

The result is a data frame:

- *docname1* is the file name of the first sample.
- *writer1* is “unknown1”.
- *docname2* is the file name of the second sample.
- *writer2* is “unknown2”.
- *score* is the similarity score between the two samples.
- *slr* is a score-based likelihood ratio that quantifies the strength
  of evidence in favor of same writer or different writer.

Display the slr data frame. We hide the file path columns here so that
the data frame fits on this page.

``` r
slr
```

                docname1  writer1           docname2  writer2 score     slr
    1 w0030_s01_pWOZ_r01 unknown1 w0030_s01_pWOZ_r02 unknown2 0.955 135.159

### Interpret the Score-base Likelihood Ratio

View a verbal interpretation of the score-based likelihood ratio.

``` r
interpret_slr(slr)
```

    [1] "A score-based likelihood ratio of 135.2 means the likelihood of observing a similarity score of 0.955 if the documents were written by the same person is 135.2 times greater than the likelihood of observing this score if the documents were written by different writers."
