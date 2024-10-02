# Welcome


HandwriterRF is designed to assist forensic document examiners by
performing a statistical analysis on two handwriting samples of unknown
origin. Two hypotheses are considered:

*H*<sub>*p*</sub> : The two documents were written by the same writer.
*H*<sub>*d*</sub> : The two documents were written by different writers.

The statistical analysis produces a *score-based likelihood ratio
(SLR)*. An SLR greater than one, indicates that the evidence supports
*H*<sub>*p*</sub> over *H*<sub>*d*</sub>, and the larger the SLR, the
stronger the support. An SLR less than one, indicates that the evidence
supports *H*<sub>*d*</sub> over *H*<sub>*p*</sub>, and the closer the
SLR is to zero, the stronger the support.

# Quick Start

## Installation

HandwriterRF requires R and RStudio IDE.

-   Install R from [POSIT](https://posit.co/download/rstudio-desktop/)
-   Install RStudio IDE from
    [POSIT](https://posit.co/download/rstudio-desktop/)

Install the handwriterRF R package. Open RStudio, navigate to the
console window, and type

``` r
install.packages("devtools")
devtools::install_github("CSAFE-ISU/handwriterRF")
```

## Compare Two Handwriting Samples

Scan and save two handwriting samples as PNG images. Open RStudio,
navigate to the console window, and type:

``` r
library(handwriterRF)

calculate_slr(sample1_path = "path/to/handwriting_sample1.png",
              sample2_path = "path/to/handwriting_sample2.png")
```

The result is a *score-based likelihood ratio (SLR)* that quantifies the
strength of evidence in favor of “same writer” or “different writer.”
