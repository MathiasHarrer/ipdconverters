# The `ipdconverters` Package

![R-CMD-check](https://github.com/mathiasharrer/ipdconverters/actions/workflows/R-CMD-check.yml/badge.svg)

*Please note that this is a **read-only** repository. Unsolicited PRs requests or issue reports will not be responded to.*

## Overview

`ipdconverters` is an R package that enables the conversion of various depression and anxiety scale scores into a **common metric**. Please note that this package is early-stage/experimental and mostly used for internal purposes.

## Installation

You can install the development version of **ipdconverters** from GitHub:

``` r
# Install devtools if not already installed
install.packages("remotes")

# Install ipdconverters
remotes::install_github("mathiasharrer/ipdconverters")
```

## Usage

Load the package and convert scores:

``` r
library(ipdconverters)

# Convert CES-D scores to common metric
x <- c(10, 15, 20, 25)
cesd(x)

# Convert back to original scale
cesd.rev(x)
```

## Citation

> Harrer M, Sprenger A, Illing S, Ciharova M, Buntrock C (2025). *ipdconverters: IPD Common Metric Converters for Depression & Anxiety Scores [BETA]*. R package version 0.0.9000, <https://github.com/mathiasharrer/ipdconverters>.
