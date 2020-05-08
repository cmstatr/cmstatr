
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cmstatr <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![R build
status](https://github.com/ComtekAdvancedStructures/cmstatr/workflows/R-CMD-check/badge.svg)](https://github.com/ComtekAdvancedStructures/cmstatr/actions?workflow=R-CMD-check)

# What It Does

The `cmstatr` package provides functions for performing statistical
analysis of composite material data. The statistical methods implemented
are those described in CMH-17-1G.

# Installation

This package is not yet on CRAN (but it will be soon). For now, install
from `github` using `devtools`:

``` r
install.packages("devtools")
devtools::install_github("ComtekAdvancedStructures/cmstatr", build_vignettes = TRUE,
                         build_opts = c("--no-resave-data", "--no-manual"))
```

# Usage

To compute a B-Basis value from an example data set packaged with
`cmstatr` you can do the following:

``` r
library(dplyr)
library(cmstatr)

carbon.fabric.2 %>%
  filter(test == "FC") %>%
  filter(condition == "RTD") %>%
  basis_normal(strength, batch)
#> 
#> Call:
#> basis_normal(data = ., x = strength, batch = batch)
#> 
#> Distribution:  Normal    ( n =  18 )
#> B-Basis:   ( p =  0.9 , conf =  0.95 )
#> 76.88082
```

For more examples of usage of the `cmstatr` package, see the tutorial
vignette, which can be loaded as follows, once the package is installed:

``` r
vignette("cmstatr_Tutorial")
```

There is also a vignette showing some examples of the types of graphs
that are typically produced when analyzing composite materials. You can
load this vignette with:

``` r
vignette("cmstatr_Graphing")
```

# Philosophical Notes

This package expects [`tidy
data`](https://www.jstatsoft.org/article/view/v059i10). That is,
individual observations should be in rows and variables in columns.

Where possible, this package uses general solutions. Look-up tables are
avoided wherever possible.

# Issues

If you’ve found a bug, please open an issue in this repository and
describe the bug. Please include a [reproducible
example](https://reprex.tidyverse.org/) of the bug. If you’re able to
fix the bug, please do so by submitting a pull request.

If your bug is related to a particular data set, sharing that data set
will help to fix the bug. If you cannot share the data set, please strip
any identifying information and optionally scale the data by an
unspecified factor so that the bug can be reproduced and diagnosed.

# Contributing

Contributions to `cmstatr` are always welcomed. For small changes
(fixing typos or improving the documentation), go ahead and submit a
pull request. For more significant changes, such as new features, please
discuss the proposed change in an issue first.

## Contribution Guidelines

  - Please create a git branch for each pull request (PR)
  - Before submitting a pull request, please make sure that `R CMD
    CHECK` passes with no errors, warnings or notes
  - New and modified code should follow the style guide enforced by the
    [`lintr`](https://cran.r-project.org/package=lintr) package
  - Document all exported functions using
    [`roxygen2`](https://cran.r-project.org/package=roxygen2)
  - Write tests using
    [`testthat`](https://cran.r-project.org/package=testthat). If your
    contribution fixes a bug, then the test(s) that you add should fail
    before your bug-fix patch is applied and should pass after the code
    is patched.
  - For changes that affect the user, add a bullet at the top of
    `NEWS.md` below the current development version
