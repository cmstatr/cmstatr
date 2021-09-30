
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cmstatr <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R build
status](https://github.com/cmstatr/cmstatr/workflows/R-CMD-check/badge.svg)](https://github.com/cmstatr/cmstatr/actions?workflow=R-CMD-check)
[![`Codecov` test
coverage](https://codecov.io/gh/cmstatr/cmstatr/branch/master/graph/badge.svg)](https://codecov.io/gh/cmstatr/cmstatr?branch=master)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.02265/status.svg)](https://doi.org/10.21105/joss.02265)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/cmstatr)](https://cran.r-project.org/package=cmstatr)
[![](https://cranlogs.r-pkg.org/badges/cmstatr)](https://cran.r-project.org/package=cmstatr)
<!-- badges: end -->

# What It Does

The `cmstatr` package provides functions for performing statistical
analysis of composite material data. The statistical methods implemented
are those described in [CMH-17-1G](https://www.cmh17.org/). This package
focuses on calculating basis values (lower tolerance bounds) for
material strength properties, as well as performing the associated
diagnostic tests. Functions are also provided for testing for
equivalency between alternate samples and the “qualification” or
“baseline” samples.

Additional details about the package are available in the paper by
Kloppenborg (2020, <https://doi.org/10.21105/joss.02265>).

# Installation

To install `cmstatr` from CRAN, simply run:

``` r
install.packages("cmstatr")
```

If you want the latest development version, you can install it from
`github` using `devtools`. This will also install the dependencies
required to build the vignettes. Optionally, change the value of the
argument `ref` to install `cmstatr` from a different branch of the
repository.

``` r
install.packages(c("devtools", "rmarkdown", "dplyr", "tidyr"))
devtools::install_github("cmstatr/cmstatr", build_vignettes = TRUE,
                         ref = "master",
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
#> Distribution:  Normal    ( n = 18 )
#> B-Basis:   ( p = 0.9 , conf = 0.95 )
#> 76.88082
```

For more examples of usage of the `cmstatr` package, see the tutorial
vignette, which can be [viewed
online](https://www.cmstatr.net/articles/cmstatr_Tutorial.html), or can
be loaded as follows, once the package is installed:

``` r
vignette("cmstatr_Tutorial")
```

There is also a vignette showing some examples of the types of graphs
that are typically produced when analyzing composite materials. You can
view this [vignette
online](https://www.cmstatr.net/articles/cmstatr_Graphing.html), or you
can load this vignette with:

``` r
vignette("cmstatr_Graphing")
```

# Philosophical Notes

This package expects
[`tidy data`](https://www.jstatsoft.org/article/view/v059i10). That is,
individual observations should be in rows and variables in columns.

Where possible, this package uses general solutions. Look-up tables are
avoided wherever possible.

# Issues

If you’ve found a bug, please open an issue in this repository and
describe the bug. Please include a [reproducible
example](https://reprex.tidyverse.org/) of the bug. If you’re able to
fix the bug, you can do so by submitting a pull request.

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

-   Please create a git branch for each pull request (PR)
-   Before submitting a pull request, please make sure that
    `R CMD check` passes with no errors, warnings or notes
-   New and modified code should follow the style guide enforced by the
    [`lintr`](https://cran.r-project.org/package=lintr) package
-   Document all exported functions using
    [`roxygen2`](https://cran.r-project.org/package=roxygen2)
-   Write tests using
    [`testthat`](https://cran.r-project.org/package=testthat). If your
    contribution fixes a bug, then the test(s) that you add should fail
    before your bug-fix patch is applied and should pass after the code
    is patched.
-   For changes that affect the user, add a bullet at the top of
    `NEWS.md` below the current development version

## Development

Testing is performed using `testthat`. Edition 3 of that package is used
and parallel processing enabled. If you wish to use more than two CPUs,
set the environment variable `TESTTHAT_CPUS` to the number of CPUs that
you want to use. One way of doing this is to create the file `.Rprofile`
with the following contents. This file is ignored both by `git` and also
in `.Rbuildingore`.

``` r
Sys.setenv(TESTTHAT_CPUS = 8)
```
