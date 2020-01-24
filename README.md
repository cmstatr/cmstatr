[![R build status](https://github.com/ComtekAdvancedStructures/cmstatr/workflows/R-CMD-check/badge.svg)](https://github.com/ComtekAdvancedStructures/cmstatr/actions?workflow=R-CMD-check)

# cmstatr
Statistical tools for composite materials.

# What It Does
The `cmstatr` package provides functions for performing statistical analysis
of composite material data. The statistical methods implemented are those
described in CMH-17-1G.

# Installation
This package is not yet on CRAN. For now, install from `github` using
`devtools`:

```r
install.packages("devtools")
devtools::install_github("ComtekAdvancedStructures/cmstatr", build_vignettes = TRUE,
                         build_opts = c("--no-resave-data", "--no-manual"))
```

# Usage
For example usage of the `cmstatr` package, see the vignettes, which can be
loaded as follows, once the package is installed:

```r
RShowDoc("cmstatr_Tutorial", package = "cmstatr")
```

# Philosophical Notes
This package expects
[`tidy data`](https://www.jstatsoft.org/article/view/v059i10).
That is, individual observations should be in rows and variables in columns.

Where possible, this package uses general solutions. Lookup tables are avoided
wherever possible.

# Contributing
Contributions to `cmstatr` are always welcomed. For small changes (fixing typos
or improving the documentation), go ahead and submit a pull request. For more
significant changes, such as new features, please discuss the proposed change
in an issue first.

If you've found a bug, please open an issue and describe the bug. Please
include a [reproducible example](https://reprex.tidyverse.org/) of the bug.
If you're able to fix the bug, please do so by submitting a pull request.
Please also add a test to prevent a later regression (this new test should
fail before your fix is implemented and pass once it is).

## Contribution Guidelines
- Please create a git branch for each pull request (PR)
- Before submitting a pull request, please make sure that `R CMD CHECK`
  passes with no errors, warnings or notes
- New and modified code should follow the style guide enforced by the
  [`lintr`](https://cran.r-project.org/package=lintr)
  package
- Document all exported functions using
  [`roxygen2`](https://cran.r-project.org/package=roxygen2)
- Write tests using [`testthat`](https://cran.r-project.org/package=testthat)
- For changes that affect the user, add a bullet at the top of `NEWS.md` below
  the current development version
