# cmstatr
Statistical tools for composite materials

# Philosophical Notes
This package expects [`tidy data`](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html). That is, individual observations should be in rows and variables in columns.

When graphing is appropriate, [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html) will be used. This package will not include wrappers for `ggplot2`, but may include extensions to `ggplot2`

# For Developers
## Documentation
All exported functions should have complete documentation in the form of [`roxygen2`](https://cran.r-project.org/package=roxygen2) documentation that will compile into Rd documentation. Additionally, important features of this package should be explained through vignettes to allow new users to start using this package more easily.

## Testing
Please ensure that all code is tested. Tests are located in the `tests/testthat` folder. Additionally, any code that computes results should be validated with results published in literature or to existing accepted software.

## Code Style
When writing code, please lint the code using the package [`lintr`](https://cran.r-project.org/web/packages/lintr/README.html).
