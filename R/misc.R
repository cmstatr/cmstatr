# To add extra release questions that devtools::release asks
# Ref. https://devtools.r-lib.org/reference/release.html
release_questions <- function() {
  c(
    "Did you re-build the hk_ext.Rmd using `rebuild-long-running-vignette.R`?"
  )
}

# To allow the vignette to build correctly
#' @importFrom stats dnorm
NULL
