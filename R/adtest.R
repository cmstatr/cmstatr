
#' Anderson-Darling test for goodness of fit
#'
#' @description
#' Calculates the Anderson-Darling test statistic for a sample, given
#' a particular distribution and computes the significance level.
#'
#' @param df a data.frame-like object (optional)
#' @param x a numeric vector or a variable in the data.frame
#'
#' @return
#' an object of class \code{anderson_darling}. This object has the following
#' fields.
#'
#' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{dist}}{the distribution used}
#'   \item{\code{data}}{a copy of the data analyzed}
#'   \item{\code{n}}{the number of observations in the sample}
#'   \item{\code{A}}{the Anderson-Darling test statistic}
#'   \item{\code{osl}}{the significance level, assuming the
#'     parameters of the distribution are estimated from the data}
#' }
#'
#' @details
#' The Anderson-Darling test statistic is calculated for the distribution
#' given.
#'
#' The significance level is calculated assuming that the parameters
#' of the distribution are unknown; these parameters are estimate from the
#' data.
#'
#' The function \code{anderson_darling_normal} computes the Anderson-Darling
#' test statistic given a normal distribution with mean and standard devaition
#' equal to the sample mean and standard deviation.
#'
#' The function \code{anderson_darling_lognormal} is the same as
#' \code{anderson_darling_normal} except that the data is log transformed
#' first.
#'
#' The function \code{anderson_darling_weibull} computes the Anderson-Darling
#' test statistic given a Weibull distribution with shape and scale parameters
#' estimate from the data using a maximum likelihood estimate.
#'
#' This function uses the formula for observed significance
#' level published in CMH-17-1G. These formulae depend on the particular
#' distribution used.
#'
#' The results of this function have been validated against
#' published values in Lawless (1982).
#'
#' @references
#' J. F. Lawless, \emph{Statistical models and methods for lifetime data}.
#' New York: Wiley, 1982.
#'
#' "Composites Materials Handbook, Volume 1. Polymer Matrix
#' Composites Guideline for Characterization of Structural
#' Materials," SAE International, CMH-17-1G, Mar. 2012.
#'
#' @importFrom rlang enquo eval_tidy
#'
#' @name anderson_darling
NULL

# A non-exported function for an Anderson-Darling goodness of fit test
# The function \code{dist} should be
# the cumulative distribution function. Additional parameters, such as
# the parameters for the distribution, can be passed through the argument
# to the function \code{...} to the function \code{dist}.
anderson_darling <- function(df = NULL, x, call, ad_p_unknown_param_fcn,
                             dist_name, dist, ...) {
  F0 <- dist

  x <- enquo(x)
  x0 <- eval_tidy(x, df)

  x0_sorted <- sort(x0)
  n <- length(x0_sorted)
  ii <- 1:n
  U <- F0(x0_sorted, ...)
  A <- -n - sum( (2 * ii - 1) / n * (log(U) + log(1 - rev(U))))

  res <- list(
    call = call,
    dist = dist_name,
    data = x0,
    n = n,
    A = A,
    osl = ad_p_unknown_param_fcn(A, n)
  )

  class(res) <- "anderson_darling"

  return(res)
}

#' @export
print.anderson_darling <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Distribution function: ", x$dist, "\n")
  cat("Sample size: ", x$n, "\n")

  cat("Test statistic: A = ", x$A, "\n")
  cat(
    "Significance: ",
    x$osl,
    " (assuming unknown parameters)\n"
  )
}

#' @importFrom rlang enquo eval_tidy
#' @importFrom stats pnorm
#'
#' @rdname anderson_darling
#'
#' @export
anderson_darling_normal <- function(df = NULL, x) {
  x <- enquo(x)
  x0 <- eval_tidy(x, df)
  return(anderson_darling(x = x0, call = match.call(),
                          ad_p_unknown_param_fcn = ad_p_normal_unknown_param,
                          dist_name = "Normal",
                          dist = pnorm, mean = mean(x0), sd = sd(x0)))
}

#' @importFrom rlang enquo eval_tidy
#' @importFrom stats pnorm
#'
#' @rdname anderson_darling
#'
#' @export
anderson_darling_lognormal <- function(df = NULL, x) {
  x <- enquo(x)
  x0 <- eval_tidy(x, df)
  x0 <- log(x0)
  return(anderson_darling(x = x0, call = match.call(),
                          ad_p_unknown_param_fcn = ad_p_normal_unknown_param,
                          dist_name = "Lognormal",
                          dist = pnorm, mean = mean(x0), sd = sd(x0)))
}

#' @importFrom rlang enquo eval_tidy
#' @importFrom stats pweibull
#' @importFrom MASS fitdistr
#'
#' @rdname anderson_darling
#'
#' @export
anderson_darling_weibull <- function(df = NULL, x) {
  x <- enquo(x)
  x0 <- eval_tidy(x, df)
  dist <- fitdistr(x0, "weibull")

  return(anderson_darling(x = x0, call = match.call(),
                          dist = pweibull,
                          dist_name = "Weibull",
                          ad_p_unknown_param_fcn = ad_p_weibull_unknown_param,
                          shape = dist$estimate[["shape"]],
                          scale = dist$estimate[["scale"]]))
}

ad_p_normal_unknown_param <- function(z, n) {
  ad_star <- (1 + 4 / n - 25 / n ^ 2) * z
  osl <- 1 / (1 + exp(-0.48 + 0.78 * log(ad_star) + 4.58 * ad_star))
  return(osl)
}

ad_p_weibull_unknown_param <- function(z, n) {
  ad_star <- (1 + 0.2 / sqrt(n)) * z
  osl <- 1 / (1 + exp(-0.1 + 1.24 * log(ad_star) + 4.48 * ad_star))
  return(osl)
}
