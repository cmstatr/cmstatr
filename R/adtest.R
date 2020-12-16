
#' Anderson--Darling test for goodness of fit
#'
#' @description
#' Calculates the Anderson--Darling test statistic for a sample given
#' a particular distribution, and determines whether to reject the
#' hypothesis that a sample is drawn from that distribution.
#'
#' @param data a data.frame-like object (optional)
#' @param x a numeric vector or a variable in the data.frame
#' @param alpha the required significance level of the test.
#'              Defaults to 0.05.
#'
#' @return
#' an object of class `anderson_darling`. This object has the following
#' fields.
#'
#' - `call` the expression used to call this function
#' - `dist` the distribution used
#' - `data` a copy of the data analyzed
#' - `n` the number of observations in the sample
#' - `A` the Anderson--Darling test statistic
#' - `osl` the observed significance level (p-value),
#'   assuming the
#'   parameters of the distribution are estimated from the data
#' - `alpha` the required significance level for the test.
#'   This value is given by the user.
#' - `reject_distribution` a logical value indicating whether
#'   the hypothesis that the data is drawn from the specified distribution
#'   should be rejected
#'
#' @details
#' The Anderson--Darling test statistic is calculated for the distribution
#' given by the user.
#'
#' The observed significance level (OSL), or p-value, is calculated assuming
#' that the parameters
#' of the distribution are unknown; these parameters are estimate from the
#' data.
#'
#' The function `anderson_darling_normal` computes the Anderson--Darling
#' test statistic given a normal distribution with mean and standard deviation
#' equal to the sample mean and standard deviation.
#'
#' The function `anderson_darling_lognormal` is the same as
#' `anderson_darling_normal` except that the data is log transformed
#' first.
#'
#' The function `anderson_darling_weibull` computes the Anderson--Darling
#' test statistic given a Weibull distribution with shape and scale parameters
#' estimated from the data using a maximum likelihood estimate.
#'
#' The test statistic, `A`, is modified to account for
#' the fact that the parameters of the population are not known,
#' but are instead estimated from the sample. This modification is
#' a function of the sample size only, and is different for each
#' distribution (normal/lognormal or Weibull). Several such modifications
#' have been proposed. This function uses the modification published in
#' Stephens (1974), Lawless (1982) and CMH-17-1G. Some other implementations
#' of the Anderson-Darling test, such as the implementation in the
#' `nortest` package, use other modifications, such as the one
#' published in D'Agostino and Stephens (1986). As such, the p-value
#' reported by this function may differ from the p-value reported
#' by implementations of the Anderson--Darling test that use
#' different modifiers. Only the unmodified
#' test statistic is reported in the result of this function, but
#' the modified test statistic is used to compute the OSL (p-value).
#'
#' This function uses the formulae for observed significance
#' level (OSL) published in CMH-17-1G. These formulae depend on the particular
#' distribution used.
#'
#' The results of this function have been validated against
#' published values in Lawless (1982).
#'
#'
#' @references
#' J. F. Lawless, *Statistical models and methods for lifetime data*.
#' New York: Wiley, 1982.
#'
#' "Composite Materials Handbook, Volume 1. Polymer Matrix
#' Composites Guideline for Characterization of Structural
#' Materials," SAE International, CMH-17-1G, Mar. 2012.
#'
#' M. A. Stephens, “EDF Statistics for Goodness of Fit and Some
#' Comparisons,”
#' Journal of the American Statistical Association, vol. 69, no. 347.
#' pp. 730–737, 1974.
#'
#' R. D’Agostino and M. Stephens, Goodness-of-Fit Techniques.
#' New York: Marcel Dekker, 1986.
#'
#' @examples
#' library(dplyr)
#'
#' carbon.fabric %>%
#'   filter(test == "FC") %>%
#'   filter(condition == "RTD") %>%
#'   anderson_darling_normal(strength)
#' ## Call:
#' ## anderson_darling_normal(data = ., x = strength)
#' ##
#' ## Distribution:  Normal ( n = 18 )
#' ## Test statistic:  A = 0.9224776
#' ## OSL (p-value):  0.01212193  (assuming unknown parameters)
#' ## Conclusion: Sample is not drawn from a Normal distribution (alpha = 0.05)
#'
#' @importFrom rlang enquo eval_tidy
#'
#' @name anderson_darling
NULL

# A non-exported function for an Anderson--Darling goodness of fit test
# The function \code{dist} should be
# the cumulative distribution function. Additional parameters, such as
# the parameters for the distribution, can be passed through the argument
# to the function \code{...} to the function \code{dist}.
anderson_darling <- function(x0, call, ad_p_unknown_param_fcn,
                             dist_name, dist, alpha, ...) {
  f0 <- dist
  x0_sorted <- sort(x0)
  n <- length(x0_sorted)
  ii <- 1:n
  u <- f0(x0_sorted, ...)
  a <- -n - sum((2 * ii - 1) / n * (log(u) + log(1 - rev(u))))

  res <- list(
    call = call,
    dist = dist_name,
    data = x0,
    n = n,
    A = a,  # nolint
    osl = ad_p_unknown_param_fcn(a, n),
    alpha = alpha
  )
  res$reject_distribution <- res$osl <= res$alpha

  class(res) <- "anderson_darling"

  return(res)
}


#' Glance at an `anderson_darling` object
#'
#' @description
#' Glance accepts an object of type `anderson_darling` and
#' returns a [tibble::tibble()] with
#' one row of summaries.
#'
#' Glance does not do any calculations: it just gathers the results in a
#' tibble.
#'
#' @param x an `anderson_darling` object
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#'
#' @return
#' A one-row [tibble::tibble()] with the following
#' columns:
#'
#' - `dist` the distribution used
#' - `n` the number of observations in the sample
#' - `A` the Anderson--Darling test statistic
#' - `osl` the observed significance level (p-value),
#'     assuming the
#'     parameters of the distribution are estimated from the data
#'  - `alpha` the required significance level for the test.
#'    This value is given by the user.
#' - `reject_distribution` a logical value indicating whether
#'    the hypothesis that the data is drawn from the specified distribution
#'    should be rejected
#'
#'
#' @seealso
#' [anderson_darling()]
#'
#' @examples
#' x <- rnorm(100, 100, 4)
#' ad <- anderson_darling_weibull(x = x)
#' glance(ad)
#'
#' ## # A tibble: 1 x 6
#' ##   dist        n     A        osl alpha reject_distribution
#' ##   <chr>   <int> <dbl>      <dbl> <dbl> <lgl>
#' ## 1 Weibull   100  2.62 0.00000207  0.05 TRUE
#'
#' @method glance anderson_darling
#' @importFrom tibble tibble
#'
#' @export
glance.anderson_darling <- function(x, ...) {  # nolint
  with(
    x,
    tibble::tibble(
      dist = dist,
      n = n,
      A = A,  # nolint
      osl = osl,
      alpha = alpha,
      reject_distribution = reject_distribution
    )
  )
}

#' @export
print.anderson_darling <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Distribution: ", x$dist, "( n =", x$n, ")", "\n")

  cat("Test statistic:  A =", x$A, "\n")
  cat(
    "OSL (p-value): ",
    x$osl,
    " (assuming unknown parameters)\n"
  )
  if (x$reject_distribution) {
    cat("Conclusion: Sample is not drawn from a",
        x$dist,
        "distribution ( alpha =", x$alpha, ")")
  } else {
    cat("Conclusion: Sample is drawn from a",
        x$dist,
        "distribution ( alpha =", x$alpha, ")")
  }
}

#' @importFrom rlang enquo eval_tidy
#' @importFrom stats pnorm
#'
#' @rdname anderson_darling
#'
#' @export
anderson_darling_normal <- function(data = NULL, x, alpha = 0.05) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  x0 <- eval_tidy(enquo(x), data)
  return(anderson_darling(x0, call = match.call(),
                          ad_p_unknown_param_fcn = ad_p_normal_unknown_param,
                          dist_name = "Normal",
                          alpha = alpha,
                          dist = pnorm, mean = mean(x0), sd = sd(x0)))
}

#' @importFrom rlang enquo eval_tidy
#' @importFrom stats pnorm
#'
#' @rdname anderson_darling
#'
#' @export
anderson_darling_lognormal <- function(data = NULL, x, alpha = 0.05) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  x0 <- eval_tidy(enquo(x), data)
  x0 <- log(x0)
  return(anderson_darling(x0, call = match.call(),
                          ad_p_unknown_param_fcn = ad_p_normal_unknown_param,
                          dist_name = "Lognormal",
                          alpha = alpha,
                          dist = pnorm, mean = mean(x0), sd = sd(x0)))
}

#' @importFrom rlang enquo eval_tidy
#' @importFrom stats pweibull
#' @importFrom MASS fitdistr
#'
#' @rdname anderson_darling
#'
#' @export
anderson_darling_weibull <- function(data = NULL, x, alpha = 0.05) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  x0 <- eval_tidy(enquo(x), data)
  dist <- fitdistr(x0, "weibull")
  return(anderson_darling(x0, call = match.call(),
                          dist = pweibull,
                          dist_name = "Weibull",
                          alpha = alpha,
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
