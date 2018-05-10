
#' Anderson-Darling test for goodness of fit
#'
#' @description
#' Calculates the Anderson-Darling test statistic for a sample, given
#' a particular distribution and computes the significance level.
#'
#' @param df a data.frame-like object (optional)
#' @param x a numeric vector or a variable in the data.frame
#' @param dist the cumulative distribution function considered
#' @param ... additional arguments to pass to \code{dist}
#'
#' @return
#' an object of class \code{anderson_darling}. This object has the following
#' fields.
#'
#' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{dist_fcn}}{the distribution function used}
#'   \item{\code{data}}{a copy of the data analyzed}
#'   \item{\code{n}}{the number of observations in the sample}
#'   \item{\code{A}}{the Anderson-Darling test statistic}
#'   \item{\code{p_known_param}}{the significance level, assuming the parameters
#'     of the distribution are known}
#'   \item{\code{p_unknown_param}}{the significance level, assuming the
#'     parameters of the distribution are unknown}
#' }
#'
#' @details
#' The Anderson-Darling test statistic is calculated for the distribution
#' given by the function \code{dist}. The function \code{dist} should be
#' the cumulative distribution function. Additional parameters, such as
#' the parameters for the distribution, can be passed through the argument
#' to the function \code{...} to the function \code{dist}.
#'
#' The significance level is calculated both assuming that the parameters
#' of the distribution are known and unknown. The significance level assuming
#' that the parameters are known are given in \code{p_known_param} and are
#' computed using the function \code{\link{ad_p_known_param}}. The
#' significance level assuming that the parameters of the distribution are
#' unknown are given in \code{p_unknown_param} and are computed using the
#' function \code{\link{ad_p_unknown_param}}.
#'
#' @references
#' J. F. Lawless, \emph{Statistical models and methods for lifetime data}.
#' New York: Wiley, 1982.
#'
#' @importFrom rlang enquo eval_tidy
#'
#' @export
anderson_darling <- function(df = NULL, x, dist, ...) {  # TODO: Update documentation
  dist_fcn <- as.character(substitute(dist))
  if (is.function(dist)) {
    F0 <- dist
  } else if (is.character(dist)) {
    F0 <- get(dist, mode = "function")
  } else {
    stop("dist must be a function or character string with the function name")
  }

  x <- enquo(x)
  x0 <- eval_tidy(x, df)

  x0_sorted <- sort(x0)
  n <- length(x0_sorted)
  ii <- 1:n
  U <- F0(x0_sorted, ...)
  A <- -n - sum( (2 * ii - 1) / n * (log(U) + log(1 - rev(U))))

  res <- list(
    call = match.call(),
    dist_fcn = dist_fcn,
    data = x0,
    n = n,
    A = A,
    p_known_param = ad_p_known_param(A, n),
    p_unknown_param = ad_p_unknown_param(A, n)
  )

  class(res) <- "anderson_darling"

  return(res)
}

#' @export
print.anderson_darling <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Distribution function: ", x$dist_fcn, "\n")
  cat("Sample size: ", x$n, "\n")

  cat("Test statistic: A = ", x$A, "\n")
  cat("Significance: ", x$p_known_param, " (assuming parameters are known)\n")
  cat("Significance: ", x$p_unknown_param, " (assuming parameters are unknown)\n")
}



#' @rdname ad_p_known_parameters
#' @export
ad_p_inf_known_param <- function(z, abs_tol = .Machine$double.eps ^ 0.5) {
  if (z < 0.01) {
    return(1)  # According to Marsaglia, ADInf(0.01) = 0.528e-52
  }

  max_iter <- 100

  warning_message <- FALSE

  erfc <- function(x) 2 * pnorm(x * sqrt(2), lower.tail = FALSE)

  adinf <- 0

  r <- 1 / z
  ad_inc_mult <- r

  for (j in 0:max_iter) {
    # Calculate f
    t <- (4 * j + 1) ^ 2 * pi ^ 2 / (8 * z)
    cm2 <- pi * exp(-t) * (2 * t) ^ (-1 / 2)
    cm1 <- pi * (pi / 2) ^ (1 / 2) * erfc(t ^ (1 / 2))
    f <- cm2 + cm1 * z / 8
    for (n in 2:max_iter) {
      cn <- ( ( (n - 1) - 1 / 2 - t) * cm1 + t * cm2) / (n - 1)
      f_inc <- cn * (z / 8) ^ n / factorial(n)
      f <- f + f_inc
      if (abs(f_inc) < abs(abs_tol / ad_inc_mult)) break
      cm2 <- cm1
      cm1 <- cn
    }
    if (n >= max_iter) warning_message <- TRUE

    ad_inc <- ad_inc_mult * f
    adinf <- adinf + ad_inc
    if (abs(ad_inc) < abs_tol ) break

    r <- r * (1 / 2 - (j + 1)) / (j + 1)
    ad_inc_mult <- (4 * j + 5) * r
  }
  if (j >= max_iter) warning_message <- TRUE

  if (warning_message) {
    warning(paste0(
      "Solution did not converge within requested tolerance (abs.tol=",
      abs_tol,
      ")"
    ))
  }

  return(1.0 - adinf)
}

#' significance level for the Anderson-Darling test statistic
#' when the parameters of the distribution are known
#'
#' @description
#' Calculates the significance level from an Anderson-Darling
#' test statistic. These functions assume that the parameters
#' of the distribution are known.
#'
#' @param z the test statistic
#' @param n the sample size
#' @param abs_tol the requested accuracy of the calculation
#'
#' @return the significance level.
#'
#' @details
#' \code{ad_p_inf_known_param} uses the limiting distribution (i.e. for
#' infinite sample size). \code{ad_p_known_param} applies a correction
#' for sample size. Since the
#' Anderson-Darling test statistic converges very quickly, the correction
#' only has a practical effect for very small sample sizes (around 5 or less).
#' Nevertheless, it is recommended that \code{ad_p_known_param} be used as
#' the correction
#' is not computationally intensive to apply.
#'
#' The method of Marsaglia and Marsaglia is used to compute the significance
#' level. See references below.
#'
#' @importFrom stats pnorm
#'
#' @references
#' G. Marsaglia and J. Marsaglia, “Evaluating the Anderson-Darling
#' Distribution,” Journal of Statistical Software, vol. 9, no. 2. 25-Feb-2004.
#'
#' @name ad_p_known_parameters
#' @export
ad_p_known_param <- function(z, n, abs_tol = .Machine$double.eps ^ 0.5) {
  if (z < 0.01) {
    return(1)  # According to Marsaglia, ADInf(0.01) = 0.528e-52
  }

  p <- 1.0 - ad_p_inf_known_param(z, abs_tol = abs_tol)

  # calculate the correction for n
  cn <- 0.01265 + 0.1757 / n
  if (p < cn) {
    x <- p / cn
    g1 <- sqrt(x) * (1 - x) * (49 * x - 102)
    errfix <- (0.0037 / n ^ 3 + 0.00078 / n ^ 2 + 0.00006 / n) * g1
  } else if (p < 0.8) {
    x <- (p - cn) / (0.8 - cn)
    g2 <- 8.259 - 1.91864 * x
    g2 <- 14.458 - g2 * x
    g2 <- -0.00022633 + (6.54034 - (14.6538 - g2 * x) * x) * x
    errfix <- (0.04213 / n + 0.01365 / n ^ 2) * g2
  } else {
    g3 <- 1116.360 - 255.7844 * p
    g3 <- 1950.646 - g3 * p
    g3 <- -130.2137 + (745.2337 - (1705.091 - g3 * p) * p) * p
    errfix <- g3 / n
  }

  return(1.0 - (p + errfix))
}

#' significance level for the Anderson-Darling test statistic
#' when the parameters of the distribution are unknown
#'
#' @description
#' Calculates the significance level from an Anderson-Darling
#' test statistic. These functions assume that the parameters
#' of the distribution are unknown.
#'
#' @param z the test statistic
#' @param n the number of observations
#'
#' @return the significance level (numeric)
#'
#' @details
#' This function uses the formula for observed significance
#' level published in CMH-17-1G.
#'
#' The results of this function have been validated against
#' published values in Lawless (1982).
#'
#' @references
#' “Composites Materials Handbook, Volume 1. Polymer Matrix
#' Composites Guideline for Characterization of Structural
#' Materials,” SAE International, CMH-17-1G, Mar. 2012.
#'
#' J. F. Lawless, \emph{Statistical models and methods for lifetime data}.
#' New York: Wiley, 1982.
#'
#' @export
ad_p_unknown_param <- function(z, n) {
  ad_star <- (1 + 4 / n - 25 / n ^ 2) * z
  osl <- 1 / (1 + exp(-0.48 + 0.78 * log(ad_star) + 4.58 * ad_star))
  return(osl)
}
