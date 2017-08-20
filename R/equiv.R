
#' Determine equivalency or equivalency thresholds based on both minimum
#' individual and mean.
#'
#' @param alpha the acceptable probability of a type I error
#' @param data_sample (optional) a vector of observations from the sample for
#' which equivalency is being tested. Defaults to NULL
#' @param n_sample (optional) the number of observations in the sample for
#' which equivalency will be tested. Defaults to NULL
#' @param data_qual (optional) a vector of observations from the
#' "qualification" data to which equivalency is being tested. Defaults to NULL
#' @param mean_qual (optional) the mean from the "qualification" data to which
#' equivalency is being tested. Defaults to NULL
#' @param sd_qual (optional) the standard deviation from the "qualification"
#' data to which equivalency is being tested. Defaults to NULL
#' @param modcv (optional) a boolean value indicating whether a modified CV
#' should be used. Defaults to FALSE, in which case the standard deviation
#' supplied (or calculated from data_qual) will be used directly.
#'
#' @return
#' Returns an object of class \code{equiv_mean_extremum}. This object is a list
#' with the following named elements:
#'
#' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{alpha}}{the value of alpha passed to this function}
#'   \item{\code{n_sample}}{the number of observations in the sample for which
#'     equivalency is being checked. This is either the value \code{n_sample}
#'     passed to this function or the length of the vector \code{data_sample}.}
#'   \item{\code{k1}}{the factor used to calculate the minimum individual
#'     threshold. The minimum individual threshold is calculated as
#'     \eqn{Wmin = qual_mean - k1 * qual_sd}}
#'   \item{\code{k2}}{the factor used to calculate the threshold for mean. The
#'     threshold for mean is calculated as
#'     \eqn{Wmean = qual_mean - k2 * qual_sd}}
#'   \item{\code{modcv}}{logical value indicating whether the acceptance
#'     thresholds are calcualted using the modified CV approach}
#'   \item{\code{cv}}{the coefficient of variation of the qualification data.
#'     This value is not modified, even if \code{modcv=TRUE}}
#'   \item{\code{cv_star}}{The modified coefficient of variation. If
#'     \code{modcv=FALSE}, this will be \code{NULL}}
#'   \item{\code{threshold_min_indiv}}{The calculated threshold value for
#'     minimum individual}
#'   \item{\code{threshold_mean}}{The calculated threshold value for mean}
#'   \item{\code{test_min_indiv}}{a character vector of either "PASS" or
#'     "FAIL" indicating whehter the data from \code{data_sample} passes the
#'     test for minimum individual. If \code{data_sample} was not supplied,
#'     this value will be \code{NULL}}
#'   \item{\code{test_mean}}{a character vector of either "PASS" or
#'     "FAIL" indicating whehter the data from \code{data_sample} passes the
#'     test for mean. If \code{data_sample} was not supplied, this value will
#'     be  \code{NULL}}
#'   \item{\code{min_sample}}{The minimum value from the vector
#'     \code{data_sample}. if \code{data_sample} was not supplied, this will
#'     have a value of \code{NULL}}
#'   \item{\code{mean_sample}}{The mean value from the vector
#'     \code{data_sample}. If \code{data_sample} was not supplied, this will
#'     have a value of \code{NULL}}
#' }
#'
#' @details
#' There are several optional arguments to this function. However, you can't
#' omit all of the optional arguments. You must supply either data_sample or
#' n_sample, but not both. You must also supply either data_qual or both
#' mean_qual and sd_qual, but if you supply data_qual you should not supply
#' either mean_qual or sd_qual (and visa-versa). This function will issue a
#' warning or error if you violate any of these rules.
#'
#' If \code{modcv} is TRUE, the standard deviation used to calculate the
#' thresholds will be replaced with a standard deviation calculated by
#' multiplying \code{CV_star * mean_qual} where \code{mean_qual} is either the
#' value supplied or the value calculated by \code{mean(data_qual)} and
#' \eqn{CV* = 0.06} if \eqn{CV < 0.04}, \eqn{CV* = cv / 2 + 0.04}
#' if \eqn{0.04 <= cv <= 0.08} and \eqn{CV* = CV} otherwise.
#'
#' @examples
#' equiv_mean_extremum(alpha = 0.01, n_sample = 6,
#'                     mean_qual = 100, sd_qual = 5.5, modcv = TRUE)
#' #
#' # Call:
#' # equiv_mean_extremum(alpha = 0.01, n_sample = 6, mean_qual = 100,
#' #                     sd_qual = 5.5, modcv = TRUE)
#' #
#' # Modified CV used: CV* = 0.0675 ( CV = 0.055 )
#' #
#' # For alpha = 0.01 and n = 6
#' # ( k1 = 3.128346 and k2 = 1.044342 )
#' #               Min Individual      Sample Mean
#' # Thresholds:         78.88367         92.95069
#'
#' @seealso
#' \code{\link{k_equiv}}
#'
#' @export
equiv_mean_extremum <- function(alpha, data_sample = NULL, n_sample = NULL,
                                data_qual = NULL, mean_qual = NULL,
                                sd_qual = NULL, modcv = FALSE) {
  res <- list()
  class(res) <- "equiv_mean_extremum"

  res$call <- match.call()

  if (!is.null(data_sample)) {
    if (!is.null(n_sample)) {
      warning("Both data_sample and n_sample were supplied. n_sample ignored.")
    }
    n_sample <- length(data_sample)
  }

  if (!is.null(data_qual)) {
    if (!is.null(mean_qual)) {
      warning("Both data_qual and mean_qual were supplied. mean_qual ignored.")
    }
    mean_qual <- mean(data_qual)

    if (!is.null(sd_qual)) {
      warning("Both data_qual and sd_qual were supplied. sd_qual ignored.")
    }
    sd_qual <- stats::sd(data_qual)
  }

  if (is.null(n_sample)) {
    stop(paste("Number of observations in sample not defined.",
               "You must provide either data_sample or n_sample"))
  }

  if (is.null(mean_qual)) {
    stop(paste("Qualification mean not defined.",
               "You must provide either data_qual or mean_qual"))
  }

  if (is.null(sd_qual)) {
    stop(paste("Qualification SD not defined.",
               "You must provide either data_qual or sd_qual"))
  }

  k <- k_equiv(alpha = alpha, n = n_sample)

  res$alpha <- alpha
  res$n_sample <- n_sample
  res$k1 <- k[1]
  res$k2 <- k[2]

  cv <- sd_qual / mean_qual
  res$modcv <- modcv
  res$cv <- cv
  res$cv_star <- NULL
  if (modcv) {

    if (cv < 0.04) {
      cv <- 0.06
    } else if (cv < 0.08) {
      cv <- cv / 2 + 0.04
    } else {
      cv <- cv
    }
    res$cv_star <- cv
    sd_qual <- cv * mean_qual
  }

  res$threshold_min_indiv <- mean_qual - sd_qual * res$k1
  res$threshold_mean <- mean_qual - sd_qual * res$k2

  res$test_min_indiv <- NULL
  res$test_mean <- NULL
  res$min_sample <- NULL
  res$mean_sample <- NULL
  if (!is.null(data_sample)) {
    res$min_sample <- min(data_sample)
    res$mean_sample <- mean(data_sample)
    res$test_min_indiv <- ifelse(res$min_sample >= res$threshold_min_indiv,
                                 "PASS", "FAIL")
    res$test_mean <- ifelse(res$mean_sample >= res$threshold_mean,
                            "PASS", "FAIL")
  }

  return(res)
}

#' @export
print.equiv_mean_extremum <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  printrow <- function(c1, c2, c3) {
    cat(format(c1, justify = "right", width = 16L, ...),
        format(c2, justify = "right", width = 16L, ...),
        format(c3, justify = "right", width = 16L, ...),
        "\n")
  }

  if (x$modcv) {
    cat("Modified CV used: CV* =", format(x$cv_star, ...),
        "( CV =", format(x$cv, ...),
        ")\n\n")
  }

  cat("For alpha =", format(x$alpha, ...),
       "and n =", format(x$n_sample, ...),
      "\n( k1 =", format(x$k1, ...),
      "and k2 =", format(x$k2, ...), ")\n")
  printrow("", "Min Individual", "Sample Mean")
  if (!is.null(x$min_sample)) {
    printrow("Sample:", x$min_sample, x$mean_sample)
  }
  printrow("Thresholds:", x$threshold_min_indiv, x$threshold_mean)
  if (!is.null(x$test_min_indiv)) {
    printrow("Equivalency:", x$test_min_indiv, x$test_mean)
  }
}


#' Return the k-factors for use in equivalency testing.
#'
#' @param alpha the acceptable probability of a type I error
#' @param n the number of observations in the sample to test for equivalency
#' @return a vector with elements c(k1, k2). k1 is for testing the sample
#'   extremum. k2 is for testing the sample mean
#' @details
#'   The k-factors returned by this function are used for equivalency testing,
#'   considering both the sample mean and sample extremum (normally minimum).
#'   The results of this function match those published by Vangel within
#'   0.05\% for \eqn{n > 2} and \eqn{\alpha \le 0.25}. Those results published
#'   by Vangel are identical to those published in CMH-17-1G.
#'
#'   This function uses numerical integration and numerical optimization to
#'   find values of the factors \eqn{k_1} and \eqn{k_2} based on Vangel's
#'   saddlepoint approximation.
#'
#'   The value \eqn{n} refers to the number of observations in the sample
#'   being compared with the original population (the qualification sample is
#'   usually assumed to be equal to the population statistics).
#'
#'   The value of \eqn{alpha} is the acceptable probability of a type I error.
#'   Normally, this is set to 0.05 for material or process equivalency and 0.01
#'   when setting lot acceptance limits. Though, in principle, this parameter
#'   can be set to any number between 0 and 1. This function, however, has only
#'   been validated in the range of \eqn{1e-5 \le alpha \le 0.5}.
#' @references
#'   M. G. Vangel. Lot Acceptance and Compliance Testing Using the Sample Mean
#'   and an Extremum, Technometrics, vol. 44, no. 3. pp. 242–249.
#' @examples
#' qual_mean <- 100
#' qual_sd <- 3.5
#' k <- k_equiv(0.01, 5)
#' acceptance_limit_min <- qual_mean - qual_sd * k[1]
#' acceptance_limit_mean <- qual_mean - qual_sd * k[2]
#' @export
k_equiv <- function(alpha, n) {
  # If you are making changes to this function, you should run the full
  # validation test in test/test-equiv.R by setting full_test = TRUE.
  # The full validation test is not run by default as it takes several
  # minutes to run, which is impractical for normal test-driven-development
  # workflows.

  if (alpha <= 0) {
    stop("alpha must be positive")
  }
  if (alpha >= 1) {
    stop("alpha must be less than 1")
  }
  if (alpha < 1e-5 || alpha > 0.5) {
    warning(paste("k-factor solution has only been validated",
                  "for 1e-5 <= alpha <= 0.5"))
  }
  if (n < 2) {
    stop("n must be greater than 2")
  }

  # In order to speed up convergence, a linear model was fit to estimate the
  # values of k1 and k2. These estimated values are used as a starting point
  # for the numerical optimization
  theta <- c(0.318 * log(n) - 0.285 * log(alpha) + 1.114,
             -0.16751 * log(n)
             - 0.29011 * log(alpha)
             + 0.08414 * log(n) * log(alpha)
             + 0.65848)

  # The function f returns a penalty value for use in numerical optimization.
  # The first parameter, k, should be a vector of c(k1, k2).
  f <- function(k, n, alpha) {
    Fx1 <- 1 - (stats::pnorm(-k[1], lower.tail = FALSE)) ** n
    Fxbar <- stats::pnorm(sqrt(n) * (-k[2]))

    logh <- function(t) {
      stats::dnorm(t, log = TRUE) - stats::pnorm(t, lower.tail = FALSE,
                                                 log.p = TRUE)
    }
    h <- function(t) {
      exp(stats::dnorm(t, log = TRUE) - stats::pnorm(t, lower.tail = FALSE,
                                                     log.p = TRUE))
    }
    h_minus_t <- function(t) {
      ifelse(t > 60, t ** -1, h(t) - t)
    }

    lambda_hat <- stats::uniroot(
      function(lambda) {
        ( (n - 1) / n) * h_minus_t(lambda) - k[1] + k[2]
      },
      interval = c(-1000, 1000),
      extendInt = "yes"
    )$root

    A <- function(t, n) {
      hmt <- h_minus_t(t)
      exp(
        - (n - 1) * logh(t) +
          (n - 1) ** 2 / (2 * n) * hmt ** 2 +
          (n - 1) * t * hmt
      ) * sqrt(ifelse(t > 60, t ** -2, 1 - h(t) * hmt))
    }

    Fx1xbar.numerator <- stats::pnorm(sqrt(n) * (-k[2])) * stats::integrate(
      function(t) {
        A(t, n)
      },
      lower = -Inf,
      upper = lambda_hat,
      subdivisions = 1000L
    )$value + stats::integrate(
      function(t) {
        stats::pnorm(sqrt(n) * (-k[1] + (n - 1) / n * h_minus_t(t))) * A(t, n)
      },
      lower = lambda_hat,
      upper = Inf,
      subdivisions = 1000L,
      rel.tol = 1e-8
    )$value

    Fx1xbar.denominator <- stats::integrate(function(t) A(t, n),
                                            lower = -Inf, upper = Inf)$value
    Fx1xbar <- Fx1xbar.numerator / Fx1xbar.denominator

    # Use the sum of the absolute values of the two functions being solved as
    # the penalty function. However, it was found empirically that the first
    # function listed is more sensitive, so give it a higher weight to aid in
    # finding hte correct solution
    return(
      abs(Fx1 + Fxbar - Fx1xbar - alpha) * 100 + abs(Fx1 - Fxbar)
    )
  }

  # Perform the constrained numeric optimization to find the values of k1 and
  # k2. The constrait enforces k1 > k2, which is required such that all values
  # are real
  res <- stats::constrOptim(
    theta,
    f,
    grad = NULL,
    ui = c(1, -1),
    ci = 0,
    n = n,
    alpha = alpha
  )
  if (res$convergence != 0) {
    warning("k-factor search did not converge. The results may be unreliable.")
  }

  return(res$par)
}
