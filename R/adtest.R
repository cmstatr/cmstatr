
#' Anderson-Darling test for goodness of fit
#'
#' @description
#' TODO: Write this
#'
#' @param x a numeric vector
#' @param dist the cumulative distribution function considered
#' @param ... additional arguments to pass to \code{dist}
#'
#' @return
#'
#' @details
#' TODO: Write this
#'
#' @references
#' J. F. Lawless, \emph{Statistical models and methods for lifetime data}.
#' New York: Wiley, 1982.
#'
#' @export
anderson_darling <- function(x, dist, ...) {
  if (is.function(dist)) {
    F0 <- dist
  } else if (is.character(dist)) {
    F0 <- get(dist, mode = "function")
  } else {
    stop("dist must be a function or character string with the function name")
  }

  stopifnot(is.numeric(x))

  x <- sort(x)
  n <- length(x)
  ii <- 1:n
  U <- F0(x, ...)
  An2 <- -n - sum( (2 * ii) / n * (log(U) + log(1 - rev(U))))
  return(An2)
}



#' @rdname ad_p
#' @export
ad_p_inf <- function(z, abs_tol = .Machine$double.eps ^ 0.5) {
  max_iter <- 100

  warning_message <- FALSE

  adinf <- 0

  for (j in 0:max_iter) {
    ad_inc_mult <- choose(-1 / 2, j) * (4 * j + 1)

    # Calculate f
    t <- (4 * j + 1) ^ 2 * pi ^ 2 / (8 * z)
    cm2 <- pi * exp(-t) * (2 * t) ^ (-1 / 2)
    cm1 <- pi * (pi / 2) ^ (1 / 2) * erfc(t ^ (1 / 2))
    f <- cm2 + cm1 * z / 8
    for (n in 2:max_iter) {
      cn <- ( (n - 1 / 2 - t) * cm1 + t * cm2) / n
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
  }
  if (j >= max_iter) warning_message <- TRUE

  if (warning_message) {
    warning(paste0(
      "Solution did not converge within requested tolerance (abs.tol=",
      abs_tol,
      ")"
    ))
  }

  return(1.0 - adinf / z)
}

#' \eqn{\alpha} from Anderson-Darling test statistic
#'
#' @description
#' Calculates the significance level (\eqn{\alpha}) from an Anderson-Darling
#' test statistic.
#'
#' @param z the test statistic
#' @param n the sample size
#' @param abs_tol the requested accuracy of the calculation
#'
#' @return the significance level (\eqn{\alpha}).
#'
#' @details
#' \code{ad_p_inf} uses the limiting distribution (i.e. for infinite sample
#' size). \code{ad_p} applies a correction for sample size. Since the
#' Anderson-Darling test statistic converges very quickly, the correction
#' only has a practical effect for very small sample sizes (around 5 or less).
#' Nevertheless, it is recommended that \code{ad_p} be used as the correction
#' is not computationally intensive to apply.
#'
#' The method of Marsaglia and Marsaglia is used to compute the significance
#' level. See references below.
#'
#' @references
#' G. Marsaglia and J. Marsaglia, “Evaluating the Anderson-Darling
#' Distribution,” Journal of Statistical Software, vol. 9, no. 2. 25-Feb-2004.
#'
#' @importFrom pracma erfc
#'
#' @name ad_p
#' @export
ad_p <- function(z, n, abs_tol = .Machine$double.eps ^ 0.5) {
  p <- 1.0 - ad_p_inf(z, abs_tol = abs_tol)

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
