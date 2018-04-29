
#' Anderson-Darling test for goodness of fit
#'
#' @description
#' TODO: Write this
#'
#' @param x a vector whos distribution is being evaluated
#' @param dist the cummulative distribution function considered
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

#'
#'
#' @references
#' G. Marsaglia and J. Marsaglia, “Evaluating the Anderson-Darling
#' Distribution,” Journal of Statistical Software, vol. 9, no. 2. 25-Feb-2004.
#'
#' @export
ad_inf <- function(z, abs_tol = .Machine$double.eps ^ 0.5) {
  max_iter <- 100

  warning_message <- FALSE

  adinf <- 0

  for (j in 0:max_iter) {
    ad_inc_mult <- choose(-1 / 2, j) * (4 * j + 1)

    # Calculate f
    t <- (4 * j + 1) ^ 2 * pi ^ 2 / (8 * z)
    cm2 <- pi * exp(-t) * (2 * t) ^ (-1 / 2)
    cm1 <- pi * (pi / 2) ^ (1 / 2) * pracma:::erfc(t ^ (1 / 2))
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
