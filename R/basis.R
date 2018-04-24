#' Calculate k factor for basis values (\eqn{kB}, \eqn{kA}) with normal
#' distribution
#'
#' @param n the number of observations (i.e. coupons)
#' @param p should be 0.90 for B-Basis and 0.99 for A-Basis
#' @param conf confidence interval. Should be 0.95 for both A- and B-Basis
#'
#' @details
#' This function calculates the k factors for use in determining A- and
#' B-Basis values for normally distributed data. To get \eqn{kB}, set
#' \code{p = 0.90} and \code{conf = 0.95}. To get \eqn{kA}, set
#' \code{p = 0.99} and \code{conf = 0.95}.
#'
#' This function has been validated against the \eqn{kB} tables in
#' CMH-17-1G for each value of \eqn{n} from \eqn{n = 2} to \eqn{n = 95}.
#' It has been validated against the \eqn{kA} tables in CMH-17-1G for each
#' value of \eqn{n} from \eqn{n = 2} to \eqn{n = 75}. Larger values of \eqn{n}
#' also match the tables in CMH-17-1G, but R
#' emits warnings that "full precision may not have been achieved." When
#' validating the results of this function against the tables in CMH-17-1G,
#' the maximum allowable difference between the two is 0.002. The tables in
#' CMH-17-1G give values to three decimal places.
#'
#' @return the calculated factor
#'
#' @importFrom stats qnorm qt
#'
#' @export
k_factor_normal <- function(n, p = 0.90, conf = 0.95) {
  z <- qnorm(p)
  t <- qt(0.95, df = n - 1, ncp = z * sqrt(n))
  return(t / sqrt(n))
}

#' Calculate basis values (\eqn{kB}, \eqn{kA})
#'
#' @description
#' Calculate the basis value for a given data set. There are various functions
#' to calculate the basis values for different distributions. For \eqn{kB},
#' set \eqn{p=0.90} and \eqn{conf=0.95}; for \eqn{kA}, set \eqn{p=0.90} and
#' \eqn{conf=0.95}
#'
#' @param df a data.frame
#' @param x the variable in the data.frame for which to find the basis value
#' @param p should be 0.90 for B-Basis and 0.99 for A-Basis
#' @param conf confidence interval. Should be 0.95 for both A- and B-Basis
#'
#' @details
#' \code{df} is an optional argument. If \code{df} is given, it should be a
#' \code{data.frame} (or similar object). When \code{df} is specified, the
#' value of \code{x} is expected to be a variable within \code{df}. If
#' \code{df} is not specified, \code{x} must be a vector.
#'
#' \code{basis_normal} calculate the basis value by subtracting \eqn{k} times
#' the standard deviation from the mean using. \eqn{k} is given by
#' the function \code{\link{k_factor_normal}}.
#'
#' \code{basis_lognormal} calculates the basis value in the same way
#' that \code{basis_normal} does, except that the natural logarithm of the
#' data is taken.
#'
#' @return an object of class \code{basis}
#' This object has the following fields:
#' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{distribution}}{the distribution used (normal, etc.)}
#'   \item{\code{p}}{the value of \eqn{p} supplied}
#'   \item{\code{conf}}{the value of \eqn{conf} supplied}
#'   \item{\code{data}}{a copy of the data used in the calculation}
#'   \item{\code{n}}{the number of observations}
#'   \item{\code{basis}}{the basis value computed}
#' }
#'
#' @name basis
NULL


#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats sd
#' @export
basis_normal <- function(df = NULL, x, p = 0.90, conf = 0.95) {
  res <- list()
  class(res) <- "basis"

  res$call <- match.call()
  res$distribution <- "Normal"
  res$p <- p
  res$conf <- conf

  x <- enquo(x)
  res$data <- eval_tidy(x, df)
  res$n <- length(res$data)
  k <- k_factor_normal(n = res$n, p = p, conf = conf)
  res$basis <- mean(res$data) - k * sd(res$data)

  return(res)
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats sd
#' @export
basis_lognormal <- function(df = NULL, x, p = 0.90, conf = 0.95) {
  res <- list()
  class(res) <- "basis"

  res$call <- match.call()
  res$distribution <- "Lognormal"
  res$p <- p
  res$conf <- conf

  x <- enquo(x)
  res$data <- eval_tidy(x, df)
  res$n <- length(res$data)
  k <- k_factor_normal(n = res$n, p = p, conf = conf)
  res$basis <- exp(mean(log(res$data)) - k * sd(log(res$data)))

  return(res)
}

#' @export
print.basis <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Distribution: ", x$distribution, "\t")
  cat("( n = ", x$n, ")\n")

  if (x$conf == 0.95 & x$p == 0.9) {
    cat("B-Basis: ", x$basis, " ( p = ", x$p, ", conf = ", x$conf, ")\n\n")
  }
  else if (x$conf == 0.95 & x$p == 0.99) {
    cat("A-Basis: ", x$basis, " ( p = ", x$p, ", conf = ", x$conf, ")\n\n")
  }
  else {
    cat("Basis: ", x$basis, " ( p = ", x$p, ", conf = ", x$conf, ")\n\n")
  }
}
