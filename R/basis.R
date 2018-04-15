


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
#' @export
k_factor_normal <- function(n, p = 0.90, conf = 0.95) {
  z <- qnorm(p)
  t <- qt(0.95, df = n - 1, ncp = z * sqrt(n))
  return(t / sqrt(n))
}
