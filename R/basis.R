


#' Calculate k factor for basis values (kB, kA) with normal distribution
#'
#' @param n the number of observations (ie. coupons)
#' @param p should be 0.90 for B-Basis and 0.99 for A-Basis
#' @param conf confidence interval. Should be 0.95 for both A- and B-Basis
#'
#' @details
#' This function calculates the k factors for use in determining A- and
#' B-Basis values for normally distributed data. To get kB, set p = 0.90
#' and conf = 0.95. To get kA, set p = 0.99 and conf = 0.95.
#'
#' This function has been validated against the kB tables in
#' CMH-17-1G for each value of n from n = 2 to n = 95. It has been validated
#' against the kA tables in CMH-17-1G for each value of n from n = 2 to
#' n = 75. Larger values of n also match the tables in CMH-17-1G, but R
#' emits warnings that "full precision may not have been achieved." When
#' validating the results of this function against the tables in CMH-17-1G,
#' the maximum allowable difference between the two is 0.002. The tables in
#' CMH-17-1G give values to three decimal places.
#'
#' @return the calculated factor
#' @export
basis_factor_normal <- function(n, p = 0.90, conf = 0.95) {
  z <- qnorm(p)
  t <- qt(0.95, df = n - 1, ncp = z * sqrt(n))
  return(t / sqrt(n))
}
