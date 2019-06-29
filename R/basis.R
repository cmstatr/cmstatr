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
  t <- qt(conf, df = n - 1, ncp = z * sqrt(n))
  return(t / sqrt(n))
}

#' Calculate basis values
#'
#' @description
#' Calculate the basis value for a given data set. There are various functions
#' to calculate the basis values for different distributions. For B-Basis,
#' set \eqn{p=0.90} and \eqn{conf=0.95}; for A-Basis, set \eqn{p=0.90} and
#' \eqn{conf=0.95}
#'
#' @param data a data.frame
#' @param x the variable in the data.frame for which to find the basis value
#' @param groups the variable in the data.frame representing the groups
#' @param p should be 0.90 for B-Basis and 0.99 for A-Basis
#' @param conf confidence interval. Should be 0.95 for both A- and B-Basis
#'
#' @details
#' \code{data} is an optional argument. If \code{data} is given, it should
#' be a
#' \code{data.frame} (or similar object). When \code{data} is specified, the
#' value of \code{x} is expected to be a variable within \code{data}. If
#' \code{data} is not specified, \code{x} must be a vector.
#'
#' \code{basis_normal} calculate the basis value by subtracting \eqn{k} times
#' the standard deviation from the mean using. \eqn{k} is given by
#' the function \code{\link{k_factor_normal}}.
#'
#' \code{basis_lognormal} calculates the basis value in the same way
#' that \code{basis_normal} does, except that the natural logarithm of the
#' data is taken.
#'
#' \code{basis_weibull} calculates the basis value for data distributed
#' according to a Weibull distribution. The confidence interval for the
#' quantile requested is calculated using the conditional method, as
#' described in Lawless (1982) Section 4.1.2b. This has good agreement
#' with tables published in CMH-17-1G. Results differ between this function
#' and STAT17 by approximately 0.5%.
#'
#' \code{basis_pooled_sd} calculates basis values by pooling the data from
#' several groups together. \code{x} specifies the data (normally strength)
#' and \code{group} indicates the group corresponding with each observation.
#' This method is described in CMH-17-1G and matches the pooling method
#' implemented in ASAP.
#'
#' @return an object of class \code{basis}
#' This object has the following fields:
#' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{distribution}}{the distribution used (normal, etc.)}
#'   \item{\code{p}}{the value of \eqn{p} supplied}
#'   \item{\code{conf}}{the value of \eqn{conf} supplied}
#'   \item{\code{data}}{a copy of the data used in the calculation}
#'   \item{\code{groups}}{a copy of the groups variable.
#'                        Only used for pooling methods.}
#'   \item{\code{n}}{the number of observations}
#'   \item{\code{r}}{the number of groups, if a pooling method was used.
#'                   Otherwise it is NULL.}
#'   \item{\code{basis}}{the basis value computed. This is a number
#'                       except when pooling methods are used, in
#'                       which case it is a data.frame.}
#' }
#'
#' @references
#' J. F. Lawless, Statistical Models and Methods for Lifetime Data.
#' New York: John Wiley & Sons, 1982.
#'
#' “Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @name basis
NULL

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats sd
#' @export
basis_normal <- function(data = NULL, x, p = 0.90, conf = 0.95) {
  res <- list()
  class(res) <- "basis"

  res$call <- match.call()
  res$distribution <- "Normal"
  res$p <- p
  res$conf <- conf
  res$groups <- NULL

  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  res$data <- eval_tidy(enquo(x), data)

  res$n <- length(res$data)
  k <- k_factor_normal(n = res$n, p = p, conf = conf)
  res$basis <- mean(res$data) - k * sd(res$data)

  return(res)
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats sd
#' @export
basis_lognormal <- function(data = NULL, x, p = 0.90, conf = 0.95) {
  res <- list()
  class(res) <- "basis"

  res$call <- match.call()
  res$distribution <- "Lognormal"
  res$p <- p
  res$conf <- conf
  res$groups <- NULL

  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  res$data <- eval_tidy(enquo(x), data)

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

  cat("( n = ", x$n)
  if (!is.null(x$r)) {
    cat(", r = ", x$r)
  }
  cat(" )\n")

  if (x$conf == 0.95 & x$p == 0.9) {
    cat("B-Basis: ", " ( p = ", x$p, ", conf = ", x$conf, ")\n")
  }
  else if (x$conf == 0.95 & x$p == 0.99) {
    cat("A-Basis: ", " ( p = ", x$p, ", conf = ", x$conf, ")\n")
  }
  else {
    cat("Basis: ", " ( p = ", x$p, ", conf = ", x$conf, ")\n")
  }

  if (is.numeric(x$basis)) {
    cat(x$basis, "\n")
  } else if (is.data.frame(x$basis)) {
    col_width <- max(nchar(as.character(x$basis[["group"]]))) + 2
    for (j in 1:nrow(x$basis)) {
      cat(format(x$basis[["group"]][j], width = col_width))
      cat(x$basis[["value"]][j], "\n")
    }
  } else {
    stop("`basis` is an unexpected data type")
  }

  cat("\n")
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats qweibull integrate pchisq uniroot
#' @importFrom MASS fitdistr
#'
#' @export
basis_weibull <- function(data = NULL, x, p = 0.90, conf = 0.95) {
  res <- list()
  class(res) <- "basis"

  res$call <- match.call()
  res$distribution <- "Weibull"
  res$p <- p
  res$conf <- conf
  res$groups <- NULL

  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  res$data <- eval_tidy(enquo(x), data)

  res$n <- length(res$data)

  dist <- fitdistr(res$data, "weibull")

  alpha_hat <- dist$estimate[["scale"]]
  beta_hat <- dist$estimate[["shape"]]

  # The data must be transformed to fit an extreme value distribution
  data_evd <- log(res$data)
  u_hat <- log(alpha_hat)
  b_hat <- 1 / beta_hat

  # Next, find the ancillary statistic for the data
  a <- (data_evd - u_hat) / b_hat

  k_integrand <- function(z) {
    return(
      z ^ (res$n - 2) * exp( (z - 1) * sum(a)) /
        ( (1 / res$n) * sum( exp(a * z))) ^ res$n
    )
  }

  k_inv <- integrate(Vectorize(k_integrand), lower = 0, upper = Inf)
  k <- 1 / k_inv$value

  incomplete_gamma <- function(ki, xi) {
    pchisq(xi * 2, ki * 2)
  }

  h2 <- function(z) {
    return(
      k * z ^ (res$n - 2) * exp( (z - 1) * sum(a)) /
        ( (1 / res$n) * sum( exp(a * z))) ^ res$n
    )
  }

  wp <- log(-log(p))

  pr_zp_integrand <- function(z, t) {
    h2(z) * incomplete_gamma(res$n, exp(wp + t * z) * sum(exp(a * z)))
  }

  pr_fcn <- function(t) {
    int_res <- integrate(Vectorize(function(z) pr_zp_integrand(z, t)), 0, Inf)
    return(int_res$value - 0.95)
  }

  res_root <- uniroot(pr_fcn, c(-10, 10), extendInt = "yes")

  res$basis <- exp(u_hat - res_root$root * b_hat)

  return(res)
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @export
basis_pooled_sd <- function(data = NULL, x, groups, p = 0.90, conf = 0.95) {
  res <- list()
  class(res) <- "basis"

  res$call <- match.call()
  res$distribution <- "Normal - Pooled Standard Deviation"
  res$p <- p
  res$conf <- conf

  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  res$data <- eval_tidy(enquo(x), data)

  verify_tidy_input(
    df = data,
    x = groups,
    c = match.call(),
    arg_name = "groups")
  res$groups <- eval_tidy(enquo(groups), data)

  res$n <- length(res$data)
  res$r <- length(levels(as.factor(res$groups)))

  pooled_sd <- sqrt(
    sum(
      sapply(levels(as.factor(res$groups)), function(g) {
        xj_bar <- mean(res$data[res$groups == g])
        sum( (res$data[res$groups == g] - xj_bar) ^ 2)
      })
    ) / (res$n - res$r))

  basis <- sapply(levels(as.factor(res$groups)), function(g) {
    nj <- length(res$data[res$groups == g])
    z <- qnorm(p)
    kj <- qt(conf, df = res$n - res$r, ncp = z * sqrt(nj)) / sqrt(nj)
    xj_bar <- mean(res$data[res$groups == g])
    xj_bar - kj * pooled_sd
  })

  res$basis <- data.frame(group = names(basis), value = basis)

  return(res)
}
