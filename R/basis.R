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
#' @param method the method for Hanson-Koopmans nonparametric basis values.
#'               should be "optimum-order" for B-Basis and "woodward-frawley"
#'               for A-Basis.
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
#' \code{basis_hk_ext} calculates the basis value using the Extended
#' Hanson-Koopmans method, as described in CMH-17-1G and Vangel (1994).
#' For nonparametric distributions, this function should be used for samples
#' up to n=28 for B-Basis and up to 299 for A-Basis.
#' This method uses a pair of order statistics to determine the basis value.
#' CMH-17-1G suggests that for A-Basis, the first and last order statistic
#' is used: this is called the "woodward-frawley" method in this package,
#' after the paper in which this approach is described (as referenced
#' by Vangel (1994)). For B-Basis, another approach is used whereby the
#' first and \code{j-th} order statistic are used to calculate the basis value.
#' In this approach, the \code{j-th} order statistic is selected to minimize
#' the difference between the tolerance limit (assuming that the order
#' statistics are equal to the expected values from a standard normal
#' distribution) and the population quantile for a standard normal
#' distribution. This approach is described in Vangel (1994). The results
#' of this function have been
#' verified against example results from the program STAT-17: agreement is
#' typically well within 0.2%, however, for a few sample sizes, the
#' agreement can be as poor as 1% with the result of this function being
#' more conservative than STAT-17.
#'
#' \code{basis_nonpara_large_sample} calculates the basis value
#' using the large sample method described in CMH-17-1G. This method uses
#' a sum of binomials to determine the rank of the ordered statistic
#' corresponding with the desired tolerance limit (basis value). Results
#' of this function have been verified against results of the STAT-17
#' program.
#'
#' \code{basis_anova} calculates basis values using the ANOVA method.
#' \code{x} specifies the data (normally strength) and \code{group}
#' indicates the group corresponding to each observation. This method is
#' described in CMH-17-1G. This function has been verified against the
#' results of the STAT-17 program.
#'
#' \code{basis_pooled_sd} calculates basis values by pooling the data from
#' several groups together. \code{x} specifies the data (normally strength)
#' and \code{group} indicates the group corresponding to each observation.
#' This method is described in CMH-17-1G and matches the pooling method
#' implemented in ASAP 2008.
#'
#' \code{basis_pooled_cv} calculates basis values by pooling the data from
#' several groups together. \code{x} specifies the data (normally strength)
#' and \code{group} indicates the group corresponding to each observation.
#' This method is described in CMH-17-1G.
#'
#' @return an object of class \code{basis}
#' This object has the following fields:
#' \item{\code{call}}{the expression used to call this function}
#' \item{\code{distribution}}{the distribution used (normal, etc.)}
#' \item{\code{p}}{the value of \eqn{p} supplied}
#' \item{\code{conf}}{the value of \eqn{conf} supplied}
#' \item{\code{data}}{a copy of the data used in the calculation}
#' \item{\code{groups}}{a copy of the groups variable.
#'                      Only used for pooling and ANOVA methods.}
#' \item{\code{n}}{the number of observations}
#' \item{\code{r}}{the number of groups, if a pooling method was used.
#'                 Otherwise it is NULL.}
#' \item{\code{basis}}{the basis value computed. This is a number
#'                     except when pooling methods are used, in
#'                     which case it is a data.frame.}
#'
#' @seealso \code{\link{hk_ext_z_j_opt}}
#' @seealso \code{\link{k_factor_normal}}
#'
#' @references
#' J. F. Lawless, Statistical Models and Methods for Lifetime Data.
#' New York: John Wiley & Sons, 1982.
#'
#' “Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' M. Vangel, “One-Sided Nonparametric Tolerance Limits,”
#' Communications in Statistics - Simulation and Computation,
#' vol. 23, no. 4. pp. 1137–1154, 1994.
#'
#' @name basis
NULL

new_basis <- function() {
  res <- list()
  class(res) <- "basis"

  res$call <- NA
  res$distribution <- NA
  res$p <- NA
  res$conf <- NA
  res$groups <- NA
  res$data <- NA
  res$n <- NA
  res$r <- NA
  res$basis <- NA

  return(res)
}


#' Glance at a basis object
#'
#' @description
#' Glance accepts an object of type basis and returns a
#' \code{\link[tibble:tibble]{tibble::tibble}} with
#' one row of summaries.
#'
#' Glance does not do any calculations: it just gathers the results in a
#' tibble.
#'
#' @param x a basis object
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#'
#' @return
#' A one-row \code{\link[tibble:tibble]{tibble::tibble}} with the following
#' columns:
#'
#' \item{\code{p}}{the the proportion of the population that the basis value
#'        should be below. Normally 0.90 or 0.99}
#' \item{\code{conf}}{The confidence level. Normally 0.95}
#' \item{\code{distribution}}{A string representing the distribution assumed
#'        when calculating the basis value}
#' \item{\code{r}}{the sample size}
#' \item{\code{r}}{the number of groups used in the calculation. This will
#'        be \code{NA} for single-point basis values}
#' \item{\code{basis}}{the basis value}
#'
#'
#' @seealso
#' \code{\link{basis}}
#'
#' @examples
#' x <- rnorm(20, 100, 5)
#' b <- basis_normal(x = x)
#' glance(b)
#'
#' # ## A tibble: 1 x 6
#' #       p  conf distribution  n r     basis
#' #   <dbl> <dbl> <chr>     <int> <lgl> <dbl>
#' # 1   0.9  0.95 Normal       20 NA    87.8
#'
#' @method glance basis
#' @importFrom tibble tibble
#'
#' @export
glance.basis <- function(x, ...) {  # nolint
  with(
    x,
    tibble::tibble(
      p = p,
      conf = conf,
      distribution = distribution,
      n = n,
      r = r,
      basis = basis
    )
  )
}

#' @export
print.basis <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Distribution: ", x$distribution, "\t")

  cat("( n = ", x$n)
  if (!is.null(x$r) & !is.na(x$r)) {
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
    for (j in seq(along.with = x$basis$group)) {
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
#' @importFrom stats sd
#' @export
basis_normal <- function(data = NULL, x, p = 0.90, conf = 0.95) {
  res <- new_basis()

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
  res <- new_basis()

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
    for (j in seq(along.with = x$basis$group)) {
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
  res <- new_basis()

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
      z ^ (res$n - 2) * exp((z - 1) * sum(a)) /
        ((1 / res$n) * sum(exp(a * z))) ^ res$n
    )
  }

  k_inv <- integrate(Vectorize(k_integrand), lower = 0, upper = Inf)
  k <- 1 / k_inv$value

  incomplete_gamma <- function(ki, xi) {
    pchisq(xi * 2, ki * 2)
  }

  h2 <- function(z) {
    return(
      k * z ^ (res$n - 2) * exp((z - 1) * sum(a)) /
        ((1 / res$n) * sum(exp(a * z))) ^ res$n
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
basis_pooled_cv <- function(data = NULL, x, groups, p = 0.90, conf = 0.95) {
  res <- new_basis()

  res$call <- match.call()
  res$distribution <- "Normal - Pooled CV"
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

  norm_data <- normalize_group_mean(res$data, res$groups)
  res$n <- length(res$data)
  res$r <- length(levels(as.factor(res$groups)))

  pooled_sd <- sqrt(sum((norm_data - 1) ^ 2) / (res$n - res$r))

  basis <- sapply(levels(as.factor(res$groups)), function(g) {
    nj <- length(res$data[res$groups == g])
    z <- qnorm(p)
    kj <- qt(conf, df = res$n - res$r, ncp = z * sqrt(nj)) / sqrt(nj)
    xj_bar <- mean(res$data[res$groups == g])
    xj_bar * (1 - kj * pooled_sd)
  })

  res$basis <- data.frame(group = names(basis), value = basis)

  return(res)
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @export
basis_pooled_sd <- function(data = NULL, x, groups, p = 0.90, conf = 0.95) {
  res <- new_basis()

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
        sum((res$data[res$groups == g] - xj_bar) ^ 2)
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

#' @importFrom stats pbeta dbeta
hk_ext_h <- function(z, n, i, j, p) {
  if (!(1 <= i && i < j && j <= n)) {
    stop("Error: The condition 1 <= i < j <= n must be true.")
  }

  # for z >= 1
  qb <- pbeta(1 - p, j, n - j + 1)
  int <- integrate(function(t) {
      pbeta(((1 - p) / t) ^ (1 / z), i, j - i) * dbeta(t, j, n - j + 1)
    },
    lower = 1 - p, upper = 1)
  if (int$message != "OK") {
    warning(int$message)
  }
  qb + int$value
}

#' Calculate values related to the Extended Hanson-Koopmans method
#'
#' @description
#' Calculates values related to the Extended Hanson-Koopmans method
#' as described by Vangel (1994).
#'
#' @param n the sample size
#' @param i the first order statistic (1 <= i < j)
#' @param j the second order statistic (i < j <= n)
#' @param p the population quantile of interest (normally 0.90 or 0.99)
#' @param conf the confidence bound (normally 0.95)
#'
#' @return
#' For \code{hk_ext_z}, the return value is a numeric value representing
#' the parameter z (denoted as k in CMH-17-1G).
#'
#' For \code{hk_ext_z_j_opt}, the return value is named list containing
#' \code{z} and \code{k}. The former is the value of z, as defined by
#' Vangel (1994), and the latter is the corresponding order statistic.
#'
#' @details
#' Hanson (1964) presents a nonparametric method for determining
#' tolerance limits based on consecutive order statistics.
#' Vangel (1994) extends this method using non-consecutive order statistics.
#'
#' The extended Hanson-Koopmans method calculates a tolerance limit
#' (basis value) based on two order statistics and a weighting value
#' \code{z}. The value of \code{z} is based on the sample size, which
#' order statistics are selected, the desired quantile and the desired
#' confidence interval.
#'
#' The function \code{hk_ext_z} calculates the weighting variable \code{z}
#' based on selected order statistics \code{i} and \code{j}.
#'
#' The function \code{hk_ext_z_j_opt} determines the value of \code{j} and
#' the corresponding value of \code{z}, assuming \code{i=1}. The value
#' of \code{j} is selected such that the computed tolerance limit is
#' nearest to the desired population quantile for a standard normal
#' distribution when the order statistics are equal to the expected
#' value of the order statistics for the standard normal distribution.
#'
#' @references
#' M. Vangel, “One-Sided Nonparametric Tolerance Limits,”
#' Communications in Statistics - Simulation and Computation,
#' vol. 23, no. 4. pp. 1137–1154, 1994.
#'
#' D. L. Hanson and L. H. Koopmans,
#' “Tolerance Limits for the Class of Distributions with Increasing
#' Hazard Rates,” The Annals of Mathematical Statistics,
#' vol. 35, no. 4. pp. 1561–1570, 1964.
#'
#' @name hk_ext

#' @rdname hk_ext
#' @export
hk_ext_z <- function(n, i, j, p, conf) {
  res <- uniroot(
    function(z) {
      hk_ext_h(z, n, i, j, p) - conf
    },
    lower = 1, upper = 10,
    extendInt = "upX")
  z <- res$root
  z
}

#' @rdname hk_ext
#' @export
hk_ext_z_j_opt <- function(n, p, conf) {
  i <- 1  # i is always 1

  # an approximation of the expected value of the order statistic
  # for a standard normal distribution. This approximation is
  # known to be incorrect, especially in the tails of the
  # distribution. However, it appears to be close enough
  # for the purposes of determining which value of j is optimum
  # per Vangel's approach.
  expected_order_statistic <- function(i, n) {
    qnorm((i - 0.5) / n)
  }

  # Try all the allowable values of j to find the value of T
  # that is closest to the population quantile for a standard
  # normal distribution
  j <- (i + 1):n
  z_vals <- sapply(j, function(ji) {
    hk_ext_z(n, i, ji, p, conf)
  })

  err_vals <- sapply(seq(along.with = j), function(index) {
    ji <- j[index]
    zi <- z_vals[index]
    e1 <- expected_order_statistic(i, n)
    e2 <- expected_order_statistic(ji, n)
    abs(zi * e1 + (1 - zi) * e2 - qnorm(p))
  })

  list(
    z = z_vals[err_vals == min(err_vals)],
    j = j[err_vals == min(err_vals)]
  )
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#'
#' @export
basis_hk_ext <- function(data = NULL, x, p = 0.90, conf = 0.95,
                       method = c("optimum-order", "woodward-frawley")) {
  method <- match.arg(method)
  res <- new_basis()

  res$call <- match.call()
  res$distribution <- paste0(
    "Nonparametric (Extended Hanson-Koopmans, ",
    ifelse(method == "optimum-order", "optimum two-order-statistic method",
           "Woodward-Frawley method"),
    ")")
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

  if (method == "optimum-order") {
    zj <- hk_ext_z_j_opt(res$n, p, conf)
    z <- zj$z
    j <- zj$j
  } else if (method == "woodward-frawley") {
    j <- res$n
    z <- hk_ext_z(res$n, 1, j, p, conf)
  } else {
    stop("Invalid value for method.")
  }

  x_ordered <- sort(res$data)
  res$basis <- x_ordered[j] * (x_ordered[1] / x_ordered[j]) ^ z

  return(res)
}

#' Rank for distribution-free tolerance limits
#'
#' @description
#' Calculates the rank order for finding distribution-free tolerance
#' limits for large samples. This function should only be used for
#' computing B-Basis for samples larger than 28 or A-Basis for samples
#' larger than 298. This function is used by
#' \code{\link{basis_nonpara_large_sample}}.
#'
#' @param n the sample size
#' @param p the desired quantile for the tolerance limit
#' @param conf the confidence limit for the desired tolerance limit
#'
#' @return
#' The rank corresponding with the desired tolerance limit
#'
#' @details
#' This function uses the sum of binomial terms to determine the rank
#' of the ordered statistic that corresponds with the desired tolerance
#' limit. This approach does not assume any particular distribution. This
#' approach is described by Guenther (1969) and by CMH-17-1G.
#'
#' The results of this function have been verified against the tables in
#' CMH-17-1G and agreement was found for all sample sizes published in
#' CMH-17-1G for both A- and B-Basis, as well as the sample sizes
#' \code{n+1} and \code{n-1}, where
#' \code{n} is the sample size published in CMH-17-1G.
#'
#' The tables in CMH-17-1G purportedly list the smallest sample sizes
#' for which a particular rank can be used. That is, for a sample size
#' one less than the \code{n} published in the table, the next lowest rank
#' would be used. In some cases, the results of this function disagree by a
#' rank of one for sample sizes one less than the \code{n} published in the
#' table. This indicates a disagreement in that sample size at which
#' the rank should change. This is likely due to numerical
#' differences in this function and the procedure used to generate the tables.
#' However, the disagreement is limited to sample 6500 for A-Basis; no
#' discrepancies have been noted for B-Basis. Since these sample sizes are
#' uncommon for composite materials
#' testing, and the difference between subsequent order statistics will be
#' very small for samples this large, this difference will have no practical
#' effect on computed tolerance limits.
#'
#' @references
#' W. Guenther, “Determination of Sample Size for Distribution-Free
#' Tolerance Limits,” Jan. 1969
#'
#' “Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @seealso \code{\link{basis_nonpara_large_sample}}
#'
#' @export
nonpara_binomial_rank <- function(n, p, conf) {
  p <- 1 - p

  e_fcn <- function(r) {
    sum(sapply(r:n, function(w) {
      exp(lchoose(n, w) + w * log(p) + (n - w) * log(1 - p))
    }))
  }

  r1 <- 1
  e1 <- e_fcn(r1)

  if (e1 < conf) {
    stop(paste0(
      "Sample size ", n, " is too small to compute a non-parametric ",
      "tolerance limit for p=", p, " and conf=", conf))
  }

  r2 <- n
  e2 <- e_fcn(r2)

  if (e2 > conf) {
    stop(paste0(
      "No rank found for n=", n, ", p=", p, " conf=", conf))
  }

  for (i in 1:n) {
    # use a for loop just to give it a limit to prevent infinite loope
    if (abs(r2 - r1) == 1) {
      break
    }
    rm <- round((r1 + r2) / 2, digits = 0)
    em <- e_fcn(rm)

    # nolint start
    # We know that the following holds, and we want this to continue to hold:
    # E1 > conf
    # E2 < conf
    # nolint end
    if (em > conf) {
      r1 <- rm
      e1 <- em
    } else {
      r2 <- rm
      e2 <- em
    }
  }
  r1
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#'
#' @export
basis_nonpara_large_sample <- function(data = NULL, x, p = 0.90,
                                       conf = 0.95) {
  res <- new_basis()

  res$call <- match.call()
  res$distribution <- "Nonparametric (large sample)"
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

  x_ordered <- sort(res$data)
  r <- nonpara_binomial_rank(res$n, p, conf)
  res$basis <- x_ordered[r]

  return(res)
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @export
basis_anova <- function(data = NULL, x, groups, p = 0.90, conf = 0.95) {
  # TODO: Must have at least two groups
  res <- new_basis()

  res$call <- match.call()
  res$distribution <- "ANOVA"
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

  grand_mean <- mean(res$data)

  ssb <- sum(sapply(
    levels(as.factor(res$groups)),
    function(g) {
      group_data <- res$data[res$groups == g]
      length(group_data) * mean(group_data) ^ 2
    }
  )) - res$n * grand_mean ^ 2

  sst <- sum(sapply(
    res$data,
    function(xi) {
      xi ^ 2
    }
  )) - res$n * grand_mean ^ 2

  sse <- sst - ssb

  msb <- ssb / (res$r - 1)
  mse <- sse / (res$n - res$r)

  n_star <- sum(sapply(
    levels(as.factor(res$groups)),
    function(g) {
      group_data <- res$data[res$groups == g]
      length(group_data) ^ 2 / res$n
    }
  ))

  effective_batch <- (res$n - n_star) / (res$r - 1)

  pop_sd <- sqrt(
    msb / effective_batch + (effective_batch - 1) / effective_batch * mse
  )

  u <- msb / mse

  k0 <- k_factor_normal(res$n, p, conf)
  k1 <- k_factor_normal(res$r, p, conf)

  w <- sqrt(u / (u + effective_batch - 1))

  tol_factor <- (k0 - k1 / sqrt(effective_batch) + (k1 - k0) * w) /
    (1 - 1 / sqrt(effective_batch))

  res$basis <- grand_mean - tol_factor * pop_sd

  return(res)
}
