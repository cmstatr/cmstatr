#' Calculate k factor for basis values (\eqn{kB}, \eqn{kA}) with normal
#' distribution
#'
#' @param n the number of observations (i.e. coupons)
#' @param p should be 0.90 for B-Basis and 0.99 for A-Basis
#' @param conf confidence interval. Should be 0.95 for both A- and B-Basis
#'
#' @details
#' This function calculates the k factors used when determining A- and
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
#' set \eqn{p=0.90} and \eqn{conf=0.95}; for A-Basis, set \eqn{p=0.99} and
#' \eqn{conf=0.95}. These functions also preform some automated diagnostic
#' tests of the data prior to calculating the basis values. These diagnostic
#' tests can be overridden if needed.
#'
#' @param data a data.frame
#' @param x the variable in the data.frame for which to find the basis value
#' @param batch the variable in the data.frame that contains the batches.
#' @param groups the variable in the data.frame representing the groups
#' @param p should be 0.90 for B-Basis and 0.99 for A-Basis
#' @param conf confidence interval. Should be 0.95 for both A- and B-Basis
#' @param override a list of names of diagnostic tests to override,
#'                 if desired.
#' @param modcv a logical value indicating whether the modified CV approach
#'              should be used. Only applicable to pooling methods.
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
#' the standard deviation from the mean. \eqn{k} is given by
#' the function \code{\link{k_factor_normal}}. \code{basis_normal} also
#' performs a diagnostic test for outliers (using
#' \code{\link{maximum_normed_residual}})
#' and a diagnostic test for normality (using
#' \code{\link{anderson_darling_normal}}).
#' If the argument \code{batch} is given, this function also performs
#' a diagnostic test for outliers within
#' each batch (using \code{\link{maximum_normed_residual}})
#' and a diagnostic test for between batch variability (using
#' \code{\link{ad_ksample}}). The argument \code{batch} is only used
#' for these diagnostic tests.
#'
#' \code{basis_lognormal} calculates the basis value in the same way
#' that \code{basis_normal} does, except that the natural logarithm of the
#' data is taken.
#'
#' \code{basis_lognormal} function also performs
#' a diagnostic test for outliers (using
#' \code{\link{maximum_normed_residual}})
#' and a diagnostic test for normality (using
#' \code{\link{anderson_darling_lognormal}}).
#' If the argument \code{batch} is given, this function also performs
#' a diagnostic test for outliers within
#' each batch (using \code{\link{maximum_normed_residual}})
#' and a diagnostic test for between batch variability (using
#' \code{\link{ad_ksample}}). The argument \code{batch} is only used
#' for these diagnostic tests.
#'
#' \code{basis_weibull} calculates the basis value for data distributed
#' according to a Weibull distribution. The confidence interval for the
#' quantile requested is calculated using the conditional method, as
#' described in Lawless (1982) Section 4.1.2b. This has good agreement
#' with tables published in CMH-17-1G. Results differ between this function
#' and STAT17 by approximately 0.5\%.
#'
#' \code{basis_weibull} function also performs
#' a diagnostic test for outliers (using
#' \code{\link{maximum_normed_residual}})
#' and a diagnostic test for normality (using
#' \code{\link{anderson_darling_weibull}}).
#' If the argument \code{batch} is given, this function also performs
#' a diagnostic test for outliers within
#' each batch (using \code{\link{maximum_normed_residual}})
#' and a diagnostic test for between batch variability (using
#' \code{\link{ad_ksample}}). The argument \code{batch} is only used
#' for these diagnostic tests.
#'
#' \code{basis_hk_ext} calculates the basis value using the Extended
#' Hanson-Koopmans method, as described in CMH-17-1G and Vangel (1994).
#' For nonparametric distributions, this function should be used for samples
#' up to n=28 for B-Basis and up to \eqn{n=299} for A-Basis.
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
#' distribution. This approach is described in Vangel (1994). This second
#' method (for use when calculating B-Basis values) is called
#' "optimum-order" in this package.
#' The results of \code{basis_hk_ext} have been
#' verified against example results from the program STAT-17. Agreement is
#' typically well within 0.2\%, however, for a few sample sizes, the
#' agreement can be as poor as 1\% with the result of this function
#' being more conservative than STAT-17.
#'
#' \code{basis_hk_ext} also performs
#' a diagnostic test for outliers (using
#' \code{\link{maximum_normed_residual}})
#' and performs a pair of tests that the sample size and method selected
#' follow the guidance described above.
#' If the argument \code{batch} is given, this function also performs
#' a diagnostic test for outliers within
#' each batch (using \code{\link{maximum_normed_residual}})
#' and a diagnostic test for between batch variability (using
#' \code{\link{ad_ksample}}). The argument \code{batch} is only used
#' for these diagnostic tests.
#'
#' \code{basis_nonpara_large_sample} calculates the basis value
#' using the large sample method described in CMH-17-1G. This method uses
#' a sum of binomials to determine the rank of the ordered statistic
#' corresponding with the desired tolerance limit (basis value). Results
#' of this function have been verified against results of the STAT-17
#' program.
#'
#' \code{basis_nonpara_large_sample} also performs
#' a diagnostic test for outliers (using
#' \code{\link{maximum_normed_residual}})
#' and performs a test that the sample size is sufficiently large.
#' If the argument \code{batch} is given, this function also performs
#' a diagnostic test for outliers within
#' each batch (using \code{\link{maximum_normed_residual}})
#' and a diagnostic test for between batch variability (using
#' \code{\link{ad_ksample}}). The argument \code{batch} is only used
#' for these diagnostic tests.
#'
#' \code{basis_anova} calculates basis values using the ANOVA method.
#' \code{x} specifies the data (normally strength) and \code{groups}
#' indicates the group corresponding to each observation. This method is
#' described in CMH-17-1G. This function automatically performs a diagnostic
#' test for outliers within each group
#' (using \code{\link{maximum_normed_residual}}) and a test for between
#' group variability (using \code{\link{ad_ksample}}) as well as checking
#' that the data contains at least 5 groups.
#' This function has been verified against the results of the STAT-17 program.
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
#' \code{basis_pooled_sd} and \code{basis_pooled_cv} both automatically
#' perform a number of diagnostic tests. Using
#' \code{\link{maximum_normed_residual}}, they check that there are no
#' outliers within each group and batch (provided that \code{batch}) is
#' specified. They check the between batch variability using
#' \code{\link{ad_ksample}}. They check that there are no outliers within
#' each group (pooling all batches) using
#' \code{\link{maximum_normed_residual}}. They check for the normality
#' of the pooled data using \code{\link{anderson_darling_normal}}.
#' \code{basis_pooled_sd} checks for equality of variance of all
#' data using \code{\link{levene_test}} and \code{basis_pooled_cv}
#' checks for equality of variances of all data after transforming it
#' using \code{\link{normalize_group_mean}}
#' using \code{\link{levene_test}}.
#'
#' @return an object of class \code{basis}
#' This object has the following fields:
#' \item{\code{call}}{the expression used to call this function}
#' \item{\code{distribution}}{the distribution used (normal, etc.)}
#' \item{\code{p}}{the value of \eqn{p} supplied}
#' \item{\code{conf}}{the value of \eqn{conf} supplied}
#' \item{\code{modcv}}{a logical value indicating whether the modified
#'                     CV approach was used. Only applicable to pooling
#'                     methods.}
#' \item{\code{data}}{a copy of the data used in the calculation}
#' \item{\code{groups}}{a copy of the groups variable.
#'                      Only used for pooling and ANOVA methods.}
#' \item{\code{batch}}{a copy of the batch data used for diagnostic tests}
#' \item{\code{modcv_transformed_data}}{the data after the modified CV
#'                                      transformation}
#' \item{\code{override}}{a vector of the names of diagnostic tests that
#'                        were overridden. \code{NULL} if none were
#'                        overridden}
#' \item{\code{diagnostic_failures}}{a vector containing any diagnostic tests
#'                                   that produced failures}
#' \item{\code{n}}{the number of observations}
#' \item{\code{r}}{the number of groups, if a pooling method was used.
#'                 Otherwise it is NULL.}
#' \item{\code{basis}}{the basis value computed. This is a number
#'                     except when pooling methods are used, in
#'                     which case it is a data.frame.}
#'
#' @seealso \code{\link{hk_ext_z_j_opt}}
#' @seealso \code{\link{k_factor_normal}}
#' @seealso \code{\link{transform_mod_cv}}
#' @seealso \code{\link{maximum_normed_residual}}
#' @seealso \code{\link{anderson_darling_normal}}
#' @seealso \code{\link{anderson_darling_lognormal}}
#' @seealso \code{\link{anderson_darling_weibull}}
#' @seealso \code{\link{ad_ksample}}
#' @seealso \code{\link{normalize_group_mean}}
#'
#' @references
#' J. F. Lawless, Statistical Models and Methods for Lifetime Data.
#' New York: John Wiley & Sons, 1982.
#'
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' M. Vangel, “One-Sided Nonparametric Tolerance Limits,”
#' Communications in Statistics - Simulation and Computation,
#' vol. 23, no. 4. pp. 1137–1154, 1994.
#'
#' @name basis
NULL

new_basis <- function(
  call,
  distribution,
  modcv,
  p,
  conf,
  override,
  data,
  groups,
  batch
) {
  res <- list()
  class(res) <- "basis"

  res$call <- call
  res$distribution <- distribution
  res$modcv <- modcv
  res$p <- p
  res$conf <- conf
  res$data <- data
  res$groups <- groups
  res$batch <- batch
  res$modcv_transformed_data <- NA
  res$override <- override
  res$diagnostic_failures <- character(0)
  res$n <- length(res$data)
  res$r <- NA
  if (!is.null(groups) & !all(is.na(groups))) {
    res$r <- length(levels(as.factor(groups)))
  }
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
#' \item{\code{modcv}}{a logical value indicating whether the modified
#'                     CV approach was used. Only applicable to pooling
#'                     methods.}
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
      modcv = modcv,
      n = n,
      r = r,
      basis = basis
    )
  )
}

print_diagnostic_helper <- function(heading, vec) {
  if (!is.null(vec) & length(vec) > 0) {
    cat(heading, "\n")
    cat("    `")
    cat(paste(vec, collapse = "`,\n    `"))
    cat("`\n")
  }
}

#' @export
print.basis <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Distribution: ", x$distribution, "\t")

  cat("( n = ", x$n)
  if (!is.null(x$r) & !all(is.na(x$r))) {
    cat(", r = ", x$r)
  }
  cat(" )\n")

  if (x$modcv == TRUE) {
    cat("Modified CV Approach Used", "\n")
  }

  print_diagnostic_helper("The following diagnostic tests failed:",
                          x$diagnostic_failures)
  print_diagnostic_helper("The following diagnostic tests were overridden:",
                          x$override)

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
    stop("`basis` is an unexpected data type")  # nocov
  }

  cat("\n")
}

single_point_rules <- list(
  outliers_within_batch = function(x, batch, ...) {
    group_mnr <- vapply(unique(batch), function(b) {
      x_group <- x[batch == b]
      mnr <- maximum_normed_residual(x = x_group)
      mnr$n_outliers == 0
    }, FUN.VALUE = logical(1L))
    ifelse(all(group_mnr), "",
           paste0("Maximum normed residual test detected ",
                  "outliers within one or more batch"))
  },
  between_batch_variability = function(x, batch, ...) {
    adk <- ad_ksample(x = x, groups = batch, alpha = 0.025)
    ifelse(!adk$reject_same_dist, "",
          paste0("Anderson-Darling k-Sample test indicates that ",
                 "batches are drawn from different distributions"))
  },
  outliers = function(x, ...) {
    mnr <- maximum_normed_residual(x = x)
    ifelse(mnr$n_outliers == 0, "",
           paste0("Maximum normed residual test detected outliers ",
                  "within data"))
  }
)

basis_normal_rules <- single_point_rules
basis_normal_rules[["anderson_darling_normal"]] <-
  function(x, ...) {
    ad <- anderson_darling_normal(x = x)
    ifelse(!ad$reject_distribution, "",
           paste0("Anderson-Darling test rejects hypothesis that data ",
                  "is drawn from a normal distribution"))
  }

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats sd
#' @export
basis_normal <- function(data = NULL, x, batch = NULL, p = 0.90, conf = 0.95,
                         override = c()) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = batch,
    c = match.call(),
    arg_name = "batch")

  res <- new_basis(
    call = match.call(),
    distribution = "Normal",
    modcv = FALSE,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = NA,
    batch = eval_tidy(enquo(batch), data)
  )

  check_result <- perform_checks(basis_normal_rules, x = res$data,
                                 batch = res$batch, override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

  k <- k_factor_normal(n = res$n, p = p, conf = conf)

  cv <- sd(res$data) / mean(res$data)

  res$basis <- mean(res$data) * (1 - k * cv)

  return(res)
}

basis_lognormal_rules <- single_point_rules
basis_lognormal_rules[["anderson_darling_lognormal"]] <-
  function(x, ...) {
    ad <- anderson_darling_lognormal(x = x)
    ifelse(!ad$reject_distribution, "",
           paste0("Anderson-Darling test rejects hypothesis that data ",
                  "is drawn from a log-normal distribution"))
  }

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats sd
#' @export
basis_lognormal <- function(data = NULL, x, batch = NULL, p = 0.90,
                            conf = 0.95, override = c()) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = batch,
    c = match.call(),
    arg_name = "batch")

  res <- new_basis(
    call = match.call(),
    distribution = "Lognormal",
    modcv = FALSE,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = NA,
    batch = eval_tidy(enquo(batch), data)
  )

  check_result <- perform_checks(basis_lognormal_rules, x = res$data,
                                 batch = res$batch, override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

  k <- k_factor_normal(n = res$n, p = p, conf = conf)
  res$basis <- exp(mean(log(res$data)) - k * sd(log(res$data)))

  return(res)
}

basis_weibull_rules <- single_point_rules
basis_weibull_rules[["anderson_darling_weibull"]] <-
  function(x, ...) {
    ad <- anderson_darling_weibull(x = x)
    ifelse(!ad$reject_distribution, "",
           paste0("Anderson-Darling test rejects hypothesis that data ",
                  "is drawn from a Weibull distribution"))
  }

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats qweibull integrate pchisq uniroot
#' @importFrom MASS fitdistr
#'
#' @export
basis_weibull <- function(data = NULL, x, batch = NULL, p = 0.90,
                          conf = 0.95, override = c()) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = batch,
    c = match.call(),
    arg_name = "batch")

  res <- new_basis(
    call = match.call(),
    distribution = "Weibull",
    modcv = FALSE,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = NA,
    batch = eval_tidy(enquo(batch), data)
  )

  check_result <- perform_checks(basis_weibull_rules, x = res$data,
                                 batch = res$batch, override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

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
    return(int_res$value - conf)
  }

  res_root <- uniroot(pr_fcn, c(0, 10), extendInt = "yes")

  res$basis <- exp(u_hat - res_root$root * b_hat)

  return(res)
}

pooled_rules <- list(
  outliers_within_batch = function(x, groups, batch, ...) {
    group_batch_mnr <- vapply(unique(groups), function(g) {
      batch_mnr <- vapply(unique(batch), function(b) {
        x_group <- x[batch == b & groups == g]
        if (length(x_group) == 0) {
          return(TRUE)
        }
        mnr <- maximum_normed_residual(x = x_group)
        return(mnr$n_outliers == 0)
      }, FUN.VALUE = logical(1L))
      all(batch_mnr)
    }, FUN.VALUE = logical(1L))
    ifelse(all(group_batch_mnr), "",
           paste0("Maximum normed residual test detected ",
                  "outliers within one or more batch and group"))
  },
  between_group_variability = function(x_ad, groups, batch, ...) {
    group_adk <- vapply(unique(groups), function(g) {
      x_group <- x_ad[groups == g]
      batch_group <- batch[groups == g]
      adk <- ad_ksample(x = x_group, groups = batch_group)
      !adk$reject_same_dist
    }, FUN.VALUE = logical(1L))
    ifelse(all(group_adk), "",
           paste0("Anderson-Darling k-Sample test indicates that ",
                  "batches are drawn from different distributions"))
  },
  outliers_within_group = function(x, groups, ...) {
    group_mnr <- vapply(unique(groups), function(g) {
      x_group <- x[groups == g]
      mnr <- maximum_normed_residual(x = x_group)
      return(mnr$n_outliers == 0)
    }, FUN.VALUE = logical(1L))
    ifelse(all(group_mnr), "",
           paste0("Maximum normed residual test detected ",
                  "outliers within one or more group"))
  },
  pooled_data_normal = function(x_ad, groups, ...) {
    norm_x <- normalize_group_mean(x = x_ad, group = groups)
    ad <- anderson_darling_normal(x = norm_x)
    ifelse(!ad$reject_distribution, "",
           paste0("Anderson-Darling test rejects hypothesis that pooled ",
                  "data is drawn from a normal distribution"))
  }
)

pooled_rules_cv <- pooled_rules
pooled_rules_cv[["normalized_variance_equal"]] <- function(x, groups, ...) {
  norm_x <- normalize_group_mean(x = x, group = groups)
  lev <- levene_test(x = norm_x, groups = groups)
  return(ifelse(!lev$reject_equal_variance, "",
                paste0("Levene's test rejected the hypothesis that the ",
                       "variance of all groups are equal")))
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @export
basis_pooled_cv <- function(data = NULL, x, groups, batch = NULL,
                            p = 0.90, conf = 0.95, modcv = FALSE,
                            override = c()) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = groups,
    c = match.call(),
    arg_name = "groups")

  verify_tidy_input(
    df = data,
    x = batch,
    c = match.call(),
    arg_name = "batch")

  res <- new_basis(
    call = match.call(),
    distribution = "Normal - Pooled CV",
    modcv = modcv,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = eval_tidy(enquo(groups), data),
    batch = eval_tidy(enquo(batch), data)
  )

  if (modcv == TRUE) {
    res$modcv <- TRUE
    res$modcv_transformed_data <- transform_mod_cv_grouped(res$data, res$groups)
    data_to_use <- res$modcv_transformed_data
    x_ad <- transform_mod_cv_ad(res$data, res$groups, res$batch)
  } else {
    res$modcv <- FALSE
    data_to_use <- res$data
    x_ad <- data_to_use
  }

  check_result <- perform_checks(pooled_rules_cv, x = data_to_use,
                                 x_ad = x_ad,
                                 groups = res$groups,
                                 batch = res$batch, override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

  norm_data <- normalize_group_mean(data_to_use, res$groups)

  pooled_sd <- sqrt(sum((norm_data - 1) ^ 2) / (res$n - res$r))

  basis <- vapply(levels(as.factor(res$groups)), function(g) {
    nj <- length(data_to_use[res$groups == g])
    z <- qnorm(p)
    suppressWarnings(
      kj <- qt(conf, df = res$n - res$r, ncp = z * sqrt(nj)) / sqrt(nj)
    )
    xj_bar <- mean(data_to_use[res$groups == g])
    xj_bar * (1 - kj * pooled_sd)
  }, FUN.VALUE = numeric(1L))

  res$basis <- data.frame(group = names(basis), value = basis)

  return(res)
}

pooled_rules_sd <- pooled_rules
pooled_rules_sd[["pooled_variance_equal"]] <- function(x, groups, ...) {

  lev <- levene_test(x = x, groups = groups)
  return(ifelse(!lev$reject_equal_variance, "",
                paste0("Levene's test rejected the hypothesis that the ",
                       "variance of all conditions are equal")))
}

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @export
basis_pooled_sd <- function(data = NULL, x, groups, batch = NULL,
                            p = 0.90, conf = 0.95, modcv = FALSE,
                            override = c()) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = groups,
    c = match.call(),
    arg_name = "groups")

  verify_tidy_input(
    df = data,
    x = batch,
    c = match.call(),
    arg_name = "batch")

  res <- new_basis(
    call = match.call(),
    distribution = "Normal - Pooled Standard Deviation",
    modcv = modcv,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = eval_tidy(enquo(groups), data),
    batch = eval_tidy(enquo(batch), data)
  )

  if (modcv == TRUE) {
    res$modcv <- TRUE
    res$modcv_transformed_data <- transform_mod_cv_grouped(res$data, res$groups)
    data_to_use <- res$modcv_transformed_data
    x_ad <- transform_mod_cv_ad(res$data, res$groups, res$batch)
  } else {
    res$modcv <- FALSE
    data_to_use <- res$data
    x_ad <- data_to_use
  }

  check_result <- perform_checks(pooled_rules_sd, x = data_to_use,
                                 x_ad = x_ad,
                                 groups = res$groups,
                                 batch = res$batch, override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

  pooled_sd <- sqrt(
    sum(
      vapply(levels(as.factor(res$groups)), function(g) {
        xj_bar <- mean(data_to_use[res$groups == g])
        sum((data_to_use[res$groups == g] - xj_bar) ^ 2)
      }, FUN.VALUE = numeric(1L))
    ) / (res$n - res$r))

  basis <- vapply(levels(as.factor(res$groups)), function(g) {
    nj <- length(data_to_use[res$groups == g])
    z <- qnorm(p)
    suppressWarnings(
      kj <- qt(conf, df = res$n - res$r, ncp = z * sqrt(nj)) / sqrt(nj)
    )
    xj_bar <- mean(data_to_use[res$groups == g])
    xj_bar - kj * pooled_sd
  }, FUN.VALUE = numeric(1L))

  res$basis <- data.frame(group = names(basis), value = basis)

  return(res)
}

#' @importFrom stats pbeta dbeta
hk_ext_h <- function(z, n, i, j, p) {
  if (!(1 <= i && i < j && j <= n)) {
    # This function is called internally, so i, j and n should always be valid
    stop("Error: The condition 1 <= i < j <= n must be true.")  # nocov
  }

  # for z >= 1
  qb <- pbeta(1 - p, j, n - j + 1)
  int <- integrate(function(t) {
      pbeta(((1 - p) / t) ^ (1 / z), i, j - i) * dbeta(t, j, n - j + 1)
    },
    lower = 1 - p, upper = 1)
  if (int$message != "OK") {
    warning(int$message)  # nocov
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
  z_vals <- vapply(j, function(ji) {
    hk_ext_z(n, i, ji, p, conf)
  }, FUN.VALUE = numeric(1L))

  err_vals <- vapply(seq(along.with = j), function(index) {
    ji <- j[index]
    zi <- z_vals[index]
    e1 <- expected_order_statistic(i, n)
    e2 <- expected_order_statistic(ji, n)
    abs(zi * e1 + (1 - zi) * e2 - qnorm(p))
  }, FUN.VALUE = numeric(1L))

  list(
    z = z_vals[err_vals == min(err_vals)],
    j = j[err_vals == min(err_vals)]
  )
}

basis_hk_ext_rules <- single_point_rules
basis_hk_ext_rules[["correct_method_used"]] <-
  function(method, p, conf, ...) {
    if (p == 0.90 & conf == 0.95) {
      # B-Basis
      return(ifelse(method == "optimum-order", "",
                    paste0("For B-Basis, the optimum order method ",
                           "should be used")))
    } else if (p == 0.99 & conf == 0.95) {
      # A-Basis
      return(ifelse(method == "woodward-frawley", "",
                    paste0("For A-Basis, the Woodward-Frawley method ",
                           "should be used")))
    } else {
      return("")
    }
  }
basis_hk_ext_rules[["sample_size"]] <-
  function(n, p, conf, ...) {
    if (p == 0.90 & conf == 0.95) {
      # B-Basis
      return(ifelse(n <= 28, "",
                    paste0("For B-Basis, Hanson-Koopmans should only be ",
                           "used for samples of 28 or fewer observations")))
    } else if (p == 0.99 & conf == 0.95) {
      # A-Basis
      return(ifelse(n <= 299, "",
                    paste0("For A-Basis, Hanson-Koopmans should only be ",
                           "used for samples of 299 or fewer observations")))
    } else {
      return("")
    }
  }

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#'
#' @export
basis_hk_ext <- function(data = NULL, x, batch = NULL, p = 0.90, conf = 0.95,
                       method = c("optimum-order", "woodward-frawley"),
                       override = c()) {
  method <- match.arg(method)

  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = batch,
    c = match.call(),
    arg_name = "batch")

  res <- new_basis(
    call = match.call(),
    distribution = paste0(
      "Nonparametric (Extended Hanson-Koopmans, ",
      ifelse(method == "optimum-order", "optimum two-order-statistic method",
             "Woodward-Frawley method"),
      ")"),
    modcv = FALSE,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = NA,
    batch = eval_tidy(enquo(batch), data)
  )

  check_result <- perform_checks(basis_hk_ext_rules, x = res$data,
                                 batch = res$batch, n = res$n,
                                 p = res$p, conf = res$conf,
                                 method = method,
                                 override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

  if (method == "optimum-order") {
    zj <- hk_ext_z_j_opt(res$n, p, conf)
    z <- zj$z
    j <- zj$j
  } else if (method == "woodward-frawley") {
    j <- res$n
    z <- hk_ext_z(res$n, 1, j, p, conf)
  } else {
    stop("Invalid value for method.")  # nocov
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
#' discrepancies have been identified for B-Basis. Since these sample sizes are
#' uncommon for composite materials
#' testing, and the difference between subsequent order statistics will be
#' very small for samples this large, this difference will have no practical
#' effect on computed tolerance limits.
#'
#' @references
#' W. Guenther, “Determination of Sample Size for Distribution-Free
#' Tolerance Limits,” Jan. 1969.
#' Available online: https://www.duo.uio.no/handle/10852/48686
#'
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @seealso \code{\link{basis_nonpara_large_sample}}
#'
#' @export
nonpara_binomial_rank <- function(n, p, conf) {
  p <- 1 - p

  e_fcn <- function(r) {
    sum(vapply(r:n, function(w) {
      exp(lchoose(n, w) + w * log(p) + (n - w) * log(1 - p))
    }, FUN.VALUE = numeric(1L)))
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

nonpara_large_sample_rules <- single_point_rules
nonpara_large_sample_rules[["sample_size"]] <-
  function(n, p, conf, ...) {
    if (p == 0.90 & conf == 0.95) {
      # B-Basis
      return(ifelse(n >= 28, "",
                    paste0("This method should only be used for ",
                           "B-Basis for sample sizes larger than 28")))
    } else if (p == 0.99 & conf == 0.95) {
      # A-Basis
      return(ifelse(n >= 299, "",
                    paste0("This method should only be used for ",
                           "A-Basis for sample sizes larger than 299")))
    } else {
      return(TRUE)
    }
  }

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#'
#' @export
basis_nonpara_large_sample <- function(data = NULL, x, batch = NULL,
                                       p = 0.90, conf = 0.95,
                                       override = c()) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = batch,
    c = match.call(),
    arg_name = "batch")

  res <- new_basis(
    call = match.call(),
    distribution = "Nonparametric (large sample)",
    modcv = FALSE,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = NA,
    batch = eval_tidy(enquo(batch), data)
  )

  check_result <- perform_checks(nonpara_large_sample_rules,
                                 x = res$data, batch = res$batch, n = res$n,
                                 p = res$p, conf = res$conf,
                                 override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

  x_ordered <- sort(res$data)
  r <- nonpara_binomial_rank(res$n, p, conf)
  res$basis <- x_ordered[r]

  return(res)
}

anova_rules <- list(
  outliers_within_group = function(x, groups, ...) {
    group_mnr <- vapply(unique(groups), function(b) {
      x_group <- x[groups == b]
      mnr <- maximum_normed_residual(x = x_group)
      mnr$n_outliers == 0
    }, FUN.VALUE = logical(1L))
    ifelse(all(group_mnr), "",
           paste0("Maximum normed residual test detected ",
                  "outliers within one or more batch"))
  },
  equality_of_variance = function(x, groups, ...) {
    lt <- levene_test(x = x, groups = groups)
    ifelse(!lt$reject_equal_variance, "",
           paste0("Levene's test rejected the hypothesis that the ",
                  "variance of all groups is equal"))
  },
  number_of_groups = function(r, ...) {
    ifelse(r >= 5, "",
           "ANOVA should only be used for 5 or more groups")
  }
)

#' @rdname basis
#' @importFrom rlang enquo eval_tidy
#' @export
basis_anova <- function(data = NULL, x, groups, p = 0.90, conf = 0.95,
                        override = c()) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  verify_tidy_input(
    df = data,
    x = groups,
    c = match.call(),
    arg_name = "groups")

  res <- new_basis(
    call = match.call(),
    distribution = "ANOVA",
    modcv = FALSE,
    p = p,
    conf = conf,
    override = override,
    data = eval_tidy(enquo(x), data),
    groups = eval_tidy(enquo(groups), data),
    batch = NA
  )

  if (res$r < 2) {
    stop("ANOVA cannot be computed with fewer than 2 groups")
  }

  check_result <- perform_checks(rules = anova_rules,
                                 x = res$data, groups = res$groups,
                                 r = res$r, override = override)
  res$diagnostic_failures <- names(check_result[!check_result])

  grand_mean <- mean(res$data)

  ssb <- sum(vapply(
    levels(as.factor(res$groups)),
    function(g) {
      group_data <- res$data[res$groups == g]
      length(group_data) * mean(group_data) ^ 2
    },
    FUN.VALUE = numeric(1L)
  )) - res$n * grand_mean ^ 2

  sst <- sum(vapply(
    res$data,
    function(xi) {
      xi ^ 2
    },
    FUN.VALUE = numeric(1L)
  )) - res$n * grand_mean ^ 2

  sse <- sst - ssb

  msb <- ssb / (res$r - 1)
  mse <- sse / (res$n - res$r)

  n_star <- sum(vapply(
    levels(as.factor(res$groups)),
    function(g) {
      group_data <- res$data[res$group == g]
      length(group_data) ^ 2 / res$n
    },
    FUN.VALUE = numeric(1L)
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
