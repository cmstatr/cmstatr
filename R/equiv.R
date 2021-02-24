
#' Test for decrease in mean or minimum individual
#'
#' @description
#' This test is used when determining if a new process or
#' manufacturing location produces material properties that are
#' "equivalent" to an existing dataset, and hence the existing
#' basis values are applicable to the new dataset. This test is also
#' sometimes used for determining if a new batch of material is acceptable.
#' This function determines thresholds based on both minimum
#' individual and mean, and optionally evaluates a sample against those
#' thresholds. The joint distribution between the sample mean
#' and sample minimum is used to generate these thresholds.
#' When there is no true difference between the existing ("qualification")
#' and the new population from which the sample is obtained, there is a
#' probability of \eqn{\alpha} of falsely concluding that there is a
#' difference in mean or variance. It is assumed that both the original
#' and new populations are normally distributed.
#' According to Vangel (2002), this test provides improved power compared
#' with a test of mean and standard deviation.
#'
#' @param df_qual (optional) a data.frame containing the qualification data.
#' Defaults to NULL.
#' @param alpha the acceptable probability of a type I error
#' @param data_sample (optional) a vector of observations from the sample for
#' which equivalency is being tested. Defaults to NULL
#' @param n_sample (optional) the number of observations in the sample for
#' which equivalency will be tested. Defaults to NULL
#' @param data_qual (optional) a vector of observations from the
#' "qualification" data to which equivalency is being tested. Or the column of
#' `df_qual` that contains this data. Defaults to NULL
#' @param mean_qual (optional) the mean from the "qualification" data to which
#' equivalency is being tested. Defaults to NULL
#' @param sd_qual (optional) the standard deviation from the "qualification"
#' data to which equivalency is being tested. Defaults to NULL
#' @param modcv (optional) a boolean value indicating whether a modified CV
#' should be used. Defaults to FALSE, in which case the standard deviation
#' supplied (or calculated from `data_qual`) will be used directly.
#'
#' @return
#' Returns an object of class `equiv_mean_extremum`. This object is a list
#' with the following named elements:
#'
#' - `call` the expression used to call this function
#' - `alpha` the value of alpha passed to this function
#' - `n_sample` the number of observations in the sample for which
#'   equivalency is being checked. This is either the value `n_sample`
#'   passed to this function or the length of the vector `data_sample`.
#' - `k1` the factor used to calculate the minimum individual
#'   threshold. The minimum individual threshold is calculated as
#'   \eqn{W_{min} = qual\,mean - k_1 \cdot qual\,sd}{
#'   Wmin = qual_mean - k1 * qual_sd}
#' - `k2` the factor used to calculate the threshold for mean. The
#'   threshold for mean is calculated as
#'   \eqn{W_{mean} = qual\,mean - k_2 \cdot qual\,sd}{
#'   Wmean = qual_mean - k2 * qual_sd}
#' - `modcv` logical value indicating whether the acceptance
#'   thresholds are calculated using the modified CV approach
#' - `cv` the coefficient of variation of the qualification data.
#'   This value is not modified, even if `modcv=TRUE`
#' - `cv_star` The modified coefficient of variation. If
#'   `modcv=FALSE`, this will be `NULL`
#' - `threshold_min_indiv` The calculated threshold value for
#'   minimum individual
#' - `threshold_mean` The calculated threshold value for mean
#' - `result_min_indiv` a character vector of either "PASS" or
#'   "FAIL" indicating whether the data from `data_sample` passes the
#'   test for minimum individual. If `data_sample` was not supplied,
#'   this value will be `NULL`
#' - `result_mean` a character vector of either "PASS" or
#'   "FAIL" indicating whether the data from `data_sample` passes the
#'   test for mean. If `data_sample` was not supplied, this value will
#'   be  `NULL`
#' - `min_sample` The minimum value from the vector
#'   `data_sample`. if `data_sample` was not supplied, this will
#'   have a value of `NULL`
#' - `mean_sample` The mean value from the vector
#'   `data_sample`. If `data_sample` was not supplied, this will
#'   have a value of `NULL`
#'
#' @details
#' This function is used to
#' determine acceptance limits for a sample mean and sample minimum.
#' These acceptance limits are often used to set acceptance limits for
#' material strength for each lot of material, or each new manufacturing
#' site. When a sample meets the criteria that its mean and its minimum are
#' both greater than these limits, then one may accept the lot of material
#' or the new manufacturing site.
#'
#' This procedure is used to ensure that the strength of material processed
#' at a second site, or made with a new batch of material are not degraded
#' relative to the data originally used to determine basis values for the
#' material. For more information about the use of this procedure, see
#' CMH-17-1G or PS-ACE 100-2002-006.
#'
#' There are several optional arguments to this function. However, you can't
#' omit all of the optional arguments. You must supply either
#' `data_sample` or `n_sample`, but not both. You must also supply
#' either `data_qual` (and `df_qual` if `data_qual` is a
#' variable name and not a vector) or both `mean_qual` and `sd_qual`,
#' but if you supply `data_qual` (and possibly `df_qual`) you should
#' not supply either `mean_qual` or `sd_qual` (and visa-versa). This
#' function will issue a warning or error if you violate any of these rules.
#'
#' If `modcv` is TRUE, the standard deviation used to calculate the
#' thresholds will be replaced with a standard deviation calculated
#' using the Modified Coefficient of Variation (CV) approach.
#' The Modified CV approach is a way of adding extra variance to the
#' qualification data in the case that the qualification data has less
#' variance than expected, which sometimes occurs when qualification testing
#' is performed in a short period of time.
#' Using the Modified CV approach, the standard deviation is calculated by
#' multiplying `CV_star * mean_qual` where `mean_qual` is either the
#' value supplied or the value calculated by `mean(data_qual)` and
#' \eqn{CV*} is the value computed by [calc_cv_star()].
#'
#' @examples
#' equiv_mean_extremum(alpha = 0.01, n_sample = 6,
#'                     mean_qual = 100, sd_qual = 5.5, modcv = TRUE)
#' ##
#' ## Call:
#' ## equiv_mean_extremum(mean_qual = 100, sd_qual = 5.5, n_sample = 6,
#' ##     alpha = 0.01, modcv = TRUE)
#' ##
#' ## Modified CV used: CV* = 0.0675 ( CV = 0.055 )
#' ##
#' ## For alpha = 0.01 and n = 6
#' ## ( k1 = 3.128346 and k2 = 1.044342 )
#' ##                   Min Individual   Sample Mean
#' ##      Thresholds:    78.88367        92.95069
#'
#' @seealso [k_equiv()]
#' @seealso [calc_cv_star()]
#'
#' @references
#' M. G. Vangel. Lot Acceptance and Compliance Testing Using the Sample Mean
#' and an Extremum, Technometrics, vol. 44, no. 3. pp. 242–249. 2002.
#'
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' Federal Aviation Administration, “Material Qualification and Equivalency
#' for Polymer Matrix Composite Material Systems,” PS-ACE 100-2002-006,
#' Sep. 2003.
#'
#' @importFrom rlang enquo eval_tidy
#'
#' @export
equiv_mean_extremum <- function(df_qual = NULL, data_qual = NULL,
                                mean_qual = NULL, sd_qual = NULL,
                                data_sample = NULL, n_sample = NULL,
                                alpha, modcv = FALSE) {
  res <- list()
  class(res) <- "equiv_mean_extremum"

  res$call <- match.call()

  if (alpha <= 0) {
    stop("alpha must be positive")
  }
  if (alpha >= 1) {
    stop("alpha must be less than 1")
  }

  if (!is.null(df_qual)) {
    data_qual_enq <- enquo(data_qual)
    data_qual <- eval_tidy(data_qual_enq, df_qual)
  }

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
    cv <- calc_cv_star(cv)
    res$cv_star <- cv
    sd_qual <- cv * mean_qual
  }

  res$threshold_min_indiv <- mean_qual - sd_qual * res$k1
  res$threshold_mean <- mean_qual - sd_qual * res$k2

  res$result_min_indiv <- NULL
  res$result_mean <- NULL
  res$min_sample <- NULL
  res$mean_sample <- NULL
  if (!is.null(data_sample)) {
    res$min_sample <- min(data_sample)
    res$mean_sample <- mean(data_sample)
    res$result_min_indiv <- ifelse(res$min_sample >= res$threshold_min_indiv,
                                   "PASS", "FAIL")
    res$result_mean <- ifelse(res$mean_sample >= res$threshold_mean,
                              "PASS", "FAIL")
  }

  return(res)
}

#' Glance at an `equiv_mean_extremum` object
#'
#' @description
#' Glance accepts an object of type `equiv_mean_extremum` and returns a
#' [tibble::tibble()] with
#' one row of summaries.
#'
#' Glance does not do any calculations: it just gathers the results in a
#' tibble.
#'
#' @param x an equiv_mean_extremum object returned from
#'          [equiv_mean_extremum()]
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#'
#' @return
#' A one-row [tibble::tibble()] with the following
#' columns:
#'
#' - `alpha` the value of alpha passed to this function
#' - `n_sample` the number of observations in the sample for which
#'   equivalency is being checked. This is either the value `n_sample`
#'   passed to this function or the length of the vector `data_sample`.
#' - `modcv` logical value indicating whether the acceptance
#'   thresholds are calculated using the modified CV approach
#' - `threshold_min_indiv` The calculated threshold value for
#'   minimum individual
#' - `threshold_mean` The calculated threshold value for mean
#' - `result_min_indiv` a character vector of either "PASS" or
#'   "FAIL" indicating whether the data from `data_sample` passes the
#'   test for minimum individual. If `data_sample` was not supplied,
#'   this value will be `NULL`
#' - `result_mean` a character vector of either "PASS" or
#'   "FAIL" indicating whether the data from `data_sample` passes the
#'   test for mean. If `data_sample` was not supplied, this value will
#'   be  `NULL`
#' - `min_sample` The minimum value from the vector
#'   `data_sample`. if `data_sample` was not supplied, this will
#'   have a value of `NULL`
#' - `mean_sample` The mean value from the vector
#'   `data_sample`. If `data_sample` was not supplied, this will
#'   have a value of `NULL`
#'
#'
#' @seealso
#' [equiv_mean_extremum()]
#'
#' @examples
#' x0 <- rnorm(30, 100, 4)
#' x1 <- rnorm(5, 91, 7)
#' eq <- equiv_mean_extremum(data_qual = x0, data_sample = x1, alpha = 0.01)
#' glance(eq)
#'
#' ## # A tibble: 1 x 9
#' ##   alpha n_sample modcv threshold_min_indiv threshold_mean
#' ##   <dbl>    <int> <lgl>               <dbl>          <dbl>
#' ## 1  0.01        5 FALSE                86.2           94.9
#' ## # ... with 4 more variables: result_min_indiv <chr>, result_mean <chr>,
#' ## #   min_sample <dbl>, mean_sample <dbl>
#'
#' @method glance equiv_mean_extremum
#' @importFrom tibble tibble
#'
#' @export
glance.equiv_mean_extremum <- function(x, ...) {  # nolint
  res <- tibble::tibble(
    alpha = x[["alpha"]],
    n_sample = x[["n_sample"]],
    modcv = x[["modcv"]],
    threshold_min_indiv = x[["threshold_min_indiv"]],
    threshold_mean = x[["threshold_mean"]]
  )

  if (!is.null(x[["result_min_indiv"]])) {
    res[["result_min_indiv"]] <- x[["result_min_indiv"]]
  }
  if (!is.null(x[["result_mean"]])) {
    res[["result_mean"]] <- x[["result_mean"]]
  }
  if (!is.null(x[["min_sample"]])) {
    res[["min_sample"]] <- x[["min_sample"]]
  }
  if (!is.null(x[["mean_sample"]])) {
    res[["mean_sample"]] <- x[["mean_sample"]]
  }
  res
}


#' @export
print.equiv_mean_extremum <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  justify <- c("right", "centre", "centre")
  col_width <- c(16L, 16L, 16L)

  if (x$modcv) {
    cat("Modified CV used: CV* =", format(x$cv_star, ...),
        "( CV =", format(x$cv, ...),
        ")\n\n")
  }

  cat("For alpha =", format(x$alpha, ...),
      "and n =", format(x$n_sample, ...),
      "\n( k1 =", format(x$k1, ...),
      "and k2 =", format(x$k2, ...), ")\n")

  cat(format_row(list("", "Min Individual", "Sample Mean"),
                 justify, col_width, ...))

  if (!is.null(x$min_sample)) {
    cat(format_row(list("Sample:", x$min_sample, x$mean_sample),
                   justify, col_width, ...))
  }
  cat(format_row(list("Thresholds:", x$threshold_min_indiv, x$threshold_mean),
                 justify, col_width, ...))
  if (!is.null(x$result_min_indiv)) {
    cat(format_row(list("Equivalency:", x$result_min_indiv, x$result_mean),
                   justify, col_width, ...))
  }
}


#' k-factors for determining acceptance based on sample mean and an extremum
#'
#' @param alpha the acceptable probability of a type I error
#' @param n the number of observations in the sample to test
#' @return a vector with elements c(k1, k2). k1 is for testing the sample
#'   extremum. k2 is for testing the sample mean
#' @details
#'   The k-factors returned by this function are used for determining
#'   whether to accept a new dataset.
#'
#'   This function is used as part of the procedure for
#'   determining acceptance limits for a sample mean and sample minimum.
#'   These acceptance limits are often used to set acceptance limits for
#'   material strength for each lot of material, or each new manufacturing
#'   site. When a sample meets the criteria that its mean and its minimum are
#'   both greater than these limits, then one may accept the lot of material
#'   or the new manufacturing site.
#'
#'   This procedure is used to ensure that the strength of material processed
#'   at a second site, or made with a new batch of material are not degraded
#'   relative to the data originally used to determine basis values for the
#'   material. For more information about the use of this procedure, see
#'   CMH-17-1G or PS-ACE 100-2002-006.
#'
#'   According to Vangel (2002), the use of mean and extremum for this purpose
#'   is more powerful than the use of mean and standard deviation.
#'
#'   The results of this function match those published by Vangel within
#'   0.05\% for \eqn{n > 2} and \eqn{\alpha \le 0.25}. Those results published
#'   by Vangel are identical to those published in CMH-17-1G.
#'
#'   This function uses numerical integration and numerical optimization to
#'   find values of the factors \eqn{k_1} and \eqn{k_2} based on Vangel's
#'   saddle point approximation.
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
#'
#' @references
#' M. G. Vangel. Lot Acceptance and Compliance Testing Using the Sample Mean
#' and an Extremum, Technometrics, vol. 44, no. 3. pp. 242–249. 2002.
#'
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' Federal Aviation Administration, “Material Qualification and Equivalency
#' for Polymer Matrix Composite Material Systems,” PS-ACE 100-2002-006,
#' Sep. 2003.
#'
#' @seealso
#' [equiv_mean_extremum()]
#'
#' @examples
#' qual_mean <- 100
#' qual_sd <- 3.5
#' k <- k_equiv(0.01, 5)
#' print("Minimum Individual Acceptance Limit:")
#' print(qual_mean - qual_sd * k[1])
#' print("Minimum Average Acceptance Limit:")
#' print(qual_mean - qual_sd * k[2])
#'
#' ## [1] "Minimum Individual Acceptance Limit:"
#' ## [1] 89.24981
#' ## [1] "Minimum Average Acceptance Limit:"
#' ## [1] 96.00123
#'
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
    fx1 <- 1 - (stats::pnorm(-k[1], lower.tail = FALSE)) ** n
    fxbar <- stats::pnorm(sqrt(n) * (-k[2]))

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
        ((n - 1) / n) * h_minus_t(lambda) - k[1] + k[2]
      },
      interval = c(-1000, 1000),
      extendInt = "yes"
    )$root

    a_fcn <- function(t, n) {
      hmt <- h_minus_t(t)
      exp(
        - (n - 1) * logh(t) +
          (n - 1) ** 2 / (2 * n) * hmt ** 2 +
          (n - 1) * t * hmt
      ) * sqrt(ifelse(t > 60, t ** -2, 1 - h(t) * hmt))
    }

    fx1xbar_numerator <- stats::pnorm(sqrt(n) * (-k[2])) * stats::integrate(
      function(t) {
        a_fcn(t, n)
      },
      lower = -Inf,
      upper = lambda_hat,
      subdivisions = 1000L
    )$value + stats::integrate(
      function(t) {
        stats::pnorm(
          sqrt(n) * (-k[1] + (n - 1) / n * h_minus_t(t))
        ) * a_fcn(t, n)
      },
      lower = lambda_hat,
      upper = Inf,
      subdivisions = 1000L,
      rel.tol = 1e-8
    )$value

    fx1xbar_denominator <- stats::integrate(function(t) a_fcn(t, n),
                                            lower = -Inf, upper = Inf)$value
    fx1xbar <- fx1xbar_numerator / fx1xbar_denominator

    # Use the sum of the absolute values of the two functions being solved as
    # the penalty function. However, it was found empirically that the first
    # function listed is more sensitive, so give it a higher weight to aid in
    # finding hte correct solution
    return(
      abs(fx1 + fxbar - fx1xbar - alpha) * 100 + abs(fx1 - fxbar)
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
  # nocov start
  if (res$convergence != 0) {
    warning(
      "k-factor search did not converge. The results are unreliable."
      )
  }
  # nocov end

  return(res$par)
}


#' Equivalency based on change in mean value
#'
#' @description
#' Checks for change in the mean value between a qualification data set and
#' a sample. This is normally used to check for properties such as modulus.
#' This function is a wrapper for a two-sample t--test.
#'
#' @param df_qual (optional) a data.frame containing the qualification data.
#' Defaults to NULL.
#' @param alpha the acceptable probability of a Type I error
#' @param data_sample a vector of observations from the sample being compared
#'   for equivalency
#' @param n_sample the number of observations in the sample being compared for
#'   equivalency
#' @param mean_sample the mean of the sample being compared for equivalency
#' @param sd_sample the standard deviation of the sample being compared for
#'   equivalency
#' @param data_qual (optional) a vector of observations from the
#'   "qualification" data to which equivalency is being tested. Or the column of
#'   `df_qual` that contains this data. Defaults to NULL
#' @param n_qual the number of observations in the qualification data to which
#'   the sample is being compared for equivalency
#' @param mean_qual the mean from the qualification data to which the sample
#'   is being compared for equivalency
#' @param sd_qual the standard deviation from the qualification data to which
#'   the sample is being compared for equivalency
#' @param modcv a logical value indicating whether the modified CV approach
#'   should be used. Defaults to `FALSE`
#'
#' @return
#' - `call` the expression used to call this function
#' - `alpha` the value of alpha passed to this function
#' - `n_sample` the number of observations in the sample for which
#'   equivalency is being checked. This is either the value `n_sample`
#'   passed to this function or the length of the vector `data_sample`.
#' - `mean_sample` the mean of the observations in the sample for
#'   which equivalency is being checked. This is either the value
#'   `mean_sample` passed to this function or the mean of the vector
#'   `data-sample`.
#' - `sd_sample` the standard deviation of the observations in the
#'   sample for which equivalency is being checked. This is either the value
#'   `mean_sample` passed to this function or the standard deviation of
#'   the vector `data-sample`.
#' - `n_qual` the number of observations in the qualification data
#'   to which the sample is being compared for equivalency. This is either
#'   the value `n_qual` passed to this function or the length of the
#'   vector `data_qual`.
#' - `mean_qual` the mean of the qualification data to which the
#'   sample is being compared for equivalency. This is either the value
#'   `mean_qual` passed to this function or the mean of the vector
#'   `data_qual`.
#' - `sd_qual` the standard deviation of the qualification data to
#'   which the sample is being compared for equivalency. This is either the
#'   value `mean_qual` passed to this function or the standard deviation
#'   of the vector `data_qual`.
#' - `modcv` logical value indicating whether the equivalency
#'   calculations were performed using the modified CV approach
#' - `sp` the value of the pooled standard deviation. If
#'   `modecv = TRUE`, this pooled standard deviation includes the
#'   modification to the qualification CV.
#' - `t0` the test statistic
#' - `t_req` the t-value for \eqn{\alpha / 2} and
#'   \eqn{df = n1 + n2 -2}
#' - `threshold` a vector with two elements corresponding to the
#'   minimum and maximum values of the sample mean that would result in a
#'   pass
#' - `result` a character vector of either "PASS" or "FAIL"
#'   indicating the result of the test for change in mean
#'
#' @details
#' There are several optional arguments to this function. Either (but not both)
#' `data_sample` or all of `n_sample`, `mean_sample` and
#' `sd_sample` must be supplied. And, either (but not both)
#' `data_qual`
#' (and also `df_qual` if `data_qual` is a column name and not a
#' vector) or all of `n_qual`, `mean_qual` and `sd_qual` must
#' be supplied. If these requirements are violated, warning(s) or error(s) will
#' be issued.
#'
#' This function uses a two-sample t-test to determine if there is a difference
#' in the mean value of the qualification data and the sample. A pooled
#' standard deviation is used in the t-test. The procedure is per CMH-17-1G.
#'
#' If `modcv` is TRUE, the standard deviation used to calculate the
#' thresholds will be replaced with a standard deviation calculated
#' using the Modified Coefficient of Variation (CV) approach.
#' The Modified CV approach is a way of adding extra variance to the
#' qualification data in the case that the qualification data has less
#' variance than expected, which sometimes occurs when qualification testing
#' is performed in a short period of time.
#' Using the Modified CV approach, the standard deviation is calculated by
#' multiplying `CV_star * mean_qual` where `mean_qual` is either the
#' value supplied or the value calculated by `mean(data_qual)` and
#' \eqn{CV*} is determined using [calc_cv_star()].
#'
#' Note that the modified CV option should only be used if that data passes the
#' Anderson--Darling test.
#'
#' @examples
#' equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02,
#'                   sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
#'                   sd_qual = 0.162, modcv = TRUE)
#'
#' ## Call:
#' ## equiv_change_mean(n_qual = 28, mean_qual = 9.24, sd_qual = 0.162,
#' ##                   n_sample = 9, mean_sample = 9.02, sd_sample = 0.15785,
#' ##                   alpha = 0.05,modcv = TRUE)
#' ##
#' ## For alpha = 0.05
#' ## Modified CV used
#' ##                   Qualification        Sample
#' ##           Number        28               9
#' ##             Mean       9.24             9.02
#' ##               SD      0.162           0.15785
#' ##           Result               PASS
#' ##    Passing Range       8.856695 to 9.623305
#'
#' @references
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @seealso [calc_cv_star()]
#' @seealso [stats::t.test()]
#'
#' @export
equiv_change_mean <- function(df_qual = NULL, data_qual = NULL,
                              n_qual = NULL, mean_qual = NULL,
                              sd_qual = NULL, data_sample = NULL,
                              n_sample = NULL, mean_sample = NULL,
                              sd_sample = NULL, alpha, modcv = FALSE) {
  if (alpha <= 0 | alpha >= 1) {
    stop("alpha must be positive and less than 1")
  }

  if (!is.null(df_qual)) {
    data_qual_enq <- enquo(data_qual)
    data_qual <- eval_tidy(data_qual_enq, df_qual)
  }

  if (!is.null(data_sample)) {
    if (!is.null(n_sample)) {
      warning("Both data_sample and n_sample supplied. n_sample ignored.")
    }
    n_sample <- length(data_sample)

    if (!is.null(mean_sample)) {
      warning("Both data_sample and mean_sample supplied. mean_sample ignored")
    }
    mean_sample <- mean(data_sample)

    if (!is.null(sd_sample)) {
      warning("Both data_sample and sd_sample supplied. sd_sample ignored")
    }
    sd_sample <- stats::sd(data_sample)
  }

  if (!is.null(data_qual)) {
    if (!is.null(n_qual)) {
      warning("Both data_qual and n_qual supplied. n_qual ignored.")
    }
    n_qual <- length(data_qual)

    if (!is.null(mean_qual)) {
      warning("Both data_qual and mean_qual supplied. mean_qual ignored")
    }
    mean_qual <- mean(data_qual)

    if (!is.null(sd_qual)) {
      warning("Both data_qual and sd_qual supplied. sd_qual ignored")
    }
    sd_qual <- stats::sd(data_qual)
  }

  verify_equiv_change_mean_var(n_sample, mean_sample, sd_sample,
                               n_qual, mean_qual, sd_qual)

  res <- list()
  class(res) <- "equiv_change_mean"

  res$call <- match.call()
  res$alpha <- alpha
  res$n_sample <- n_sample
  res$mean_sample <- mean_sample
  res$sd_sample <- sd_sample
  res$n_qual <- n_qual
  res$mean_qual <- mean_qual
  res$sd_qual <- sd_qual
  res$modcv <- modcv

  cv <- sd_qual / mean_qual
  res$modcv <- modcv
  if (modcv) {
    cv <- calc_cv_star(cv)
    sd_qual <- cv * mean_qual
  }

  sp <- sqrt(
    ((n_qual - 1) * sd_qual ** 2 + (n_sample - 1) * sd_sample ** 2) /
      (n_qual + n_sample - 2)
  )
  res$sp <- sp

  t0 <- (mean_sample - mean_qual) /
    (sp * sqrt(1 / n_qual + 1 / n_sample))
  res$t0 <- t0

  t_req <- stats::qt(alpha / 2, n_qual + n_sample - 2, lower.tail = FALSE)
  res$t_req <- t_req

  res$threshold <- c(
    mean_qual - t_req * sp * sqrt(1 / n_qual + 1 / n_sample),
    mean_qual + t_req * sp * sqrt(1 / n_qual + 1 / n_sample)
  )

  res$result <- ifelse(-t_req <= t0 & t0 <= t_req, "PASS", "FAIL")

  return(res)
}


verify_equiv_change_mean_var <- function(n_sample, mean_sample, sd_sample,
                                         n_qual, mean_qual, sd_qual) {
  if (is.null(n_sample)) {
    stop("n_sample not set")
  }
  if (is.null(mean_sample)) {
    stop("mean_sample not set")
  }
  if (is.null(sd_sample)) {
    stop("sd_sample not set")
  }
  if (is.null(n_qual)) {
    stop("n_qual not set")
  }
  if (is.null(mean_qual)) {
    stop("mean_qual not set")
  }
  if (is.null(sd_qual)) {
    stop("sd_qual not set")
  }
}


#' Glance at a `equiv_change_mean` object
#'
#' @description
#' Glance accepts an object of type `equiv_change_mean`
#'  and returns a [tibble::tibble()] with
#' one row of summaries.
#'
#' Glance does not do any calculations: it just gathers the results in a
#' tibble.
#'
#' @param x a `equiv_change_mean` object returned from
#'          [equiv_change_mean()]
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#'
#' @return
#' A one-row [tibble::tibble()] with the following
#' columns:
#'
#' - `alpha` the value of alpha passed to this function
#' - `n_sample` the number of observations in the sample for which
#'   equivalency is being checked. This is either the value `n_sample`
#'   passed to this function or the length of the vector `data_sample`.
#' - `mean_sample` the mean of the observations in the sample for
#'   which equivalency is being checked. This is either the value
#'   `mean_sample` passed to this function or the mean of the vector
#'   `data-sample`.
#' - `sd_sample` the standard deviation of the observations in the
#'   sample for which equivalency is being checked. This is either the value
#'   `mean_sample` passed to this function or the standard deviation of
#'   the vector `data-sample`.
#' - `n_qual` the number of observations in the qualification data
#'   to which the sample is being compared for equivalency. This is either
#'   the value `n_qual` passed to this function or the length of the
#'   vector `data_qual`.
#' - `mean_qual` the mean of the qualification data to which the
#'   sample is being compared for equivalency. This is either the value
#'   `mean_qual` passed to this function or the mean of the vector
#'   `data_qual`.
#' - `sd_qual` the standard deviation of the qualification data to
#'   which the sample is being compared for equivalency. This is either the
#'   value `mean_qual` passed to this function or the standard deviation
#'   of the vector `data_qual`.
#' - `modcv` logical value indicating whether the equivalency
#'   calculations were performed using the modified CV approach
#' - `sp` the value of the pooled standard deviation. If
#'   `modecv = TRUE`, this pooled standard deviation includes the
#'   modification to the qualification CV.
#' - `t0` the test statistic
#' - `t_req` the t-value for \eqn{\alpha / 2} and
#'   \eqn{df = n1 + n2 -2}
#' - `threshold_min` the minimum value of the sample mean that would
#'   result in a pass
#' - `threshold_max` the maximum value of the sample mean that would
#'   result in a pass
#' - `result` a character vector of either "PASS" or "FAIL"
#'   indicating the result of the test for change in mean
#'
#'
#' @seealso
#' [equiv_change_mean()]
#'
#' @examples
#' x0 <- rnorm(30, 100, 4)
#' x1 <- rnorm(5, 91, 7)
#' eq <- equiv_change_mean(data_qual = x0, data_sample = x1, alpha = 0.01)
#' glance(eq)
#'
#' ## # A tibble: 1 x 14
#' ##   alpha n_sample mean_sample sd_sample n_qual mean_qual sd_qual modcv
#' ##   <dbl>    <int>       <dbl>     <dbl>  <int>     <dbl>   <dbl> <lgl>
#' ## 1  0.01        5        85.8      9.93     30      100.    3.90 FALSE
#' ## # ... with 6 more variables: sp <dbl>, t0 <dbl>, t_req <dbl>,
#' ## #   threshold_min <dbl>, threshold_max <dbl>, result <chr>
#'
#' @method glance equiv_change_mean
#' @importFrom tibble tibble
#'
#' @export
glance.equiv_change_mean <- function(x, ...) {  # nolint
  with(
    x,
    tibble::tibble(
      alpha = alpha,
      n_sample = n_sample,
      mean_sample = mean_sample,
      sd_sample = sd_sample,
      n_qual = n_qual,
      mean_qual = mean_qual,
      sd_qual = sd_qual,
      modcv = modcv,
      sp = sp,
      t0 = t0,
      t_req = t_req,
      threshold_min = threshold[1],
      threshold_max = threshold[2],
      result = result
    )
  )
}


#' @export
print.equiv_change_mean <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("For alpha =", format(x$alpha, ...), "\n")

  if (x$modcv) {
    cat("Modified CV used\n")
  }

  justify3 <- c("right", "centre", "centre")
  width3 <- c(16L, 16L, 16L)

  justify2 <- c("right", "centre")
  width2 <- c(16L, 32L)

  cat(format_row(list("", "Qualification", "Sample"),
                 justify3, width3, ...))
  cat(format_row(list("Number", x$n_qual, x$n_sample),
                 justify3, width3, ...))
  cat(format_row(list("Mean", x$mean_qual, x$mean_sample),
                 justify3, width3, ...))
  cat(format_row(list("SD", x$sd_qual, x$sd_sample),
                 justify3, width3, ...))
  cat(format_row(list("Result", x$result),
                 justify2, width2, ...))
  cat(format_row(
    list("Passing Range", paste0(format(x$threshold[1], ...),
                                 " to ",
                                 format(x$threshold[2], ...))),
    justify2, width2, ...)
  )
}
