#' Detect outliers using the maximum normed residual method
#'
#' @description
#' This function detects outliers using the maximum normed residual
#' method described in CMH-17-1G. This method identifies a value
#' as an outlier if the absolute difference between the value and
#' the sample mean divided by the sample standard deviation
#' exceeds a critical value.
#'
#' @param data a data.frame
#' @param x the variable in the data.frame for which to find the MNR
#'          or a vector if `data=NULL`
#' @param alpha the significance level for the test. Defaults to 0.05
#'
#' @details
#' `data` is an optional argument. If `data` is given, it
#' should be a
#' `data.frame` (or similar object). When `data` is specified, the
#' value of `x` is expected to be a variable within `data`. If
#' `data` is not specified, `x` must be a vector.
#'
#' The maximum normed residual test is a test for outliers. The test statistic
#' is given in CMH-17-1G. Outliers are identified in the returned object.
#'
#' The maximum normed residual test statistic is defined as:
#'
#' \deqn{MNR = max \frac{\left| x_i - \bar{x} \right|}{s} }{
#'   MNR = max | x_i- x_bar | / s }
#'
#' When the value of the MNR test statistic exceeds the critical value
#' defined in Section 8.3.3.1 of CMH-17-1G, the corresponding value
#' is identified as an outlier. It is then removed from the sample, and
#' the test statistic is computed again and compared with the critical
#' value corresponding with the new sample. This process is repeated until
#' no values are identified as outliers.
#'
#' @return an object of class `mnr`
#' This object has the following fields:
#'  - `call` the expression used to call this function
#'  - `data` the original data used to compute the MNR
#'  - `alpha` the value of alpha given by the user
#'  - `mnr` the computed MNR test statistic
#'  - `crit` the critical value given the sample size and the
#'    significance level
#'  - `outliers` a data.frame containing the `index` and
#'    `value` of each of the identified outliers
#'  - `n_outliers` the number of outliers found
#'
#' @examples
#' library(dplyr)
#'
#' carbon.fabric.2 %>%
#'   filter(test=="FC" & condition=="ETW2" & batch=="A") %>%
#'   maximum_normed_residual(strength)
#'
#' ## Call:
#' ## maximum_normed_residual(data = ., x = strength)
#' ##
#' ## MNR =  1.958797  ( critical value = 1.887145 )
#' ##
#' ## Outliers ( alpha = 0.05 ):
#' ##   Index  Value
#' ##       6  44.26
#'
#' carbon.fabric.2 %>%
#'   filter(test=="FC" & condition=="ETW2" & batch=="B") %>%
#'   maximum_normed_residual(strength)
#'
#' ## Call:
#' ## maximum_normed_residual(data = ., x = strength)
#' ##
#' ## MNR =  1.469517  ( critical value = 1.887145 )
#' ##
#' ## No outliers detected ( alpha = 0.05 )
#'
#' @references
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @importFrom rlang eval_tidy enquo
#'
#' @export
maximum_normed_residual <- function(data = NULL, x, alpha = 0.05) {
  res <- list()
  class(res) <- "mnr"

  res$call <- match.call()

  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  cur_data <- eval_tidy(enquo(x), data)
  res$data <- cur_data

  res$alpha <- alpha

  indicies_cur <- seq_along(res$data)

  cur_mnr <- max(abs(res$data - mean(res$data)) / sd(res$data))
  res$mnr <- cur_mnr
  cur_crit <- maximum_normed_residual_crit(length(res$data), alpha)
  res$crit <- cur_crit

  res$outliers <- data.frame(index = c(), value = c())

  res$n_outliers <- 0

  if (is.na(cur_mnr)) {
    return(res)
  }

  for (i in 1:(length(res$data) - 2)) {
    # the `maximum_normed_residual_crit` function uses DF=n-2
    # With each loop, we remove one data point, so if we don't
    # break early, we can stop looping before n-2
    # data points (after which DF<=0)
    cur_mnr <- max(abs(cur_data - mean(cur_data)) / sd(cur_data))
    cur_crit <- maximum_normed_residual_crit(length(cur_data), alpha)
    if (cur_mnr >= cur_crit) {
      worst_index_cur <- which.max(abs(cur_data - mean(cur_data)))
      res$outliers <- rbind(
        res$outliers,
        data.frame(
          index = indicies_cur[worst_index_cur],
          value = cur_data[worst_index_cur]
        )
      )
      res$n_outliers <- res$n_outliers + 1
      cur_data <- cur_data[-worst_index_cur]
      indicies_cur <- indicies_cur[-worst_index_cur]
    } else {
      # If we didn't remove a data point, then the current MNR and the
      # critical MNR definitely won't change, so we can short-circuit
      # this function.
      break
    }
  }

  return(res)
}

maximum_normed_residual_crit <- function(n, alpha) {
  t <- qt(p = 1 - alpha / (2 * n), df = n - 2, lower.tail = TRUE, log.p = FALSE)
  return((n - 1) / sqrt(n) * sqrt(t ^ 2 / (n - 2 + t ^ 2)))
}

#' Glance at a `mnr` (maximum normed residual) object
#'
#' @description
#' Glance accepts an object of type `mnr` and returns a
#' [tibble::tibble()] with
#' one row of summaries.
#'
#' Glance does not do any calculations: it just gathers the results in a
#' tibble.
#'
#' @param x An `mnr` object
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#'
#' @return
#' A one-row [tibble::tibble()] with the following
#' columns:
#'
#' - `mnr` the computed MNR test statistic
#' - `alpha` the value of alpha used for the test
#' - `crit` the critical value given the sample size and the
#'   significance level
#' - `n_outliers` the number of outliers found
#'
#'
#' @seealso
#' [maximum_normed_residual()]
#'
#' @examples
#' x <- c(rnorm(20, 100, 5), 10)
#' m <- maximum_normed_residual(x = x)
#' glance(m)
#'
#' ## # A tibble: 1 x 4
#' ##     mnr alpha  crit n_outliers
#' ##   <dbl> <dbl> <dbl>      <dbl>
#' ## 1  4.23  0.05  2.73          1
#'
#' @method glance mnr
#' @importFrom tibble tibble
#'
#' @export
glance.mnr <- function(x, ...) {  # nolint
  with(
    x,
    tibble::tibble(
      mnr = mnr,
      alpha = alpha,
      crit = crit,
      n_outliers = n_outliers
    )
  )
}

#' Augment data with information from an `mnr` object
#'
#' @description
#' Augment accepts an `mnr` object (returned from the function
#' [maximum_normed_residual()]) and a dataset and adds the column
#' `.outlier` to the dataset. The column `.outlier` is a logical
#' vector indicating whether each observation is an outlier.
#'
#' When passing data into `augment` using the `data` argument,
#' the data must be exactly the data that was passed to
#' `maximum_normed_residual`.
#'
#' @param x an `mnr` object created by
#'          [maximum_normed_residual()]
#' @param data a `data.frame` or
#'             [tibble::tibble()]
#'             containing the original data that was passed to
#'             `maximum_normed_residual`
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#' @return
#' When `data` is supplied, `augment` returns `data`, but with
#' one column appended. When `data` is not supplied, `augment`
#' returns a new [tibble::tibble()] with the column
#' `values` containing the original values used by
#' `maximum_normed_residaul` plus one additional column. The additional
#' column is:
#'
#' - `.outler` a logical value indicating whether the observation
#'   is an outlier
#'
#' @examples
#' data <- data.frame(strength = c(80, 98, 96, 97, 98, 120))
#' m <- maximum_normed_residual(data, strength)
#'
#' # augment can be called with the original data
#' augment(m, data)
#'
#' ##   strength .outlier
#' ## 1       80    FALSE
#' ## 2       98    FALSE
#' ## 3       96    FALSE
#' ## 4       97    FALSE
#' ## 5       98    FALSE
#' ## 6      120    FALSE
#'
#' # or augment can be called without the orignal data and it will be
#' # reconstructed
#' augment(m)
#'
#' ## # A tibble: 6 x 2
#' ##   values .outlier
#' ##    <dbl> <lgl>
#' ## 1     80 FALSE
#' ## 2     98 FALSE
#' ## 3     96 FALSE
#' ## 4     97 FALSE
#' ## 5     98 FALSE
#' ## 6    120 FALSE
#'
#' @seealso
#' [maximum_normed_residual()]
#'
#' @method augment mnr
#' @importFrom tibble tibble
#'
#' @export
augment.mnr <- function(x, data = x$data, ...) {  # nolint
  if (is.data.frame(data)) {
    df <- data
  } else {
    df <- tibble::tibble(values = data)
  }

  res <- df
  res[[".outlier"]] <- FALSE
  res$.outlier[x$outliers$index] <- TRUE
  res
}

#' @export
print.mnr <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("MNR =", x$mnr, " ( critical value =", x$crit, ")\n\n")
  if (nrow(x$outliers) == 0) {
    cat("No outliers detected ( alpha =", x$alpha, ")\n\n")
  } else {
    cat("Outliers ( alpha =", x$alpha, "):\n")

    justify <- c("right", "left", "left")
    width <- c(8L, 2L, 16L)

    cat(format_row(list("Index", " ", "Value"), justify, width, ...))

    for (j in seq(along.with = x$outliers$index)) {
      cat(format_row(
        list(x$outliers[["index"]][j], " ", x$outliers[["value"]][j]),
        justify, width, ...)
      )
    }

  }
}
