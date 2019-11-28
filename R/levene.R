
#' Levene's Test for Equality of Variance
#'
#' @description
#' This function performs the Levene's test for equality of variance.
#'
#' @param data a data.frame
#' @param x the variable in the data.frame or a vector on which to perform the
#'          Levene's test (usually strength)
#' @param groups a variable in the data.frame that defines the groups
#' @param alpha the significance level (default 0.05)
#'
#' @return
#' Returns an object of class \code{adk}. This object has the following fields:
#' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{data}}{the original data used to compute the ADK}
#'   \item{\code{groups}}{a vector of the groups used in the computation}
#'   \item{\code{alpha}}{the value of alpha specified}
#'   \item{\code{n}}{the total number of observations}
#'   \item{\code{k}}{the number of groups}
#'   \item{\code{f}}{the value of the F test statistic}
#'   \item{\code{p}}{the computed p-value}
#'   \item{\code{reject_equal_variance}}{a boolean value indicating whether the
#'     null hypothesis that all samples have the same variance is rejected}
#' }
#'
#' @details
#' This function performs the Levene's test for equality of variance. The
#' data is transformed as follows:
#'
#' \deqn{wij = | xij - mi |}
#'
#' Where \eqn{mi} is median of the \eqn{ith} group. An F-Test is then
#' performed on the transformed data.
#'
#' @references
#' “Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats var.test median pf
#'
#' @export
levene_test <- function(data, x, groups, alpha = 0.05) {
  res <- list()
  class(res) <- "levene"

  res$call <- match.call()

  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")
  data_vector <- eval_tidy(enquo(x), data)

  verify_tidy_input(
    df = data,
    x = groups,
    c = match.call(),
    arg_name = "groups")
  group_vector <- eval_tidy(enquo(groups), data)

  res$data <- data_vector
  res$groups <- group_vector
  res$alpha <- alpha

  transformed_groups <- lapply(levels(as.factor(group_vector)), function(lvl) {
    group_data <- data_vector[group_vector == lvl]
    w <- abs(group_data - median(group_data))
    return(w)
  })

  n <- length(data_vector)
  k <- length(transformed_groups)

  grand_mean <- mean(unlist(transformed_groups))

  f_stat_numerator <- sum(sapply(transformed_groups, function(group_data) {
    ni <- length(group_data)
    res <- ni * (mean(group_data) - grand_mean) ^ 2 / (k - 1)
    return(res)
  }))

  f_stat_denomenator <- sum(sapply(transformed_groups, function(group_data) {
    group_data_minus_group_mean <- group_data - mean(group_data)
    res <- sum((group_data_minus_group_mean) ^ 2) / (n - k)
    return(res)
  }))

  res$n <- n
  res$k <- k
  res$f <- f_stat_numerator / f_stat_denomenator

  res$p <- pf(res$f, df1 = k - 1, df2 = n - k, lower.tail = FALSE)
  res$reject_equal_variance <- res$p <= alpha

  return(res)
}


#' Glance at a \code{levene} object
#'
#' @description
#' Glance accepts an object of type \code{levele} and returns a
#' \code{\link[tibble:tibble]{tibble::tibble}} with
#' one row of summaries.
#'
#' Glance does not do any calculations: it just gathers the results in a
#' tibble.
#'
#' @param x a \code{levene} object returned from \code{\link{levene_test}}
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#'
#' @return
#' A one-row \code{\link[tibble:tibble]{tibble::tibble}} with the following
#' columns:
#'
#' \item{\code{alpha}}{the value of alpha specified}
#' \item{\code{n}}{the total number of observations}
#' \item{\code{k}}{the number of groups}
#' \item{\code{f}}{the value of the F test statistic}
#' \item{\code{p}}{the computed p-value}
#' \item{\code{reject_equal_variance}}{a boolean value indicating whether the
#'       null hypothesis that all samples have the same variance is rejected}
#'
#'
#' @seealso
#' \code{\link{levene_test}}
#'
#' @examples
#' df <- data.frame(
#'   groups = c(rep("A", 5), rep("B", 6)),
#'   strength = c(rnorm(5, 100, 6), rnorm(6, 105, 7))
#' )
#' levene_result <- levene_test(df, strength, groups)
#' glance(levene_result)
#'
#' ## # A tibble: 1 x 6
#' ##   alpha     n     k     f      p reject_equal_variance
#' ##   <dbl> <int> <int> <dbl>  <dbl> <lgl>
#' ## 1  0.05    11     2  3.93 0.0788 FALSE
#'
#' @method glance levene
#' @importFrom tibble tibble
#'
#' @export
glance.levene <- function(x, ...) {  # nolint
  with(
    x,
    tibble::tibble(
      alpha = alpha,
      n = n,
      k = k,
      f = f,
      p = p,
      reject_equal_variance = reject_equal_variance
    )
  )
}


#' @export
print.levene <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("n = ", x$n, "\tk = ", x$k, "\n")
  cat("F = ", x$f, "\tp-value = ", x$p, "\n")
  if (x$reject_equal_variance) {
    cat("Conclusion: Samples have unequal variance ( alpha=",
        x$alpha, ")\n\n")
  } else {
    cat("Conclusion: Samples have equal variances ( alpha=",
        x$alpha, ")\n\n")
  }
}
