
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
#' @param modcv a logical value indicating whether the modified CV approach
#'              should be used.
#'
#' @return
#' Returns an object of class `adk`. This object has the following fields:
#' \item{`call`}{the expression used to call this function}
#' \item{`data`}{the original data supplied by the user}
#' \item{`groups`}{a vector of the groups used in the computation}
#' \item{`alpha`}{the value of alpha specified}
#' \item{`modcv`}{a logical value indicating whether the modified
#'                     CV approach was used.}
#' \item{`n`}{the total number of observations}
#' \item{`k`}{the number of groups}
#' \item{`f`}{the value of the F test statistic}
#' \item{`p`}{the computed p-value}
#' \item{`reject_equal_variance`}{a boolean value indicating whether the
#'   null hypothesis that all samples have the same variance is rejected}
#' \item{`modcv_transformed_data`}{the data after the modified CV
#'                                      transformation}
#' @details
#' This function performs the Levene's test for equality of variance. The
#' data is transformed as follows:
#'
#' \deqn{w_{ij} = \left| x_{ij} - m_i \right|}{wij = | xij - mi |}
#'
#' Where \eqn{m_i}{mi} is median of the \eqn{ith} group. An F-Test is then
#' performed on the transformed data.
#'
#' When `modcv=TRUE`, the data from each group is first transformed
#' according to the modified coefficient of variation (CV) rules before
#' performing Levene's test.
#'
#' @references
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @examples
#' library(dplyr)
#'
#' carbon.fabric.2 %>%
#'   filter(test == "FC") %>%
#'   levene_test(strength, condition)
#' ##
#' ## Call:
#' ## levene_test(data = ., x = strength, groups = condition)
#' ##
#' ## n = 91          k = 5
#' ## F = 3.883818    p-value = 0.00600518
#' ## Conclusion: Samples have unequal variance ( alpha = 0.05 )
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats var.test median pf
#'
#' @seealso [calc_cv_star()]
#' @seealso [transform_mod_cv()]
#'
#' @export
levene_test <- function(data = NULL, x, groups, alpha = 0.05, modcv = FALSE) {
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

  if (modcv == TRUE) {
    res$modcv <- TRUE
    res$modcv_transformed_data <- transform_mod_cv_grouped(
      data_vector, group_vector
    )
    data_vector <- res$modcv_transformed_data
  } else {
    res$modcv <- FALSE
    res$modcv_transformed_data <- NULL
  }

  transformed_groups <- lapply(levels(as.factor(group_vector)), function(lvl) {
    group_data <- data_vector[group_vector == lvl]
    w <- abs(group_data - median(group_data))
    return(w)
  })

  n <- length(data_vector)
  k <- length(transformed_groups)

  grand_mean <- mean(unlist(transformed_groups))

  f_stat_numerator <- sum(vapply(transformed_groups, function(group_data) {
    ni <- length(group_data)
    res <- ni * (mean(group_data) - grand_mean) ^ 2 / (k - 1)
    return(res)
  }, FUN.VALUE = numeric(1L)))

  f_stat_denomenator <- sum(vapply(transformed_groups, function(group_data) {
    group_data_minus_group_mean <- group_data - mean(group_data)
    res <- sum((group_data_minus_group_mean) ^ 2) / (n - k)
    return(res)
  }, FUN.VALUE = numeric(1L)))

  res$n <- n
  res$k <- k
  res$f <- f_stat_numerator / f_stat_denomenator

  res$p <- pf(res$f, df1 = k - 1, df2 = n - k, lower.tail = FALSE)
  res$reject_equal_variance <- res$p <= alpha

  return(res)
}


#' Glance at a `levene` object
#'
#' @description
#' Glance accepts an object of type `levene` and returns a
#' [tibble::tibble()] with
#' one row of summaries.
#'
#' Glance does not do any calculations: it just gathers the results in a
#' tibble.
#'
#' @param x a `levene` object returned from [levene_test()]
#' @param ... Additional arguments. Not used. Included only to match generic
#'            signature.
#'
#'
#' @return
#' A one-row [tibble::tibble()] with the following
#' columns:
#'
#' \item{`alpha`}{the value of alpha specified}
#' \item{`modcv`}{a logical value indicating whether the modified
#'                     CV approach was used.}
#' \item{`n`}{the total number of observations}
#' \item{`k`}{the number of groups}
#' \item{`f`}{the value of the F test statistic}
#' \item{`p`}{the computed p-value}
#' \item{`reject_equal_variance`}{a boolean value indicating whether the
#'       null hypothesis that all samples have the same variance is rejected}
#'
#'
#' @seealso
#' [levene_test()]
#'
#' @examples
#' df <- data.frame(
#'   groups = c(rep("A", 5), rep("B", 6)),
#'   strength = c(rnorm(5, 100, 6), rnorm(6, 105, 7))
#' )
#' levene_result <- levene_test(df, strength, groups)
#' glance(levene_result)
#'
#' ## # A tibble: 1 x 7
#' ##   alpha modcv     n     k      f     p reject_equal_variance
#' ##   <dbl> <lgl> <int> <int>  <dbl> <dbl> <lgl>
#' ## 1  0.05 FALSE    11     2 0.0191 0.893 FALSE
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
      modcv = modcv,
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

  justify <- c("left", "left")
  width <- c(16L, 16L)

  cat(format_row_equal(list("n", x$n, "k", x$k),
                       justify, width, ...))

  if (x$modcv == TRUE) {
    cat("Modified CV Approach Used", "\n")
  }

  cat(format_row_equal(list("F", x$f, "p-value", x$p),
                       justify, width, ...))
  if (x$reject_equal_variance) {
    cat("Conclusion: Samples have unequal variance ( alpha =",
        x$alpha, ")\n\n")
  } else {
    cat("Conclusion: Samples have equal variances ( alpha =",
        x$alpha, ")\n\n")
  }
}
