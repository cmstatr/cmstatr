
#' Levene's Test for Equality of Variance
#'
#' @description
#' This function performs the Levene's test for equality of variance.
#'
#' @param df a data.frame
#' @param x the variable in the data.frame on which to perform the
#'          Levene's test (usuall strength)
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
#'   \item{\code{reject_equal_variance}}{a boolean value indicating whether the null
#'     hypothesis that all samples have the same variance is rejected}
#' }
#'
#' @details
#' This function performs the Levene's test for equality of variance. The
#' data is transformed as follows:
#'
#' \deqn{wij = | xij - mi |}
#'
#' Where \eqn{mi} is medean of the ith group. An F-Test is then performed on the
#' transforemd data.
#'
#' @references
#' “Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats var.test median
#'
#' @export
levene_test <- function(df, x, groups, alpha = 0.05) {
  res <- list()
  class(res) <- "levene"

  res$call <- match.call()

  x <- enquo(x)
  groups <- enquo(groups)
  data_vector <- eval_tidy(x, df)
  res$data <- data_vector
  group_vector <- eval_tidy(groups, df)
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
    res <- sum( (group_data_minus_group_mean) ^ 2) / (n - k)
    return(res)
  }))

  res$n <- n
  res$k <- k
  res$f <- f_stat_numerator / f_stat_denomenator

  return(res)
}

f_test <- function(grouped_data) {
  ungrouped_data <- unlist(grouped_data)
  n <- length(ungrouped_data)
  k <- length(grouped_data)

  grand_mean <- mean(ungrouped_data)

  # p. 585
  f_stat_numerator <- sum(sapply(grouped_data, function(group_data) {
    ni <- length(group_data)
    res <- ni * (mean(group_data) - grand_mean) ^ 2 / (k - 1)
    return(res)
  }))

  f_stat_denomenator <- sum(sapply(grouped_data, function(group_data) {
    group_data_minus_group_mean <- group_data - mean(group_data)
    res <- sum( (group_data_minus_group_mean) ^ 2) / (n - k)
    return(res)
  }))

  return(f_stat_numerator / f_stat_denomenator)
}
