#' Detect outliers using the maximum normed resiudal method
#'
#' @param df a data.frame
#' @param x the variable in the data.frame for which to find the MNR
#'          or a vector if \code{df=NULL}
#' @param alpha the significance level for the test. Defaults to 0.05
#'
#' @details
#' \code{df} is an optional argument. If \code{df} is given, it should be a
#' \code{data.frame} (or similar object). When \code{df} is specified, the
#' value of \code{x} is expected to be a variable within \code{df}. If
#' \code{df} is not specified, \code{x} must be a vector.
#'
#' The maximum normed residual test is a test for outliers. The test statistic
#' is given in CMH-17-1G. Outliers are identified in the returned object.
#'
#' @return an object of class \code{mnr}
#' This object has the following fields:
#' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{data}}{the original data used to compute the MNR}
#'   \item{\code{mnr}}{the computed MNR test statistic}
#'   \item{\code{crit}}{the critical value given the sample size and the
#'                      significance level}
#'   \item{\code{outliers}}{a data.frame containing the \code{index} and
#'                          \code{value} of each of the identified outliers}
#'   \item{\code{n_outliers}}{the number of outliers found}
#' }
#'
#'
#' @export
maximum_normed_residual <- function(df = NULL, x, alpha = 0.05) {
  res <- list()
  class(res) <- "mnr"

  res$call <- match.call()

  x <- enquo(x)
  cur_data <- eval_tidy(x, df)
  res$data <- cur_data

  cur_mnr <- max(abs(res$data - mean(res$data)) / sd(res$data))
  res$mnr <- cur_mnr
  cur_crit <- maximum_normed_residual_critical(length(res$data), alpha)
  res$crit <- cur_crit

  res$outliers <- data.frame(index = c(), value = c())

  res$n_outliers <- 0

  for (i in 1:(length(res$data) - 1)) {
    if (cur_mnr >= cur_crit) {
      worst_index <- which.max(abs(cur_data - mean(cur_data)))
      res$outliers <- rbind(
        res$outliers,
        data.frame(index = worst_index, value = cur_data[worst_index])
      )
      res$n_outliers <- res$n_outliers + 1
      cur_data <- cur_data[-worst_index]
      cur_mnr <- max(abs(cur_data - mean(cur_data)) / sd(cur_data))
      cur_crit <- maximum_normed_residual_critical(length(cur_data), alpha)
    }
  }

  return(res)
}

maximum_normed_residual_critical <- function(n, alpha) {
  t <- qt(p = 1 - alpha / (2 * n), df = n - 2, lower.tail = TRUE, log.p = FALSE)
  return( (n - 1) / sqrt(n) * sqrt(t ^ 2 / (n - 2 + t ^ 2)))
}

#' @export
print.mnr <- function(x, ...) {
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("MNR = ", x$mnr, " ( critical value = ", x$crit, ")\n\n")
  if (nrow(x$outliers) == 0) {
    cat("No outliers detected\n\n")
  } else {
    cat("Outliers:\n")
    print(x$outliers)
  }
}
