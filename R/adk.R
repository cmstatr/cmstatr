
#' Anderson-Darling K-Sample Test
#'
#' @description
#' This function performs an Anderson-Darling k-sample test. This is used to
#' determine if several samples share a common (unspecified) distribution.
#'
#' @param df a data.frame
#' @param x the variable in the data.frame on which to perform the
#'          Anderson-Darling k-Sample test (usuall strength)
#' @param groups a variable in the data.frame that defines the groups
#' @param alpha the significance level (default 0.025)
#'
#' @return
#' Returns an object of class \code{adk}. This object has the following fields:
 #' \describe{
#'   \item{\code{call}}{the expression used to call this function}
#'   \item{\code{data}}{the original data used to compute the ADK}
#'   \item{\code{groups}}{a vector of the groups used in the computation}
#'   \item{\code{alpha}}{the value of alpha specified}
#'   \item{\code{k}}{the number of groups}
#'   \item{\code{sigma}}{the computed standard deviation of the test statistic}
#'   \item{\code{ad}}{the value of the Anderson-Darling k-Sample test
#'     statistic}
#'   \item{\code{p}}{the computed p-value}
#'   \item{\code{reject_same_pop}}{a boolean value indicating whether the null
#'     hypothesis that all samples come from the same distribution is rejected}
#'   \item{\code{raw}}{the orignal results returned from
#'     \link[kSamples]{ad.test}}
#' }
#'
#'
#' @details
#' This function is a wrapper for the \link[kSamples]{ad.test} function from
#' the package \code{kSamples}. The method "exact" is specified in the call to
#' \code{ad.test}. Refer to that package's documentation for details.
#'
#' There is a minor difference in the formulation of the Anderson-Darling
#' k-Sample test in CMH-17-1G, compared with that in the Scholz and
#' Stephens (1987). This difference does not affect the conclusion of the
#' test, but does affect the value of the test statistic itself. When
#' comparing the test statistic generaged by this function to that generated
#' by software that uses the CMH-17-1G formulation (such as ASAP, CMH17STATS,
#' etc.), the test statistic reported by this function will be greater by
#' a factor of \eqn{(k - 1)}, with a corresponding change in the critical
#' value.
#'
#' @references
#' F. W. Scholz and M. Stephens, “K-Sample Anderson-Darling Tests,” Journal
#' of the American Statistical Association, vol. 82, no. 399. pp. 918–924,
#' Sep-1987.
#'
#' “Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom kSamples ad.test
#' @export
ad_ksample <- function(df, x, groups, alpha = 0.025) {
  res <- list()
  class(res) <- "adk"

  res$call <- match.call()

  x <- enquo(x)
  groups <- enquo(groups)
  res$data <- eval_tidy(x, df)
  res$groups <- eval_tidy(groups, df)
  res$alpha <- alpha

  grps <- lapply(levels(as.factor(res[["groups"]])),
                 function(l){
                   res[["data"]][res[["groups"]] == l]
                 }
  )

  raw <- ad.test(grps, method = "exact")
  res$k <- raw$k
  res$sigma <- raw$sig
  res$ad <- raw$ad[2, 1]
  res$p <- raw$ad[2, 3]
  res$reject_same_pop <- res$p < alpha

  res$raw <- raw

  return(res)
}
