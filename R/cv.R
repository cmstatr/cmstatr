
#' Calculate the coefficient of variation
#'
#' @description
#' The coefficient of variation (CV) is the ratio of the standard
#' deviation to the mean of a sample. This function takes a vector
#' of data and calculates the CV.
#'
#' @param x a vector
#' @param na.rm logical. Should missing values be removed?
#'
#' @return
#' The calculated CV
#'
#' @examples
#' set.seed(15)  # make this example reproducible
#' x <- rnorm(100, mean = 100, sd = 5)
#' cv(x)
#' ## [1] 0.04944505
#'
#' # the cv function can also be used within a call to dplyr::summarise
#' library(dplyr)
#' carbon.fabric %>%
#' filter(test == "WT") %>%
#'   group_by(condition) %>%
#'   summarise(mean = mean(strength), cv = cv(strength))
#'
#' ## # A tibble: 3 x 3
#' ##   condition  mean     cv
#' ##   <chr>     <dbl>  <dbl>
#' ## 1 CTD        137. 0.0417
#' ## 2 ETW        135. 0.0310
#' ## 3 RTD        142. 0.0451
#'
#'
#' @export
cv <- function(x, na.rm = FALSE) {  # nolint
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)  # nolint
}
