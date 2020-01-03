
#' Calculate the coefficient of variation
#'
#' @description
#' The coefficient of variation (CV) is the ratio of the standard
#' deviation to the mean of a sample. This function takes a vector
#' of data and calculates the CV.
#'
#' @param x a vector
#'
#' @return
#' The calcualted CV
#'
#' @export
cv <- function(x) {
  sd(x) / mean(x)
}
