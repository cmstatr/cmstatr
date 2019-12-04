

#' Normalizes strength values to ply thickness
#'
#' @description
#' This function takes a strength value (or a vector of strength values) and a
#' value (or vector of the same length as strength) of the measured thicknesses
#' and returns the normalized strength.
#'
#' @param strength the strength to be normalized. Either a vector or a numeric
#' @param measured_thk the measured thickness of the samples. Must be the same
#' length as strength
#' @param nom_thk the nominal thickness. Must be a single numeric value.
#'
#' @return
#' A value (or vector) with the normalized strength values
#'
#' @details
#' It is often necessary to normalize strength values so that variation in
#' specimen thickness does not unnecessarily increase variation in strength.
#' See CMH-17, or other references, for information about the cases where
#' normalization is appropriate.
#'
#' Either cured ply thickness or laminate thickness may be used for
#' \code{measured_thk} and \code{nom_thk}, as long as CPT or laminate
#' thickness is used for both.
#'
#' The formula applied is:
#' \deqn{normalized value = test value * t_measured / t_nominal}
#'
#' If you need to normalize based on fiber volume fraction (or another method),
#' you will first need to calculate the nominal cured ply thickness (or laminate
#' thickness). Those calculations are outside the scope of this documentation.
#'
#' @export
normalize_ply_thickness <- function(strength, measured_thk, nom_thk) {
  if (length(strength) != length(measured_thk)) {
    stop("strength and measured_thk must be the same length")
  }
  if (length(nom_thk) != 1) {
    stop("nom_thk must be a single numeric value (not a vector)")
  }

  return(strength * measured_thk / nom_thk)
}


#' Normalize values to group means
#'
#' @description
#' This function computes the mean of each group, then divides each value by
#' the group mean for the group to which it belongs. This is commonly done
#' when pooling data across environments.
#'
#' @param x the variable containing the data to normalized
#' @param groups the variable containing the groups
#'
#' @return
#' Returns a vector of normalized values
#'
#' @details
#' Computes the mean for each group, then divides each value by the mean for
#' the corresponding group.
#'
#' @references
#' “Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @importFrom rlang enquo eval_tidy
#'
#' @export
normalize_group_mean <- function(x, groups) {
  if (length(x) != length(groups)) {
    stop("The length of x and groups must be equal")
  }

  if (length(x) == 0) {
    return(numeric(0))
  }

  group_means <- sapply(groups, function(g) {
    cur_group <- x[groups == g]
    group_mean <- mean(cur_group)
    return(group_mean)
  },
  USE.NAMES = FALSE)

  return(x / group_means)
}

#' Calculate the modified CV from the CV
#'
#' @description
#' This function calculates the Modified CV based on an (unmodified) CV.
#' The modified CV is calculated based on the rules in CMH-17-1G. Those
#' rules are:
#'
#' \itemize{
#'   \item{}{For CV < 4\%, CV* = 6\%}
#'   \item{}{For 4\% <= CV < 8\%, CV* = CV / 2 + 4\%}
#'   \item{}{For CV > 8\%, CV* = CV}
#' }
#'
#' @param cv The CV to modify
#'
#' @return
#' The value of the modified CV
#'
#' @references
#' "Composites Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,"
#' SAE International, CMH-17-1G, Mar. 2012.
#'
#' @export
calc_cv_star <- function(cv) {
  if (cv < 0.04) {
    cv <- 0.06
  } else if (cv < 0.08) {
    cv <- cv / 2 + 0.04
  } else {
    cv <- cv
  }
  return(cv)
}

#' Transforms data according to the modified CV rule
#'
#' @description
#' Transforms data according to the modified CV rules. Takes a vector
#' containing the observations and a vector containing the groups that
#' each observation belongs to and transforms the data. The modified CV
#' is computed within each group and each observation is transformed
#' according to:
#'
#' \deqn{\frac{S_i^*}{S_i} (x_i - \bar{x_i}) + \bar{x_i}}{
#'   Si*/Si (xi-x_bar_i) + x_bar_i
#' }
#'
#' Where Si* is the modified standard deviation (mod CV times mean) for
#' the group; Si is the standard deviation for the group, x_bar_i is
#' the group mean and xi is the observation.
#'
#' @param x a vector of data to transform
#' @param groups a vector indicating the group to which each observation in
#'        \code{x} belongs. If this is NULL, the data will be treated as
#'        unstructured (without grouping)
#'
#' @return
#' A vector of transformed data
#'
#' @examples
#' # Transform data according to the modified CV transformation
#' # and report the original and modified CV for each condition
#'
#' library(tidyverse)
#' carbon.fabric %>%
#'   filter(test == "FT") %>%
#'   mutate(trans_strength = transform_mod_cv(strength, condition)) %>%
#'   group_by(condition) %>%
#'   summarize(cv = sd(strength) / mean(strength),
#'             mod_cv = sd(trans_strength) / mean(trans_strength))
#'
#' ## # A tibble: 3 x 3
#' ##   condition     cv mod_cv
#' ##   <chr>      <dbl>  <dbl>
#' ## 1 CTD       0.0423 0.0612
#' ## 2 ETW       0.0369 0.0600
#' ## 3 RTD       0.0621 0.0711
#'
#' @seealso
#' \code{\link{calc_cv_star}}
#'
#' @export
transform_mod_cv <- function(x, groups = NULL) {
  if (is.null(groups)) {
    groups <- rep("A", length(x))
  }

  if (length(x) != length(groups)) {
    stop("x and groups must be the same length")
  }

  res <- sapply(seq(along.with = x), function(i) {
    xi <- x[i]
    cur_group <- x[groups == groups[i]]
    s <- sd(cur_group)
    x_bar <- mean(cur_group)
    cv <- s / x_bar
    cv_star <- calc_cv_star(cv)
    s_star <- cv_star * x_bar

    s_star / s * (xi - x_bar) + x_bar
  })

  res
}
