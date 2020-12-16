

#' Normalizes strength values to ply thickness
#'
#' @description
#' This function takes a vector of strength values and a
#' vector of measured thicknesses, and a nominal thickness
#' and returns the normalized strength.
#'
#' @param strength the strength to be normalized. Either a vector or a numeric
#' @param measured_thk the measured thickness of the samples. Must be the same
#' length as strength
#' @param nom_thk the nominal thickness. Must be a single numeric value.
#'
#' @return
#' The normalized strength values
#'
#' @details
#' It is often necessary to normalize strength values so that variation in
#' specimen thickness does not unnecessarily increase variation in strength.
#' See CMH-17-1G, or other references, for information about the cases where
#' normalization is appropriate.
#'
#' Either cured ply thickness or laminate thickness may be used for
#' `measured_thk` and `nom_thk`, as long as the same decision
#' made for both values.
#'
#' The formula applied is:
#' \deqn{normalized\,value = test\,value \frac{t_{measured}}{t_{nominal}}}{
#' normalized value = test value * t_measured / t_nominal}
#'
#' If you need to normalize based on fiber volume fraction (or another method),
#' you will first need to calculate the nominal cured ply thickness (or laminate
#' thickness). Those calculations are outside the scope of this documentation.
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
#' select(thickness, strength) %>%
#'   mutate(normalized_strength = normalize_ply_thickness(strength,
#'                                                        thickness,
#'                                                        0.105)) %>%
#'   head(10)
#'
#' ##    thickness strength normalized_strength
#' ## 1      0.112  142.817            152.3381
#' ## 2      0.113  135.901            146.2554
#' ## 3      0.113  132.511            142.6071
#' ## 4      0.112  135.586            144.6251
#' ## 5      0.113  125.145            134.6799
#' ## 6      0.113  135.203            145.5042
#' ## 7      0.113  128.547            138.3411
#' ## 8      0.113  127.709            137.4392
#' ## 9      0.113  127.074            136.7558
#' ## 10     0.114  126.879            137.7543
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
#' This function computes the mean of each group, then divides each
#' observation by its corresponding group mean. This is commonly done
#' when pooling data across environments.
#'
#' @param x the variable containing the data to normalized
#' @param group the variable containing the groups
#'
#' @return
#' Returns a vector of normalized values
#'
#' @details
#' Computes the mean for each group, then divides each value by the mean for
#' the corresponding group.
#'
#' @references
#' “Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,” SAE International,
#' CMH-17-1G, Mar. 2012.
#'
#' @examples
#' library(dplyr)
#' carbon.fabric.2 %>%
#' filter(test == "WT") %>%
#'   select(condition, strength) %>%
#'   mutate(condition_norm = normalize_group_mean(strength, condition)) %>%
#'   head(10)
#'
#' ##    condition strength condition_norm
#' ## 1        CTD  142.817      1.0542187
#' ## 2        CTD  135.901      1.0031675
#' ## 3        CTD  132.511      0.9781438
#' ## 4        CTD  135.586      1.0008423
#' ## 5        CTD  125.145      0.9237709
#' ## 6        CTD  135.203      0.9980151
#' ## 7        CTD  128.547      0.9488832
#' ## 8        CTD  127.709      0.9426974
#' ## 9        CTD  127.074      0.9380101
#' ## 10       CTD  126.879      0.9365706
#'
#' @importFrom rlang enquo eval_tidy
#'
#' @export
normalize_group_mean <- function(x, group) {
  if (length(x) != length(group)) {
    stop("The length of x and groups must be equal")
  }

  if (length(x) == 0) {
    return(numeric(0))
  }

  group_means <- vapply(group, function(g) {
    cur_group <- x[group == g]
    group_mean <- mean(cur_group)
    return(group_mean)
  },
  USE.NAMES = FALSE,
  FUN.VALUE = numeric(1L))

  return(x / group_means)
}

#' Calculate the modified CV from the CV
#'
#' @description
#' This function calculates the modified coefficient of variation (CV)
#' based on a (unmodified) CV.
#' The modified CV is calculated based on the rules in CMH-17-1G. Those
#' rules are:
#'
#' \itemize{
#'   \item{}{For CV < 4\\%, CV* = 6\\%}
#'   \item{}{For 4\\% <= CV < 8\\%, CV* = CV / 2 + 4\\%}
#'   \item{}{For CV > 8\\%, CV* = CV}
#' }
#'
#' @param cv The CV to modify
#'
#' @return
#' The value of the modified CV
#'
#' @seealso
#' [cv()]
#'
#' @references
#' "Composite Materials Handbook, Volume 1. Polymer Matrix Composites
#' Guideline for Characterization of Structural Materials,"
#' SAE International, CMH-17-1G, Mar. 2012.
#'
#' @examples
#' # The modified CV for values of CV smaller than 4% is 6%
#' calc_cv_star(0.01)
#' ## [1] 0.06
#'
#' # The modified CV for values of CV larger than 8% is unchanged
#' calc_cv_star(0.09)
#' ## [1] 0.09
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
#' Transforms data according to the modified coefficient of variation (CV)
#' rule. This is used to add additional variance to datasets with
#' unexpectedly low variance, which is sometimes encountered during
#' testing of new materials over short periods of time.
#'
#' Two versions of this transformation are implemented. The first version,
#' `transform_mod_cv()`, transforms the data in a single group (with
#' no other structure) according to the modified CV rules.
#'
#' The second
#' version, `transform_mod_cv_ad()`, transforms data that is structured
#' according to both condition and batch, as is commonly done for
#' the Anderson--Darling k-Sample and Anderson-Darling tests when pooling
#' across environments.
#'
#' @details
#' The modified CV transformation takes the general form:
#'
#' \deqn{\frac{S_i^*}{S_i} (x_{ij} - \bar{x_i}) + \bar{x_i}}{
#'   Si*/Si (xij-x_bar_i) + x_bar_i
#' }
#'
#' Where \eqn{S_i^*}{Si*} is the modified standard deviation
#' (mod CV times mean) for
#' the \eqn{ith} group; \eqn{S_i}{Si} is the standard deviation
#' for the \eqn{ith} group, \eqn{\bar{x_i}}{x_bar_i} is
#' the group mean and \eqn{x_{ij}}{xij} is the observation.
#'
#' `transform_mod_cv()` takes a vector
#' containing the observations and transforms the data.
#' The equation above is used, and all observations
#' are considered to be from the same group.
#'
#' `transform_mod_cv_ad()` takes a vector containing the observations
#' plus a vector containing the corresponding conditions and a vector
#' containing the batches. This function first calculates the modified
#' CV value from the data from each condition (independently). Then,
#' within each condition, the transformation
#' above is applied to produce the transformed data \eqn{x'}.
#' This transformed data is further transformed using the following
#' equation.
#'
#' \deqn{x_{ij}'' = C (x'_{ij} - \bar{x_i}) + \bar{x_i}}{
#'   x_ij'' = C (x'_ij - x_bar_i) + x_bar_i}
#'
#' Where:
#'
#' \deqn{C = \sqrt{\frac{SSE^*}{SSE'}}}{C = sqrt(SSE* / SSE')}
#'
#' \deqn{SSE^* = (n-1) (CV^* \bar{x})^2 - \sum(n_i(\bar{x_i}-\bar{x})^2)}{
#'   SSE* = (n-1) (CV* x_bar)^2 - sum(n_i(x_bar_i-x_bar)^2)}
#'
#' \deqn{SSE' = \sum(x'_{ij} - \bar{x_i})^2}{SSE' = sum(x'_ij - x_bar_i)^2}
#'
#'
#' @param x a vector of data to transform
#' @param condition a vector indicating the condition to which each
#'        observation belongs
#' @param batch a vector indicating the batch to which each observation
#'        belongs
#'
#' @return
#' A vector of transformed data
#'
#' @examples
#' # Transform data according to the modified CV transformation
#' # and report the original and modified CV for each condition
#'
#' library(dplyr)
#' carbon.fabric %>%
#' filter(test == "FT") %>%
#'   group_by(condition) %>%
#'   mutate(trans_strength = transform_mod_cv(strength)) %>%
#'   head(10)
#'
#' ## # A tibble: 10 x 6
#' ## # Groups:   condition [1]
#' ##    id         test  condition batch strength trans_strength
#' ##    <chr>      <chr> <chr>     <int>    <dbl>          <dbl>
#' ##  1 FT-RTD-1-1 FT    RTD           1     126.           126.
#' ##  2 FT-RTD-1-2 FT    RTD           1     139.           141.
#' ##  3 FT-RTD-1-3 FT    RTD           1     116.           115.
#' ##  4 FT-RTD-1-4 FT    RTD           1     132.           133.
#' ##  5 FT-RTD-1-5 FT    RTD           1     129.           129.
#' ##  6 FT-RTD-1-6 FT    RTD           1     130.           130.
#' ##  7 FT-RTD-2-1 FT    RTD           2     131.           131.
#' ##  8 FT-RTD-2-2 FT    RTD           2     124.           124.
#' ##  9 FT-RTD-2-3 FT    RTD           2     125.           125.
#' ## 10 FT-RTD-2-4 FT    RTD           2     120.           119.
#'
#' # The CV of this transformed data can be computed to verify
#' # that the resulting CV follows the rules for modified CV
#'
#' carbon.fabric %>%
#'   filter(test == "FT") %>%
#'   group_by(condition) %>%
#'   mutate(trans_strength = transform_mod_cv(strength)) %>%
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
#' @seealso [calc_cv_star()]
#' @seealso [cv()]
#'
#' @name transform_mod_cv
NULL


transform_mod_cv_2_within_condition <- function(x, batch, cv_star) {  # nolint
  if (length(x) != length(batch)) {
    stop("x and batches must be the same length")
  }

  x_prime <- transform_mod_cv_grouped(x, batch)
  n <- length(x)
  x_bar <- mean(x)

  sse_prime <- sum(vapply(seq(along.with = x), function(i) {
    x_prime_i <- x_prime[i]
    x_bar_i <- mean(x[batch == batch[i]])
    (x_prime_i - x_bar_i) ^ 2
  }, FUN.VALUE = numeric(1L)))

  sse_star <- (n - 1) * (cv_star * x_bar) ^ 2 -
    sum(vapply(unique(batch), function(gi) {
      n_i <- sum(batch == gi)
      x_bar_i <- mean(x[batch == gi])
      n_i * (x_bar_i - x_bar) ^ 2
    }, FUN.VALUE = numeric(1L)))

  c_prime <- sqrt(sse_star / sse_prime)

  res <- vapply(seq(along.with = x), function(i) {
    x_bar_i <- mean(x[batch == batch[i]])
    c_prime * (x_prime[i] - x_bar_i) + x_bar_i
  }, FUN.VALUE = numeric(1L))

  res
}

#' @rdname transform_mod_cv
#' @export
transform_mod_cv_ad <- function(x, condition, batch) {
  if (is.null(batch)) {
    batch <- rep("A", length(x))
  }

  if (is.null(condition)) {
    condition <- rep("A", length(x))
  }

  if (length(x) != length(batch)) {
    stop("x and batches must be the same length")
  }

  if (length(x) != length(condition)) {
    stop("x and conditions must be the same length")
  }

  res <- numeric(0)

  for (ci in unique(condition)) {
    x_condition <- x[condition == ci]
    cv_star <- calc_cv_star(sd(x_condition) / mean(x_condition))
    res[condition == ci] <- transform_mod_cv_2_within_condition(
      x_condition, batch[condition == ci], cv_star
    )
  }

  res
}

transform_mod_cv_grouped <- function(x, group) {
  if (length(x) != length(group)) {
    stop("x and groups must be the same length")
  }

  res <- vapply(seq(along.with = x), function(i) {
    xi <- x[i]
    cur_group <- x[group == group[i]]
    s <- sd(cur_group)
    x_bar <- mean(cur_group)
    cv <- s / x_bar
    cv_star <- calc_cv_star(cv)
    s_star <- cv_star * x_bar

    s_star / s * (xi - x_bar) + x_bar
  }, FUN.VALUE = numeric(1L))

  res
}

#' @rdname transform_mod_cv
#' @export
transform_mod_cv <- function(x) {
  group <- rep("A", length(x))
  transform_mod_cv_grouped(x, group)
}
