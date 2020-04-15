

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
#' This function computes the mean of each group, then divides each group by
#' their corresponding group mean. This is commonly done
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

  group_means <- sapply(group, function(g) {
    cur_group <- x[group == g]
    group_mean <- mean(cur_group)
    return(group_mean)
  },
  USE.NAMES = FALSE)

  return(x / group_means)
}

#' Calculate the modified CV from the CV
#'
#' @description
#' This function calculates the Modified CV based on a (unmodified) CV.
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
#' "Composite Materials Handbook, Volume 1. Polymer Matrix Composites
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
#' Two versions of this transformation are implemented. The first version,
#' \code{transform_mod_cv()}, transforms the data in a single group (with
#' no other structure) according to the modified CV rules.
#'
#' The second
#' version, \code{transform_mod_cv_ad()}, transforms data that is structured
#' according to both condition and batch, as is commonly done for
#' the Anderson-Darling k-Sample and Anderson-Darling tests when pooling
#' across environments.
#'
#' @details
#' \code{transform_mod_cv()} takes a vector
#' containing the observations and a vector containing the groups that
#' each observation belongs to and transforms the data. The modified CV
#' is computed independently within each group and each observation is
#' transformed according to:
#'
#' \deqn{\frac{S_i^*}{S_i} (x_i - \bar{x_i}) + \bar{x_i}}{
#'   Si*/Si (xi-x_bar_i) + x_bar_i
#' }
#'
#' Where Si* is the modified standard deviation (mod CV times mean) for
#' the group; Si is the standard deviation for the group, x_bar_i is
#' the group mean and xi is the observation.
#'
#' \code{transform_mod_cv_ad()} takes a vector containing the observations
#' plus a vector containing the corresponding conditions and a vector
#' containing the batches. This function first calculates the modified
#' CV value from the data from each condition (independently). Then,
#' within each condition, the transformation
#' \code{transform_mod_cv(x, batches)}
#' is applied to produce the transformed data \eqn{x'}.
#' This transformed data is further transformed using the following
#' equation.
#'
#' \deqn{x'' = C (x'_i - \bar{x_i}) + \bar{x_i}}{
#'   x'' = C (x'_i - x_bar_i) + x_bar_i}
#'
#' Where:
#'
#' \deqn{C = \sqrt{\frac{SSE^*}{SSE'}}}{C = sqrt(SSE* / SSE')}
#'
#' \deqn{SSE^* = (n-1) (CV^* \bar{x})^2 - \sum(n_i(\bar{x_i}-\bar{x})^2)}{
#'   SSE* = (n-1) (CV* x_bar)^2 - sum(n_i(x_bar_i-x_bar)^2)}
#'
#' \deqn{SSE' = \sum(x'_i - \bar{x_i})^2}{SSE' = sum(x'_i - x_bar_i)^2}
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
#' @seealso
#' \code{\link{calc_cv_star}}
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

  sse_prime <- sum(sapply(seq(along.with = x), function(i) {
    x_prime_i <- x_prime[i]
    x_bar_i <- mean(x[batch == batch[i]])
    (x_prime_i - x_bar_i) ^ 2
  }))

  sse_star <- (n - 1) * (cv_star * x_bar) ^ 2 -
    sum(sapply(unique(batch), function(gi) {
      n_i <- sum(batch == gi)
      x_bar_i <- mean(x[batch == gi])
      n_i * (x_bar_i - x_bar) ^ 2
    }))

  c_prime <- sqrt(sse_star / sse_prime)

  res <- sapply(seq(along.with = x), function(i) {
    x_bar_i <- mean(x[batch == batch[i]])
    c_prime * (x_prime[i] - x_bar_i) + x_bar_i
  })

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

  res <- sapply(seq(along.with = x), function(i) {
    xi <- x[i]
    cur_group <- x[group == group[i]]
    s <- sd(cur_group)
    x_bar <- mean(cur_group)
    cv <- s / x_bar
    cv_star <- calc_cv_star(cv)
    s_star <- cv_star * x_bar

    s_star / s * (xi - x_bar) + x_bar
  })

  res
}

#' @rdname transform_mod_cv
#' @export
transform_mod_cv <- function(x) {
  group <- rep("A", length(x))
  transform_mod_cv_grouped(x, group)
}
