#' Produce basis summary statistics for each (environmental) condition
#'
#' @description
#' Produces a `data.frame` containing the sample size and mean for each
#' condition. If a reference condition (e.g. "RTD") is specified, the ratio
#' of each condition mean value to the mean value for the reference condition
#' is also calculated. If a `basis` object returned by one of the
#' `basis_pooled` functions is given as an argument, this table also contains
#' the basis value for each condition.
#'
#' @param data a `data.frame` or a `basis` object
#' @param x the column name of the variable of interest (usually strength)
#' @param condition the column name for the condition
#' @param ref_condition a `character` representing the reference condition
#' @param ... not used
#'
#' @return a `data.frame`
#'
#' @examples
#' library(dplyr)
#' carbon.fabric.2 %>%
#'   filter(test == "WT") %>%
#'   condition_summary(strength, condition, "RTD")
#'
#' ##   condition  n     mean mean_fraction
#' ## 1       CTD 19 135.4719     0.9702503
#' ## 2       RTD 28 139.6257     1.0000000
#' ## 3       ETW 18 134.1009     0.9604312
#' ## 4      ETW2 21 130.1545     0.9321673
#'
#' carbon.fabric %>%
#'   filter(test == "FT") %>%
#'   basis_pooled_sd(strength, condition, batch) %>%
#'   condition_summary("RTD")
#'
#' ##   condition  n     mean mean_fraction    basis basis_fraction
#' ## 1       RTD 18 127.6211     1.0000000 116.8894      1.0000000
#' ## 2       ETW 18 117.8080     0.9231072 107.0762      0.9160476
#' ## 3       CTD 18 125.9629     0.9870063 115.2311      0.9858133
#'
#' @name condition_summary
NULL

condition_summary_base <- function(x, groups, ref_condition, basis) {
  if (!is.null(ref_condition)  && !ref_condition %in% groups) {
    stop("Specified condition not found.")
  }

  ref_cond_mean <- mean(x[groups == ref_condition])

  map_dfr(
    unique(groups),
    function(grp) {
      res <- data.frame(
        condition = grp,
        n = sum(groups == grp),
        mean = mean(x[groups == grp])
      )
      if (!is.null(ref_condition)) {
        res[["mean_fraction"]] <- res[["mean"]] / ref_cond_mean
      }
      if (!is.null(basis)) {
        res[["basis"]] <- basis$value[basis$group == grp]
      }
      if (!is.null(ref_condition) && !is.null(basis)) {
        res[["basis_fraction"]] <- res[["basis"]] /
          basis$value[basis$group == ref_condition]
      }
      res
    }
  )
}


#' @rdname condition_summary
#' @export
condition_summary <- function(data, ...) {
  UseMethod("condition_summary")
}

#' @rdname condition_summary
#' @export
condition_summary.data.frame <- function(data, x, condition,
                                         ref_condition = NULL, ...) {
  x <- eval_tidy(enquo(x), data)
  groups <- eval_tidy(enquo(condition), data)

  condition_summary_base(x, groups, ref_condition, basis = NULL)
}

#' @rdname condition_summary
#' @export
condition_summary.basis <- function(data, ref_condition = NULL, ...) {
  if (is.null(data$groups)) {
    stop("`groups` is empty, probably because a pooling method wasn't used.")
  }
  if (!is.data.frame(data$basis)) {
    stop("basis is not data.frame, likely because pooling method not used")
  }

  condition_summary_base(data$data, data$groups, ref_condition, data$basis)
}
