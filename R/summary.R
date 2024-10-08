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

#' @export
condition_summary <- function(data, ...) {
  UseMethod("condition_summary")
}

# describeIn....
#' @export
condition_summary.data.frame <- function(data, x, condition,
                                         ref_condition = NULL) {
  x <- eval_tidy(enquo(x), data)
  groups <- eval_tidy(enquo(condition), data)

  condition_summary_base(x, groups, ref_condition, basis = NULL)
}

# describeIn....
#' @export
condition_summary.basis <- function(data, ref_condition = NULL) {
  if (is.null(data$groups)) {
    stop("`groups` is empty, probably because a pooling method wasn't used.")
  }
  if (!is.data.frame(data$basis)) {
    stop("basis is not data.frame, likely because pooling method not used")
  }

  condition_summary_base(data$data, data$groups, ref_condition, data$basis)
}
