
#' Start an analysis for a data set
#'
#' @description
#' Accepts a \code{data.frame} (or similar object) and starts an analysis.
#' To start an analysis, this function needs to know how to interpret the
#' columns of the \code{data.frame}: the columns representing the test,
#' the environmental condition (optional) and, optionally,
#' a further sub-division of the data. Once the analysis is started,
#' individual properties can be analyzed.
#'
#' @param data a \code{data.frame} (or similar) containing the data to
#'             analyze
#' @param test the column in the data.frame indicating the test
#'             for each observation
#' @param condition (optional) the column in the data.frame indicating
#'                  the environmental condition of each observation
#' @param subgroup (optional) a column in the data.frame indicating
#'                 a further sub-division of the data, for example rows
#'                 in the data.frame containing normalized or as-measured
#'                 values
#'
#' @return an object of class \code{cmanalysis} that can be passed to
#'         \code{\link{analyze_property}}.
#'
#' @seealso
#' \code{\link{analyze_property}}
#'
#' @importFrom dplyr ungroup
#' @importFrom rlang enquo
#'
#' @export
start_analysis <- function(data, test, condition = NULL, subgroup = NULL) {
  res <- list()
  class(res) <- "cmanalysis"

  res$data <- data.frame(data)

  res$test <- enquo(test)
  res$condition <- enquo(condition)
  res$subgroup <- enquo(subgroup)

  res$properties <- list()

  return(res)
}

#' @noRd
wrap_warning <- function(w, slot_name, filter) {
  paste0(
    "Warning during evaluation of slot `",
    slot_name,
    "` for filter `",
    filter,
    "`: ",
    conditionMessage(w)
  )
}

#' @importFrom rlang as_label
#' @noRd
check_reused_obs <- function(properties, cur_property_mask, slot_name, filter) {
  for (p in properties) {
    if (any(cur_property_mask & p$mask)) {
      # One or more observation was used in a previous result
      if (!is.null(p$slots[[slot_name]])) {
        warn(paste0(
          "Observation(s) reused when calculating `",
          slot_name,
          "` for filter `",
          as_label(p$filter),
          "` and `",
          filter,
          "`."
        ))
      }
    }
  }
}

#' @importFrom rlang eval_tidy as_label
#' @noRd
unique_values <- function(variable, property_data, filter, max_values) {
  variable_vector <- unique(eval_tidy(variable, property_data))
  if (length(variable_vector) > max_values) {
    abort(paste0(
      "The filter `", as_label(filter),
      "` produced more than ", max_values, " unique value for `",
      as_label(variable), "`."
    ))
  }
  variable_vector
}

#' @importFrom rlang eval_tidy
discard_obs <- function(data, filter) {
  mask <- eval_tidy(enquo(filter), data)
  !mask
}

#' Analyze a property as part of the analysis of a data set
#'
#' @description
#' Once an analysis has been begun using \code{\link{start_analysis}},
#' individual properties can be analyzed using this function by specifying
#' which observation to consider, and one or more expressions that perform
#' the actual analysis.
#'
#' @param an a \code{cmanalysis} object returned from
#'             \code{start_analysis} or another call to
#'             \code{analyze_property}
#' @param filter an data masking expression defined in terms of the variables
#'               in the data.frame used to start the analysis.
#' @param ... expressions used to perform the analysis or to discard
#'            observations
#'
#' @details
#' \code{analyze_property} takes a subset of the entire data set, and performs
#' one or more calculations using that subset. The subset is defined as the
#' observations for which \code{filter} is \code{TRUE}.
#'
#' The computation(s) to perform are supplied in the \code{...} argument. One
#' or more function should be supplied. A \code{data.frame} will be passed to
#' these function as the first argument (or as an argument denoted with a dot,
#' for example \code{data = .}). The function(s) supplied should return objects
#' that have \code{glance} methods. Tidy evaluation is supported.
#'
#' Normally, \code{...} will be a named (for example,
#' \code{b_basis = basis_normal(strength, batch)}). This name (\code{b_basis}
#' in this example) will be used subsequently when referring to this result.
#' If a name is not supplied, the name of the function called will be used.
#'
#' If \code{filter} is \code{TRUE} for more than one value of the condition
#' variable, then all objects returned by the function(s) must be pooled across
#' environments. This is tested by ensuring that the \code{data.frame} returned
#' from calling \code{glance} on the returned object has a column named
#' \code{group} that contains all of the values in the condition variable (after
#' application of the \code{filter}.
#'
#' @examples
#' # TODO: Write this
#'
#' @seealso
#' \code{\link{start_analysis}}
#'
#' @importFrom rlang eval_tidy enquo enquos call_standardise call_modify
#' @importFrom rlang as_label eval_tidy warn abort get_expr get_env
#'
#' @export
analyze_property <- function(an, filter, ...) {
  if (!inherits(an, "cmanalysis")) {
    stop("Must pass an `cmanalysis` object created with `start_analysis`.")
  }

  mask <- eval_tidy(enquo(filter), an$data)
  dat <- an$data[mask, ]

  prop <- list()
  prop$filter <- enquo(filter)
  prop$mask <- mask

  prop$test <- unique_values(an$test, dat, prop$filter, 1)
  prop$condition <- unique_values(an$condition, dat, prop$filter, Inf)
  prop$subgroup <- unique_values(an$subgroup, dat, prop$filter, 1)

  prop$slots <- list()
  slot_list <- enquos(...)

  for (i_slot in seq_along(slot_list)) {
    slot_name <- names(slot_list)[[i_slot]]
    if (nchar(slot_name) == 0) {
      slot_name <- call_name(slot_list[[i_slot]])
    }

    check_reused_obs(an$properties, mask, slot_name,
                     as_label(prop$filter))

    cur_slot_quos <- slot_list[[i_slot]]

    prop$slots[[slot_name]] <- withCallingHandlers(
      mini_pipe(dat, cur_slot_quos),
      warning = function(w) {
        warn(wrap_warning(w, slot_name, as_label(prop$filter)))
      }
    )

    # If we're analyzing data that is pooled across environments,
    # check if the slot is pooled
    if (length(prop$condition) > 1) {
      glance_res <- glance(prop$slots[[slot_name]])
      if (!("group" %in% names(glance_res))) {
        stop(
          paste0("`", slot_name, "` is not pooled across environemnts ",
                 " for filter `", as_label(prop$filter), "`. After ",
                 "calling `glance` on the returned object, one of the ",
                 "columns must be called `group`.")
        )
      }
      condition_df <- data.frame(condition = prop$condition)
      joined <- join_conditions(glance_res, condition_df, "group",
                                "condition", slot_name)
      if (nrow(joined) != length(prop$condition)) {
        stop(
          paste0("`", slot_name, "` is not pooled across all environments ",
                 " for filter `", as_label(prop$filter), "`."
          )
        )
      }
    }
  }

  an$properties[[length(an$properties) + 1]] <- prop

  return(an)
}

#' @importFrom dplyr bind_cols inner_join
#' @importFrom rlang ensym
#' @noRd
join_conditions <- function(left_df, right_df, left_condition_name,
                            right_condition_name, slot_name) {

  if (right_condition_name %in% names(right_df)) {
    # To join by different variables, we need a named character vector
    # The following two lines are equivalent to writing
    # `by = c(condition = assumed_group_name)` within the inner_join
    join_vars <- character()
    join_vars[[left_condition_name]] <- right_condition_name
    # TODO: Document the way that the joining works
    left_df <- inner_join(left_df, right_df, by = join_vars)
  } else {
    warn(paste0("glance(", slot_name,
                ") contains produces more than one row, but no column ",
                "is named `group`, so I don't know how to join the ",
                "result with the conditions."))
  }
  left_df
}

#' Create a \code{data.frame} based on an analysis
#'
#' @description
#' Create a \code{data.frame} based on an analysis started
#' using \code{\link{start_analysis}}. One row is created for
#' each of the combinations of test, condition and sub-group.
#' Internally, \code{glance} is called on each of the results
#' calculated using \code{\link{analyze_property}}.
#'
#' @param x an \code{cmanalysis} object
#' @param row.names not used. Included to match generic signature only
#' @param optional not used. Included to match generic signature only
#' @param ... Additional parameters that are internally passed to the
#'            \code{glance} methods.
#'
#' @seealso
#' \code{\link{start_analysis}}
#' \code{\link{analyze_property}}
#'
#' @importFrom dplyr bind_cols bind_rows inner_join
#' @importFrom rlang ensym quo_is_null `:=`
#' @method as.data.frame cmanalysis
#' @export
as.data.frame.cmanalysis <- function(x, row.names = NULL,
                                     optional = FALSE, ...) {
  if (!inherits(x, "cmanalysis")) {
    stop("Must pass an `cmanalysis` object created with `start_analysis`.")
  }
  add_column <- function(tbl, name, val) {
    if (!quo_is_null(name)) {
      new_col <- tibble(!!name := val)
      if (nrow(tbl) == 0) {
        return(new_col)
      } else {
        return(bind_cols(tbl, new_col))
      }
    }
    return(tbl)
  }

  bind_rows(lapply(x$properties, function(p) {
    row <- tibble(.rows = 0L)

    row <- add_column(row, x$test, p$test)
    row <- add_column(row, x$condition, p$condition)
    row <- add_column(row, x$subgroup, p$subgroup)
    for (i in seq_along(p$slots)) {
      s_name <- names(p$slots)[i]
      g <- glance(p$slots[[i]], ...) %>%
        # Pre-pend the name of the slot to the column name to avoid collisions
        setNames(paste0(s_name, ".", names(.)))
      if (nrow(g) > 1) {
        assumed_group_name <- paste0(s_name, ".", "group")
        condition_quos <- x$condition
        condition_name <- ensym(condition_quos)
        row <- join_conditions(row, g, condition_name,
                               assumed_group_name, s_name)
      } else {
        row <- bind_cols(row, g)
      }
    }

    row
  })) %>%
    as.data.frame()
}
