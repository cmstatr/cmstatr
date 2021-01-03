

result_formatting <- list(
  "basis" = function(x, condition) {
    if (is.data.frame(x$basis)) {
      return(list(
        x$basis$value[x$basis$group == condition],
        x$distribution
      ))
    }
    return(list(x$basis, x$distribution))
  },
  "integer" = function(x, condition) list(x)
)


#' Start an analysis for a data set
#'
#' @description
#' Accepts a `data.frame` (or similar object) and starts an analysis.
#' To start an analysis, this function needs to know how to interpret the
#' columns of the `data.frame`: the columns representing the test,
#' the environmental condition (optional) and, optionally,
#' a further sub-division of the data. Once the analysis is started,
#' individual properties can be analyzed.
#'
#' @param data a `data.frame` (or similar) containing the data to
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
#' @return an object of class `cmanalysis` that can be passed to
#'         [analyze_property()].
#'
#' @seealso
#' [analyze_property()]
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

  res$result_formatting <- result_formatting

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

#' TODO: Document
#' @importFrom rlang eval_tidy
discard_obs <- function(data, filter) {
  mask <- eval_tidy(enquo(filter), data)
  !mask
}

#' Analyze a property as part of the analysis of a data set
#'
#' @description
#' Once an analysis has been begun using [start_analysis()],
#' individual properties can be analyzed using this function by specifying
#' which observation to consider, and one or more expressions that perform
#' the actual analysis.
#'
#' @param an a `cmanalysis` object returned from
#'             `start_analysis` or another call to
#'             `analyze_property`
#' @param filter an data masking expression defined in terms of the variables
#'               in the data.frame used to start the analysis.
#' @param ... expressions used to perform the analysis or to discard
#'            observations
#'
#' @details
#' `analyze_property` takes a subset of the entire data set, and performs
#' one or more calculations using that subset. The subset is defined as the
#' observations for which `filter` is `TRUE`.
#'
#' The computation(s) to perform are supplied in the `...` argument. One
#' or more function should be supplied. A `data.frame` will be passed to
#' these function as the first argument (or as an argument denoted with a dot,
#' for example `data = .`). The function(s) supplied should return objects
#' that have `glance` methods. Tidy evaluation is supported.
#'
#' Normally, `...` will be a named (for example,
#' `b_basis = basis_normal(strength, batch)`). This name (`b_basis`
#' in this example) will be used subsequently when referring to this result.
#' If a name is not supplied, the name of the function called will be used.
#'
#' If `filter` is `TRUE` for more than one value of the condition
#' variable, then all objects returned by the function(s) must be pooled across
#' environments. This is tested by ensuring that the `data.frame` returned
#' from calling `glance` on the returned object has a column named
#' `group` that contains all of the values in the condition variable (after
#' application of the `filter`.
#'
#' @examples
#' # TODO: Write this
#'
#' @seealso
#' [start_analysis()]
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

#' Create a `data.frame` from an analysis
#'
#' @description
#' Create a `data.frame` based on an analysis started
#' using [start_analysis()]. One row is created for
#' each of the combinations of test, condition and sub-group.
#' Internally, `glance` is called on each of the results
#' calculated using [analyze_property()].
#'
#' @param x an `cmanalysis` object
#' @param row.names not used. Included to match generic signature only
#' @param optional not used. Included to match generic signature only
#' @param ... Additional parameters that are internally passed to the
#'            `glance` methods.
#'
#' @seealso
#' [start_analysis()]
#' [analyze_property()]
#'
#' @importFrom dplyr bind_cols bind_rows inner_join
#' @importFrom rlang ensym quo_is_null `:=`
#' @importFrom stats setNames
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

  bound_rows <- bind_rows(lapply(x$properties, function(p) {
    row <- tibble(.rows = 0L)

    row <- add_column(row, x$test, p$test)
    row <- add_column(row, x$condition, p$condition)
    row <- add_column(row, x$subgroup, p$subgroup)
    for (i in seq_along(p$slots)) {
      s_name <- names(p$slots)[i]
      g <- glance(p$slots[[i]], ...)
        # Pre-pend the name of the slot to the column name to avoid collisions
      g <- setNames(g, paste0(s_name, ".", names(g)))
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
  }))

  as.data.frame(bound_rows)
}


#' Set formatting for a type of result
#'
#' @description
#' When building a `flextable` from an `cmanalysis` object, each result
#' must be converted into text that will be placed into one or more cells.
#' Default formatting is defined for common types of results. This default
#' formatting can be overridden or formatting for other types of results can
#' be set by this function.
#'
#' @param an a `cmanalysis` object
#' @param class the name of the `class` to set the formatting for
#'              (must be a `character` value)
#' @param fcn a `function`. See Details.
#'
#' @details
#' If you want to set the formatting for an object of `class` `basis`, you
#' would set `class="basis"`.
#'
#' The argument `fcn` must be a `function` with two arguments: the first
#' argument is the object to be formatted; the second argument is the
#' condition being formatted. The condition argument is only relevant when
#' the result is pooled across environments, but the argument must be always
#' be present.
#'
#' The function `fcn` must return a list. Each element of this list will
#' correspond with one cell in the table. Each element must be of a type
#' that can be coerced to `character`.
#'
#' For example, call:
#'
#' ```
#' set_result_formatting(a, "integer", function(x, c) {
#'   list(
#'     x,
#'     paste0("x is: ", x),
#'     paste0("2 times x is: ", x)
#'   )
#' })
#' ```
#'
#' Would cause the following cells to be created by [`create_flextable`] when
#' it encounters a result of class `integer` and a value of 10:
#'
#' | 10               |
#' | ---------------- |
#' | x is: 10         |
#' | 2 times x is: 20 |
#'
#' @examples
#' # TODO: Write examples
#'
#' @seealso
#' [start_analysis()]
#' [create_flextable()]
#'
#' @export
set_result_formatting <- function(an, class, fcn) {
  if (!inherits(an, "cmanalysis")) {
    stop("Must pass an `cmanalysis` object created with `start_analysis`.")
  }

  if (!is.character(class) | length(class) != 1) {
    stop("Argument `class` must be of type character.")
  }

  if (!is.function(fcn)) {
    stop("Argument `fcn` must be a function.")
  }

  if (length(formals(fcn)) != 2) {
    stop("Argument `fcn` must be a function with two arguments")
  }

  an$result_formatting[[class]] <- fcn

  an
}


#' Create a `flextable` from a `cmanalysis` object
#'
#' @description
#' TODO: Write
#'
#' @param an a `cmanalysis` object created with [start_analysis()]
#' @param numeric_fmt a formatting string (passed to [sprintf()]) used when
#'                    formatting numeric values
#'
#' @seealso
#' [set_result_formatting()]
#' [start_analysis()]
#'
#' @importFrom flextable align flextable merge_h merge_v theme_box
#' @importFrom flextable fix_border_issues set_header_labels
#' @importFrom tidyr pivot_wider
#'
#' @export
create_flextable <- function(an, numeric_fmt = "%.2f") {
  if (!inherits(an, "cmanalysis")) {
    stop("Must pass an `cmanalysis` object created with `start_analysis`.")
  }

  if (length(an$properties) <= 0) {
    stop("Cannot create flextable if no properties have been analyzed")
  }

  # footnote_list <- list()

  rows <- bind_rows(lapply(an$properties, function(p) {
    bind_rows(lapply(
      seq_along(p$slots),
      function(i_slot) {
        s <- p$slots[[i_slot]]

        cur_class <- class(s)
        if (!cur_class %in% names(an$result_formatting)) {
          stop(paste0("No table formatter defined for class `",
                      cur_class, "`"))
        }
        bind_rows(lapply(
          p$condition,
          function(c) {
            cur_cell_value <- an$result_formatting[[cur_class]](s, c)
            cur_cell_value <- lapply(cur_cell_value, function(v) {
              if (is.integer(v)) {
                as.character(v)
              } else if (is.numeric(v)) {
                sprintf(v, fmt = numeric_fmt)
              } else{
                v
              }
            })
            cur_cell_value <- unlist(cur_cell_value)
            slot_name <- names(p$slots)[i_slot]
            # footnote_list <<- append(
            #   footnote_list,
            #   footnote_fcn[[cur_class]](p$test, slot_name, s, c)
            # )
            tibble(
              test = p$test,
              condition = c,
              slot = slot_name,
              i = seq_along(cur_cell_value),
              value = cur_cell_value
            )
          }
        ))
      }
    ))
  }))

  rows_wider <- pivot_wider(rows, names_from = "condition",
                            values_from = "value")
  table <- flextable(rows_wider,
                     col_keys = names(rows_wider)[names(rows_wider) != "i"])
  table <- theme_box(table)

  for (p in an$properties) {
    test_name <- p$test
    if (length(p$condition) > 1) {
      # Pooled, so we will merge the cells
      for (i_slot in seq_along(p$slots)) {
        slot_name <- names(p$slots)[i_slot]

        table <- merge_h(table,
                         i = rows_wider[["test"]] == test_name &
                           rows_wider[["slot"]] == slot_name)
      }
    }
  }

  table <- set_header_labels(table, slot = "")
  table <- merge_v(table, j = ~ test + slot)
  table <- align(table, align = "center")
  table <- align(table, part = "header", align = "center",
                 j = 3:(ncol(rows_wider) - 1))
  table <- fix_border_issues(table)

  table[["cmanalysis"]] <- an
  class(table) <- c("cmtable", class(table))

  table
}
