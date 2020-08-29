
#'
#'
#' @importFrom dplyr ungroup
#' @importFrom rlang enquo
#'
#' @export
build_analysis <- function(data, property, condition = NULL, subgroup = NULL) {
  res <- list()
  class(res) <- "cmanalysis"

  res$data <- ungroup(data)

  res$property <- enquo(property)
  res$condition <- enquo(condition)
  res$subgroup <- enquo(subgroup)

  res$results <- list()

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
check_reused_obs <- function(results, current_property_mask, slot_name, filter) {
  for (r in results) {
    if (any(current_property_mask & r$mask)) {
      # One or more observation was used in a previous result
      if (!is.null(r$slots[[slot_name]])) {
        warn(paste0(
          "Observation(s) reused when calculating `",
          slot_name,
          "` for filter `",
          as_label(r$filter),
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

#'
#' @importFrom rlang eval_tidy enquo enquos call_standardise call_modify
#' @importFrom rlang as_label eval_tidy warn abort
#'
#' @export
analyze_property <- function(analysis, filter, ...) {
  mask <- eval_tidy(enquo(filter), analysis$data)
  property_data <- analysis$data[mask,]

  result <- list()
  result$filter <- enquo(filter)
  result$mask <- mask

  result$property <- unique_values(
    analysis$property, property_data, result$filter, 1)
  result$condition <- unique_values(
    analysis$condition, property_data, result$filter, Inf)
  # TODO: Check that all slots are pooled across environment
  result$subgroup <- unique_values(
    analysis$subgroup, property_data, result$filter, 1)

  result$slots <- list()
  slot_list <- enquos(...)

  for (i_slot in seq_along(slot_list)) {
    slot_name <- names(slot_list)[[i_slot]]
    slot_arg <- call_standardise(
      call_modify(slot_list[[i_slot]], data = property_data))

    check_reused_obs(analysis$results, mask, slot_name,
                     as_label(result$filter))

    result$slots[[slot_name]] <- tryCatch(
      eval_tidy(slot_arg),
      warning = function(w) {
        warn(wrap_warning(w, slot_name, as_label(result$filter)))
      }
    )
  }

  analysis$results[[length(analysis$result) + 1]] <- result

  return(analysis)
}


as_tibble.cmanalysis <- function(
  x, ..., .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)) {

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

  bind_rows(lapply(x$results, function(r) {
    row <- tibble(.rows = 0L)

    row <- add_column(row, x$property, r$property)
    row <- add_column(row, x$condition, r$condition)
    row <- add_column(row, x$subgroup, r$subgroup)

    for (i in seq_along(r$slots)) {
      s_name <- names(r$slots)[i]
      g <- glance(r$slots[[i]]) %>% setNames(paste0(s_name, ".", names(.)))
      row <- bind_cols(row, g)
    }

    row
  }))
}

