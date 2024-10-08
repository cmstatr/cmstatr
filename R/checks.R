
#' Performs a list of checks according to specified rules
#'
#' @description
#' Takes a named list of function (the rules). Each function should take
#' the arguments it requires plus ... Each function should return TRUE
#' if the rule is satisfied and FALSE otherwise.
#'
#' This function will produce warning(s) for each of the rules that fail.
#'
#' @param rules a named list of rules
#' @param override a vector of names of rules to ignore
#' @param ... arguments to pass to the rules
#'
#' @return
#' a named vector of characters. "P" if the test passed,
#' "F" if the test failed, "O" if the test was overridden,
#' and NA if the test was not run
#' due to a missing argument.
#'
#' @noRd
#'
#' @importFrom rlang list2
#' @importFrom rlang fn_fmls_names
#' @importFrom rlang warn
#' @importFrom rlang inform
perform_checks <- function(rules, override = c(), ...) {
  args <- list2(...)

  vapply(names(rules), function(cur_rule_name) {
    if (!(cur_rule_name %in% override)) {
      cur_rule <- rules[[cur_rule_name]]
      all_formal_names <- fn_fmls_names(cur_rule)
      missing_formals <- vapply(all_formal_names, function(cur_formal_name) {
        is.null(args[[cur_formal_name]]) & cur_formal_name != "..."
      },
      FUN.VALUE = logical(1L)
      )
      if (!any(missing_formals)) {
        message <- tryCatch(
          do.call(cur_rule, args),
          error = function(e) {
            stop(paste0("During evaluation of `", cur_rule_name, "`: ", e))
          })
        if (message != "") {
          warn(paste0("`", cur_rule_name, "` failed: ", message))
          return("F")
        }
        return("P")
      } else {
        inform(
          paste0("`", cur_rule_name, "` not run because parameter",
                 ifelse(sum(missing_formals) > 1, "s", ""),
                 " `",
                 paste(all_formal_names[missing_formals], collapse = "`, `"),
                 "` not specified")
        )
        return(NA_character_)
      }
    } else {
      # in override list
      return("O")
    }
  },
  FUN.VALUE = character(1L))
}

#' Gets the names of the diagnostic tests that failed
#'
#' @param x a named character vector created by perform_checks
#'
#' @return
#' A character vector of the tests that failed (if any)
#'
#' @noRd
get_check_failure_names <- function(x) {
  names(x[x == "F" & !is.na(x)])
}

process_overrides <- function(override, rules) {
  if ("all" %in% override) {
    # Remove the "all" value
    override <- override[!override %in% "all"]
    # Add all of the rules to the override vector
    override <- c(override, names(rules))
  }
  # Keep only the unique values of override
  override <- unique(override)
  # Warn if there are invalid overrides
  for (ov in override) {
    if (!ov %in% names(rules)) {
      warn(paste0("`", ov, "` is not a valid diagnostic test to override"))
    }
  }

  override
}
