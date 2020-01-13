

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
#' @noRd
#'
#' @importFrom rlang list2
#' @importFrom rlang fn_fmls_names
#' @importFrom rlang warn
#' @importFrom rlang inform
perform_checks <- function(rules, override = c(), ...) {
  args <- list2(...)

  sapply(names(rules), function(cur_rule_name) {
    if (!(cur_rule_name %in% override)) {
      cur_rule <- rules[[cur_rule_name]]
      all_formal_names <- fn_fmls_names(cur_rule)
      missing_formals <- sapply(all_formal_names, function(cur_formal_name) {
        is.null(args[[cur_formal_name]]) & cur_formal_name != "..."
      })
      if (!any(missing_formals)) {
        if (!do.call(cur_rule, args)) {
          warn(paste0("`", cur_rule_name, "` failed."))
          return(FALSE)
        }
        return(TRUE)
      } else {
        inform(
          paste0("`", cur_rule_name, "` not run because parameter",
                 ifelse(sum(missing_formals) > 1, "s", ""),
                 " `",
                 paste(all_formal_names[missing_formals], collapse = "`, `"),
                 "` not specified")
        )
        return(FALSE)
      }
    }
  })
}
