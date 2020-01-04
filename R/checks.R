

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
#' @importFrom rlang warn
perform_checks <- function(rules, override = c(), ...) {
  args <- list2(...)

  sapply(names(rules), function(cur_rule_name) {
    if (!(cur_rule_name %in% override)) {
      cur_rule <- rules[[cur_rule_name]]
      if (!do.call(cur_rule, args)) {
        warn(paste0("`", cur_rule_name, "` failed."))
      }
    }
  })
}
