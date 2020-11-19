# The code in this file is based on an older version of margrittr
# See: https://github.com/tidyverse/magrittr/commit/b83268b698a03f9c15bf7586c7c131c8142203f7  # nolint


wrap_function <- function(body, pipe, env) {
  eval(call("function", as.pairlist(alist(.=)), body), env, env)  # nolint
}

# Determine whether an expression is of the type that needs a first argument.
#
# @param a non-evaluated expression.
# @return logical - TRUE if expr is of "first-argument" type, FALSE otherwise.
is_first <- function(expr) {
  !any(vapply(expr[-1], identical, logical(1), quote(.)))
}

# Prepare a magrittr rhs of "first-argument" type.
#
# @param a an expression which passes \code{is_first}
# @return an expression prepared for functional sequence construction.
prepare_first <- function(expr) {
  as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
}

# Determine whether an expression counts as a function in a magrittr chain.
#
# @param a non-evaluated expression.
# @return logical - TRUE if expr represents a function, FALSE otherwise.
is_function <- function(expr) {
  is.symbol(expr) || is.function(expr)
}

# Prepare a magrittr rhs of function type
#
# @param a an expression which passes \code{is_function}
# @return an expression prepared for functional sequence construction.
prepare_function <- function(f) {
  as.call(list(f, quote(.)))
}


# Determine whether an non-evaluated call is parenthesized
#
# @param a non-evaluated expression
# @retun logical - TRUE if expression is parenthesized, FALSE otherwise.
is_parenthesized <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`(`))
}


freduce_single <- function(value, fcn) {
  value <- withVisible(fcn(value))
  if (value[["visible"]]) value[["value"]] else invisible(value[["value"]])
}

# pipe the data `dat` into a function
#
# @param dat the data to pipe into the function
# @param rhs_quos a quosure with function to pipe the data into
mini_pipe <- function(dat, rhs_quos) {
  rhs <- get_expr(rhs_quos)
  parent_frame <- get_env(rhs_quos)

  # the environment in which to evaluate pipeline
  env <- new.env(parent = parent_frame)

  if (is_parenthesized(rhs)) {
    rhs <- eval(rhs, env, env)
  }

  rhss <-
    if (is_function(rhs))
      prepare_function(rhs)
  else if (is_first(rhs))
    prepare_first(rhs)
  else
    rhs

  if (is.call(rhss) && identical(rhss[[1L]], quote(`function`)))
    stop("Anonymous functions myst be parenthesized", call. = FALSE)

  env[["_function_list"]] <-
    wrap_function(rhss, NULL, parent_frame)

  env[["freduce_single"]] <- freduce_single

  # Create a function which applies each of the above functions in turn.
  env[["_fseq"]] <- `class<-`(
    eval(quote(function(value) freduce_single(value, `_function_list`)),
         env, env), c("fseq", "function")
  )

  # evaluate the LHS
  env[["_lhs"]] <- dat

  # compute the result by applying the function to the LHS
  result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))

  if (result[["visible"]])
    result[["value"]]
  else
    invisible(result[["value"]])

}
