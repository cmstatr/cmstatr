# This function attempts to catch common errors, such as the
# user passing a vector into df and not specifying x (as would happen
# if the user called a function from inside summarize without using
# the named argument x).
#
# @param df a data.frame or NULL
# @param x the vector
# @param c the call
# @param arg_name the name of the vector argument being targeted
#
#' @importFrom rlang enquo eval_tidy abort quo_text call_name call_args call_args_names
verify_tidy_input <- function(df, x, c, arg_name) {
  if (is.vector(df)) {
    cname <- call_name(c)
    cargs <- call_args(c)
    cargnames <- call_args_names(c)
    abort(
      paste0(
        "Argument `", cargnames[[1]],
        "` should be either a data.frame or NULL.\n",
        "  Did you mean: `",
        cname, "(", arg_name, " = ", quo_text(cargs[[1]]), ")` ?\n"
      )
    )
  }
}
