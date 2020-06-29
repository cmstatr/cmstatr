
print_row <- function(content, justify, width, ...) {
  # content must be a list, justify and width must be vectors
  for (i in seq_along(content)) {
    cat(format(
      format(content[[i]], ...),
      justify = justify[i],
      width = width[i],
      ...))
  }
  cat("\n")
}

print_row_equal <- function(content, justify, column_width, ...) {
  # content must be a list with the content:
  # (var_name1, var1, var_name2, var2, etc)
  # the length of content will be twice the length of the other vectors
  for (i in seq_along(justify)) {
    cat(format(
      paste0(content[[2 * i - 1]],
             " = ",
             format(content[[2 * i]], ...)
             ),
      justify = justify[i],
      width = column_width[i],
      ...))
  }
  cat("\n")
}
