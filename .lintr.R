linters <- linters_with_defaults(
  line_length_linter = NULL,
  trailing_whitespace_linter = NULL,
  indentation_linter = NULL,
  return_linter = NULL
)
# Ignore the auto-generated R files
exclusions <- list(
  "R/RcppExports.R",
  "vignettes/*.R"
)
