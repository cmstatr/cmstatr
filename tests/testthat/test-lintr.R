if (requireNamespace("lintr", quietly = TRUE)) {
  test_that("Package Style", {
    lintr::expect_lint_free(
      path = getwd(),
      relative_path = FALSE,
      exclusions = list(
        "R/RcppExports.R",
        # Ignore the auto-generated R files
        "vignettes/adktest.R",
        "vignettes/cmstatr_Graphing.R",
        "vignettes/cmstatr_Tutorial.R",
        "vignettes/cmstatr_Validation.R",
        "vignettes/hk_ext.R"
      ),
      linters = lintr::linters_with_defaults(
        line_length_linter = NULL,
        trailing_whitespace_linter = NULL
      )
    )
  })
}
