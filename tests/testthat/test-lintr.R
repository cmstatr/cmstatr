if (requireNamespace("lintr", quietly = TRUE)) {
  test_that("Package Style", {
    lintr::expect_lint_free(
      path = getwd(),
      relative_path = FALSE
    )
  })
}
