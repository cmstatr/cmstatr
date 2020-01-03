context("CV")

test_that("CV produces expected results", {
  x <- rnorm(30, 100, 8)

  res <- cv(x)

  expect_equal(res, sd(x) / mean(x))
})
