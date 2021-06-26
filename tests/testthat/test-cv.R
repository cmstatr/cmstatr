suppressMessages(library(dplyr))

test_that("CV produces expected results", {
  x <- rnorm(30, 100, 8)

  res <- cv(x)

  expect_equal(res, sd(x) / mean(x))
})

test_that("CV with na.rm=TRUE works", {
  x <- rnorm(30, 100, 8)
  x <- c(x, NA, NA, NA)

  res <- cv(x, na.rm = TRUE)

  expect_equal(res, sd(x[1:30]) / mean(x[1:30]))
})

test_that("cv works inside dplyr::summarize", {
  data.frame(
    x = rnorm(30, 100, 8)
  ) %>%
    summarize(cv = cv(x),
              test = expect_equal(cv, sd(x) / mean(x)))
})

test_that("cv works inside dplyr::summarize with na.rm=TRUE", {
  data.frame(
    x = rnorm(30, 100, 8)
  ) %>%
    bind_rows(
      data.frame(x = c(NA, NA, NA))
    ) %>%
    summarize(cv = cv(x, na.rm = TRUE),
              test = expect_equal(cv, sd(x, na.rm = TRUE) /
                                    mean(x, na.rm = TRUE)))
})
