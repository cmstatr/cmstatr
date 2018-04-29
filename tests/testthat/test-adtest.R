context("anderson darling test")

test_that("AD test matches results from STAT17 (normal)", {
  data <- c(
    137.4438,
    139.5395,
    150.89,
    141.4474,
    141.8203,
    151.8821,
    143.9245,
    132.9732,
    136.6419,
    138.1723,
    148.7668,
    143.283,
    143.5429,
    141.7023,
    137.4732,
    152.338,
    144.1589,
    128.5218
  )
  res <- anderson_darling(data, pnorm, mean = mean(data), sd = sd(data))
  # expect_equal(res, 0.465)
  # OSL: 0.465
})

test_that("AD test matches results from STAT17 (lognormal)", {
  data <- c(
    137.4438,
    139.5395,
    150.89,
    141.4474,
    141.8203,
    151.8821,
    143.9245,
    132.9732,
    136.6419,
    138.1723,
    148.7668,
    143.283,
    143.5429,
    141.7023,
    137.4732,
    152.338,
    144.1589,
    128.5218
  )
  # OSL: 0.480
})

test_that("AD test matches results from STAT17 (weibull)", {
  data <- c(
    137.4438,
    139.5395,
    150.89,
    141.4474,
    141.8203,
    151.8821,
    143.9245,
    132.9732,
    136.6419,
    138.1723,
    148.7668,
    143.283,
    143.5429,
    141.7023,
    137.4732,
    152.338,
    144.1589,
    128.5218
  )
  # OSL: 0.179
})

test_that("ad_inf result match published results", {
  # Published values from:
  # M. A. Stephens, “EDF Statistics for Goodness of Fit and Some Comparisons,”
  # Journal of the American Statistical Association, vol. 69, no. 347.
  # pp. 730–737, Sep-1974.

  # The result here is alpha. We can only expect that the result will match
  # the publised results to about 0.01 as that is the accuracy generally
  # reported in literature.

  expect_equal(0.15, ad_inf(1.610), tolerance = 0.01)
  expect_equal(0.10, ad_inf(1.933), tolerance = 0.01)
  expect_equal(0.05, ad_inf(2.492), tolerance = 0.01)
  expect_equal(0.025, ad_inf(3.070), tolerance = 0.01)
  expect_equal(0.01, ad_inf(3.857), tolerance = 0.01)
})

test_that("ad_inf raises warning if tolerance is unachievable", {
  expect_warning({
    ad_inf(1.933, abs_tol = 1e-1000)
  })
})
