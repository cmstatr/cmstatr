context("anderson darling test")

test_that("AD test matches results from STAT17 (normal)", {
  data <- c(
    79.9109621761937,
    77.9447436346388,
    79.717168019752,
    87.3547460860547,
    76.2404769192413,
    75.7026911300246,
    79.5952709280298,
    76.7833784980155,
    77.5791472067831,
    78.4164523339268,
    79.2819398818745,
    77.6346481930964,
    81.2182937743241,
    81.1826431730731,
    86.0561762593461,
    82.1837784884495,
    80.7564920650884,
    79.3614980225488
  )
  res <- anderson_darling(data, pnorm, mean = mean(data), sd = sd(data))
  expect_equal(res$p, 0.0840)
})

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
  expect_equal(res$p, 0.465)
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

test_that("ad_p_inf result match published results", {
  # Published values from:
  # M. A. Stephens, “EDF Statistics for Goodness of Fit and Some Comparisons,”
  # Journal of the American Statistical Association, vol. 69, no. 347.
  # pp. 730–737, Sep-1974.

  # The result here is alpha. We can only expect that the result will match
  # the publised results to about 0.01 as that is the accuracy generally
  # reported in literature.

  expect_equal(0.15, ad_p_inf(1.610), tolerance = 0.01)
  expect_equal(0.10, ad_p_inf(1.933), tolerance = 0.01)
  expect_equal(0.05, ad_p_inf(2.492), tolerance = 0.01)
  expect_equal(0.025, ad_p_inf(3.070), tolerance = 0.01)
  expect_equal(0.01, ad_p_inf(3.857), tolerance = 0.01)
})

test_that("ad_inf raises warning if tolerance is unachievable", {
  expect_warning({
    ad_p_inf(1.933, abs_tol = 1e-1000)
  })
})

test_that("ad_p produces the same results as ad_p_inf for large n", {
  # Stephens (1974) suggests that the values publised for infinite sample size
  # hold for values of n >= 5
  # Ref:
  # M. A. Stephens, “EDF Statistics for Goodness of Fit and Some Comparisons,”
  # Journal of the American Statistical Association, vol. 69, no. 347.
  # pp. 730–737, Sep-1974.

  expect_equal(0.15, ad_p(1.610, n = 5), tolerance = 0.01)
  expect_equal(0.10, ad_p(1.933, n = 5), tolerance = 0.01)
  expect_equal(0.05, ad_p(2.492, n = 5), tolerance = 0.01)
  expect_equal(0.025, ad_p(3.070, n = 5), tolerance = 0.01)
  expect_equal(0.01, ad_p(3.857, n = 5), tolerance = 0.01)
})

test_that("ad_p produces larger values for smaller n", {
  expect_gt(ad_p(1.610, n = 2), ad_p(1.610, n = 10))
  expect_gt(ad_p(1.933, n = 2), ad_p(1.933, n = 10))
  expect_gt(ad_p(2.492, n = 2), ad_p(2.492, n = 10))
  expect_gt(ad_p(3.070, n = 2), ad_p(3.070, n = 10))
  expect_gt(ad_p(3.857, n = 2), ad_p(3.857, n = 10))
})

test_that("ad_p_inf produces results as publised in Marsaglia", {
  # From: G. Marsaglia and J. Marsaglia, “Evaluating the Anderson-Darling
  # Distribution,” Journal of Statistical Software, vol. 9, no. 2. 25-Feb-2004.
  expect_equal(1 - 0.999960465988611, ad_p_inf(9))
  expect_equal(1 - 0.999986184964588, ad_p_inf(10))
})
