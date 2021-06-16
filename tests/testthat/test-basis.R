suppressMessages(library(dplyr))

test_that("kB factors are correct for normal distribution", {
  cmh17_factors <- matrix(
    c(2, 20.581, 36, 1.725, 70, 1.582, 104, 1.522,
      3, 6.157, 37, 1.718, 71, 1.579, 105, 1.521,
      4, 4.163, 38, 1.711, 72, 1.577, 106, 1.519,
      5, 3.408, 39, 1.704, 73, 1.575, 107, 1.518,
      6, 3.007, 40, 1.698, 74, 1.572, 108, 1.517,
      7, 2.756, 41, 1.692, 75, 1.570, 109, 1.516,
      8, 2.583, 42, 1.686, 76, 1.568, 110, 1.515,
      9, 2.454, 43, 1.680, 77, 1.566, 111, 1.513,
      10, 2.355, 44, 1.675, 78, 1.564, 112, 1.512,
      11, 2.276, 45, 1.669, 79, 1.562, 113, 1.511,
      12, 2.211, 46, 1.664, 80, 1.560, 114, 1.510,
      13, 2.156, 47, 1.660, 81, 1.558, 115, 1.509,
      14, 2.109, 48, 1.655, 82, 1.556, 116, 1.508,
      15, 2.069, 49, 1.650, 83, 1.554, 117, 1.507,
      16, 2.034, 50, 1.646, 84, 1.552, 118, 1.506,
      17, 2.002, 51, 1.642, 85, 1.551, 119, 1.505,
      18, 1.974, 52, 1.638, 86, 1.549, 120, 1.504,
      19, 1.949, 53, 1.634, 87, 1.547, 121, 1.503,
      20, 1.927, 54, 1.630, 88, 1.545, 122, 1.502,
      21, 1.906, 55, 1.626, 89, 1.544, 123, 1.501,
      22, 1.887, 56, 1.623, 90, 1.542, 124, 1.500,
      23, 1.870, 57, 1.619, 91, 1.540, 125, 1.499,
      24, 1.854, 58, 1.616, 92, 1.539, 126, 1.498,
      25, 1.839, 59, 1.613, 93, 1.537, 127, 1.497,
      26, 1.825, 60, 1.609, 94, 1.536, 128, 1.496,
      27, 1.812, 61, 1.606, 95, 1.534, 129, 1.495,
      28, 1.800, 62, 1.603, 96, 1.533, 130, 1.494,
      29, 1.789, 63, 1.600, 97, 1.531, 131, 1.493,
      30, 1.778, 64, 1.597, 98, 1.530, 132, 1.492,
      31, 1.768, 65, 1.595, 99, 1.529, 133, 1.492,
      32, 1.758, 66, 1.592, 100, 1.527, 134, 1.491,
      33, 1.749, 67, 1.589, 101, 1.526, 135, 1.490,
      34, 1.741, 68, 1.587, 102, 1.525, 136, 1.489,
      35, 1.733, 69, 1.584, 103, 1.523, 137, 1.488),
    ncol = 2, byrow = TRUE
  ) %>%
    as.data.frame() %>%
    rename(n = V1) %>%
    rename(kb = V2) %>%
    filter(n <= 95) %>%
    rowwise() %>%
    mutate(calc_kb = k_factor_normal(n, p = 0.90, conf = 0.95)) %>%
    mutate(check = expect_lte(abs(calc_kb - kb), expected = 0.002,
                              label = paste0("Validation failure for ", n, ".",
                                             "CMH-17 gives kB=", kb, ",",
                                             "library gives kB=", calc_kb)))
})

test_that("kA factors are correct for normal distribution", {
  cmh17_factors <- matrix(
    c(2, 37.094, 36, 2.983, 70, 2.765, 104, 2.676,
      3, 10.553, 37, 2.972, 71, 2.762, 105, 2.674,
      4, 7.042, 38, 2.961, 72, 2.758, 106, 2.672,
      5, 5.741, 39, 2.951, 73, 2.755, 107, 2.671,
      6, 5.062, 40, 2.941, 74, 2.751, 108, 2.669,
      7, 4.642, 41, 2.932, 75, 2.748, 109, 2.667,
      8, 4.354, 42, 2.923, 76, 2.745, 110, 2.665,
      9, 4.143, 43, 2.914, 77, 2.742, 111, 2.663,
      10, 3.981, 44, 2.906, 78, 2.739, 112, 2.662,
      11, 3.852, 45, 2.898, 79, 2.736, 113, 2.660,
      12, 3.747, 46, 2.890, 80, 2.733, 114, 2.658,
      13, 3.659, 47, 2.883, 81, 2.730, 115, 2.657,
      14, 3.585, 48, 2.876, 82, 2.727, 116, 2.655,
      15, 3.520, 49, 2.869, 83, 2.724, 117, 2.654,
      16, 3.464, 50, 2.862, 84, 2.721, 118, 2.652,
      17, 3.414, 51, 2.856, 85, 2.719, 119, 2.651,
      18, 3.370, 52, 2.850, 86, 2.716, 120, 2.649,
      19, 3.331, 53, 2.844, 87, 2.714, 121, 2.648,
      20, 3.295, 54, 2.838, 88, 2.711, 122, 2.646,
      21, 3.263, 55, 2.833, 89, 2.709, 123, 2.645,
      22, 3.233, 56, 2.827, 90, 2.706, 124, 2.643,
      23, 3.206, 57, 2.822, 91, 2.704, 125, 2.642,
      24, 3.181, 58, 2.817, 92, 2.701, 126, 2.640,
      25, 3.158, 59, 2.812, 93, 2.699, 127, 2.639,
      26, 3.136, 60, 2.807, 94, 2.697, 128, 2.638,
      27, 3.116, 61, 2.802, 95, 2.695, 129, 2.636,
      28, 3.098, 62, 2.798, 96, 2.692, 130, 2.635,
      29, 3.080, 63, 2.793, 97, 2.690, 131, 2.634,
      30, 3.064, 64, 2.789, 98, 2.688, 132, 2.632,
      31, 3.048, 65, 2.785, 99, 2.686, 133, 2.631,
      32, 3.034, 66, 2.781, 100, 2.684, 134, 2.630,
      33, 3.020, 67, 2.777, 101, 2.682, 135, 2.628,
      34, 3.007, 68, 2.773, 102, 2.680, 136, 2.627,
      35, 2.995, 69, 2.769, 103, 2.678, 137, 2.626),
    ncol = 2, byrow = TRUE
  ) %>%
    as.data.frame() %>%
    rename(n = V1) %>%
    rename(ka = V2) %>%
    filter(n <= 75) %>%
    rowwise() %>%
    mutate(calc_ka = k_factor_normal(n, p = 0.99, conf = 0.95)) %>%
    mutate(check = expect_lte(abs(calc_ka - ka), expected = 0.002,
                              label = paste0("Validation failure for ", n, ".",
                                             "CMH-17 gives kA=", ka, ",",
                                             "library gives kA=", calc_ka)))
})

test_that("(normal) basis value equals mean when sd = 0", {
  expect_equal(
    (data.frame(x = rep(100, 10)) %>%
      basis_normal(x, p = 0.9, conf = 0.95,
                   override = c("anderson_darling_normal",
                                "outliers_within_batch",
                                "between_batch_variability")))$basis,
    100
  )
})

test_that("(normal) basis value approx equals percentile for large samples", {
  m <- 100
  s <- 5
  set.seed(100)  # make sure that this doesn't fail by pure chance
  q <- qnorm(0.10, m, s, lower.tail = TRUE)
  basis <- (data.frame(x = rnorm(50, m, s)) %>%
    basis_normal(x, p = 0.90, conf = 0.95,
                 override = c("outliers_within_batch",
                              "between_batch_variability")))$basis
  expect_lt(abs(basis - q), 0.1)
})

test_that("printing of basis objects works as expected", {
  set.seed(100)
  x <- c(runif(25))

  expect_output(
    print(basis_normal(x = x, p = 0.9, conf = 0.95,
                       override = c("outliers_within_batch",
                                    "between_batch_variability"))),
    "B-Basis"
  )

  expect_output(
    print(basis_normal(x = x, p = 0.99, conf = 0.95,
                       override = c("outliers_within_batch",
                                    "between_batch_variability"))),
    "A-Basis"
  )

  expect_output(
    print(basis_normal(x = x, p = 0.9, conf = 0.9,
                       override = c("outliers_within_batch",
                                    "between_batch_variability"))),
    "[^AB-]Basis"
  )

  expect_error(
    print.basis(x)  # give it the wrong type
  )
})

test_that("normal basis value matches STAT17/ASAP result", {
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

  res <- basis_normal(x = data, p = 0.9, conf = 0.95,
                      override = c("outliers_within_batch",
                                   "between_batch_variability"))
  expect_equal(res$basis, 129.287, tolerance = 0.0005)
  expect_output(print(res), "b-basis.*129\\.2", ignore.case = TRUE)
  expect_output(print(res), "normal", ignore.case = TRUE)

  res <- basis_normal(x = data, p = 0.99, conf = 0.95,
                      override = c("outliers_within_batch",
                                   "between_batch_variability"))
  expect_equal(res$basis, 120.336, tolerance = 0.0005)
  expect_output(print(res), "a-basis.*120\\.3", ignore.case = TRUE)
  expect_output(print(res), "normal", ignore.case = TRUE)

  expect_match(res$distribution, "normal", ignore.case = TRUE)
})

test_that("normal basis values produce expected diagnostic failures", {
  set.seed(100)
  x <- c(runif(25), runif(25, max = 2), 200)
  batch <- c(rep("A", 25), rep("B", 26))

  expect_snapshot(
    res <- basis_normal(x = x, batch = batch),
    cran = TRUE
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_normal"))
  expect_length(res$override, 0)

  expect_output(print(res),
                regexp = paste("failed.+",
                               "outliers_within_batch",
                               "between_batch_variability",
                               "outliers",
                               "anderson_darling_normal",
                               sep = ".+"))
  output <- capture_output(print(res))
  expect_false(grepl("overridden", output, ignore.case = TRUE))

  # overriding the diagnostics should eliminate the warnings
  res <- basis_normal(x = x, batch = batch,
                      override = c("outliers_within_batch",
                                   "between_batch_variability",
                                   "outliers",
                                   "anderson_darling_normal"))

  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_normal"))
  expect_length(res$diagnostic_failures, 0)

  expect_output(print(res),
                regexp = paste("overridden.+",
                               "outliers_within_batch",
                               "between_batch_variability",
                               "outliers",
                               "anderson_darling_normal",
                               sep = ".+"))
  output <- capture_output(print(res))
  expect_false(grepl("failed", output, ignore.case = TRUE))

  # overriding the diagnostics using "all" should do the same thing
  res <- basis_normal(x = x, batch = batch,
                      override = "all")
  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_normal"))
  expect_length(res$diagnostic_failures, 0)

  # call basis_normal without batch
  expect_snapshot(
    res <- basis_normal(x = x)
  )

  # Check that res$... contains the correct value
  expect_equal(res$diagnostic_failures,
               c("outliers",
                 "anderson_darling_normal"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_normal(x = x,
                      override = c("outliers",
                                   "anderson_darling_normal",
                                   "outliers_within_batch",
                                   "between_batch_variability"))

  expect_equal(res$override,
               c("outliers",
                 "anderson_darling_normal",
                 "outliers_within_batch",
                 "between_batch_variability"))
  expect_length(res$diagnostic_failures, 0)
})

test_that("log-normal basis value matches STAT17 result", {
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

  res <- basis_lognormal(x = data, p = 0.9, conf = 0.95, override = "all")
  expect_equal(res$basis, 129.664, tolerance = 0.0005)
  expect_output(print(res), "b-basis.*129.6", ignore.case = TRUE)
  expect_output(print(res), "normal", ignore.case = TRUE)
  expect_output(print(res), "log", ignore.case = TRUE)

  res <- basis_lognormal(x = data, p = 0.99, conf = 0.95, override = "all")
  expect_equal(res$basis, 121.710, tolerance = 0.0005)
  expect_output(print(res), "a-basis.*121.7", ignore.case = TRUE)
  expect_output(print(res), "normal", ignore.case = TRUE)
  expect_output(print(res), "log", ignore.case = TRUE)

  expect_match(res$distribution, "log", ignore.case = TRUE)
  expect_match(res$distribution, "normal", ignore.case = TRUE)
})

test_that("lognormal basis values produce expected diagnostic failures", {
  set.seed(100)
  x <- c(runif(25), runif(25, max = 2), 200)
  batch <- c(rep("A", 25), rep("B", 26))

  expect_snapshot(
    res <- basis_lognormal(x = x, batch = batch)
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_lognormal"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_lognormal(x = x, batch = batch,
                      override = c("outliers_within_batch",
                                   "between_batch_variability",
                                   "outliers",
                                   "anderson_darling_lognormal"))

  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_lognormal"))
  expect_length(res$diagnostic_failures, 0)

  # overriding the diagnostics with "all" should do the same thing
  res <- basis_lognormal(x = x, batch = batch,
                         override = "all")

  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_lognormal"))
  expect_length(res$diagnostic_failures, 0)

  # call basis_normal without batch
  expect_snapshot(
    res <- basis_lognormal(x = x)
  )

  # Check that res$... contains the correct value
  expect_equal(res$diagnostic_failures,
               c("outliers",
                 "anderson_darling_lognormal"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_lognormal(x = x,
                      override = c("outliers",
                                   "anderson_darling_lognormal",
                                   "outliers_within_batch",
                                   "between_batch_variability"))

  expect_equal(res$override,
               c("outliers",
                 "anderson_darling_lognormal",
                 "outliers_within_batch",
                 "between_batch_variability"))
  expect_length(res$diagnostic_failures, 0)
})

test_that("Weibull basis value matches STAT17 result", {
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

  # stat17 B-Basis: 125.441
  # stat17 A-Basis: 109.150

  res <- basis_weibull(x = data, p = 0.9, conf = 0.95,
                       override = c("outliers_within_batch",
                                    "between_batch_variability"))
  expect_equal(res$basis, 125.441, tolerance = 0.3)
  expect_output(print(res), "b-basis.*125", ignore.case = TRUE)
  expect_output(print(res), "weibull", ignore.case = TRUE)

  res <- basis_weibull(x = data, p = 0.99, conf = 0.95,
                       override = c("outliers_within_batch",
                                    "between_batch_variability"))
  expect_equal(res$basis, 109.150, tolerance = 0.6)
  expect_output(print(res), "a-basis.*109", ignore.case = TRUE)
  expect_output(print(res), "weibull", ignore.case = TRUE)

  expect_match(res$distribution, "weibull", ignore.case = TRUE)
})

test_that("weibull basis values produce expected diagnostic failures", {
  set.seed(100)
  x <- c(rnorm(10, 100, 2), rnorm(10, 103, 2), 120)
  batch <- c(rep("A", 10), rep("B", 11))

  expect_snapshot(
    res <- basis_weibull(x = x, batch = batch)
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_weibull"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_weibull(x = x, batch = batch,
                         override = c("outliers_within_batch",
                                      "between_batch_variability",
                                      "outliers",
                                      "anderson_darling_weibull"))

  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_weibull"))
  expect_length(res$diagnostic_failures, 0)

  # overriding the diagnostics with "all" should do the same thing
  res <- basis_weibull(x = x, batch = batch,
                       override = "all")

  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_weibull"))
  expect_length(res$diagnostic_failures, 0)

  # call basis_normal without batch
  expect_snapshot(
    res <- basis_weibull(x = x)
  )

  # Check that res$... contains the correct value
  expect_equal(res$diagnostic_failures,
               c("outliers",
                 "anderson_darling_weibull"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_weibull(x = x,
                         override = c("outliers",
                                      "anderson_darling_weibull",
                                      "outliers_within_batch",
                                      "between_batch_variability"))

  expect_equal(res$override,
               c("outliers",
                 "anderson_darling_weibull",
                 "outliers_within_batch",
                 "between_batch_variability"))
  expect_length(res$diagnostic_failures, 0)
})

test_that("Non-parametric (small sample) basis value matches STAT17 result", {
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

  res <- basis_hk_ext(x = data, p = 0.9, conf = 0.95,
                      method = "optimum-order",
                      override = c("outliers_within_batch",
                                   "between_batch_variability"))
  expect_equal(res$basis, 124.156, tolerance = 0.05)
  expect_output(print(res), "b-basis.*124", ignore.case = TRUE)
  expect_output(print(res), "nonparametric", ignore.case = TRUE)
  expect_match(res$distribution, "nonparametric.*optimum", ignore.case = TRUE)

  res <- basis_hk_ext(x = data, p = 0.99, conf = 0.95,
                      method = "woodward-frawley",
                      override = c("outliers_within_batch",
                                   "between_batch_variability"))
  expect_equal(res$basis, 99.651, tolerance = 0.05)
  expect_output(print(res), "a-basis.*99", ignore.case = TRUE)
  expect_output(print(res), "nonparametric", ignore.case = TRUE)
  expect_match(res$distribution,
               "nonparametric.*Woodward-Frawley", ignore.case = TRUE)

  expect_error(basis_hk_ext(x = data, method = "something invalid",
                            override = c("outliers_within_batch",
                                         "between_batch_variability")))
})

test_that("non-para (small) basis values produce expected diag failures", {
  set.seed(100)
  x_small <- c(rnorm(10, 100, 2), rnorm(10, 103, 2), 120)
  batch_small <- c(rep("A", 10), rep("B", 11))
  x_large <- c(rnorm(200, 100, 2), rnorm(100, 103, 2), 120)
  batch_large <- c(rep("A", 200), rep("B", 101))

  # woodward-frawley is only for A-Basis. Should fail if we calculate B-Basis
  expect_snapshot(
    res <- basis_hk_ext(
      x = x_large, batch = batch_large, method = "woodward-frawley")
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch_large)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "correct_method_used",
                 "sample_size"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_hk_ext(x = x_large, batch = batch_large,
                      method = "woodward-frawley",
                      override = c("outliers_within_batch",
                                   "between_batch_variability",
                                   "outliers",
                                   "correct_method_used",
                                   "sample_size"))

  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "correct_method_used",
                 "sample_size"))
  expect_length(res$diagnostic_failures, 0)

  # overriding the diagnostics with "all" should do the same thing
  res <- basis_hk_ext(x = x_large, batch = batch_large,
                      method = "woodward-frawley",
                      override = "all")
  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "correct_method_used",
                 "sample_size"))
  expect_length(res$diagnostic_failures, 0)

  # optimum-order is only for B-Basis. Should fail if we calculate A-Basis
  expect_snapshot(
    res <- basis_hk_ext(
      x = x_large, batch = batch_large, method = "optimum-order",
      p = 0.99, conf = 0.95)
  )

  # call basis_normal without batch
  expect_snapshot(
    res <- basis_hk_ext(x = x_small, method = "optimum-order")
  )

  # Check that res$... contains the correct value
  expect_equal(res$diagnostic_failures,
               c("outliers"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_hk_ext(x = x_small, method = "optimum-order",
                      override = c("outliers",
                                   "outliers_within_batch",
                                   "between_batch_variability"))

  expect_equal(res$override,
               c("outliers",
                 "outliers_within_batch",
                 "between_batch_variability"))
  expect_length(res$diagnostic_failures, 0)
})

test_that("Non-parametric (large sample) basis value matches STAT17 result", {
  data <- c(
    137.3603, 135.6665, 136.6914, 154.7919, 159.2037, 137.3277, 128.821,
    138.6304, 138.9004, 147.4598, 148.6622, 144.4948, 131.0851, 149.0203,
    131.8232, 146.4471, 123.8124, 126.3105, 140.7609, 134.4875, 128.7508,
    117.1854, 129.3088, 141.6789, 138.4073, 136.0295, 128.4164, 141.7733,
    134.455,  122.7383, 136.9171, 136.9232, 138.8402, 152.8294, 135.0633,
    121.052,  131.035,  138.3248, 131.1379, 147.3771, 130.0681, 132.7467,
    137.1444, 141.662,  146.9363, 160.7448, 138.5511, 129.1628, 140.2939,
    144.8167, 156.5918, 132.0099, 129.3551, 136.6066, 134.5095, 128.2081,
    144.0896, 141.8029, 130.0149, 140.8813, 137.7864
  )

  res <- basis_nonpara_large_sample(x = data, p = 0.9, conf = 0.95,
                                    override = "all")
  expect_equal(res$basis, 122.738297, tolerance = 0.005)
  expect_output(print(res), "b-basis.*122", ignore.case = TRUE)
  expect_output(print(res), "nonparametric", ignore.case = TRUE)
  expect_match(res$distribution, "nonparametric.*large", ignore.case = TRUE)
})

test_that("non-para (large) basis values produce expected diag failures", {
  set.seed(100)
  x_small <- c(rnorm(13, 100, 2), rnorm(13, 103, 2), 120)
  batch_small <- c(rep("A", 13), rep("B", 14))
  x_large <- c(rnorm(200, 100, 2), rnorm(100, 103, 2), 120)
  batch_large <- c(rep("A", 200), rep("B", 101))

  expect_snapshot(
    res <- basis_nonpara_large_sample(
      x = x_large, batch = batch_large)
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch_large)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_nonpara_large_sample(x = x_large, batch = batch_large,
                      override = c("outliers_within_batch",
                                   "between_batch_variability",
                                   "outliers"))

  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers"))
  expect_length(res$diagnostic_failures, 0)

  # overriding the diagnostics with "all" should do the same thing
  res <- basis_nonpara_large_sample(x = x_large, batch = batch_large,
                                    override = "all")
  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "sample_size"))
  expect_length(res$diagnostic_failures, 0)

  expect_snapshot(
    res <- basis_nonpara_large_sample(
      x = x_large, batch = batch_large,
      p = 0.99, conf = 0.95)
  )

  # call basis_normal without batch
  expect_snapshot(
    res <- basis_nonpara_large_sample(x = x_large)
  )

  # Check that res$... contains the correct value
  expect_equal(res$diagnostic_failures,
               c("outliers"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_nonpara_large_sample(x = x_large,
                      override = c("outliers",
                                   "outliers_within_batch",
                                   "between_batch_variability"))

  expect_equal(res$override,
               c("outliers", "outliers_within_batch",
                 "between_batch_variability"))
  expect_length(res$diagnostic_failures, 0)
})

# data from CMH-17-1G Section 8.3.11.1.1
cmh_17_8_3_11_1_1 <- tribble(
  ~batch, ~strength, ~condition,
  1, 118.3774604, "CTD", 1, 84.9581364, "RTD", 1, 83.7436035, "ETD",
  1, 123.6035612, "CTD", 1, 92.4891822, "RTD", 1, 84.3831677, "ETD",
  1, 115.2238092, "CTD", 1, 96.8212659, "RTD", 1, 94.8030433, "ETD",
  1, 112.6379744, "CTD", 1, 109.030325, "RTD", 1, 94.3931537, "ETD",
  1, 116.5564277, "CTD", 1, 97.8212659, "RTD", 1, 101.702222, "ETD",
  1, 123.1649896, "CTD", 1, 100.921519, "RTD", 1, 86.5372121, "ETD",
  2, 128.5589027, "CTD", 1, 103.699444, "RTD", 1, 92.3772684, "ETD",
  2, 113.1462103, "CTD", 2, 93.7908212, "RTD", 2, 89.2084024, "ETD",
  2, 121.4248107, "CTD", 2, 107.526709, "RTD", 2, 100.686001, "ETD",
  2, 134.3241906, "CTD", 2, 94.5769704, "RTD", 2, 81.0444192, "ETD",
  2, 129.6405117, "CTD", 2, 93.8831373, "RTD", 2, 91.3398070, "ETD",
  2, 117.9818658, "CTD", 2, 98.2296605, "RTD", 2, 93.1441939, "ETD",
  3, 115.4505226, "CTD", 2, 111.346590, "RTD", 2, 85.8204168, "ETD",
  3, 120.0369467, "CTD", 2, 100.817538, "RTD", 3, 94.8966273, "ETD",
  3, 117.1631088, "CTD", 3, 100.382203, "RTD", 3, 95.8068520, "ETD",
  3, 112.9302797, "CTD", 3, 91.5037811, "RTD", 3, 86.7842252, "ETD",
  3, 117.9114501, "CTD", 3, 100.083233, "RTD", 3, 94.4011973, "ETD",
  3, 120.1900159, "CTD", 3, 95.6393615, "RTD", 3, 96.7231171, "ETD",
  3, 110.7295966, "CTD", 3, 109.304779, "RTD", 3, 89.9010384, "ETD",
  3, 100.078562, "RTD", 3, 99.1205847, "RTD", 3, 89.3672306, "ETD",
  1, 106.357525, "ETW", 1, 99.0239966, "ETW2",
  1, 105.898733, "ETW", 1, 103.341238, "ETW2",
  1, 88.4640082, "ETW", 1, 100.302130, "ETW2",
  1, 103.901744, "ETW", 1, 98.4634133, "ETW2",
  1, 80.2058219, "ETW", 1, 92.2647280, "ETW2",
  1, 109.199597, "ETW", 1, 103.487693, "ETW2",
  1, 61.0139431, "ETW", 1, 113.734763, "ETW2",
  2, 99.3207107, "ETW", 2, 108.172659, "ETW2",
  2, 115.861770, "ETW", 2, 108.426732, "ETW2",
  2, 82.6133082, "ETW", 2, 116.260375, "ETW2",
  2, 85.3690411, "ETW", 2, 121.049610, "ETW2",
  2, 115.801622, "ETW", 2, 111.223082, "ETW2",
  2, 44.3217741, "ETW", 2, 104.574843, "ETW2",
  2, 117.328077, "ETW", 2, 103.222552, "ETW2",
  2, 88.6782903, "ETW", 3, 99.3918538, "ETW2",
  3, 107.676986, "ETW", 3, 87.3421658, "ETW2",
  3, 108.960241, "ETW", 3, 102.730741, "ETW2",
  3, 116.122640, "ETW", 3, 96.3694916, "ETW2",
  3, 80.2334815, "ETW", 3, 99.5946088, "ETW2",
  3, 106.145570, "ETW", 3, 97.0712407, "ETW2",
  3, 104.667866, "ETW",
  3, 104.234953, "ETW"
)

test_that("expected diagnostic failures are noted for pooling methods", {
  # This test follows CMH-17-1G Section
  # This section in CMH-17-1G shows the removal of one condition
  # before running Levene's test on the pooled data, so this test
  # will be skipped in this test.
  expect_snapshot(
    res <- basis_pooled_sd(cmh_17_8_3_11_1_1, strength, condition, batch)
  )

  expect_snapshot(
    res <- cmh_17_8_3_11_1_1 %>%
      filter(condition != "ETW2") %>%
      basis_pooled_sd(strength, condition, batch)
  )

  # removing both ETW and ETW2 should remove all diagnostic failures
  res <- cmh_17_8_3_11_1_1 %>%
    filter(condition != "ETW2" & condition != "ETW") %>%
    basis_pooled_sd(strength, condition, batch)

  expect_equal(res$basis$value[res$basis$group == "CTD"], 108.70,
               tolerance = 0.02)
  expect_equal(res$basis$value[res$basis$group == "RTD"], 88.52,
               tolerance = 0.02)
  expect_equal(res$basis$value[res$basis$group == "ETD"], 80.68,
               tolerance = 0.02)

  expect_snapshot(
    res <- basis_pooled_cv(cmh_17_8_3_11_1_1, strength,
                           condition, batch)
  )

  expect_snapshot(
    res <- cmh_17_8_3_11_1_1 %>%
      filter(condition != "ETW2") %>%
      basis_pooled_cv(strength, condition, batch)
  )

  # removing both ETW and ETW2 should remove all diagnostic failures
  res <- cmh_17_8_3_11_1_1 %>%
    filter(condition != "ETW2" & condition != "ETW") %>%
    basis_pooled_cv(strength, condition, batch)
})

# data from CMH-17-1G Section 8.3.11.1.2
poolable_data <- tribble(
  ~batch, ~strength, ~condition,
  1, 79.04517, "CTD",
  1, 102.6014, "CTD",
  1, 97.79372, "CTD",
  1, 92.86423, "CTD",
  1, 117.218,  "CTD",
  1, 108.7168, "CTD",
  1, 112.2773, "CTD",
  1, 114.0129, "CTD",
  2, 106.8452, "CTD",
  2, 112.3911, "CTD",
  2, 115.5658, "CTD",
  2, 87.40657, "CTD",
  2, 102.2785, "CTD",
  2, 110.6073, "CTD",
  3, 105.2762, "CTD",
  3, 110.8924, "CTD",
  3, 108.7638, "CTD",
  3, 110.9833, "CTD",
  3, 101.3417, "CTD",
  3, 100.0251, "CTD",
  1, 103.2006, "RTD",
  1, 105.1034, "RTD",
  1, 105.1893, "RTD",
  1, 100.4189, "RTD",
  2, 85.32319, "RTD",
  2, 92.69923, "RTD",
  2, 98.45242, "RTD",
  2, 104.1014, "RTD",
  2, 91.51841, "RTD",
  2, 101.3746, "RTD",
  2, 101.5828, "RTD",
  2, 99.57384, "RTD",
  2, 88.84826, "RTD",
  3, 92.18703, "RTD",
  3, 101.8234, "RTD",
  3, 97.68909, "RTD",
  3, 101.5172, "RTD",
  3, 100.0481, "RTD",
  3, 102.0544, "RTD",
  1, 63.22764, "ETW",
  1, 70.84454, "ETW",
  1, 66.43223, "ETW",
  1, 75.37771, "ETW",
  1, 72.43773, "ETW",
  1, 68.43073, "ETW",
  1, 69.72524, "ETW",
  2, 66.20343, "ETW",
  2, 60.51251, "ETW",
  2, 65.69334, "ETW",
  2, 62.73595, "ETW",
  2, 59.00798, "ETW",
  2, 62.37761, "ETW",
  3, 64.3947,  "ETW",
  3, 72.8491,  "ETW",
  3, 66.56226, "ETW",
  3, 66.56779, "ETW",
  3, 66.00123, "ETW",
  3, 59.62108, "ETW",
  3, 60.61167, "ETW",
  3, 57.65487, "ETW",
  3, 66.51241, "ETW",
  3, 64.89347, "ETW",
  3, 57.73054, "ETW",
  3, 68.94086, "ETW",
  3, 61.63177, "ETW",
  1, 54.09806, "ETW2",
  1, 58.87615, "ETW2",
  1, 61.60167, "ETW2",
  1, 60.23973, "ETW2",
  1, 61.4808,  "ETW2",
  1, 64.55832, "ETW2",
  2, 57.76131, "ETW2",
  2, 49.91463, "ETW2",
  2, 61.49271, "ETW2",
  2, 57.7281,  "ETW2",
  2, 62.11653, "ETW2",
  2, 62.69353, "ETW2",
  3, 61.38523, "ETW2",
  3, 60.39053, "ETW2",
  3, 59.17616, "ETW2",
  3, 60.17616, "ETW2",
  3, 46.47396, "ETW2",
  3, 51.16616, "ETW2"
)

test_that("Pooled SD results match ASAP results", {
  # This data fails the anderson-darling test for normality for the
  # transformed data
  expect_snapshot(
    res_b <- basis_pooled_sd(poolable_data, strength, condition,
                             override = c("pooled_variance_equal"))
  )

  expect_equal(res_b$basis$value[res_b$basis$group == "CTD"],
               93.64, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "RTD"],
               87.30, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "ETW"],
               54.33, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "ETW2"],
               47.12, tolerance = 0.01)

  expect_equal(res_b$n, 83)
  expect_equal(res_b$r, 4)
  expect_output(print(res_b), "b-basis", ignore.case = TRUE)
  expect_output(print(res_b), "pooled standard deviation", ignore.case = TRUE)
  expect_output(print(res_b), "CTD.*93\\.6", ignore.case = TRUE)
  expect_output(print(res_b), "RTD.*87\\.29", ignore.case = TRUE)
  expect_output(print(res_b), "ETW.*54\\.3", ignore.case = TRUE)
  expect_output(print(res_b), "ETW2.*47\\.07", ignore.case = TRUE)

  res_a <- basis_pooled_sd(poolable_data, strength, condition,
                           p = 0.99, conf = 0.95,
                           override = c("pooled_data_normal",
                                        "pooled_variance_equal",
                                        "outliers_within_batch",
                                        "between_group_variability"))
  expect_equal(res_a$basis$value[res_a$basis$group == "CTD"],
               86.19, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "RTD"],
               79.86, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "ETW"],
               46.84, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "ETW2"],
               39.69, tolerance = 0.01)
})

test_that("Pooled CV results match CMH17STATS", {
  # This data fails the anderson-darling test for normality for the
  # transformed data
  expect_snapshot(
    res_b <- basis_pooled_cv(poolable_data, strength, condition)
  )

  expect_equal(res_b$basis$value[res_b$basis$group == "CTD"],
               90.89, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "RTD"],
               85.37, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "ETW"],
               56.79, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "ETW2"],
               50.55, tolerance = 0.01)

  expect_equal(res_b$n, 83)
  expect_equal(res_b$r, 4)
  expect_output(print(res_b), "b-basis", ignore.case = TRUE)
  expect_output(print(res_b), "pooled CV", ignore.case = TRUE)
  expect_output(print(res_b), "CTD.*90\\.8", ignore.case = TRUE)
  expect_output(print(res_b), "RTD.*85\\.3", ignore.case = TRUE)
  expect_output(print(res_b), "ETW.*56\\.7", ignore.case = TRUE)
  expect_output(print(res_b), "ETW2.*50\\.5", ignore.case = TRUE)

  res_a <- basis_pooled_cv(poolable_data, strength, condition,
                           p = 0.99, conf = 0.95,
                           override = c("pooled_data_normal",
                                        "outliers_within_batch",
                                        "between_group_variability"))
  expect_equal(res_a$basis$value[res_a$basis$group == "CTD"],
               81.62, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "RTD"],
               76.67, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "ETW"],
               50.98, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "ETW2"],
               45.40, tolerance = 0.01)
  expect_output(print(res_a), "a-basis", ignore.case = TRUE)
})

test_that("Pooled data matches CMH17-STATS with mod CV, SD pooling", {
  # pooled SD modified CV results
  # pooled data fails Levene's test after mod CV transform
  # based on `poolable_data` dataset with ETW2 removed

  data <- filter(poolable_data, condition != "ETW2")

  res_b <- basis_pooled_sd(data, strength, condition, modcv = TRUE,
                           override = c("pooled_variance_equal",
                                        "outliers_within_batch",
                                        "between_group_variability"))

  expect_equal(res_b$basis$value[res_b$basis$group == "CTD"],
               92.25, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "RTD"],
               85.91, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "ETW"],
               52.97, tolerance = 0.01)

  expect_output(print(res_b), "Modified CV")

  res_a <- basis_pooled_sd(data, strength, condition,
                           p = 0.99, conf = 0.95, modcv = TRUE,
                           override = c("pooled_variance_equal",
                                        "outliers_within_batch",
                                        "between_group_variability"))
  expect_equal(res_a$basis$value[res_a$basis$group == "CTD"],
               83.81, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "RTD"],
               77.48, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "ETW"],
               44.47, tolerance = 0.01)
})

test_that("Pooled data matches CMH17-STATS with mod CV, CV pooling", {
  # pooled CV modified CV results
  # pooled data passes Levene's test after mod CV transform
  # based on `poolable_data` dataset with ETW2 removed

  data <- filter(poolable_data, condition != "ETW2")

  res_b <- basis_pooled_cv(data, strength, condition, modcv = TRUE,
                           override = c("outliers_within_batch",
                                        "between_group_variability"))

  expect_equal(res_b$basis$value[res_b$basis$group == "CTD"],
               90.31, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "RTD"],
               84.83, tolerance = 0.01)
  expect_equal(res_b$basis$value[res_b$basis$group == "ETW"],
               56.43, tolerance = 0.01)

  expect_output(print(res_b), "Modified CV")

  res_a <- basis_pooled_cv(data, strength, condition,
                           p = 0.99, conf = 0.95, modcv = TRUE,
                           override = c("outliers_within_batch",
                                        "between_group_variability"))
  expect_equal(res_a$basis$value[res_a$basis$group == "CTD"],
               80.57, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "RTD"],
               75.69, tolerance = 0.01)
  expect_equal(res_a$basis$value[res_a$basis$group == "ETW"],
               50.33, tolerance = 0.01)
})

vangel1994 <- tribble(
  ~n, ~z, ~p, ~conf,
  3,  13.976451, 0.90, 0.90,
  5,  4.1011886, 0.90, 0.90,
  7,  2.5440993, 0.90, 0.90,
  9,  1.9368858, 0.90, 0.90,
  11, 1.6127559, 0.90, 0.90,
  13, 1.4096961, 0.90, 0.90,
  15, 1.2695586, 0.90, 0.90,
  17, 1.1663923, 0.90, 0.90,
  19, 1.0868640, 0.90, 0.90,
  21, 1.0234110, 0.90, 0.90,
  3,  28.820048, 0.90, 0.95,
  5,  6.1981307, 0.90, 0.95,
  7,  3.4780112, 0.90, 0.95,
  9,  2.5168762, 0.90, 0.95,
  11, 2.0312134, 0.90, 0.95,
  13, 1.7377374, 0.90, 0.95,
  15, 1.5403989, 0.90, 0.95,
  17, 1.3979806, 0.90, 0.95,
  19, 1.2899172, 0.90, 0.95,
  21, 1.2048089, 0.90, 0.95,
  23, 1.1358259, 0.90, 0.95,
  25, 1.0786237, 0.90, 0.95,
  27, 1.0303046, 0.90, 0.95,
  3,  147.51275, 0.90, 0.99,
  5,  14.993461, 0.90, 0.99,
  7,  6.6442464, 0.90, 0.99,
  9,  4.2798170, 0.90, 0.99,
  11, 3.2197376, 0.90, 0.99,
  13, 2.6267547, 0.90, 0.99,
  15, 2.2493289, 0.90, 0.99,
  17, 1.9880239, 0.90, 0.99,
  19, 1.7961467, 0.90, 0.99,
  21, 1.6490109, 0.90, 0.99,
  23, 1.5323809, 0.90, 0.99,
  25, 1.4374854, 0.90, 0.99,
  27, 1.3586292, 0.90, 0.99,
  29, 1.2919549, 0.90, 0.99,
  31, 1.2347570, 0.90, 0.99,
  33, 1.1850813, 0.90, 0.99,
  35, 1.1414809, 0.90, 0.99,
  37, 1.1028613, 0.90, 0.99,
  39, 1.0683787, 0.90, 0.99,
  41, 1.0373720, 0.90, 0.99,
  43, 1.0093159, 0.90, 0.99,
  3,  20.478521, 0.95, 0.90,
  5,  5.8872014, 0.95, 0.90,
  7,  3.6322326, 0.95, 0.90,
  9,  2.7593956, 0.95, 0.90,
  11, 2.2953853, 0.95, 0.90,
  13, 2.0054547, 0.95, 0.90,
  15, 1.8057261, 0.95, 0.90,
  17, 1.6588820, 0.95, 0.90,
  19, 1.5457939, 0.95, 0.90,
  21, 1.4556317, 0.95, 0.90,
  23, 1.3817937, 0.95, 0.90,
  25, 1.3200198, 0.95, 0.90,
  27, 1.2674334, 0.95, 0.90,
  29, 1.2220187, 0.95, 0.90,
  31, 1.1823195, 0.95, 0.90,
  33, 1.1472560, 0.95, 0.90,
  35, 1.1160097, 0.95, 0.90,
  37, 1.0879479, 0.95, 0.90,
  39, 1.0625739, 0.95, 0.90,
  41, 1.0394913, 0.95, 0.90,
  43, 1.0183802, 0.95, 0.90,
  3,  42.149579, 0.95, 0.95,
  5,  8.8719351, 0.95, 0.95,
  7,  4.9501721, 0.95, 0.95,
  9,  3.5743714, 0.95, 0.95,
  11, 2.8819079, 0.95, 0.95,
  13, 2.4645176, 0.95, 0.95,
  15, 2.1843450, 0.95, 0.95,
  17, 1.9824011, 0.95, 0.95,
  19, 1.8293163, 0.95, 0.95,
  21, 1.7088376, 0.95, 0.95,
  23, 1.6112408, 0.95, 0.95,
  25, 1.5303474, 0.95, 0.95,
  27, 1.4620403, 0.95, 0.95,
  29, 1.4034674, 0.95, 0.95,
  31, 1.3525889, 0.95, 0.95,
  33, 1.3079057, 0.95, 0.95,
  35, 1.2682903, 0.95, 0.95,
  37, 1.2328780, 0.95, 0.95,
  39, 1.2009936, 0.95, 0.95,
  41, 1.1721022, 0.95, 0.95,
  43, 1.1457739, 0.95, 0.95,
  45, 1.1216591, 0.95, 0.95,
  47, 1.0994706, 0.95, 0.95,
  49, 1.0789699, 0.95, 0.95,
  51, 1.0599573, 0.95, 0.95,
  53, 1.0422642, 0.95, 0.95,
  55, 1.0257472, 0.95, 0.95,
  57, 1.0102836, 0.95, 0.95
)

test_that("Extended Hanson-Koopman matches median results from Vangel 1994", {
  # Vangel (1994) provides extensive tables of z for the case where i=1 and
  # j is the median observation. This test checks the results of this
  # package's function against those tables. Only the odd values of n
  # are checked so that the median is a single observation.

  vangel1994 %>%
    rowwise() %>%
    mutate(
      z_calc = hk_ext_z(n, 1, ceiling(n / 2), p, conf)
    ) %>%
    mutate(expect_equal(z, z_calc, tolerance = 0.00005,
                        label = paste0("Mismatch in `z` for n=", n,
                                       ", p=", p,
                                       " conf=", conf, ".\n",
                                       "z_vangel=", z,
                                       ", z_calc=", z_calc, "\n")))
})


cmh_17_1g_8_5_14 <- tribble(
  ~n, ~r, ~k,
  2, 2, 35.177,
  3, 3, 7.859,
  4, 4, 4.505,
  5, 4, 4.101,
  6, 5, 3.064,
  7, 5, 2.858,
  8, 6, 2.382,
  9, 6, 2.253,
  10, 6, 2.137,
  11, 7, 1.897,
  12, 7, 1.814,
  13, 7, 1.738,
  14, 8, 1.599,
  15, 8, 1.540,
  16, 8, 1.485,
  17, 8, 1.434,
  18, 9, 1.354,
  19, 9, 1.311,
  20, 10, 1.253,
  21, 10, 1.218,
  22, 10, 1.184,
  23, 11, 1.143,
  24, 11, 1.114,
  25, 11, 1.087,
  26, 11, 1.060,
  27, 11, 1.035,
  28, 12, 1.010
)

test_that("Extended Hanson-Koopman matches CMH-17-1G Table 8.5.14", {
  # CMH-17-1G uses the optimal order statistic approach suggested by
  # Vangel (1994) for computing B-Basis values. There are a few values
  # of n where this package's implementation finds a different optimum order
  # statistic than CMH-17-1G uses. In these cases, the order statistic that
  # this package and CMH-17-1G are both very nearly optimal. These differences
  # are ignored in this test.

  cmh_17_1g_8_5_14 %>%
    rowwise() %>%
    mutate(z = hk_ext_z_j_opt(n, 0.90, 0.95)$z) %>%
    mutate(j = hk_ext_z_j_opt(n, 0.90, 0.95)$j) %>%
    filter(
      n != 10 & n != 13 & n != 16 & n != 17 & n != 19 & n != 22 & n != 27
      ) %>%
    mutate(expect_equal(j, r,
                        label = paste0("Mismatch in `j`/`r` for n=", n, ", ",
                                       "r_B_cmh=", r,
                                       ", j=", j, "\n"))) %>%
    mutate(expect_equal(z, k, tolerance = 0.005,
                        label = paste0("Mismatch in `k`/`z` for n=", n, ", ",
                                       "k_B_cmh=", k,
                                       ", z_calc=", z, "\n")))
})

test_that("Hanson-Koopman results match STAT17 for several values of n", {
  data <- c(
    139.6734,
    143.0032,
    130.4757,
    144.8327,
    138.7818,
    136.7693,
    148.636,
    131.0095,
    131.4933,
    142.8856,
    158.0198,
    145.2271,
    137.5991,
    139.8298,
    140.8557,
    137.6148,
    131.3614,
    152.7795,
    145.8792,
    152.9207,
    160.0989,
    145.192,
    128.6383,
    141.5992,
    122.5297,
    159.8209,
    151.672,
    159.0156
  )

  res <- basis_hk_ext(x = head(data, 28), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 122.36798, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 27), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 121.96939, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 26), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 121.57073, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 23), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 127.11286, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 22), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 128.82397, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 21), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 128.52107, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 20), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 128.20999, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 19), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 127.74060, tolerance = 0.15)

  res <- basis_hk_ext(x = head(data, 18), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 127.36697, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 17), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 127.02732, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 16), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 126.23545, tolerance = 0.3)

  res <- basis_hk_ext(x = head(data, 15), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 125.68740, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 14), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 125.17500, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 13), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 124.07851, tolerance = 1.3)
  # worst agreement, ensure that it's conservative
  expect_lt(res$basis, 124.07851)

  res <- basis_hk_ext(x = head(data, 12), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 121.17418, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 11), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 120.26382, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 10), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 120.75149, tolerance = 0.05)

  res <- basis_hk_ext(x = head(data, 9), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 119.80108, tolerance = 0.001)
})

cmh_17_1g_8_5_15 <- tribble(
  ~n, ~k,
  2, 80.0038,
  3, 16.9122,
  4, 9.49579,
  5, 6.89049,
  6, 5.57681,
  7, 4.78352,
  8, 4.25011,
  9, 3.86502,
  10, 3.57267,
  11, 3.34227,
  12, 3.1554,
  13, 3.00033,
  14, 2.86924,
  15, 2.75672,
  16, 2.65889,
  17, 2.5729,
  18, 2.4966,
  19, 2.42833,
  20, 2.36683,
  21, 2.31106,
  22, 2.2602,
  23, 2.21359,
  24, 2.17067,
  25, 2.131,
  26, 2.09419,
  27, 2.05991,
  28, 2.0279,
  29, 1.99791,
  30, 1.96975,
  31, 1.94324,
  32, 1.91822,
  33, 1.89457,
  34, 1.87215,
  35, 1.85088,
  36, 1.83065,
  37, 1.81139,
  38, 1.79301,
  39, 1.77546,
  40, 1.75868,
  41, 1.7426,
  42, 1.72718,
  43, 1.71239,
  44, 1.69817,
  45, 1.68449,
  46, 1.67132,
  47, 1.65862,
  48, 1.64638,
  49, 1.63456,
  50, 1.62313,
  52, 1.60139,
  54, 1.58101,
  56, 1.56184,
  58, 1.54377,
  60, 1.5267,
  62, 1.51053,
  64, 1.4952,
  66, 1.48063,
  68, 1.46675,
  70, 1.45352,
  72, 1.44089,
  74, 1.42881,
  76, 1.41724,
  78, 1.40614,
  80, 1.39549,
  82, 1.38525,
  84, 1.37541,
  86, 1.36592,
  88, 1.35678,
  90, 1.34796,
  92, 1.33944,
  94, 1.3312,
  96, 1.32324,
  98, 1.31553,
  100, 1.30806,
  105, 1.29036,
  110, 1.27392,
  115, 1.25859,
  120, 1.24425,
  125, 1.2308,
  130, 1.21814,
  135, 1.2062,
  140, 1.19491,
  145, 1.18421,
  150, 1.17406,
  155, 1.1644,
  160, 1.15519,
  165, 1.1464,
  170, 1.13801,
  175, 1.12997,
  180, 1.12226,
  185, 1.11486,
  190, 1.10776,
  195, 1.10092,
  200, 1.09434,
  205, 1.08799,
  210, 1.08187,
  215, 1.07595,
  220, 1.07024,
  225, 1.06471,
  230, 1.05935,
  235, 1.05417,
  240, 1.04914,
  245, 1.04426,
  250, 1.03952,
  275, 1.01773
)

test_that("Extended Hanson-Koopman matches CMH-17-1G Table 8.5.15", {
  # for A-Basis, CMH-17-1G uses the order statistics for 1 and n
  # to compute the tolerance limits. This test verifies that the code
  # in this package computes the same values of z (k, as CMH-17 calls it)
  cmh_17_1g_8_5_15 %>%
    rowwise() %>%
    mutate(z = hk_ext_z(n, 1, n, 0.99, 0.95)) %>%
    mutate(expect_equal(z, k, tolerance = 0.00005,
                        label = paste0("Mismatch in `k`/`z` for n=", n, ", ",
                                       "k_A_cmh=", k,
                                       ", z_calc=", z, "\n")))
})

cmh_17_1g_8_5_13 <- tribble(
  ~n, ~ra,
  299, 1,
  473, 2,
  628, 3,
  773, 4,
  913, 5,
  1049, 6,
  1182, 7,
  1312, 8,
  1441, 9,
  1568, 10,
  1693, 11,
  1818, 12,
  1941, 13,
  2064, 14,
  2185, 15,
  2306, 16,
  2426, 17,
  2546, 18,
  2665, 19,
  2784, 20,
  2902, 21,
  3020, 22,
  3137, 23,
  3254, 24,
  3371, 25,
  3487, 26,
  3603, 27,
  3719, 28,
  3834, 29,
  3949, 30,
  4064, 31,
  4179, 32,
  4293, 33,
  4407, 34,
  4521, 35,
  4635, 36,
  4749, 37,
  4862, 38,
  4975, 39,
  5088, 40,
  5201, 41,
  5314, 42,
  5427, 43,
  5539, 44,
  5651, 45,
  5764, 46,
  5876, 47,
  5988, 48,
  6099, 49,
  6211, 50,
  6323, 51,
  6434, 52,
  6545, 53,
  6657, 54,
  6769, 55,
  6879, 56,
  6990, 57,
  7100, 58,
  7211, 59,
  7322, 60,
  7432, 61,
  7543, 62,
  7653, 63,
  7763, 64,
  7874, 65,
  7984, 66,
  8094, 67,
  8204, 68,
  8314, 69,
  8423, 70,
  8533, 71,
  8643, 72,
  8753, 73,
  8862, 74,
  8972, 75,
  9081, 76,
  9190, 77,
  9300, 78,
  9409, 79,
  9518, 80,
  9627, 81,
  9736, 82,
  9854, 83,
  9954, 84,
  10063, 85,
  10172, 86,
  10281, 87,
  10390, 88,
  10498, 89,
  10607, 90,
  10716, 91,
  10824, 92,
  10933, 93,
  11041, 94,
  11150, 95,
  11258, 96,
  11366, 97,
  11475, 98,
  11583, 99,
  11691, 100
)

test_that("Non-parametric ranks for A-Basis match CMH-17-1G Table 8.5.13", {
  skip_on_cran()  # this test is a long-running test

  cmh_17_1g_8_5_13 %>%
    mutate(ra_lag = lag(ra)) %>%
    rowwise() %>%
    mutate(r_calc = nonpara_binomial_rank(n, 0.99, 0.95)) %>%
    mutate(expect_equal(ra, r_calc,
                        label = paste0(
                          "Mismatch in r for n=", n,
                          ". rA=", ra,
                          ", r_calc=", r_calc
                          ))) %>%
    filter(n > 299 & n < 6500) %>%
    # the rank for one sample larger should be the same
    mutate(r_calc_plus = nonpara_binomial_rank(n + 1, 0.99, 0.95)) %>%
    mutate(expect_equal(ra, r_calc_plus,
                        label = paste0(
                          "Mismatch in r for n=", n + 1,
                          ". rA=", ra, ", ",
                          "r_calc=", r_calc_plus
                        ))) %>%
    # the rank for one sample smaller should be the previous one
    mutate(r_calc_minus = nonpara_binomial_rank(n - 1, 0.99, 0.95)) %>%
    mutate(expect_equal(ra_lag, r_calc_minus,
                        label = paste0(
                          "Mismatch in r for n=", n - 1,
                          ". rA=", ra_lag, ", ",
                          "r_calc=", r_calc_minus
                        )))
})

test_that("nonpara_binomial_rank raises and error when sample too small", {
  expect_error(nonpara_binomial_rank(298, 0.99, 0.95),
               "p.*0\\.99.*conf.*0\\.95")
})

test_that("nonpara_binomial_rank raises an error when it can't converge", {
  expect_error(nonpara_binomial_rank(4000, 0.00001, 0.01),
               "p.*1e-05.*conf.*0\\.01")
})

cmh_17_1g_8_5_12 <- tribble(
  ~n, ~rb,
  29, 1,
  46, 2,
  61, 3,
  76, 4,
  89, 5,
  103, 6,
  116, 7,
  129, 8,
  142, 9,
  154, 10,
  167, 11,
  179, 12,
  191, 13,
  203, 14,
  215, 15,
  227, 16,
  239, 17,
  251, 18,
  263, 19,
  275, 20,
  298, 22,
  321, 24,
  345, 26,
  368, 28,
  391, 30,
  413, 32,
  436, 34,
  459, 36,
  481, 38,
  504, 40,
  526, 42,
  549, 44,
  571, 46,
  593, 48,
  615, 50,
  638, 52,
  660, 54,
  682, 56,
  704, 58,
  726, 60,
  781, 65,
  836, 70,
  890, 75,
  945, 80,
  999, 85,
  1053, 90,
  1107, 95,
  1161, 100,
  1269, 110,
  1376, 120,
  1483, 130,
  1590, 140,
  1696, 150,
  1803, 160,
  1909, 170,
  2015, 180,
  2120, 190,
  2226, 200,
  2331, 210,
  2437, 220,
  2542, 230,
  2647, 240,
  2752, 250,
  2857, 260,
  2962, 270,
  3066, 280,
  3171, 290,
  3276, 300,
  3380, 310,
  3484, 320,
  3589, 330,
  3693, 340,
  3797, 350,
  3901, 360,
  4005, 370,
  4109, 380,
  4213, 390,
  4317, 400,
  4421, 410,
  4525, 420,
  4629, 430,
  4733, 440,
  4836, 450,
  4940, 460,
  5044, 470,
  5147, 480,
  5251, 490,
  5354, 500,
  5613, 525,
  5871, 550,
  6130, 575,
  6388, 600,
  6645, 625,
  6903, 650,
  7161, 675,
  7418, 700,
  7727, 730,
  8036, 760,
  8344, 790,
  8652, 820,
  8960, 850,
  9268, 880,
  9576, 910,
  9884, 940,
  10191, 970,
  10499, 1000
)

test_that("Non-parametric ranks for BA-Basis match CMH-17-1G Table 8.5.12", {
  skip_on_cran()  # this test is a long-running test

  cmh_17_1g_8_5_12 %>%
    mutate(rb_lag = lag(rb)) %>%
    rowwise() %>%
    mutate(r_calc = nonpara_binomial_rank(n, 0.9, 0.95)) %>%
    mutate(expect_equal(rb, r_calc,
                        label = paste0(
                          "Mismatch in r for n=", n,
                          ". rB=", rb,
                          ", r_calc=", r_calc
                        ))) %>%
    # the rank for one sample larger should be the same
    mutate(r_calc_plus = nonpara_binomial_rank(n + 1, 0.9, 0.95)) %>%
    mutate(expect_equal(rb, r_calc_plus,
                        label = paste0(
                          "Mismatch in r for n=", n + 1,
                          ". rB=", rb, ", ",
                          "r_calc=", r_calc_plus
                        ))) %>%
    filter(n > 29 & n <= 275) %>%
    # the rank for one sample smaller should be the previous one
    # above n=275, Table 8.5.12 does not have consecutive ranks, so we can't
    # use the lag trick below to check sample sizes of n-1
    mutate(r_calc_minus = nonpara_binomial_rank(n - 1, 0.9, 0.95)) %>%
    mutate(expect_equal(rb_lag, r_calc_minus,
                        label = paste0(
                          "Mismatch in r for n=", n - 1,
                          ". rB=", rb_lag, ", ",
                          "r_calc=", r_calc_minus
                        )))
})

cmh_17_1g_8_3_11_1_1_etw2 <- tribble(
  ~batch, ~strength,
  1, 99.0239966,
  1, 103.341238,
  1, 100.30213,
  1, 98.4634133,
  1, 92.264728,
  1, 103.487693,
  1, 113.734763,
  2, 108.172659,
  2, 108.426732,
  2, 116.260375,
  2, 121.04961,
  2, 111.223082,
  2, 104.574843,
  2, 103.222552,
  3, 99.3918538,
  3, 87.3421658,
  3, 102.730741,
  3, 96.3694916,
  3, 99.5946088,
  3, 97.0712407
)

test_that("ANOVA results match STAT17 for sample data", {
  # Sample data from CMH-17-1G Section 8.3.11.2.2

  res <- cmh_17_1g_8_3_11_1_1_etw2 %>%
    basis_anova(strength, batch,
                override = c("equality_of_variance",
                             "number_of_groups"))

  expect_equal(res$basis, 63.2, tolerance = 0.05)
  expect_output(print(res), "b-basis.*63\\.2", ignore.case = TRUE)
  expect_output(print(res), "ANOVA", ignore.case = TRUE)
  expect_match(res$distribution, "ANOVA", ignore.case = TRUE)

  res <- cmh_17_1g_8_3_11_1_1_etw2 %>%
    basis_anova(strength, batch, p = 0.99, conf = 0.95,
                override = c("equality_of_variance",
                             "number_of_groups"))

  expect_equal(res$basis, 34.6, tolerance = 0.05)
  expect_output(print(res), "a-basis.*34\\.", ignore.case = TRUE)
  expect_output(print(res), "ANOVA", ignore.case = TRUE)
  expect_match(res$distribution, "ANOVA", ignore.case = TRUE)
})

test_that("ANOVA produces an error when there is only one group", {
  strength <- rep(10, 1)
  batch <- rep(10, 1)

  expect_error(
    basis_anova(x = strength, group = batch),
    "fewer than 2"
  )
})

test_that("anova basis values produce expected diagnostic failures", {
  set.seed(100)
  x <- c(rnorm(30, 100, 1), rnorm(30, 100, 10), 80)
  batch <- c(rep("A", 30), rep("B", 30), "A")

  expect_snapshot(
    res <- basis_anova(x = x, group = batch)
  )

  # Check that res$... contains the correct value
  expect_equal(res$group, batch)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_group",
                 "equality_of_variance",
                 "number_of_groups"))
  expect_length(res$override, 0)

  # overriding the diagnostics should eliminate the warnings
  res <- basis_anova(x = x, group = batch,
                     override = c("outliers_within_group",
                                  "equality_of_variance",
                                  "number_of_groups"))

  expect_equal(res$override,
               c("outliers_within_group",
                 "equality_of_variance",
                 "number_of_groups"))
  expect_length(res$diagnostic_failures, 0)

  # overriding the diagnostics with "all" should do the same thing
  res <- basis_anova(x = x, group = batch,
                     override = "all")
  expect_equal(res$override,
               c("outliers_within_group",
                 "equality_of_variance",
                 "number_of_groups"))
  expect_length(res$diagnostic_failures, 0)
})

test_that("ANOVA method matches STAT17 when between-batch var. is small", {
  data <- tribble(
    ~x, ~batch,
    105.04953017290813, 1,
    105.74515635546253, 1,
    99.7549396676824, 1,
    107.44219439303261, 1,
    100.17657481474124, 1,
    106.601810738431, 1,
    101.15202811896768, 2,
    90.63466521331704, 2,
    106.93692070778634, 2,
    116.14555531325212, 2,
    100.20555336225114, 2,
    103.89002397699194, 2,
    110.50367678215923, 3,
    95.34690617376182, 3,
    105.03624331633935, 3,
    105.83852344481843, 3,
    105.8785931848096, 3,
    103.97623814685818, 3,
    94.92344509669459, 4,
    89.35739844589054, 4,
    110.45073142288507, 4,
    108.32807015574465, 4,
    104.35498641239826, 4,
    109.39785860273314, 4,
    102.88966425996772, 5,
    105.08208381529616, 5,
    109.82310733067601, 5,
    108.64289487358796, 5,
    99.87084985403291, 5,
    96.7651412720645, 5
  )
  res <- basis_anova(data, x, batch, override = "all")
  expect_equal(res$basis, 93.2, tolerance = 0.05)
})

test_that("glance.basis produces expected value", {
  # Sample data from CMH-17-1G Section 8.3.11.2.2

  res <- cmh_17_1g_8_3_11_1_1_etw2 %>%
    basis_anova(strength, batch,
                override = c("number_of_groups"))

  glance_res <- glance(res)

  expect_equal(glance_res[["p"]][1], 0.9)
  expect_equal(glance_res[["conf"]][1], 0.95)
  expect_equal(glance_res[["distribution"]][1], "ANOVA")
  expect_equal(glance_res[["n"]][1], nrow(cmh_17_1g_8_3_11_1_1_etw2))
  expect_equal(glance_res[["r"]][1], 3)
  expect_equal(glance_res[["basis"]][1], 63.2, tolerance = 0.05)

  glance_res_2 <- glance(res, TRUE)

  for (gn in names(glance_res)) {
    expect_equal(glance_res[[gn]], glance_res_2[[gn]])
  }

  expect_equal(glance_res_2[["outliers_within_group"]], "P")
  expect_equal(glance_res_2[["equality_of_variance"]], "P")
  expect_equal(glance_res_2[["number_of_groups"]], "O")

  expect_warning({
    glance_res_3 <- cmh_17_1g_8_3_11_1_1_etw2 %>%
      basis_anova(strength, batch) %>%
      glance(TRUE)
  })

  expect_equal(glance_res_3[["outliers_within_group"]], "P")
  expect_equal(glance_res_3[["equality_of_variance"]], "P")
  expect_equal(glance_res_3[["number_of_groups"]], "F")
})

test_that("glance for pooled methods works", {
  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    basis_pooled_sd(strength, condition, batch,
                    override = c("outliers_within_batch")) %>%
    glance(TRUE)

  # 3 conditions should produce 3 basis values and hence 3 rows
  expect_equal(nrow(res), 3)
})

test_that("pooled methods process override='all'", {

  res <- basis_pooled_sd(poolable_data, strength, condition, modcv = TRUE,
                         override = "all")
  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_group_variability",
                 "outliers_within_group",
                 "pooled_data_normal",
                 "pooled_variance_equal"))
  expect_length(res$diagnostic_failures, 0)

  res <- basis_pooled_cv(poolable_data, strength, condition, modcv = TRUE,
                         override = "all")
  expect_equal(res$override,
               c("outliers_within_batch",
                 "between_group_variability",
                 "outliers_within_group",
                 "pooled_data_normal",
                 "normalized_variance_equal"))
  expect_length(res$diagnostic_failures, 0)
})
