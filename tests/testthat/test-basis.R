suppressMessages(library(dplyr))

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

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          res <- basis_normal(x = x, batch = batch),
          "outliers_within_batch"
        ),
        "between_batch_variability"
      ),
      "outliers"
    ),
    "anderson_darling_normal"
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

  expect_equal(class(res$diagnostic_obj$outliers), "mnr")
  expect_gt(res$diagnostic_obj$outliers$n_outliers, 0)

  expect_length(res$diagnostic_obj$outliers_within_batch, 2)  # two batches
  expect_equal(class(res$diagnostic_obj$outliers_within_batch$A), "mnr")
  expect_equal(class(res$diagnostic_obj$outliers_within_batch$B), "mnr")
  expect_equal(res$diagnostic_obj$outliers_within_batch$A$n_outliers, 0)
  expect_gt(res$diagnostic_obj$outliers_within_batch$B$n_outliers, 0)

  expect_equal(class(res$diagnostic_obj$between_batch_variability), "adk")
  expect_true(res$diagnostic_obj$between_batch_variability$reject_same_dist)

  expect_equal(class(res$diagnostic_obj$anderson_darling_normal),
               "anderson_darling")
  expect_true(res$diagnostic_obj$anderson_darling_normal$reject_distribution)

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
  expect_warning(
    expect_warning(
      expect_message(
        expect_message(
          res <- basis_normal(x = x),
          "outliers_within_batch"
        ),
        "between_batch_variability"
      ),
      "outliers"
    ),
    "anderson_darling_normal"
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

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          res <- basis_lognormal(x = x, batch = batch),
          "outliers_within_batch"
        ),
        "between_batch_variability"
      ),
      "outliers"
    ),
    "anderson_darling_lognormal"
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_lognormal"))
  expect_length(res$override, 0)

  expect_equal(class(res$diagnostic_obj$anderson_darling_lognormal),
               "anderson_darling")
  expect_true(res$diagnostic_obj$anderson_darling_lognormal$reject_distribution)

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
  expect_warning(
    expect_warning(
      expect_message(
        expect_message(
          res <- basis_lognormal(x = x),
          "outliers_within_batch"
        ),
        "between_batch_variability"
      ),
      "outliers"
    ),
    "anderson_darling_lognormal"
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

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          res <- basis_weibull(x = x, batch = batch),
          "outliers_within_batch"
        ),
        "between_batch_variability"
      ),
      "outliers"
    ),
    "anderson_darling_weibull"
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers",
                 "anderson_darling_weibull"))
  expect_length(res$override, 0)

  expect_equal(class(res$diagnostic_obj$anderson_darling_weibull),
               "anderson_darling")
  expect_true(res$diagnostic_obj$anderson_darling_weibull$reject_distribution)

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
  expect_warning(
    expect_warning(
      expect_message(
        expect_message(
          res <- basis_weibull(x = x),
          "outliers_within_batch"
        ),
        "between_batch_variability"
      ),
      "outliers"
    ),
    "anderson_darling_weibull"
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
  expect_equal(res$basis, 124.156, tolerance = 0.002)
  expect_output(print(res), "b-basis.*124", ignore.case = TRUE)
  expect_output(print(res), "nonparametric", ignore.case = TRUE)
  expect_match(res$distribution, "nonparametric.*optimum", ignore.case = TRUE)

  res <- basis_hk_ext(x = data, p = 0.99, conf = 0.95,
                      method = "woodward-frawley",
                      override = c("outliers_within_batch",
                                   "between_batch_variability"))
  expect_equal(res$basis, 99.651, tolerance = 0.002)
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
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            res <- basis_hk_ext(
              x = x_large, batch = batch_large, method = "woodward-frawley"),
            "outliers_within_batch"
          ),
          "between_batch_variability"
        ),
        "outliers"
      ),
      "correct_method_used"
    ),
    "sample_size"
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

  expect_type(res$diagnostic_obj$correct_method_used, "logical")
  expect_false(res$diagnostic_obj$correct_method_used)
  expect_type(res$diagnostic_obj$sample_size, "logical")
  expect_false(res$diagnostic_obj$sample_size)

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
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            res <- basis_hk_ext(
              x = x_large, batch = batch_large, method = "optimum-order",
              p = 0.99, conf = 0.95),
            "outliers_within_batch"
          ),
          "between_batch_variability"
        ),
        "outliers"
      ),
      "correct_method_used"
    ),
    "sample_size"
  )

  # call basis_normal without batch
  expect_warning(
    expect_message(
      expect_message(
        res <- basis_hk_ext(x = x_small, method = "optimum-order"),
        "outliers_within_batch"
      ),
      "between_batch_variability"
    ),
    "outliers"
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

  expect_warning(
    expect_warning(
      expect_warning(
        res <- basis_nonpara_large_sample(
          x = x_large, batch = batch_large),
        "outliers_within_batch"
      ),
      "between_batch_variability"
    ),
    "outliers"
  )

  # Check that res$... contains the correct value
  expect_equal(res$batch, batch_large)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_batch",
                 "between_batch_variability",
                 "outliers"))
  expect_length(res$override, 0)

  expect_type(res$diagnostic_obj$sample_size, "logical")
  expect_true(res$diagnostic_obj$sample_size)

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

  expect_warning(
    expect_warning(
      expect_warning(
        res <- basis_nonpara_large_sample(
          x = x_large, batch = batch_large,
          p = 0.99, conf = 0.95),
        "outliers_within_batch"
      ),
      "between_batch_variability"
    ),
    "outliers"
  )

  # call basis_normal without batch
  expect_warning(
    expect_message(
      expect_message(
        res <- basis_nonpara_large_sample(x = x_large),
        "outliers_within_batch"
      ),
      "between_batch_variability"
    ),
    "outliers"
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
  expect_equal(res$basis, 127.74060, tolerance = 0.002)

  res <- basis_hk_ext(x = head(data, 18), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 127.36697, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 17), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 127.02732, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 16), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 126.23545, tolerance = 0.002)

  res <- basis_hk_ext(x = head(data, 15), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 125.68740, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 14), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 125.17500, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 13), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 124.07851, tolerance = 0.002)

  res <- basis_hk_ext(x = head(data, 12), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 121.17418, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 11), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 120.26382, tolerance = 0.001)

  res <- basis_hk_ext(x = head(data, 10), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 120.75149, tolerance = 0.002)

  res <- basis_hk_ext(x = head(data, 9), p = 0.9, conf = 0.95,
                      method = "optimum-order", override = "all")
  expect_equal(res$basis, 119.80108, tolerance = 0.001)
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

  expect_warning(
    expect_warning(
      expect_warning(
        res <- basis_anova(x = x, group = batch),
        "outliers_within_group"
      ),
      "equality_of_variance"
    ),
    "number_of_groups"
  )

  # Check that res$... contains the correct value
  expect_equal(res$group, batch)
  expect_equal(res$diagnostic_failures,
               c("outliers_within_group",
                 "equality_of_variance",
                 "number_of_groups"))
  expect_length(res$override, 0)

  expect_length(res$diagnostic_obj$outliers_within_group, 2)
  expect_equal(class(res$diagnostic_obj$outliers_within_group$A), "mnr")
  expect_equal(res$diagnostic_obj$outliers_within_group$A$n_outliers, 2)
  expect_equal(class(res$diagnostic_obj$outliers_within_group$B), "mnr")
  expect_equal(res$diagnostic_obj$outliers_within_group$B$n_outliers, 0)

  expect_equal(class(res$diagnostic_obj$equality_of_variance), "levene")
  expect_true(res$diagnostic_obj$equality_of_variance$reject_equal_variance)

  expect_false(res$diagnostic_obj$number_of_groups)

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
