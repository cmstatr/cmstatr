context("equiv")

suppressMessages(library(dplyr))

test_that("k-factor warnings and errors are raised", {
  expect_error(k_equiv(-0.01, 3))
  expect_error(k_equiv(1.01, 3))
  expect_warning(k_equiv(1e-9, 3))
  expect_warning(k_equiv(0.75, 3))
  expect_error(k_equiv(0.05, 1))
})

test_that("k-factors match those published in literature", {

  if (requireNamespace("tidyr", quietly = TRUE)) {

    # the files k1.vangel.csv and k2.vangel.csv contain the k factors
    # published in Vangel's 2002 paper.

    k1_vangel <- read.csv(system.file("extdata", "k1.vangel.csv",
                                      package = "cmstatr")) %>%
      tidyr::gather(n, k1, X2:X10) %>%
      mutate(n = as.numeric(substring(n, 2)))

    k2_vangel <- read.csv(system.file("extdata", "k2.vangel.csv",
                                      package = "cmstatr")) %>%
      tidyr::gather(n, k2, X2:X10) %>%
      mutate(n = as.numeric(substring(n, 2)))

    # This test is super slow, so we will only run a sampling of the values
    # that
    # would normally be used in the validation, unless the flag full_test is
    # set to TRUE
    full_test <- FALSE
    diff_df <- inner_join(k1_vangel, k2_vangel, by = c("alpha", "n")) %>%
      mutate(error_threshold = case_when(n == 2 & alpha > 0.25 ~ 0.10,
                                         n == 2 & alpha <= 0.25 ~ 0.02,
                                         n > 2 & alpha > 0.25 ~ 0.005,
                                         TRUE ~ 5e-4))

    diff_df <- diff_df %>%
      sample_n(ifelse(full_test, length(diff_df$k1), 5)) %>%
      tidyr::gather(fct, vangel, k1:k2) %>%
      group_by(alpha, n) %>%
      mutate(calc = k_equiv(first(alpha), first(n))) %>%
      mutate(diff = (vangel - calc) / vangel) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(check = expect_lte(abs(diff), error_threshold, label =
                                  paste0("Validation failure for ",
                                         "alpha=", alpha,
                                         ", n=", n,
                                         " computed ", fct, "=", calc,
                                         " but validation ", fct, "=", vangel,
                                         ".\n")))
  }
})

test_that("check equiv_mean_extremum against HYTEQ using some example data", {
  data_sample <- c(145.055, 148.329, 142.667, 141.795, 144.139,
                   135.923, 136.177, 133.523, 134.350)
  res <- equiv_mean_extremum(alpha = 0.05, data_sample = data_sample,
                             mean_qual = 141.310, sd_qual = 6.415)

  expect_equal(res$alpha, 0.05)
  expect_equal(res$n_sample, 9)
  expect_equal(res$modcv, FALSE)
  expect_equal(res$cv, 6.415 / 141.310, tolerance = 1e-8)
  expect_equal(res$threshold_min_indiv, 123.725, tolerance = 1e-2)
  expect_equal(res$threshold_mean, 137.197, tolerance = 1e-2)
  expect_equal(res$result_min_indiv, "PASS")
  expect_equal(res$result_mean, "PASS")

  expect_output(print(res), "alpha\\W*=\\W*0.05")
  expect_output(print(res), "n\\W*=\\W*9")
  expect_output(print(res), "Sample[s:]*\\W*133.5\\d*\\W*140.2\\d*")
  expect_output(print(res), "Threshold[s:]*\\W*123.7\\d*\\W*137.[12]")
  expect_output(print(res), "Equiv\\w*\\W*PASS\\W*PASS")
})

test_that("check equiv_mean_extremum against HYTEQ using an example (modCV)", {
  data_sample <- c(145.055, 148.329, 142.667, 141.795, 144.139,
                   135.923, 136.177, 133.523, 134.350)
  res <- equiv_mean_extremum(alpha = 0.05, data_sample = data_sample,
                             mean_qual = 141.310, sd_qual = 6.415,
                             modcv = TRUE)

  expect_equal(res$alpha, 0.05)
  expect_equal(res$n_sample, 9)
  expect_equal(res$modcv, TRUE)
  expect_equal(res$cv, 6.415 / 141.310, tolerance = 1e-8)
  expect_equal(res$cv_star, 0.0627, tolerance = 1e-3)
  expect_equal(res$threshold_min_indiv, 117.024, tolerance = 1e-2)
  expect_equal(res$threshold_mean, 135.630, tolerance = 1e-2)
  expect_equal(res$result_min_indiv, "PASS")
  expect_equal(res$result_mean, "PASS")

  #ensure print indicates it's modCV
  expect_output(print(res), "[Mm]od\\w*\\WCV")

  expect_output(print(res), "alpha\\W*=\\W*0.05")
  expect_output(print(res), "n\\W*=\\W*9")
  expect_output(print(res), "Sample[s:]*\\W*133.5\\d*\\W*140.2\\d*")
  expect_output(print(res), "Threshold[s:]*\\W*117.0\\d*\\W*135.6")
  expect_output(print(res), "Equiv\\w*\\W*PASS\\W*PASS")
})

test_that("check three ways of specifying qual data are same (mean_ext)", {
  data_qual <- c(145.055, 148.329, 142.667, 141.795, 144.139,
                   135.923, 136.177, 133.523, 134.350)
  data_qual_df <- data.frame(strength = data_qual)

  res1 <- equiv_mean_extremum(alpha = 0.05, data_qual = data_qual,
                              n_sample = 5)
  res2 <- equiv_mean_extremum(alpha = 0.05, df_qual = data_qual_df,
                              data_qual = strength, n_sample = 5)
  res3 <- equiv_mean_extremum(alpha = 0.05, mean_qual = mean(data_qual),
                              sd_qual = sd(data_qual), n_sample = 5)

  # the calls will be different, and that's okay
  res1$call <- NULL
  res2$call <- NULL
  res3$call <- NULL

  expect_equal(res1, res2)
  expect_equal(res1, res3)
})

test_that("check that glance.equiv_mean_extremum produces expected results", {
  data_sample <- c(145.055, 148.329, 142.667, 141.795, 144.139,
                   135.923, 136.177, 133.523, 134.350)
  res <- equiv_mean_extremum(alpha = 0.05, data_sample = data_sample,
                             mean_qual = 141.310, sd_qual = 6.415,
                             modcv = TRUE)
  res <- glance(res)

  expect_equal(res$alpha[1], 0.05)
  expect_equal(res$n_sample[1], 9)
  expect_equal(res$modcv[1], TRUE)
  expect_equal(res$threshold_min_indiv[1], 117.024, tolerance = 1e-2)
  expect_equal(res$threshold_mean[1], 135.630, tolerance = 1e-2)
  expect_equal(res$result_min_indiv[1], "PASS")
  expect_equal(res$result_mean[1], "PASS")
  expect_equal(res$min_sample[1], min(data_sample))
  expect_equal(res$mean_sample[1], mean(data_sample))

  res <- equiv_mean_extremum(alpha = 0.05, mean_qual = 141.310,
                             sd_qual = 6.415, n_sample = 9, modcv = TRUE)
  res <- glance(res)

  expect_equal(ncol(res), 5)
  expect_equal(res$alpha[1], 0.05)
  expect_equal(res$n_sample[1], 9)
  expect_equal(res$modcv[1], TRUE)
  expect_equal(res$threshold_min_indiv[1], 117.024, tolerance = 1e-2)
  expect_equal(res$threshold_mean[1], 135.630, tolerance = 1e-2)
})

test_that("check equiv_change_mean against HYTEQ using some example data", {
  res <- equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02,
                           sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
                           sd_qual = 0.162)

  expect_equal(res$alpha, 0.05)
  expect_equal(res$n_sample, 9)
  expect_equal(res$mean_sample, 9.02)
  expect_equal(res$sd_sample, 0.15785)
  expect_equal(res$n_qual, 28)
  expect_equal(res$mean_qual, 9.24)
  expect_equal(res$sd_qual, 0.162)
  expect_equal(res$sp, 0.1608, tolerance = 5e-4)
  expect_equal(res$t0, -3.570, tolerance = 5e-3)
  expect_equal(res$t_req, 2.030, tolerance = 5e-3)
  expect_equal(res$threshold, c(9.115, 9.365), tolerance = 5e-3)
  expect_equal(res$modcv, FALSE)
  expect_equal(res$result, "FAIL")

  expect_output(print(res), "alpha\\W*=\\W*0.05")
  expect_output(print(res), "Number\\W*28\\W*9")
  expect_output(print(res), "Mean\\W*9.24\\d*\\W*9.02\\d*")
  expect_output(print(res), "Result\\W*FAIL")
  expect_output(print(res), "Range\\W*9.11[45]\\d*\\W*\\w*\\W*9.365\\d*")
})

test_that("check equiv_change_mean against HYTEQ using an example (modCV)", {
  res <- equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02,
                           sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
                           sd_qual = 0.162, modcv = TRUE)

  expect_equal(res$alpha, 0.05)
  expect_equal(res$n_sample, 9)
  expect_equal(res$mean_sample, 9.02)
  expect_equal(res$sd_sample, 0.15785)
  expect_equal(res$n_qual, 28)
  expect_equal(res$mean_qual, 9.24)
  expect_equal(res$sd_qual, 0.162)
  expect_equal(res$sp, 0.4927, tolerance = 5e-4)
  expect_equal(res$t0, -1.165, tolerance = 5e-3)
  expect_equal(res$t_req, 2.03, tolerance = 5e-3)
  expect_equal(res$threshold, c(8.857, 9.623), tolerance = 5e-3)
  expect_equal(res$modcv, TRUE)
  expect_equal(res$result, "PASS")

  #ensure print indicates it's modCV
  expect_output(print(res), "[Mm]od\\w*\\WCV")

  expect_output(print(res), "alpha\\W*=\\W*0.05")
  expect_output(print(res), "Number\\W*28\\W*9")
  expect_output(print(res), "Mean\\W*9.24\\d*\\W*9.02\\d*")
  expect_output(print(res), "Result\\W*PASS")
  expect_output(print(res), "Range\\W*8.85[67]\\d*\\W*\\w*\\W*9.623\\d*")
})

test_that("check four ways of specifying qual data are same (chg in mean)", {
  data_qual <- c(145.055, 148.329, 142.667, 141.795, 144.139,
                 135.923, 136.177, 133.523, 134.350)
  data_qual_df <- data.frame(strength = data_qual)
  data_sample <- c(145.055, 148.329, 142.667, 141.795, 144.139)

  res1 <- equiv_change_mean(alpha = 0.05, data_qual = data_qual,
                            n_sample = length(data_sample),
                            mean_sample = mean(data_sample),
                            sd_sample = sd(data_sample))
  res2 <- equiv_change_mean(alpha = 0.05, df_qual = data_qual_df,
                            data_qual = strength,
                            n_sample = length(data_sample),
                            mean_sample = mean(data_sample),
                            sd_sample = sd(data_sample))
  res3 <- equiv_change_mean(alpha = 0.05, mean_qual = mean(data_qual),
                            sd_qual = sd(data_qual),
                            n_qual = length(data_qual),
                            n_sample = length(data_sample),
                            mean_sample = mean(data_sample),
                            sd_sample = sd(data_sample))
  res4 <- equiv_change_mean(alpha = 0.05, data_qual = data_qual,
                            data_sample = data_sample)

  # the calls will be different, and that's okay
  res1$call <- NULL
  res2$call <- NULL
  res3$call <- NULL
  res4$call <- NULL

  expect_equal(res1, res2)
  expect_equal(res1, res3)
  expect_equal(res1, res4)
})

test_that("glance.equiv_change_mean produces expected results", {
  res <- equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02,
                           sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
                           sd_qual = 0.162)

  res <- glance(res)

  expect_equal(res$alpha[1], 0.05)
  expect_equal(res$n_sample[1], 9)
  expect_equal(res$mean_sample[1], 9.02)
  expect_equal(res$sd_sample[1], 0.15785)
  expect_equal(res$n_qual[1], 28)
  expect_equal(res$mean_qual[1], 9.24)
  expect_equal(res$sd_qual[1], 0.162)
  expect_equal(res$sp[1], 0.1608, tolerance = 5e-4)
  expect_equal(res$t0[1], -3.570, tolerance = 5e-3)
  expect_equal(res$t_req[1], 2.030, tolerance = 5e-3)
  expect_equal(res$threshold_min[1], 9.115, tolerance = 5e-3)
  expect_equal(res$threshold_max[1], 9.365, tolerance = 5e-3)
  expect_equal(res$modcv[1], FALSE)
  expect_equal(res$result[1], "FAIL")
})

test_that("equiv_mean_extremum produces expected errors and warnings", {
  expect_error(
    equiv_mean_extremum(
      alpha = -0.05, n_sample = 9,
      mean_qual = 9.24,
      sd_qual = 0.162),
    "alpha"
  )

  expect_error(
    equiv_mean_extremum(
      alpha = 1.05, n_sample = 9,
      mean_qual = 9.24,
      sd_qual = 0.162),
    "alpha"
  )

  expect_warning(
    equiv_mean_extremum(
      alpha = 0.05,
      data_sample = runif(9),
      n_sample = 9,
      mean_qual = 9.24,
      sd_qual = 0.162),
    "n_sample"
  )

  expect_warning(
    equiv_mean_extremum(
      alpha = 0.05,
      n_sample = 9,
      data_qual = runif(28),
      mean_qual = 9.24,
      sd_qual = 0.162),
    "mean_qual"
  )

  expect_warning(
    equiv_mean_extremum(
      alpha = 0.05,
      n_sample = 9,
      data_qual = runif(28),
      mean_qual = 9.24,
      sd_qual = 0.162),
    "sd_qual"
  )

  expect_error(
    equiv_mean_extremum(
      alpha = 0.05,
      mean_sample = 9.02,
      sd_sample = 0.15785,
      n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "sample"
  )

  expect_error(
    equiv_mean_extremum(
      alpha = 0.05,
      n_sample = 9,
      sd_qual = 0.162),
    "mean_qual"
  )

  expect_error(
    equiv_mean_extremum(
      alpha = 0.05,
      n_sample = 9,
      mean_qual = 9.24),
    "sd_qual"
  )

  expect_error(
    equiv_mean_extremum(
      alpha = 0.05,
      mean_qual = 9.24,
      sd_qual = 0.162),
    "n_sample"
  )
})

test_that("equiv_change_mean produces expected errors and warnings", {
  expect_error(
    equiv_change_mean(
      alpha = -0.05, n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "alpha"
  )

  expect_error(
    equiv_change_mean(
      alpha = 1.05, n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "alpha"
  )

  expect_warning(
    equiv_change_mean(
      alpha = 0.05,
      data_sample = runif(9),
      n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "n_sample"
  )

  expect_error(
    equiv_change_mean(
      alpha = 0.05,
      n_sample = 9,
      sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "mean_sample"
  )

  expect_error(
    equiv_change_mean(
      alpha = 0.05,
      n_sample = 9, mean_sample = 9.02,
      n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "sd_sample"
  )

  expect_error(
    equiv_change_mean(
      alpha = 0.05,
      n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785,
      mean_qual = 9.24,
      sd_qual = 0.162),
    "n_qual"
  )

  expect_warning(
    equiv_change_mean(
      alpha = 0.05,
      n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785,
      data_qual = runif(28),
      n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "mean_qual"
  )

  expect_warning(
    equiv_change_mean(
      alpha = 0.05,
      n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785,
      data_qual = runif(28),
      n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "sd_qual"
  )

  expect_error(
    equiv_change_mean(
      alpha = 0.05,
      mean_sample = 9.02,
      sd_sample = 0.15785,
      n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162),
    "sample"
  )

  expect_error(
    equiv_change_mean(
      alpha = 0.05,
      n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785,
      n_qual = 28,
      sd_qual = 0.162),
    "mean_qual"
  )

  expect_error(
    equiv_change_mean(
      alpha = 0.05,
      n_sample = 9, mean_sample = 9.02,
      sd_sample = 0.15785,
      n_qual = 28, mean_qual = 9.24),
    "sd_qual"
  )
})
