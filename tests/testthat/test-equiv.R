context("equiv")

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

test_that("k-factor warnings and errors are raised", {
  expect_error(k_equiv(-0.01, 3))
  expect_error(k_equiv(1.01, 3))
  expect_warning(k_equiv(1e-9, 3))
  expect_warning(k_equiv(0.75, 3))
  expect_error(k_equiv(0.05, 1))
})

test_that("k-factors match those published in literature", {

  # the files k1.vangel.csv and k2.vangel.csv contain the k factors
  # published in Vangel's 2002 paper.

  k1.vangel <- read.csv(system.file("extdata", "k1.vangel.csv",
                                    package = "cmstatr")) %>%
    gather(n, k1, X2:X10) %>%
    mutate(n = as.numeric(substring(n, 2)))

  k2.vangel <- read.csv(system.file("extdata", "k2.vangel.csv",
                                    package = "cmstatr")) %>%
    gather(n, k2, X2:X10) %>%
    mutate(n = as.numeric(substring(n, 2)))

  # This test is super slow, so we will only run a sampling of the values that
  # would normally be used in the validation, unless the flag full_test is set
  # to TRUE
  full_test <- FALSE
  diff.df <- inner_join(k1.vangel, k2.vangel, by = c("alpha", "n")) %>%
    mutate(error_threshold = case_when(n == 2 & alpha > 0.25 ~ 0.10,
                                       n == 2 & alpha <= 0.25 ~ 0.02,
                                       n > 2 & alpha > 0.25 ~ 0.005,
                                       TRUE ~ 5e-4))

  diff.df <- diff.df %>%
    sample_n(ifelse(full_test, length(diff.df$k1), 5)) %>%
    gather(fct, vangel, k1:k2) %>%
    group_by(alpha, n) %>%
    mutate(calc = k_equiv(first(alpha), first(n))) %>%
    mutate(diff = (vangel - calc) / vangel) %>%
    ungroup()%>%
    rowwise() %>%
    mutate(check = expect_lte(abs(diff), error_threshold, label =
                                paste0("Validation failure for ",
                                       "alpha=", alpha,
                                       ", n=", n,
                                       " computed ", fct, "=", calc,
                                       " but validation ", fct, "=", vangel,
                                       ".\n")))
})

test_that("check equiv_mean_extremum against HYTEQ using some example data", {
  data_sample <- c(145.055, 148.329, 142.667, 141.795, 144.139, 135.923, 136.177, 133.523, 134.350)
  res <- equiv_mean_extremum(alpha = 0.05, data_sample = data_sample,
                             mean_qual = 141.310, sd_qual = 6.415)

  expect_equal(res$alpha, 0.05)
  expect_equal(res$n_sample, 9)
  expect_equal(res$modcv, FALSE)
  expect_equal(res$cv, 6.415 / 141.310, tolerance = 1e-8)
  expect_equal(res$threshold_min_indiv, 123.725, tolerance = 1e-2)
  expect_equal(res$threshold_mean, 137.197, tolerance = 1e-2)
  expect_equal(res$test_min_indiv, "PASS")
  expect_equal(res$test_mean, "PASS")

  expect_output(print(res), "alpha\\W*=\\W*0.05")
  expect_output(print(res), "n\\W*=\\W*9")
  expect_output(print(res), "Sample[s:]*\\W*133.5\\d*\\W*140.2\\d*")
  expect_output(print(res), "Threshold[s:]*\\W*123.7\\d*\\W*137.[12]")
  expect_output(print(res), "Equiv\\w*\\W*PASS\\W*PASS")
})
