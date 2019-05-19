context("Anderson-Darling k-Sample")

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(kSamples))  # nolint

test_that("kSamples package gives results that match published example", {
  # Reproduce the example from:
  # F. W. Scholz and M. . Stephens, “K-Sample Anderson-Darling Tests,” Journal
  #   of the American Statistical Association, vol. 82, no. 399.
  #   pp. 918–924, Sep-1987.

  df <- data.frame(
    smoothness = c(
      38.7, 41.5, 43.8, 44.5, 45.5, 46.0, 47.7, 58.0,
      39.2, 39.3, 39.7, 41.4, 41.8, 42.9, 43.3, 45.8,
      34.0, 35.0, 39.0, 40.0, 43.0, 43.0, 44.0, 45.0,
      34.0, 34.8, 34.8, 35.4, 37.2, 37.8, 41.2, 42.8
    ),
    lab = c(rep("A", 8), rep("B", 8), rep("C", 8), rep("D", 8))
  )

  res <- ad.test(smoothness ~ lab, data = df)

  expect_equal(res[["sig"]], 1.2038, tolerance = 1e-4)

  ad <- res[["ad"]]

  expect_equal(ad["version 1:", "AD"], 8.3559, tolerance = 1e-3)
  expect_equal(ad["version 1:", " asympt. P-value"], 0.0023, tolerance = 1e-3)
  expect_equal(ad["version 2:", "AD"], 8.3926, tolerance = 1e-3)
  expect_equal(ad["version 2:", " asympt. P-value"], 0.0022, tolerance = 1e-3)
})

test_that("ADK test match ASAP", {
  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.456, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.604, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.778, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "FT") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.777, tolerance = 0.003)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "FT") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.355, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "FT") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.432, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "WC") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.384, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "WC") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.723, tolerance = 0.003)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "WC") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.145, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "FC") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.865, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "FC") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.934, tolerance = 0.002)
  expect_false(res$reject_same_pop)

  res <- carbon.fabric %>%
    filter(test == "FC") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.501, tolerance = 0.002)
  expect_false(res$reject_same_pop)
})
