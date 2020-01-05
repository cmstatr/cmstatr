context("Anderson-Darling k-Sample")

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(kSamples))  # nolint

test_that("kSamples package gives results that match published example", {
  # Reproduce the example from:
  # F. W. Scholz and M. Stephens, “K-Sample Anderson-Darling Tests,” Journal
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
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.604, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.778, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "FT") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.777, tolerance = 0.003)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "FT") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.355, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "FT") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.432, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "WC") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.384, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "WC") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.723, tolerance = 0.003)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "WC") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 1.145, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "FC") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.865, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "FC") %>%
    filter(condition == "ETW") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.934, tolerance = 0.002)
  expect_false(res$reject_same_dist)

  res <- carbon.fabric %>%
    filter(test == "FC") %>%
    filter(condition == "CTD") %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.501, tolerance = 0.002)
  expect_false(res$reject_same_dist)
})

test_that("ADK test matches example from CMH-17-1G", {
  # Reference the example in section 8.3.11.1.1
  etw <- tribble(
    ~batch, ~strength,
    1, 106.357525,
    1, 105.898733,
    1, 88.4640082,
    1, 103.901744,
    1, 80.2058219,
    1, 109.199597,
    1, 61.0139431,
    2, 99.3207107,
    2, 115.86177,
    2, 82.6133082,
    2, 85.3690411,
    2, 115.801622,
    2, 44.3217741,
    2, 117.328077,
    2, 88.6782903,
    3, 107.676986,
    3, 108.960241,
    3, 116.12264,
    3, 80.2334815,
    3, 106.14557,
    3, 104.667866,
    3, 104.234953
  )

  # ETW:
  #  ADK = 0.793 / same distribution
  res <- etw %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 0.793, tolerance = 0.003)
  expect_false(res$reject_same_dist)
  expect_output(print(res), ".*N.*22")
  expect_output(print(res), ".*k.*3")
  expect_output(print(res), "Conclusion: Samples come")

  etw2 <- tribble(
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

  # ETW2:
  #  ADK = 3.024 / different distribution
  res <- etw2 %>%
    ad_ksample(strength, batch)
  expect_equal(res$ad / (res$k - 1), 3.024, tolerance = 0.003)
  expect_true(res$reject_same_dist)
  expect_output(print(res), ".*N.*20")
  expect_output(print(res), ".*k.*3")
  expect_output(print(res), "Conclusion: Samples do not come")
})

test_that("glance.adk produces expected output", {
  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    filter(condition == "RTD") %>%
    ad_ksample(strength, batch)

  glance_res <- glance(res)

  expect_equal(glance_res[["alpha"]][1], 0.025)
  expect_equal(glance_res[["n"]][1], 18)
  expect_equal(glance_res[["k"]][1], 3)
  expect_equal(glance_res[["sigma"]], 0.944, tolerance = 0.001)
  expect_equal(glance_res[["ad"]][1], 0.456 * (res$k - 1))
  expect_equal(glance_res[["p"]][1], 0.96, tolerance = 0.001)
  expect_equal(glance_res[["reject_same_dist"]], FALSE)

})
