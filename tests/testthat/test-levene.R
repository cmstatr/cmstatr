context("Levene's Test")

suppressMessages(library(dplyr))

# From CMH-17-1G Section 8.3.11.1.1
df <- tribble(
  ~condition, ~batch, ~strength,
  "CTD", 1, 118.3774604,
  "CTD", 1, 123.6035612,
  "CTD", 1, 115.2238092,
  "CTD", 1, 112.6379744,
  "CTD", 1, 116.5564277,
  "CTD", 1, 123.1649896,
  "CTD", 2, 128.5589027,
  "CTD", 2, 113.1462103,
  "CTD", 2, 121.4248107,
  "CTD", 2, 134.3241906,
  "CTD", 2, 129.6405117,
  "CTD", 2, 117.9818658,
  "CTD", 3, 115.4505226,
  "CTD", 3, 120.0369467,
  "CTD", 3, 117.1631088,
  "CTD", 3, 112.9302797,
  "CTD", 3, 117.9114501,
  "CTD", 3, 120.1900159,
  "CTD", 3, 110.7295966,
  "RTD", 1, 84.951364,
  "RTD", 1, 92.4891822,
  "RTD", 1, 96.8212659,
  "RTD", 1, 109.030325,
  "RTD", 1, 97.8918182,
  "RTD", 1, 100.921517,
  "RTD", 1, 103.699444,
  "RTD", 2, 93.790812,
  "RTD", 2, 107.526709,
  "RTD", 2, 94.5769704,
  "RTD", 2, 93.8831373,
  "RTD", 2, 98.2296605,
  "RTD", 2, 111.34659,
  "RTD", 2, 100.817538,
  "RTD", 3, 100.382203,
  "RTD", 3, 91.5037811,
  "RTD", 3, 100.083233,
  "RTD", 3, 95.6393615,
  "RTD", 3, 109.304779,
  "RTD", 3, 999.1205847,
  "RTD", 3, 100.078562,
  "ETD", 1, 83.7436035,
  "ETD", 1, 84.3831677,
  "ETD", 1, 94.8030433,
  "ETD", 1, 94.3931537,
  "ETD", 1, 101.702222,
  "ETD", 1, 86.5372121,
  "ETD", 1, 92.3772684,
  "ETD", 2, 89.2084024,
  "ETD", 2, 100.686001,
  "ETD", 2, 81.0444192,
  "ETD", 2, 91.339807,
  "ETD", 2, 93.1441939,
  "ETD", 2, 85.8204168,
  "ETD", 3, 94.8966273,
  "ETD", 3, 95.806852,
  "ETD", 3, 86.7842252,
  "ETD", 3, 94.4011973,
  "ETD", 3, 96.7231171,
  "ETD", 3, 89.9010384,
  "ETD", 3, 89.32672306,
  "ETW", 1, 106.357525,
  "ETW", 1, 105.898733,
  "ETW", 1, 88.4640082,
  "ETW", 1, 103.901744,
  "ETW", 1, 80.2058219,
  "ETW", 1, 109.199597,
  "ETW", 1, 61.0139431,
  "ETW", 2, 99.3207107,
  "ETW", 2, 115.86177,
  "ETW", 2, 82.6133082,
  "ETW", 2, 85.3690411,
  "ETW", 2, 115.801622,
  "ETW", 2, 44.3217741,
  "ETW", 2, 117.328077,
  "ETW", 2, 88.6782903,
  "ETW", 3, 107.676986,
  "ETW", 3, 108.960241,
  "ETW", 3, 116.12264,
  "ETW", 3, 80.2334815,
  "ETW", 3, 106.14557,
  "ETW", 3, 104.667866,
  "ETW", 3, 104.234953,
  "ETW2", 1, 99.0239966,
  "ETW2", 1, 103.341238,
  "ETW2", 1, 100.30213,
  "ETW2", 1, 98.4634133,
  "ETW2", 1, 92.264728,
  "ETW2", 1, 103.487693,
  "ETW2", 1, 113.734763,
  "ETW2", 2, 108.172659,
  "ETW2", 2, 108.426732,
  "ETW2", 2, 116.260375,
  "ETW2", 2, 121.04961,
  "ETW2", 2, 111.223082,
  "ETW2", 2, 104.574843,
  "ETW2", 2, 103.222552,
  "ETW2", 3, 99.3918538,
  "ETW2", 3, 87.3421658,
  "ETW2", 3, 102.730741,
  "ETW2", 3, 96.3694916,
  "ETW2", 3, 99.5946088,
  "ETW2", 3, 97.0712407
)

test_that("Levene's test matches ASAP", {
  res <- df %>%
    levene_test(strength, condition)

  expect_equal(res$f, 0.896, tolerance = 0.005)
  expect_false(res$reject_equal_variance)
  expect_output(print(res), ".*n.*102")
  expect_output(print(res), ".*k.*5")
  expect_output(print(res), "Conclusion: Samples have equal variance")
})

test_that("Levene's test matches results from STAT17", {
  res <- df %>%
    filter(condition == "CTD") %>%
    levene_test(strength, batch)
  expect_equal(res$f, 3.850, tolerance = 0.005)

  res <- df %>%
    filter(condition == "RTD") %>%
    levene_test(strength, batch)
  expect_equal(res$f, 0.973, tolerance = 0.005)

  res <- df %>%
    filter(condition == "ETD") %>%
    levene_test(strength, batch)
  expect_equal(res$f, 0.723, tolerance = 0.005)

  res <- df %>%
    filter(condition == "ETW2") %>%
    levene_test(strength, batch)
  expect_equal(res$f, 0.123, tolerance = 0.005)
})

test_that("glance produces expected results", {
  res <- df %>%
    levene_test(strength, condition, alpha = 0.05)

  glance_res <- glance(res)

  expect_equal(glance_res$alpha[1], 0.05)
  expect_equal(glance_res$n[1], 102)
  expect_equal(glance_res$k[1], 5)
  expect_equal(glance_res$f[1], 0.896, tolerance = 0.005)
  expect_equal(glance_res$p[1], 0.469, tolerance = 0.005)
  expect_false(glance_res$reject_equal_variance[1])

})
