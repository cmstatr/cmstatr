context("normalize")

suppressMessages(library(dplyr))

test_that("normalization to ply thickness takes correct argument lengths", {
  expect_error(normalize_ply_thickness(c(1, 2), 1, 1))
  expect_error(normalize_ply_thickness(1, c(1, 2), 1))
  expect_error(normalize_ply_thickness(c(1, 2), c(1, 2), c(1, 2)))
})

test_that("normalization to ply thickness produces expected numeric results", {
  expect_equal(normalize_ply_thickness(1, 1, 0.25), 4)
  expect_equal(normalize_ply_thickness(c(1, 2),
                                       c(0.5, 0.25),
                                       0.25),
               c(2, 2)
  )
})

test_that("normalize to group mean produces means of one for all groups", {
  # since each group is normalized to its own mean, the mean of the normalized
  # values within each group should be equal to one.
  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    mutate(norm_str = normalize_group_mean(strength, condition)) %>%
    group_by(condition) %>%
    summarise(mean_norm_str = mean(norm_str)) %>%
    rowwise() %>%
    mutate(test = expect_equal(mean_norm_str, 1))
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

test_that("Modified CV transform produces values that match CMH17-STATS", {
  res <- poolable_data %>%
    mutate(trans_strength = transform_mod_cv(strength, condition)) %>%
    group_by(condition) %>%
    summarise(cv = sd(strength) / mean(strength),
              mod_cv = sd(trans_strength) / mean(trans_strength))

  expect_equal(res$cv[res$condition == "CTD"], 0.0933, tolerance = 1e-4)
  expect_equal(res$mod_cv[res$condition == "CTD"], 0.0933, tolerance = 1e-4)
  expect_equal(res$cv[res$condition == "RTD"], 0.0580, tolerance = 1e-4)
  expect_equal(res$mod_cv[res$condition == "RTD"], 0.0690, tolerance = 1e-4)
  expect_equal(res$cv[res$condition == "ETW"], 0.0723, tolerance = 1e-4)
  expect_equal(res$mod_cv[res$condition == "ETW"], 0.0761, tolerance = 1e-4)
})

