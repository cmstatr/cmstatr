suppressMessages(library(dplyr))


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
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            res <- basis_pooled_sd(cmh_17_8_3_11_1_1, strength, condition,
                                   batch),
            "outliers_within_batch"
          ),
          "between_group_variability"
        ),
        "outliers_within_group"
      ),
      "pooled_data_normal"
    ),
    "pooled_variance_equal"
  )

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          res <- cmh_17_8_3_11_1_1 %>%
            filter(condition != "ETW2") %>%
            basis_pooled_sd(strength, condition, batch),
          "outliers_within_batch"
        ),
        "outliers_within_group"
      ),
      "pooled_data_normal"
    ),
    "pooled_variance_equal"
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

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            res <- basis_pooled_cv(cmh_17_8_3_11_1_1, strength,
                                   condition, batch),
            "outliers_within_batch"
          ),
          "between_group_variability"
        ),
        "outliers_within_group"
      ),
      "pooled_data_normal"
    ),
    "normalized_variance_equal"
  )

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          res <- cmh_17_8_3_11_1_1 %>%
            filter(condition != "ETW2") %>%
            basis_pooled_cv(strength, condition, batch),
          "outliers_within_batch"
        ),
        "outliers_within_group"
      ),
      "pooled_data_normal"
    ),
    "normalized_variance_equal"
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
  expect_warning(
    expect_message(
      expect_message(
        res_b <- basis_pooled_sd(poolable_data, strength, condition,
                                 override = c("pooled_variance_equal")),
        "outliers_within_batch"
      ),
      "between_group_variability"
    ),
    "pooled_data_normal"
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
  expect_warning(
    expect_message(
      expect_message(
        res_b <- basis_pooled_cv(poolable_data, strength, condition),
        "outliers_within_batch"
      ),
      "between_group_variability"
    ),
    "pooled_data_normal"
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
