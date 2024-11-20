suppressMessages(library(dplyr))


# data from CMH-17-1G Section 8.3.11.1.1
cmh_17_8_3_11_1_1 <- tribble(
  ~batch, ~strength, ~condition,
  "A", 118.3774604, "CTD", "A", 84.9581364, "RTD", "A", 83.7436035, "ETD",
  "A", 123.6035612, "CTD", "A", 92.4891822, "RTD", "A", 84.3831677, "ETD",
  "A", 115.2238092, "CTD", "A", 96.8212659, "RTD", "A", 94.8030433, "ETD",
  "A", 112.6379744, "CTD", "A", 109.030325, "RTD", "A", 94.3931537, "ETD",
  "A", 116.5564277, "CTD", "A", 97.8212659, "RTD", "A", 101.702222, "ETD",
  "A", 123.1649896, "CTD", "A", 100.921519, "RTD", "A", 86.5372121, "ETD",
  "B", 128.5589027, "CTD", "A", 103.699444, "RTD", "A", 92.3772684, "ETD",
  "B", 113.1462103, "CTD", "B", 93.7908212, "RTD", "B", 89.2084024, "ETD",
  "B", 121.4248107, "CTD", "B", 107.526709, "RTD", "B", 100.686001, "ETD",
  "B", 134.3241906, "CTD", "B", 94.5769704, "RTD", "B", 81.0444192, "ETD",
  "B", 129.6405117, "CTD", "B", 93.8831373, "RTD", "B", 91.3398070, "ETD",
  "B", 117.9818658, "CTD", "B", 98.2296605, "RTD", "B", 93.1441939, "ETD",
  "C", 115.4505226, "CTD", "B", 111.346590, "RTD", "B", 85.8204168, "ETD",
  "C", 120.0369467, "CTD", "B", 100.817538, "RTD", "C", 94.8966273, "ETD",
  "C", 117.1631088, "CTD", "C", 100.382203, "RTD", "C", 95.8068520, "ETD",
  "C", 112.9302797, "CTD", "C", 91.5037811, "RTD", "C", 86.7842252, "ETD",
  "C", 117.9114501, "CTD", "C", 100.083233, "RTD", "C", 94.4011973, "ETD",
  "C", 120.1900159, "CTD", "C", 95.6393615, "RTD", "C", 96.7231171, "ETD",
  "C", 110.7295966, "CTD", "C", 109.304779, "RTD", "C", 89.9010384, "ETD",
  "C", 100.078562, "RTD", "C", 99.1205847, "RTD", "C", 89.3672306, "ETD",
  "A", 106.357525, "ETW", "A", 99.0239966, "ETW2",
  "A", 105.898733, "ETW", "A", 103.341238, "ETW2",
  "A", 88.4640082, "ETW", "A", 100.302130, "ETW2",
  "A", 103.901744, "ETW", "A", 98.4634133, "ETW2",
  "A", 80.2058219, "ETW", "A", 92.2647280, "ETW2",
  "A", 109.199597, "ETW", "A", 103.487693, "ETW2",
  "A", 61.0139431, "ETW", "A", 113.734763, "ETW2",
  "B", 99.3207107, "ETW", "B", 108.172659, "ETW2",
  "B", 115.861770, "ETW", "B", 108.426732, "ETW2",
  "B", 82.6133082, "ETW", "B", 116.260375, "ETW2",
  "B", 85.3690411, "ETW", "B", 121.049610, "ETW2",
  "B", 115.801622, "ETW", "B", 111.223082, "ETW2",
  "B", 44.3217741, "ETW", "B", 104.574843, "ETW2",
  "B", 117.328077, "ETW", "B", 103.222552, "ETW2",
  "B", 88.6782903, "ETW", "C", 99.3918538, "ETW2",
  "C", 107.676986, "ETW", "C", 87.3421658, "ETW2",
  "C", 108.960241, "ETW", "C", 102.730741, "ETW2",
  "C", 116.122640, "ETW", "C", 96.3694916, "ETW2",
  "C", 80.2334815, "ETW", "C", 99.5946088, "ETW2",
  "C", 106.145570, "ETW", "C", 97.0712407, "ETW2",
  "C", 104.667866, "ETW",
  "C", 104.234953, "ETW"
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

  expect_length(res$diagnostic_obj$outliers_within_batch, 5)
  expect_length(res$diagnostic_obj$outliers_within_batch$RTD, 3)
  expect_equal(class(res$diagnostic_obj$outliers_within_batch$CTD$A), "mnr")
  expect_equal(res$diagnostic_obj$outliers_within_batch$CTD$A$n_outliers, 0)

  expect_length(res$diagnostic_obj$between_group_variability, 5)
  expect_equal(class(res$diagnostic_obj$between_group_variability$RTD), "adk")
  expect_false(
    res$diagnostic_obj$between_group_variability$RTD$reject_same_dist)

  expect_length(res$diagnostic_obj$outliers_within_group, 5)
  expect_equal(class(res$diagnostic_obj$outliers_within_group$CTD), "mnr")
  expect_equal(res$diagnostic_obj$outliers_within_group$CTD$n_outliers, 0)

  expect_equal(class(res$diagnostic_obj$pooled_data_normal), "anderson_darling")
  expect_true(res$diagnostic_obj$pooled_data_normal$reject_distribution)

  expect_equal(class(res$diagnostic_obj$pooled_variance_equal), "levene")
  expect_true(res$diagnostic_obj$pooled_variance_equal$reject_equal_variance)

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

  expect_equal(class(res_b$diagnostic_obj$normalized_variance_equal), "levene")
  expect_false(
    res_b$diagnostic_obj$normalized_variance_equal$reject_equal_variance)

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
