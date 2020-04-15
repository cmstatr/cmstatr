context("Workflow")

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(purrr))

# In this test case, the "steps" refer to the flowchart in CMH-17-1G
# Figure 8.3.1.1(a)

test_that("carbon.fabric.2 MNR test matches CMH17-STATS (step 7)", {
  # Test for outliers within each batch and condition
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    group_by(batch, condition) %>%
    nest() %>%
    mutate(mnr = map(data, ~maximum_normed_residual(.x, strength)),
           tidied = map(mnr, glance)) %>%
    select(-c(mnr, data)) %>%
    unnest(tidied) %>%
    ungroup()

  res %>%
    group_by(condition) %>%
    summarize(n_outliers = sum(n_outliers)) %>%
    mutate(expected_outliers = case_when(condition == "ETW2" ~ 1,
                                         TRUE ~ 0)) %>%
    rowwise() %>%
    mutate(expect_equal(n_outliers, expected_outliers))
})

test_that("carbon.fabric.2 ADK matches CMH17-STATS (step 10)", {
  # Test for between-batch variability within each condition
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    group_by(condition) %>%
    nest() %>%
    mutate(adk = map(data, ~ad_ksample(.x, strength, batch)),
           tidied = map(adk, glance)) %>%
    select(-c(adk, data)) %>%
    unnest(tidied) %>%
    ungroup()

  res %>%
    mutate(adk_expected = case_when(condition == "CTD" ~ 0.351,
                                    condition == "RTD" ~ 0.448,
                                    condition == "ETD" ~ 0.453,
                                    condition == "ETW" ~ 1.627,
                                    condition == "ETW2" ~ 0.784,
                                    TRUE ~ 0)) %>%
    # There is a difference between the way that the test statisic is defined
    # in cmstatr and CMH17-STATS and a corresponding difference in the
    # critical value. See documentation for ad_ksample()
    mutate(adk_expected = adk_expected * (k - 1)) %>%
    rowwise() %>%
    mutate(expect_equal(ad, adk_expected, tolerance = 0.005),
           expect_false(reject_same_dist))
})

test_that("carbon.fabric.2 MNR test matches CMH17-STATS (step 18)", {
  # Test for outliers within each condition
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    group_by(condition) %>%
    nest() %>%
    mutate(mnr = map(data, ~maximum_normed_residual(.x, strength)),
           tidied = map(mnr, glance)) %>%
    select(-c(mnr, data)) %>%
    unnest(tidied) %>%
    ungroup()

  res %>%
    rowwise() %>%
    mutate(expect_equal(n_outliers, 0))
})

test_that("carbon.fabric.2 Normality of pooled norm data matches (step 23)", {
  # Check normality of pooled set of normalized data using Anderson Darling
  # test
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    mutate(norm_strength = normalize_group_mean(strength, condition)) %>%
    anderson_darling_normal(norm_strength)

  expect_equal(res$osl, 0.5765, tolerance = 1e-4)
})

test_that("carbon.fabric.2 equality of variances matches (step 28)", {
  # Test for equality of variances among condition groups
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    levene_test(strength, condition)

  expect_equal(res$f, 3.884, tolerance = 0.001)
})

test_that("carbon.fabric.2 pooled sd basis values match (step 30)", {
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    basis_pooled_sd(strength, condition,
                    override = c("pooled_variance_equal"))

  res$basis %>%
    mutate(expected = case_when(group == "CTD" ~ 89.22,
                                group == "RTD" ~ 80.06,
                                group == "ETW" ~ 48.53,
                                group == "ETW2" ~ 42.27,
                                group == "ETD" ~ 66.56,
                                TRUE ~ 0)) %>%
    mutate(expect_equal(value, expected, tolerance = 0.005))
})

test_that("carbon.fabric.2 equality of normalized variance (step 31)", {
  # Test for equality of normalized variacnes among condition groups
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    mutate(norm_strength = normalize_group_mean(strength, condition)) %>%
    levene_test(norm_strength, condition)

  expect_equal(res$f, 1.065, tolerance = 0.0005)
})

test_that("carbon.fabric.2 pooled CV basis values match (step 39-41)", {
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    basis_pooled_cv(strength, condition, batch,
                    override = c("outliers_within_batch"))

  res$basis %>%
    mutate(expected = case_when(group == "CTD" ~ 86.91,
                                group == "RTD" ~ 78.83,
                                group == "ETW" ~ 51.04,
                                group == "ETW2" ~ 45.51,
                                group == "ETD" ~ 66.93,
                                TRUE ~ 0)) %>%
    mutate(expect_equal(value, expected, tolerance = 0.005))
})

test_that("carbon.fabric.2 ADK matches CMH17-STATS - modCV (step 10)", {
  # Test for between-batch variability within each condition (modified CV)
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    mutate(trans_strength = transform_mod_cv_ad(strength, condition, batch)) %>%
    group_by(condition) %>%
    nest() %>%
    mutate(adk = map(data, ~ad_ksample(.x, trans_strength, batch)),
           tidied = map(adk, glance)) %>%
    select(-c(adk, data)) %>%
    unnest(tidied) %>%
    ungroup()

  res %>%
    mutate(adk_expected = case_when(condition == "CTD" ~ 0.351,
                                    condition == "RTD" ~ 0.401,
                                    condition == "ETD" ~ 0.495,
                                    condition == "ETW" ~ 1.051,
                                    condition == "ETW2" ~ 0.670,
                                    TRUE ~ 0)) %>%
    # There is a difference between the way that the test statisic is defined
    # in cmstatr and CMH17-STATS and a corresponding difference in the
    # critical value. See documentation for ad_ksample()
    mutate(adk_expected = adk_expected * (k - 1)) %>%
    rowwise() %>%
    mutate(expect_equal(ad, adk_expected, tolerance = 0.005),
           expect_false(reject_same_dist))
})

test_that("carbon.fabric.2 Normality of pooled norm data - modCV (step 23)", {
  # Check normality of pooled set of normalized data using Anderson Darling
  # test (modified CV)
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    mutate(trans_strength = transform_mod_cv_ad(strength, condition, batch)) %>%
    mutate(norm_strength = normalize_group_mean(trans_strength, condition)) %>%
    anderson_darling_normal(norm_strength)

  expect_equal(res$osl, 0.6036, tolerance = 1e-4)
})

test_that("carbon.fabric.2 equality of variances - modCV (step 28)", {
  # Test for equality of variances among condition groups
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    group_by(condition) %>%
    mutate(trans_strength = transform_mod_cv(strength)) %>%
    ungroup() %>%
    levene_test(trans_strength, condition)

  expect_equal(res$f, 2.862, tolerance = 0.075)

  # Test again, using the modcv argument
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    levene_test(strength, condition, modcv = TRUE)

  expect_equal(res$f, 2.862, tolerance = 0.075)
})

test_that("carbon.fabric.2 pooled sd basis values - modCV (step 30)", {
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    group_by(condition) %>%
    mutate(trans_strength = transform_mod_cv(strength)) %>%
    ungroup() %>%
    basis_pooled_sd(trans_strength, condition,
                    override = c("pooled_variance_equal"))

  res$basis %>%
    mutate(expected = case_when(group == "CTD" ~ 88.64,
                                group == "RTD" ~ 79.48,
                                group == "ETW" ~ 47.95,
                                group == "ETW2" ~ 41.68,
                                group == "ETD" ~ 65.97,
                                TRUE ~ 0)) %>%
    mutate(expect_equal(value, expected, tolerance = 0.005))

  # Test again, using the modcv argument
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    basis_pooled_sd(strength, condition, modcv = TRUE,
                    override = c("pooled_variance_equal"))

  res$basis %>%
    mutate(expected = case_when(group == "CTD" ~ 88.64,
                                group == "RTD" ~ 79.48,
                                group == "ETW" ~ 47.95,
                                group == "ETW2" ~ 41.68,
                                group == "ETD" ~ 65.97,
                                TRUE ~ 0)) %>%
    mutate(expect_equal(value, expected, tolerance = 0.005))
})

test_that("carbon.fabric.2 equality of norm variance - modCV (step 31)", {
  # Test for equality of normalized variacnes among condition groups
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    group_by(condition) %>%
    mutate(trans_strength = transform_mod_cv(strength)) %>%
    ungroup() %>%
    mutate(norm_strength = normalize_group_mean(trans_strength, condition)) %>%
    levene_test(norm_strength, condition)

  expect_equal(res$f, 0.226, tolerance = 0.05)

  # Test again, using the modcv argument
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    mutate(norm_strength = normalize_group_mean(strength, condition)) %>%
    levene_test(norm_strength, condition, modcv = TRUE)

  expect_equal(res$f, 0.226, tolerance = 0.05)
})

test_that("carbon.fabric.2 pooled CV basis values - modCV (step 39-41)", {
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    group_by(condition) %>%
    mutate(trans_strength = transform_mod_cv(strength)) %>%
    ungroup() %>%
    basis_pooled_cv(trans_strength, condition)

  res$basis %>%
    mutate(expected = case_when(group == "CTD" ~ 85.98,
                                group == "RTD" ~ 77.99,
                                group == "ETW" ~ 50.50,
                                group == "ETW2" ~ 45.02,
                                group == "ETD" ~ 66.22,
                                TRUE ~ 0)) %>%
    mutate(expect_equal(value, expected, tolerance = 0.005))

  # Test again, using the modcv argument
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    basis_pooled_cv(strength, condition, modcv = TRUE)

  res$basis %>%
    mutate(expected = case_when(group == "CTD" ~ 85.98,
                                group == "RTD" ~ 77.99,
                                group == "ETW" ~ 50.50,
                                group == "ETW2" ~ 45.02,
                                group == "ETD" ~ 66.22,
                                TRUE ~ 0)) %>%
    mutate(expect_equal(value, expected, tolerance = 0.005))
})

test_dat <- tribble(
  ~cond, ~batch, ~strength,
  1, "A", 97.89583,
  1, "A", 109.76291,
  1, "A", 107.66701,
  1, "A", 112.69444,
  1, "A", 99.71637,
  1, "A", 108.68871,
  1, "B", 106.37708,
  1, "B", 110.96946,
  1, "B", 102.78598,
  1, "D", 100.83316,
  1, "D", 105.66699,
  1, "D", 103.14165,
  1, "D", 105.36496,
  1, "D", 101.76552,
  1, "D", 103.81940,
  1, "E", 105.59004,
  1, "E", 107.74373,
  1, "E", 108.88401,
  1, "E", 103.72913,
  1, "E", 107.07630,
  1, "E", 104.94092,
  1, "C", 103.99478,
  1, "C", 104.57191,
  1, "C", 104.65751,
  1, "C", 106.26021,
  1, "C", 108.80221,
  1, "C", 104.45781,
  1, "C", 105.71117,
  1, "C", 104.68779,
  1, "C", 96.90000,
  2, "A", 108.89755,
  2, "A", 112.26424,
  2, "A", 114.14414,
  2, "A", 109.56092,
  2, "A", 112.81857,
  2, "A", 110.04231,
  2, "B", 105.92060,
  2, "B", 104.76159,
  2, "B", 105.39842,
  2, "D", 97.83402,
  2, "D", 103.60583,
  2, "D", 101.81152,
  2, "D", 100.55827,
  2, "D", 104.10650,
  2, "D", 103.56471,
  2, "E", 105.18761,
  2, "E", 103.81970,
  2, "E", 104.31939,
  2, "E", 105.21502,
  2, "E", 109.61211,
  2, "E", 108.43762,
  2, "C", 104.08681,
  2, "C", 109.60930,
  2, "C", 107.17149,
  2, "C", 107.10460,
  2, "C", 102.88207,
  2, "C", 106.91041,
  2, "C", 104.40203,
  2, "C", 105.43848,
  2, "C", 109.50752,
  3, "B", 93.22953,
  3, "B", 97.09608,
  3, "B", 97.05682,
  3, "A", 102.57570,
  3, "A", 98.16441,
  3, "A", 97.67216,
  3, "A", 103.10081,
  3, "A", 106.05421,
  3, "A", 100.43507,
  3, "B", 101.64746,
  3, "B", 103.24892,
  3, "B", 101.74833,
  3, "D", 93.48767,
  3, "D", 89.77051,
  3, "D", 95.24123,
  3, "D", 96.81138,
  3, "D", 86.88577,
  3, "D", 89.46870,
  3, "E", 96.80020,
  3, "E", 97.87896,
  3, "E", 97.91697,
  3, "E", 98.73682,
  3, "E", 96.20074,
  3, "E", 99.22644,
  3, "C", 102.06662,
  3, "C", 102.04738,
  3, "C", 96.40601,
  3, "C", 99.34420,
  3, "C", 99.20519,
  3, "C", 101.22471,
  3, "C", 99.60168,
  3, "C", 106.73832,
  3, "C", 110.09276
)

test_that("MNR within each batch and condition matches CMH17-STATS", {
  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = FALSE)
  })

  expect_true("outliers_within_batch" %in%
                res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = TRUE)
  })

  expect_true("outliers_within_batch" %in%
                res_modcv$diagnostic_failures)

  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = FALSE)
  })

  expect_true("outliers_within_batch" %in%
                res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = TRUE)
  })

  expect_true("outliers_within_batch" %in%
                res_modcv$diagnostic_failures)

  res_mnr <- test_dat %>%
    group_by(cond, batch) %>%
    nest() %>%
    mutate(mnr = map(data, ~maximum_normed_residual(.x, strength)),
           tidied = map(mnr, glance)) %>%
    select(-c(data, mnr)) %>%
    unnest(cols = c(tidied))

  res_mnr_outlier_batch <- res_mnr %>%
    filter(cond == 1 & batch == "C")
  expect_true(res_mnr_outlier_batch$n_outliers > 0)

  res_mnr_no_outlier_batch <- res_mnr %>%
    filter(!(cond == 1 & batch == "C"))
  expect_equal(sum(res_mnr_no_outlier_batch$n_outliers > 0), 0)
})

test_that("ADK for between-batch var. within each cond matches CMH17-STATS", {
  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = FALSE)
  })

  expect_true("between_group_variability" %in%
                res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = TRUE)
  })

  expect_false("between_group_variability" %in%
                 res_modcv$diagnostic_failures)

  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = FALSE)
  })

  expect_true("between_group_variability" %in%
                res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = TRUE)
  })

  expect_false("between_group_variability" %in%
                 res_modcv$diagnostic_failures)

  test_dat %>%
    group_by(cond) %>%
    nest() %>%
    mutate(adk = map(data, ~ad_ksample(.x, strength, batch)),
           tidied = map(adk, glance)) %>%
    select(-c(data, adk)) %>%
    unnest(cols = c(tidied)) %>%
    mutate(expected = case_when(cond == 1 ~ 1.243,
                                cond == 2 ~ 3.376,
                                cond == 3 ~ 2.793)) %>%
    mutate(expect_equal(ad / (k - 1), expected, tolerance = 7e-3))

  test_dat %>%
    mutate(strength = transform_mod_cv_ad(strength, cond, batch)) %>%
    group_by(cond) %>%
    nest() %>%
    mutate(adk = map(data, ~ad_ksample(.x, strength, batch)),
           tidied = map(adk, glance)) %>%
    select(-c(data, adk)) %>%
    unnest(cols = c(tidied)) %>%
    mutate(expected = case_when(cond == 1 ~ 0.504,
                                cond == 2 ~ 1.106,
                                cond == 3 ~ 1.508)) %>%
    mutate(expect_equal(ad / (k - 1), expected, tolerance = 2e-3))
})

test_that("MNR within each condition matches CMH17-STATS", {
  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = FALSE)
  })

  expect_false("outliers_within_group" %in%
                res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = TRUE)
  })

  expect_false("outliers_within_group" %in%
                res_modcv$diagnostic_failures)

  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = FALSE)
  })

  expect_false("outliers_within_group" %in%
                res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = TRUE)
  })

  expect_false("outliers_within_group" %in%
                res_modcv$diagnostic_failures)

  res_mnr <- test_dat %>%
    group_by(cond) %>%
    nest() %>%
    mutate(mnr = map(data, ~maximum_normed_residual(.x, strength)),
           tidied = map(mnr, glance)) %>%
    select(-c(data, mnr)) %>%
    unnest(cols = c(tidied))

  expect_equal(sum(res_mnr$n_outliers > 0), 0)
})

test_that("OSL of data from each batch CMH17-STATS", {
  test_dat %>%
    group_by(cond) %>%
    nest() %>%
    mutate(osl = map(data, ~anderson_darling_normal(.x, strength)),
           tidied = map(osl, glance)) %>%
    select(-c(data, osl)) %>%
    unnest(cols = c(tidied)) %>%
    mutate(expected = case_when(cond == 1 ~ 0.485,
                                cond == 2 ~ 0.306,
                                cond == 3 ~ 0.287)) %>%
    mutate(expect_equal(osl, expected, tolerance = 1e-3))

  test_dat %>%
    mutate(strength = transform_mod_cv_ad(strength, cond, batch)) %>%
    group_by(cond) %>%
    nest() %>%
    mutate(osl = map(data, ~anderson_darling_normal(.x, strength)),
           tidied = map(osl, glance)) %>%
    select(-c(data, osl)) %>%
    unnest(cols = c(tidied)) %>%
    mutate(expected = case_when(cond == 1 ~ 0.494,
                                cond == 2 ~ 0.467,
                                cond == 3 ~ 0.718)) %>%
    mutate(expect_equal(osl, expected, tolerance = 1e-3))
})

test_that("OSL of pooled data after norm to group mean matches CMH17-STATS", {
  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = FALSE)
  })

  expect_false("pooled_data_normal" %in%
                 res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = TRUE)
  })

  expect_false("pooled_data_normal" %in%
                 res_modcv$diagnostic_failures)

  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = FALSE)
  })

  expect_false("pooled_data_normal" %in%
                 res_unmodified$diagnostic_failures)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = TRUE)
  })

  expect_false("pooled_data_normal" %in%
                 res_modcv$diagnostic_failures)

  res <- test_dat %>%
    mutate(normalized_strength = normalize_group_mean(strength, cond)) %>%
    anderson_darling_normal(normalized_strength)
  expect_equal(res$osl, 0.0798, tolerance = 1e-3)

  res <- test_dat %>%
    mutate(strength = transform_mod_cv_ad(strength, cond, batch)) %>%
    mutate(normalized_strength = normalize_group_mean(strength, cond)) %>%
    anderson_darling_normal(normalized_strength)
  expect_equal(res$osl, 0.7045, tolerance = 1e-4)
})

test_that("Levene's test among condition groups matches CMH17-STATS", {
  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = FALSE)
  })

  expect_false("pooled_variance_equal" %in%
                 res_unmodified$diagnostic_failures)

  lev <- test_dat %>%
    levene_test(strength, groups = cond)
  expect_equal(lev$f, 1.051, tolerance = 1e-3)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = TRUE)
  })

  expect_false("pooled_variance_equal" %in%
                 res_modcv$diagnostic_failures)

  lev <- test_dat %>%
    group_by(cond) %>%
    mutate(strength = transform_mod_cv(strength)) %>%
    ungroup() %>%
    levene_test(strength, groups = cond)
  expect_equal(lev$f, 0.005, tolerance = 0.05)

  expect_warning({
    res_unmodified <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = FALSE)
  })

  lev <- test_dat %>%
    mutate(strength = normalize_group_mean(strength, cond)) %>%
    levene_test(strength, groups = cond)
  expect_equal(lev$f, 1.631, tolerance = 0.05)

  expect_warning({
    res_modcv <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = TRUE)
  })

  lev <- test_dat %>%
    group_by(cond) %>%
    mutate(strength = transform_mod_cv(strength)) %>%
    ungroup() %>%
    mutate(strength = normalize_group_mean(strength, cond)) %>%
    levene_test(strength, groups = cond)
  expect_equal(lev$f, 0.064, tolerance = 0.05)
})

test_that("Basis values match CMH17-STATS", {
  expect_warning({
    res <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = FALSE)
  })
  res$basis %>%
    mutate(expected = case_when(group == 1 ~ 98.21,
                                group == 2 ~ 99.27,
                                group == 3 ~ 92.22)) %>%
    mutate(expect_equal(value, expected, tolerance = 1e-2))

  expect_warning({
    res <- test_dat %>%
      basis_pooled_cv(strength, cond, batch, modcv = TRUE)
  })
  res$basis %>%
    mutate(expected = case_when(group == 1 ~ 94.49,
                                group == 2 ~ 95.51,
                                group == 3 ~ 88.76)) %>%
    mutate(expect_equal(value, expected, tolerance = 1e-2))

  expect_warning({
    res <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = FALSE)
  })
  res$basis %>%
    mutate(expected = case_when(group == 1 ~ 98.42,
                                group == 2 ~ 99.55,
                                group == 3 ~ 92.00)) %>%
    mutate(expect_equal(value, expected, tolerance = 1e-2))

  expect_warning({
    res <- test_dat %>%
      basis_pooled_sd(strength, cond, batch, modcv = TRUE)
  })
  res$basis %>%
    mutate(expected = case_when(group == 1 ~ 94.71,
                                group == 2 ~ 95.84,
                                group == 3 ~ 88.32)) %>%
    mutate(expect_equal(value, expected, tolerance = 1e-2))
})
