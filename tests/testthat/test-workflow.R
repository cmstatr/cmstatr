context("Workflow")

library(tidyverse)

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
    basis_pooled_sd(strength, condition)

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
    basis_pooled_cv(strength, condition)

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
    mutate(trans_strength = transform_mod_cv_2(strength, condition, batch)) %>%
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
    mutate(trans_strength = transform_mod_cv_2(strength, condition, batch)) %>%
    mutate(norm_strength = normalize_group_mean(trans_strength, condition)) %>%
    anderson_darling_normal(norm_strength)

  expect_equal(res$osl, 0.6036, tolerance = 1e-4)
})

test_that("carbon.fabric.2 equality of variances - modCV (step 28)", {
  # Test for equality of variances among condition groups
  res <- carbon.fabric.2 %>%
    filter(test == "FC") %>%
    mutate(trans_strength = transform_mod_cv(strength, condition)) %>%
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
    mutate(trans_strength = transform_mod_cv(strength, condition)) %>%
    basis_pooled_sd(trans_strength, condition)

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
    basis_pooled_sd(strength, condition, modcv = TRUE)

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
    mutate(trans_strength = transform_mod_cv(strength, condition)) %>%
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
    mutate(trans_strength = transform_mod_cv(strength, condition)) %>%
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
