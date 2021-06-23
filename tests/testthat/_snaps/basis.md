# normal basis values produce expected diagnostic failures

    Code
      res <- basis_normal(x = x, batch = batch)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch
      `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers` failed: Maximum normed residual test detected outliers within data
      `anderson_darling_normal` failed: Anderson-Darling test rejects hypothesis that data is drawn from a normal distribution

---

    Code
      res <- basis_normal(x = x)
    Message <message>
      `outliers_within_batch` not run because parameter `batch` not specified
      `between_batch_variability` not run because parameter `batch` not specified
    Warning <warning>
      `outliers` failed: Maximum normed residual test detected outliers within data
      `anderson_darling_normal` failed: Anderson-Darling test rejects hypothesis that data is drawn from a normal distribution

# lognormal basis values produce expected diagnostic failures

    Code
      res <- basis_lognormal(x = x, batch = batch)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch
      `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers` failed: Maximum normed residual test detected outliers within data
      `anderson_darling_lognormal` failed: Anderson-Darling test rejects hypothesis that data is drawn from a log-normal distribution

---

    Code
      res <- basis_lognormal(x = x)
    Message <message>
      `outliers_within_batch` not run because parameter `batch` not specified
      `between_batch_variability` not run because parameter `batch` not specified
    Warning <warning>
      `outliers` failed: Maximum normed residual test detected outliers within data
      `anderson_darling_lognormal` failed: Anderson-Darling test rejects hypothesis that data is drawn from a log-normal distribution

# weibull basis values produce expected diagnostic failures

    Code
      res <- basis_weibull(x = x, batch = batch)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch
      `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers` failed: Maximum normed residual test detected outliers within data
      `anderson_darling_weibull` failed: Anderson-Darling test rejects hypothesis that data is drawn from a Weibull distribution

---

    Code
      res <- basis_weibull(x = x)
    Message <message>
      `outliers_within_batch` not run because parameter `batch` not specified
      `between_batch_variability` not run because parameter `batch` not specified
    Warning <warning>
      `outliers` failed: Maximum normed residual test detected outliers within data
      `anderson_darling_weibull` failed: Anderson-Darling test rejects hypothesis that data is drawn from a Weibull distribution

# non-para (small) basis values produce expected diag failures

    Code
      res <- basis_hk_ext(x = x_large, batch = batch_large, method = "woodward-frawley")
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch
      `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers` failed: Maximum normed residual test detected outliers within data
      `correct_method_used` failed: For B-Basis, the optimum order method should be used
      `sample_size` failed: For B-Basis, Hanson-Koopmans should only be used for samples of 28 or fewer observations

---

    Code
      res <- basis_hk_ext(x = x_large, batch = batch_large, method = "optimum-order",
        p = 0.99, conf = 0.95)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch
      `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers` failed: Maximum normed residual test detected outliers within data
      `correct_method_used` failed: For A-Basis, the Woodward-Frawley method should be used
      `sample_size` failed: For A-Basis, Hanson-Koopmans should only be used for samples of 299 or fewer observations

---

    Code
      res <- basis_hk_ext(x = x_small, method = "optimum-order")
    Message <message>
      `outliers_within_batch` not run because parameter `batch` not specified
      `between_batch_variability` not run because parameter `batch` not specified
    Warning <warning>
      `outliers` failed: Maximum normed residual test detected outliers within data

# non-para (large) basis values produce expected diag failures

    Code
      res <- basis_nonpara_large_sample(x = x_large, batch = batch_large)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch
      `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers` failed: Maximum normed residual test detected outliers within data

---

    Code
      res <- basis_nonpara_large_sample(x = x_large, batch = batch_large, p = 0.99,
        conf = 0.95)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch
      `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers` failed: Maximum normed residual test detected outliers within data

---

    Code
      res <- basis_nonpara_large_sample(x = x_large)
    Message <message>
      `outliers_within_batch` not run because parameter `batch` not specified
      `between_batch_variability` not run because parameter `batch` not specified
    Warning <warning>
      `outliers` failed: Maximum normed residual test detected outliers within data

# expected diagnostic failures are noted for pooling methods

    Code
      res <- basis_pooled_sd(cmh_17_8_3_11_1_1, strength, condition, batch)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers_within_group` failed: Maximum normed residual test detected outliers within one or more group
      `pooled_data_normal` failed: Anderson-Darling test rejects hypothesis that pooled data is drawn from a normal distribution
      `pooled_variance_equal` failed: Levene's test rejected the hypothesis that the variance of all conditions are equal

---

    Code
      res <- cmh_17_8_3_11_1_1 %>% filter(condition != "ETW2") %>% basis_pooled_sd(
        strength, condition, batch)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `outliers_within_group` failed: Maximum normed residual test detected outliers within one or more group
      `pooled_data_normal` failed: Anderson-Darling test rejects hypothesis that pooled data is drawn from a normal distribution
      `pooled_variance_equal` failed: Levene's test rejected the hypothesis that the variance of all conditions are equal

---

    Code
      res <- basis_pooled_cv(cmh_17_8_3_11_1_1, strength, condition, batch)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions
      `outliers_within_group` failed: Maximum normed residual test detected outliers within one or more group
      `pooled_data_normal` failed: Anderson-Darling test rejects hypothesis that pooled data is drawn from a normal distribution
      `normalized_variance_equal` failed: Levene's test rejected the hypothesis that the variance of all groups are equal

---

    Code
      res <- cmh_17_8_3_11_1_1 %>% filter(condition != "ETW2") %>% basis_pooled_cv(
        strength, condition, batch)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `outliers_within_group` failed: Maximum normed residual test detected outliers within one or more group
      `pooled_data_normal` failed: Anderson-Darling test rejects hypothesis that pooled data is drawn from a normal distribution
      `normalized_variance_equal` failed: Levene's test rejected the hypothesis that the variance of all groups are equal

# Pooled SD results match ASAP results

    Code
      res_b <- basis_pooled_sd(poolable_data, strength, condition, override = c(
        "pooled_variance_equal"))
    Message <message>
      `outliers_within_batch` not run because parameter `batch` not specified
      `between_group_variability` not run because parameter `batch` not specified
    Warning <warning>
      `pooled_data_normal` failed: Anderson-Darling test rejects hypothesis that pooled data is drawn from a normal distribution

# Pooled CV results match CMH17STATS

    Code
      res_b <- basis_pooled_cv(poolable_data, strength, condition)
    Message <message>
      `outliers_within_batch` not run because parameter `batch` not specified
      `between_group_variability` not run because parameter `batch` not specified
    Warning <warning>
      `pooled_data_normal` failed: Anderson-Darling test rejects hypothesis that pooled data is drawn from a normal distribution

# anova basis values produce expected diagnostic failures

    Code
      res <- basis_anova(x = x, group = batch)
    Warning <warning>
      `outliers_within_group` failed: Maximum normed residual test detected outliers within one or more batch
      `equality_of_variance` failed: Levene's test rejected the hypothesis that the variance of all groups is equal
      `number_of_groups` failed: ANOVA should only be used for 5 or more groups

