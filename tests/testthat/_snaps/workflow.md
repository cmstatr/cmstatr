# MNR within each batch and condition matches CMH17-STATS

    Code
      res_unmodified <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

---

    Code
      res_unmodified <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

# ADK for between-batch var. within each cond matches CMH17-STATS

    Code
      res_unmodified <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

---

    Code
      res_unmodified <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

# MNR within each condition matches CMH17-STATS

    Code
      res_unmodified <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

---

    Code
      res_unmodified <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

# OSL of pooled data after norm to group mean matches CMH17-STATS

    Code
      res_unmodified <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

---

    Code
      res_unmodified <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

# Levene's test among condition groups matches CMH17-STATS

    Code
      res_unmodified <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

---

    Code
      res_unmodified <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res_modcv <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

# Basis values match CMH17-STATS

    Code
      res <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res <- test_dat %>% basis_pooled_cv(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

---

    Code
      res <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = FALSE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group
      `between_group_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions

---

    Code
      res <- test_dat %>% basis_pooled_sd(strength, cond, batch, modcv = TRUE)
    Warning <warning>
      `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch and group

