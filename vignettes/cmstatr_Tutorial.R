## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
library(cmstatr)
library(tidyverse)

## ------------------------------------------------------------------------
library(cmstatr)
library(tidyverse)

carbon.fabric.2 %>%
  head(10)

## ------------------------------------------------------------------------
norm_data <- carbon.fabric.2 %>%
  filter(test == "WT" | test == "FC") %>%
  mutate(strength.norm = normalize_ply_thickness(strength, thickness / nplies, 0.0079))

norm_data %>%
  head(10)

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "WT" & condition == "RTD") %>%
  anderson_darling_normal(strength.norm)

## ----include=FALSE-------------------------------------------------------
# Verify that the AD test always provides the same conclusion
# If this assertion fails, the Vignette needs to be re-written
if (0.05 >= (norm_data %>%
  filter(test == "WT" & condition == "RTD") %>%
  anderson_darling_normal(strength.norm))$osl) {
  stop("Unexpected vale for Anderson-Darling test")
  }

## ------------------------------------------------------------------------
carbon.fabric.2 %>%
  filter(test == "WT" & condition == "RTD") %>%
  basis_normal(strength)

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  group_by(condition, batch) %>%
  summarise(
    n_outliers = maximum_normed_residual(x = strength.norm)$n_outliers
    )

## ----include=FALSE-------------------------------------------------------
if ((norm_data %>%
  filter(test == "FC") %>%
  group_by(condition, batch) %>%
  summarise(
    n_outliers = maximum_normed_residual(x = strength.norm)$n_outliers
    ) %>%
  ungroup() %>%
  summarise(n_outliers = sum(n_outliers)))[[1]] != 0) {
  stop("Unexpected number of outliers")
  }

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  group_by(condition) %>%
  summarise(different_dist =
           ad_ksample(x = strength.norm, groups = batch)$reject_same_dist
  )

## ----include=FALSE-------------------------------------------------------
if (!all(!(norm_data %>%
  filter(test == "FC") %>%
  group_by(condition) %>%
  summarise(different_dist =
           ad_ksample(x = strength.norm, groups = batch)$reject_same_dist
  ))$different_dist)) {
  stop("Unexpected ADK result")
  }

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  group_by(condition) %>%
  summarise(n_outliers = maximum_normed_residual(x = strength.norm)$n_outliers)

## ----include=FALSE-------------------------------------------------------
if ((norm_data %>%
  filter(test == "FC") %>%
  group_by(condition) %>%
  summarise(
    n_outliers = maximum_normed_residual(x = strength.norm)$n_outliers
    ) %>%
  ungroup() %>%
  summarise(n_outliers = sum(n_outliers)))[[1]] != 0) {
  stop("Unexpected number of outliers")
  }

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  levene_test(strength.norm, condition)

## ----include=FALSE-------------------------------------------------------
if (!(norm_data %>%
  filter(test == "FC") %>%
  levene_test(strength.norm, condition))$reject_equal_variance) {
  stop("Unexpected result from Levene's test")
  }

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  mutate(
    strength_norm_group = normalize_group_mean(strength.norm, condition)) %>%
  levene_test(strength_norm_group, condition)

## ----include=FALSE-------------------------------------------------------
if ((norm_data %>%
  filter(test == "FC") %>%
  mutate(
    strength_norm_group = normalize_group_mean(strength.norm, condition)) %>%
  levene_test(strength_norm_group, condition))$reject_equal_variance) {
  stop("Unexpected value from Levene's test")
  }

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  mutate(
    strength_norm_group = normalize_group_mean(strength.norm, condition)) %>%
  anderson_darling_normal(strength_norm_group)

## ----include=FALSE-------------------------------------------------------
if ((norm_data %>%
  filter(test == "FC") %>%
  mutate(
    strength_norm_group = normalize_group_mean(strength.norm, condition)) %>%
  anderson_darling_normal(strength_norm_group))$osl <= 0.05) {
  stop("Unexpected value from AD test")
  }

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  basis_pooled_sd(strength.norm, condition)

## ------------------------------------------------------------------------
carbon.fabric %>%
  filter(test == "WT" & condition == "RTD") %>%
  equiv_mean_extremum(strength, n_sample = 5, alpha = 0.01)

