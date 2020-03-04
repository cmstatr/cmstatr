## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
library(cmstatr)
library(tidyverse)

## ------------------------------------------------------------------------
carbon.fabric.2 %>%
  head(10)

## ------------------------------------------------------------------------
norm_data <- carbon.fabric.2 %>%
  filter(test == "WT" | test == "FC") %>%
  mutate(strength.norm = normalize_ply_thickness(strength,
                                                 thickness / nplies,
                                                 0.0079))

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
norm_data %>%
  filter(test == "WT" & condition == "RTD") %>%
  basis_normal(strength.norm)

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "WT" & condition == "RTD") %>%
  basis_normal(strength.norm, 
               override = c("outliers_within_batch",
                            "between_batch_variability"))

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "WT" & condition == "RTD") %>%
  basis_normal(strength.norm, batch)

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "WT" & condition == "RTD") %>%
  ad_ksample(strength.norm, batch)

## ------------------------------------------------------------------------
StatNcdf <- ggproto(
  "stat_ncdf",
  Stat,
  compute_group = function(data, scales, n) {
    x <- data$x
    x <- seq(from = min(x), to = max(x), length.out = n)
    y = pnorm(x, mean(x), sd(x), lower.tail = TRUE)
    data.frame(x = x, y = y)
  },
  required_aes = c("x")
  )

stat_ncdf <- function(mapping = NULL, data = NULL, geom = "smooth",
                      position = "identity", show.legend = NA,
                      inherit.aes = TRUE, n = 100, ...) {
    layer(
      stat = StatNcdf, data = data, geom = geom, position = position,
      mapping = mapping, show.legend = show.legend,
      inherit.aes = inherit.aes, params = list(n = n, ...)
    )
}

norm_data %>%
  filter(test == "WT" & condition == "RTD") %>%
  group_by(batch) %>%
  ggplot(aes(x = strength.norm, color = batch)) +
  stat_ecdf(geom = "point", pad = TRUE) +
  stat_ncdf() +
  ggtitle("Distribution of Data For Each Batch")

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  group_by(condition, batch) %>%
  nest() %>%
  mutate(mnr = map(data,
                   ~maximum_normed_residual(data = .x, x = strength.norm)),
         tidied = map(mnr, glance)) %>%
  select(-c(mnr, data)) %>%  # remove unneeded columns
  unnest(tidied)

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
  nest() %>%
  mutate(adk = map(data, ~ad_ksample(data = .x,
                                     x = strength.norm,
                                     groups = batch)),
         tidied = map(adk, glance)) %>%
  select(-c(data, adk)) %>%  # remove unneeded columns
  unnest(tidied)

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
  nest() %>%
  mutate(mnr = map(data, ~maximum_normed_residual(data = .x,
                                                  x = strength.norm)),
         tidied = map(mnr, glance)) %>%
  select(-c(mnr, data)) %>%  # remove unneeded columns
  unnest(tidied)

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
  basis_pooled_cv(strength.norm, condition, batch)

## ------------------------------------------------------------------------
norm_data %>%
  mutate(condition = ordered(condition,
                             c("CTD", "RTD", "ETD", "ETW", "ETW2"))) %>%
  filter(test == "FC") %>%
  basis_pooled_cv(strength.norm, condition, batch)

## ------------------------------------------------------------------------
carbon.fabric.2 %>%
  filter(test == "FC" & condition == "RTD") %>%
  equiv_mean_extremum(strength, n_sample = 5, alpha = 0.01)

