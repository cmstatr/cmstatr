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
  summarise(n_outliers = maximum_normed_residual(strength.norm)$n_outliers)

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC") %>%
  group_by(condition) %>%
  mutate(different_dist =
           ad_ksample(x = strength.norm, groups = batch)$reject_same_dist
  )

## ------------------------------------------------------------------------
norm_data %>%
  filter(test == "FC" & condition == "RTD") %>%
  ad_ksample(strength.norm, batch)

## ----include=FALSE-------------------------------------------------------
if (0.025 >= (norm_data %>%
   filter(test == "FC" & condition == "RTD") %>%
   ad_ksample(strength.norm, batch))$p ) {
  stop("Unexpected value of AD-K test")
   }

## ------------------------------------------------------------------------
# TODO: Write this!


## ------------------------------------------------------------------------
carbon.fabric %>%
  filter(test == "WT" & condition == "RTD") %>%
  equiv_mean_extremum(strength, n_sample = 5, alpha = 0.01)

