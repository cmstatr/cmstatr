## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(cmstatr)

carbon.fabric

## ------------------------------------------------------------------------
library(tidyverse)

## ------------------------------------------------------------------------
carbon.fabric %>%
  filter(test == "WT" & condition == "RTD") %>%
  basis_normal(strength)

## ------------------------------------------------------------------------
carbon.fabric %>%
  filter(test == "WT" & condition == "RTD") %>%
  equiv_mean_extremum(strength, n_sample = 5, alpha = 0.01)

