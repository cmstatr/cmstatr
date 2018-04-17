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

