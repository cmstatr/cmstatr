## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(tidyverse)
library(cmstatr)

## ------------------------------------------------------------------------
expand.grid(
  n = c(5, 100),
  AD = c(0, 10 ^ (seq(-1.1, 0.5, length.out = 100)))
) %>%
  rowwise() %>%
  mutate(
    p_known_param = ad_p_known_param(AD, n),
    p_unknown_param = ad_p_unknown_param(AD, n)
  ) %>%
  gather(stat, value, p_known_param, p_unknown_param) %>%
  group_by(n, stat) %>%
  ggplot(aes(x = AD, y = value, color = stat)) +
  geom_line() +
  facet_grid(n ~ .)

