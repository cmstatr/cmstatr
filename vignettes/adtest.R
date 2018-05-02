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
  AD = c(0, 10 ^ (seq(-1.1, 0.7, length.out = 100)))
) %>%
  rowwise() %>%
  mutate(
    p = ad_p(AD, n),
    OSL = ad_osl(AD, n)
  ) %>%
  gather(stat, value, p, OSL) %>%
  group_by(n, stat) %>%
  ggplot(aes(x = AD, y = value, color = stat)) +
  geom_line() +
  facet_grid(n ~ .)

