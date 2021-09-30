## ----setup, include = FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = ""
)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)


## ---------------------------------------------------------------------------------------------------------------------
factors <- tribble(
  ~n, ~j_pre_080, ~z_pre_080, ~j_post_080, ~z_post_080, ~j_cmh, ~z_cmh,
  2, 2, 35.1768141883907, 2, 35.1768141883907, 2, 35.177,
  3, 3, 7.85866787768029, 3, 7.85866787768029, 3, 7.859,
  4, 4, 4.50522447199018, 4, 4.50522447199018, 4, 4.505,
  5, 4, 4.10074820079326, 4, 4.10074820079326, 4, 4.101,
  6, 5, 3.06444416024793, 5, 3.06444416024793, 5, 3.064,
  7, 5, 2.85751000593839, 5, 2.85751000593839, 5, 2.858,
  8, 6, 2.38240998122575, 6, 2.38240998122575, 6, 2.382,
  9, 6, 2.25292053841772, 6, 2.25292053841772, 6, 2.253,
  10, 7, 1.98762060673102, 6, 2.13665759924781, 6, 2.137,
  11, 7, 1.89699586212496, 7, 1.89699586212496, 7, 1.897,
  12, 7, 1.81410756892749, 7, 1.81410756892749, 7, 1.814,
  13, 8, 1.66223343216608, 7, 1.73773765993598, 7, 1.738,
  14, 8, 1.59916281901889, 8, 1.59916281901889, 8, 1.599,
  15, 8, 1.54040000806181, 8, 1.54040000806181, 8, 1.54,
  16, 9, 1.44512878109546, 8, 1.48539432060546, 8, 1.485,
  17, 9, 1.39799975474842, 9, 1.39799975474842, 8, 1.434,
  18, 9, 1.35353033609361, 9, 1.35353033609361, 9, 1.354,
  19, 10, 1.28991705486727, 9, 1.31146980117942, 9, 1.311,
  20, 10, 1.25290765871981, 9, 1.27163203813793, 10, 1.253,
  21, 10, 1.21771654027026, 10, 1.21771654027026, 10, 1.218,
  22, 11, 1.17330587650406, 10, 1.18418267046374, 10, 1.184,
  23, 11, 1.14324511741536, 10, 1.15218647199938, 11, 1.143,
  24, 11, 1.11442082880151, 10, 1.12153586685854, 11, 1.114,
  25, 11, 1.08682185727661, 11, 1.08682185727661, 11, 1.087,
  26, 11, 1.06032912052507, 11, 1.06032912052507, 11, 1.06,
  27, 12, 1.03307994274081, 11, 1.03485308510789, 11, 1.035,
  28, 12, 1.00982188136729, 11, 1.01034609051393, 12, 1.01
)


## ----include=FALSE----------------------------------------------------------------------------------------------------
factors %>%
  filter(j_pre_080 == j_post_080 & j_pre_080 == j_cmh) %>%
  mutate(testthat::expect_equal(z_pre_080, z_post_080),
         testthat::expect_equal(z_post_080, z_cmh, tolerance = 1e-4))


## ----include=FALSE----------------------------------------------------------------------------------------------------
factors %>%
  rowwise() %>%
  mutate(testthat::expect_equal(z_post_080,
                                cmstatr::hk_ext_z_j_opt(n, 0.9, 0.95)$z),
         testthat::expect_equal(j_post_080,
                                cmstatr::hk_ext_z_j_opt(n, 0.9, 0.95)$j))


## ---------------------------------------------------------------------------------------------------------------------
factors %>%
  filter(j_pre_080 == j_post_080 & j_pre_080 == j_cmh)


## ---------------------------------------------------------------------------------------------------------------------
factor_diff <- factors %>%
  filter(j_pre_080 != j_post_080 | j_pre_080 != j_cmh | j_post_080 != j_cmh)
factor_diff


## ---------------------------------------------------------------------------------------------------------------------
mu_normal <- 100
sd_normal <- 6

set.seed(1234567)  # make this reproducible

tolerance_limit <- function(x, j, z) {
  x[j] * (x[1] / x[j]) ^ z
}

sim_normal <- pmap_dfr(factor_diff, function(n, j_pre_080, z_pre_080,
                                             j_post_080, z_post_080,
                                             j_cmh, z_cmh) {
  map_dfr(1:10000, function(i_sim) {
    x <- sort(rnorm(n, mu_normal, sd_normal))
    tibble(
      n = n,
      b_pre_080 = tolerance_limit(x, j_pre_080, z_pre_080),
      b_post_080 = tolerance_limit(x, j_post_080, z_post_080),
      b_cmh = tolerance_limit(x, j_cmh, z_cmh),
      x = list(x)
    )
  }
  )
})
sim_normal


## ----distribution-normal, fig.width=7, fig.height=5-------------------------------------------------------------------
sim_normal %>%
  pivot_longer(cols = b_pre_080:b_cmh, names_to = "Factors") %>%
  ggplot(aes(x = value, color = Factors)) +
  geom_density() +
  facet_wrap(n ~ .) +
  theme_bw() +
  ggtitle("Distribution of Tolerance Limits for Various Values of n")


## ---------------------------------------------------------------------------------------------------------------------
x_p_normal <- qnorm(0.9, mu_normal, sd_normal, lower.tail = FALSE)
x_p_normal


## ---------------------------------------------------------------------------------------------------------------------
sim_normal %>%
  mutate(below_pre_080 = b_pre_080 < x_p_normal,
         below_post_080 = b_post_080 < x_p_normal,
         below_cmh = b_cmh < x_p_normal) %>%
  group_by(n) %>%
  summarise(
    prop_below_pre_080 = sum(below_pre_080) / n(),
    prop_below_post_080 = sum(below_post_080) / n(),
    prop_below_cmh = sum(below_cmh) / n()
  )


## ---------------------------------------------------------------------------------------------------------------------
shape_weibull <- 60
scale_weibull <- 100

set.seed(234568)  # make this reproducible

sim_weibull <- pmap_dfr(factor_diff, function(n, j_pre_080, z_pre_080,
                                              j_post_080, z_post_080,
                                              j_cmh, z_cmh) {
  map_dfr(1:10000, function(i_sim) {
    x <- sort(rweibull(n, shape_weibull, scale_weibull))
    tibble(
      n = n,
      b_pre_080 = tolerance_limit(x, j_pre_080, z_pre_080),
      b_post_080 = tolerance_limit(x, j_post_080, z_post_080),
      b_cmh = tolerance_limit(x, j_cmh, z_cmh),
      x = list(x)
    )
  }
  )
})
sim_weibull


## ----distribution-Weibull, fig.width=7, fig.height=5------------------------------------------------------------------
sim_weibull %>%
  pivot_longer(cols = b_pre_080:b_cmh, names_to = "Factors") %>%
  ggplot(aes(x = value, color = Factors)) +
  geom_density() +
  facet_wrap(n ~ .) +
  theme_bw() +
  ggtitle("Distribution of Tolerance Limits for Various Values of n")


## ---------------------------------------------------------------------------------------------------------------------
x_p_weibull <- qweibull(0.9, shape_weibull, scale_weibull, lower.tail = FALSE)
x_p_weibull


## ---------------------------------------------------------------------------------------------------------------------
sim_weibull %>%
  mutate(below_pre_080 = b_pre_080 < x_p_weibull,
         below_post_080 = b_post_080 < x_p_weibull,
         below_cmh = b_cmh < x_p_weibull) %>%
  group_by(n) %>%
  summarise(
    prop_below_pre_080 = sum(below_pre_080) / n(),
    prop_below_post_080 = sum(below_post_080) / n(),
    prop_below_cmh = sum(below_cmh) / n()
  )


## ---------------------------------------------------------------------------------------------------------------------
sessionInfo()

