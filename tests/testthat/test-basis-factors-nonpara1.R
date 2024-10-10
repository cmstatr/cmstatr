suppressMessages(library(dplyr))

cmh_17_1g_8_5_13 <- tribble(
  ~n, ~ra,
  299, 1,
  473, 2,
  628, 3,
  773, 4,
  913, 5,
  1049, 6,
  1182, 7,
  1312, 8,
  1441, 9,
  1568, 10,
  1693, 11,
  1818, 12,
  1941, 13,
  2064, 14,
  2185, 15,
  2306, 16,
  2426, 17,
  2546, 18,
  2665, 19,
  2784, 20,
  2902, 21,
  3020, 22,
  3137, 23,
  3254, 24,
  3371, 25,
  3487, 26,
  3603, 27,
  3719, 28,
  3834, 29,
  3949, 30,
  4064, 31,
  4179, 32,
  4293, 33,
  4407, 34,
  4521, 35,
  4635, 36,
  4749, 37,
  4862, 38,
  4975, 39,
  5088, 40,
  5201, 41,
  5314, 42,
  5427, 43,
  5539, 44,
  5651, 45,
  5764, 46,
  5876, 47,
  5988, 48,
  6099, 49,
  6211, 50,
  6323, 51,
  6434, 52,
  6545, 53,
  6657, 54,
  6769, 55,
  6879, 56,
  6990, 57,
  7100, 58,
  7211, 59,
  7322, 60,
  7432, 61,
  7543, 62,
  7653, 63,
  7763, 64,
  7874, 65,
  7984, 66,
  8094, 67,
  8204, 68,
  8314, 69,
  8423, 70,
  8533, 71,
  8643, 72,
  8753, 73,
  8862, 74,
  8972, 75,
  9081, 76,
  9190, 77,
  9300, 78,
  9409, 79,
  9518, 80,
  9627, 81,
  9736, 82,
  9854, 83,
  9954, 84,
  10063, 85,
  10172, 86,
  10281, 87,
  10390, 88,
  10498, 89,
  10607, 90,
  10716, 91,
  10824, 92,
  10933, 93,
  11041, 94,
  11150, 95,
  11258, 96,
  11366, 97,
  11475, 98,
  11583, 99,
  11691, 100
)

test_that("Non-parametric ranks for A-Basis match CMH-17-1G Table 8.5.13", {
  skip_on_cran()  # this test is a long-running test

  cmh_17_1g_8_5_13 %>%
    mutate(ra_lag = lag(ra)) %>%
    rowwise() %>%
    mutate(r_calc = nonpara_binomial_rank(n, 0.99, 0.95)) %>%
    mutate(expect_equal(ra, r_calc,
                        label = paste0(
                          "Mismatch in r for n=", n,
                          ". rA=", ra,
                          ", r_calc=", r_calc
                        ))) %>%
    filter(n > 299 & n < 6500) %>%
    # the rank for one sample larger should be the same
    mutate(r_calc_plus = nonpara_binomial_rank(n + 1, 0.99, 0.95)) %>%
    mutate(expect_equal(ra, r_calc_plus,
                        label = paste0(
                          "Mismatch in r for n=", n + 1,
                          ". rA=", ra, ", ",
                          "r_calc=", r_calc_plus
                        ))) %>%
    # the rank for one sample smaller should be the previous one
    mutate(r_calc_minus = nonpara_binomial_rank(n - 1, 0.99, 0.95)) %>%
    mutate(expect_equal(ra_lag, r_calc_minus,
                        label = paste0(
                          "Mismatch in r for n=", n - 1,
                          ". rA=", ra_lag, ", ",
                          "r_calc=", r_calc_minus
                        )))
})

test_that("nonpara_binomial_rank raises and error when sample too small", {
  expect_error(nonpara_binomial_rank(298, 0.99, 0.95),
               "p.*0\\.99.*conf.*0\\.95")
})

test_that("nonpara_binomial_rank raises an error when it can't converge", {
  expect_error(nonpara_binomial_rank(4000, 0.00001, 0.01),
               "p.*1e-05.*conf.*0\\.01")
})
