suppressMessages(library(dplyr))

cmh_17_1g_8_5_12 <- tribble(
  ~n, ~rb,
  29, 1,
  46, 2,
  61, 3,
  76, 4,
  89, 5,
  103, 6,
  116, 7,
  129, 8,
  142, 9,
  154, 10,
  167, 11,
  179, 12,
  191, 13,
  203, 14,
  215, 15,
  227, 16,
  239, 17,
  251, 18,
  263, 19,
  275, 20,
  298, 22,
  321, 24,
  345, 26,
  368, 28,
  391, 30,
  413, 32,
  436, 34,
  459, 36,
  481, 38,
  504, 40,
  526, 42,
  549, 44,
  571, 46,
  593, 48,
  615, 50,
  638, 52,
  660, 54,
  682, 56,
  704, 58,
  726, 60,
  781, 65,
  836, 70,
  890, 75,
  945, 80,
  999, 85,
  1053, 90,
  1107, 95,
  1161, 100,
  1269, 110,
  1376, 120,
  1483, 130,
  1590, 140,
  1696, 150,
  1803, 160,
  1909, 170,
  2015, 180,
  2120, 190,
  2226, 200,
  2331, 210,
  2437, 220,
  2542, 230,
  2647, 240,
  2752, 250,
  2857, 260,
  2962, 270,
  3066, 280,
  3171, 290,
  3276, 300,
  3380, 310,
  3484, 320,
  3589, 330,
  3693, 340,
  3797, 350,
  3901, 360,
  4005, 370,
  4109, 380,
  4213, 390,
  4317, 400,
  4421, 410,
  4525, 420,
  4629, 430,
  4733, 440,
  4836, 450,
  4940, 460,
  5044, 470,
  5147, 480,
  5251, 490,
  5354, 500,
  5613, 525,
  5871, 550,
  6130, 575,
  6388, 600,
  6645, 625,
  6903, 650,
  7161, 675,
  7418, 700,
  7727, 730,
  8036, 760,
  8344, 790,
  8652, 820,
  8960, 850,
  9268, 880,
  9576, 910,
  9884, 940,
  10191, 970,
  10499, 1000
)

test_that("Non-parametric ranks for B/A-Basis match CMH-17-1G Table 8.5.12", {
  skip_on_cran()  # this test is a long-running test

  cmh_17_1g_8_5_12 %>%
    mutate(rb_lag = lag(rb)) %>%
    rowwise() %>%
    mutate(r_calc = nonpara_binomial_rank(n, 0.9, 0.95)) %>%
    mutate(expect_equal(rb, r_calc,
                        label = paste0(
                          "Mismatch in r for n=", n,
                          ". rB=", rb,
                          ", r_calc=", r_calc
                        ))) %>%
    # the rank for one sample larger should be the same
    mutate(r_calc_plus = nonpara_binomial_rank(n + 1, 0.9, 0.95)) %>%
    mutate(expect_equal(rb, r_calc_plus,
                        label = paste0(
                          "Mismatch in r for n=", n + 1,
                          ". rB=", rb, ", ",
                          "r_calc=", r_calc_plus
                        ))) %>%
    filter(n > 29 & n <= 275) %>%
    # the rank for one sample smaller should be the previous one
    # above n=275, Table 8.5.12 does not have consecutive ranks, so we can't
    # use the lag trick below to check sample sizes of n-1
    mutate(r_calc_minus = nonpara_binomial_rank(n - 1, 0.9, 0.95)) %>%
    mutate(expect_equal(rb_lag, r_calc_minus,
                        label = paste0(
                          "Mismatch in r for n=", n - 1,
                          ". rB=", rb_lag, ", ",
                          "r_calc=", r_calc_minus
                        )))
})
