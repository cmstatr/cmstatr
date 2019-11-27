context("mnr")

suppressMessages(library(tidyverse))

cmh_17_cv <- tribble(
  ~n, ~c,
  3, 1.154,
  4, 1.481,
  5, 1.715,
  6, 1.887,
  7, 2.02,
  8, 2.127,
  9, 2.215,
  10, 2.29,
  11, 2.355,
  12, 2.412,
  13, 2.462,
  14, 2.507,
  15, 2.548,
  16, 2.586,
  17, 2.62,
  18, 2.652,
  19, 2.681,
  20, 2.708,
  21, 2.734,
  22, 2.758,
  23, 2.78,
  24, 2.802,
  25, 2.822,
  26, 2.841,
  27, 2.859,
  28, 2.876,
  29, 2.893,
  30, 2.908,
  31, 2.924,
  32, 2.938,
  33, 2.952,
  34, 2.965,
  35, 2.978,
  36, 2.991,
  37, 3.003,
  38, 3.014,
  39, 3.025,
  40, 3.036,
  41, 3.047,
  42, 3.057,
  43, 3.067,
  44, 3.076,
  45, 3.085,
  46, 3.094,
  47, 3.103,
  48, 3.112,
  49, 3.12,
  50, 3.128,
  51, 3.136,
  52, 3.144,
  53, 3.151,
  54, 3.159,
  55, 3.166,
  56, 3.173,
  57, 3.18,
  58, 3.187,
  59, 3.193,
  60, 3.2,
  61, 3.206,
  62, 3.212,
  63, 3.218,
  64, 3.224,
  65, 3.23,
  66, 3.236,
  67, 3.241,
  68, 3.247,
  69, 3.252,
  70, 3.258,
  71, 3.263,
  72, 3.268,
  73, 3.273,
  74, 3.278,
  75, 3.283,
  76, 3.288,
  77, 3.292,
  78, 3.297,
  79, 3.302,
  80, 3.306,
  81, 3.311,
  82, 3.315,
  83, 3.319,
  84, 3.323,
  85, 3.328,
  86, 3.332,
  87, 3.336,
  88, 3.34,
  89, 3.344,
  90, 3.348,
  91, 3.352,
  92, 3.355,
  93, 3.359,
  94, 3.363,
  95, 3.366,
  96, 3.37,
  97, 3.374,
  98, 3.377,
  99, 3.381,
  100, 3.384,
  101, 3.387,
  102, 3.391,
  103, 3.394,
  104, 3.397,
  105, 3.401,
  106, 3.404,
  107, 3.407,
  108, 3.41,
  109, 3.413,
  110, 3.416,
  111, 3.419,
  112, 3.422,
  113, 3.425,
  114, 3.428,
  115, 3.431,
  116, 3.434,
  117, 3.437,
  118, 3.44,
  119, 3.442,
  120, 3.445,
  121, 3.448,
  122, 3.451,
  123, 3.453,
  124, 3.456,
  125, 3.459,
  126, 3.461,
  127, 3.464,
  128, 3.466,
  129, 3.469,
  130, 3.471,
  131, 3.474,
  132, 3.476,
  133, 3.479,
  134, 3.481,
  135, 3.483,
  136, 3.486,
  137, 3.488,
  138, 3.491,
  139, 3.493,
  140, 3.495,
  141, 3.497,
  142, 3.5,
  143, 3.502,
  144, 3.504,
  145, 3.506,
  146, 3.508,
  147, 3.511,
  148, 3.513,
  149, 3.515,
  150, 3.517,
  151, 3.519,
  152, 3.521,
  153, 3.523,
  154, 3.525,
  155, 3.527,
  156, 3.529,
  157, 3.531,
  158, 3.533,
  159, 3.535,
  160, 3.537,
  161, 3.539,
  162, 3.541,
  163, 3.543,
  164, 3.545,
  165, 3.547,
  166, 3.549,
  167, 3.551,
  168, 3.552,
  169, 3.554,
  170, 3.556,
  171, 3.558,
  172, 3.56,
  173, 3.561,
  174, 3.563,
  175, 3.565,
  176, 3.567,
  177, 3.568,
  178, 3.57,
  179, 3.572,
  180, 3.574,
  181, 3.575,
  182, 3.577,
  183, 3.579,
  184, 3.58,
  185, 3.582,
  186, 3.584,
  187, 3.585,
  188, 3.587,
  189, 3.588,
  190, 3.59,
  191, 3.592,
  192, 3.593,
  193, 3.595,
  194, 3.596,
  195, 3.598,
  196, 3.599,
  197, 3.601,
  198, 3.603,
  199, 3.604,
  200, 3.606
)

test_that(
  "Critical MNR values match those published in CMH-17-1G, Table 8.5.7", {
    cmh_17_cv %>%
      rowwise() %>%
      mutate(calc_mnr_crit = maximum_normed_residual_crit(n, 0.05)) %>%
      mutate(check = expect_lte(abs(calc_mnr_crit - c), 0.001))
})

test_that(
  "MNR calculation matches example in CMH-17-1G Section 8.3.11.1.1", {
    # What follows is part of the ETW data from Section 8.3.11.1.1
    df <- tribble(
      ~batch, ~strength,
      2, 99.3207107,
      2, 115.86177,
      2, 82.6133082,
      2, 85.3690411,
      2, 115.801622,
      2, 44.3217741,
      2, 117.328077,
      2, 88.6782903,
      3, 107.676986,
      3, 108.960241,
      3, 116.12264,
      3, 80.2334815,
      3, 106.14557,
      3, 104.667866,
      3, 104.234953
    )

    # first do some checks to ensure that the above data has been typed
    # correctly
    df %>%
      filter(batch == 2) %>%
      summarise(check_n = expect_equal(n(), 8),
                check_mean = expect_lte(abs(mean(strength) - 93.662), 0.001),
                check_sd = expect_lte(abs(sd(strength) - 24.568), 0.001))

    # Check that the MNR test results match
    res <- df %>%
      filter(batch == 2) %>%
      maximum_normed_residual(strength, alpha = 0.05)
    expect_lte(abs(res$mnr - 2.008), 0.001)
    expect_lte(abs(res$crit - 2.127), 0.001)
    expect_equal(nrow(res$outliers), 0)  # no outliers for this batch
    expect_equal(res$n_outliers, 0)

    # check the print function
    expect_output(print(res), "no outliers", ignore.case = TRUE)
    expect_output(print(res), "MNR.*2.008", ignore.case = TRUE)
    expect_output(print(res), ".*crit.*2\\.12", ignore.case = TRUE)

    # check for typographical errors in the data above
    df %>%
      filter(batch == 3) %>%
      summarise(check = expect_equal(n(), 7),
                check_mean = expect_lte(abs(mean(strength) - 104.006), 0.001),
                check_sd = expect_lte(abs(sd(strength) - 11.218), 0.001))

    # Check that the MNR test results match
    res <- df %>%
      filter(batch == 3) %>%
      maximum_normed_residual(strength, alpha = 0.05)
    expect_lte(abs(res$mnr - 2.119), 0.001)
    expect_lte(abs(res$crit - 2.02), 0.001)
    expect_equal(nrow(res$outliers), 1)  # one outlier for this batch
    expect_equal(res$n_outliers, 1)

    # check the print function
    expect_output(print(res), "outliers", ignore.case = TRUE)
    expect_output(print(res), "MNR.*2.119", ignore.case = TRUE)
    expect_output(print(res), ".*crit.*2\\.01", ignore.case = TRUE)
    # check that the outlier was shown in the print statement
    expect_output(print(res), "4.*80\\.23348", ignore.case = TRUE)

})

test_that("Both vectors and data.frames can be passed to the MNR function", {
  df <- tribble(
    ~batch, ~strength,
    3, 107.676986,
    3, 108.960241,
    3, 116.12264,
    3, 80.2334815,
    3, 106.14557,
    3, 104.667866,
    3, 104.234953
  )

  # check that passing a data.frame works
  res1 <- df %>%
    maximum_normed_residual(strength, alpha = 0.05)
  expect_lte(abs(res1$mnr - 2.119), 0.001)
  expect_lte(abs(res1$crit - 2.02), 0.001)
  expect_equal(nrow(res1$outliers), 1)  # one outlier for this batch
  expect_equal(res1$n_outliers, 1)

  # check that passing a vector works
  res2 <- maximum_normed_residual(x = df$strength, alpha = 0.05)
  expect_lte(abs(res2$mnr - 2.119), 0.001)
  expect_lte(abs(res2$crit - 2.02), 0.001)
  expect_equal(nrow(res2$outliers), 1)  # one outlier for this batch
  expect_equal(res2$n_outliers, 1)
})

test_that("glance method returns expected results", {
  df <- tribble(
    ~batch, ~strength,
    3, 107.676986,
    3, 108.960241,
    3, 116.12264,
    3, 80.2334815,
    3, 106.14557,
    3, 104.667866,
    3, 104.234953
  )

  mnr_res <- df %>%
    maximum_normed_residual(strength, alpha = 0.05)
  glance_res <- glance(mnr_res)

  expect_equal(glance_res$mnr, 2.119, tolerance = 0.001)
  expect_equal(glance_res$crit, 2.02, tolerance = 0.001)
  expect_equal(glance_res$n_outliers, 1)
})
