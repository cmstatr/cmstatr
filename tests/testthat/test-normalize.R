context("normalize")

suppressMessages(library(dplyr))

test_that("normalization to ply thickness takes correct argument lengths", {
  expect_error(normalize_ply_thickness(c(1, 2), 1, 1))
  expect_error(normalize_ply_thickness(1, c(1, 2), 1))
  expect_error(normalize_ply_thickness(c(1, 2), c(1, 2), c(1, 2)))
})

test_that("normalization to ply thickness produces expected numeric results", {
  expect_equal(normalize_ply_thickness(1, 1, 0.25), 4)
  expect_equal(normalize_ply_thickness(c(1, 2),
                                       c(0.5, 0.25),
                                       0.25),
               c(2, 2)
  )
})

test_that("normalize to group mean produces means of one for all groups", {
  # since each group is normalized to its own mean, the mean of the normalized
  # values within each group should be equal to one.
  res <- carbon.fabric %>%
    filter(test == "WT") %>%
    mutate(norm_str = normalize_group_mean(strength, condition)) %>%
    group_by(condition) %>%
    summarise(mean_norm_str = mean(norm_str)) %>%
    rowwise() %>%
    mutate(test = expect_equal(mean_norm_str, 1))
})
