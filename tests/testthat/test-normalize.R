context("normalize")

test_that("normalization takes correct argument lengths", {
  expect_error(normalize_ply_thickness(c(1, 2), 1, 1))
  expect_error(normalize_ply_thickness(1, c(1, 2), 1))
  expect_error(normalize_ply_thickness(c(1, 2), c(1, 2), c(1, 2)))
})

test_that("normalization produces expected numeric results", {
  expect_equal(normalize_ply_thickness(1, 1, 0.25), 4)
  expect_equal(normalize_ply_thickness(c(1, 2),
                                       c(0.5, 0.25),
                                       0.25),
               c(2, 2)
  )
})
