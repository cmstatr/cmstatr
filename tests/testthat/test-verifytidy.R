context("Verify tidy data")

library(rlang)

# A mock function that uses process_tidy_vector_input
mock_fcn <- function(data = NULL, x) {
  verify_tidy_input(
    df = data,
    x = x,
    c = match.call(),
    arg_name = "x")

  vec <- eval_tidy(enquo(x), data)
  return(vec)
}

test_that("A vector is returned", {
  res <- mock_fcn(carbon.fabric, strength)
  expect_true(is.vector(res))

  res <- mock_fcn(carbon.fabric, "strength")
  expect_true(is.vector(res))

  res <- mock_fcn(NULL, c(1, 2, 3))
  expect_true(is.vector(res))
})

test_that("Correct warning messages are given", {
  # Should give the name of the data argument in mock_fcn
  expect_error(
    mock_fcn(c(1, 2, 3)),
    regexp = ".*`data`"
  )

  expect_error(
    mock_fcn(data = c(1, 2, 3)),
    regexp = ".*`data`"
  )

  expect_error(
    mock_fcn(c(1, 2, 3)),
    regexp = ".*mock_fcn\\(x = c\\(1, 2, 3\\)\\)"
  )

  expect_error(
    mock_fcn(carbon.fabric$strength),
    regexp = ".*mock_fcn\\(x = carbon\\.fabric\\$strength\\)"
  )
})
