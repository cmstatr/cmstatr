context("Checks")

test_that("perform_checks produces warnings unless overriden", {
  sample_rules <- list(
    positive = function(pos, ...) {
      pos > 0
    },
    negative = function(neg, ...) {
      neg < 0
    },
    zero = function(z, ...) {
      z == 0
    }
  )

  expect_warning(perform_checks(sample_rules, pos = -1, neg = -1, z = 0),
                 regexp = "positive")
  perform_checks(sample_rules, pos = -1, neg = -1, z = 0,
                 override = "positive")
  expect_warning(perform_checks(sample_rules, pos = 1, neg = 1, z = 0),
                 regexp = "negative")
  perform_checks(sample_rules, pos = 1, neg = 1, z = 0,
                 override = "negative")
  expect_warning(perform_checks(sample_rules, pos = 1, neg = -1, z = 1),
                 regexp = "zero")
  perform_checks(sample_rules, pos = 1, neg = -1, z = 1,
                 override = "zero")

  # should produce no warnings
  perform_checks(sample_rules, pos = 1, neg = -1, z = 0)
})
