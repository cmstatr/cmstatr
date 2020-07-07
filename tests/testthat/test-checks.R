context("Checks")

test_that("perform_checks produces warnings unless overriden", {
  sample_rules <- list(
    positive = function(pos, ...) {
      ifelse(pos > 0, "", "Not positive")
    },
    negative = function(neg, ...) {
      ifelse(neg < 0, "", "Not negative")
    },
    zero = function(z, ...) {
      ifelse(z == 0, "", "Not zero")
    }
  )

  expect_warning(res <- perform_checks(sample_rules, pos = -1, neg = -1, z = 0),
                 regexp = "positive")
  names(res) <- NULL
  expect_equal(res, c("F", "P", "P"), )
  perform_checks(sample_rules, pos = -1, neg = -1, z = 0,
                 override = "positive")
  expect_warning(res <- perform_checks(sample_rules, pos = 1, neg = 1, z = 0),
                 regexp = "negative")
  names(res) <- NULL
  expect_equal(res, c("P", "F", "P"))
  perform_checks(sample_rules, pos = 1, neg = 1, z = 0,
                 override = "negative")
  expect_warning(res <- perform_checks(sample_rules, pos = 1, neg = -1, z = 1),
                 regexp = "zero")
  names(res) <- NULL
  expect_equal(res, c("P", "P", "F"))
  perform_checks(sample_rules, pos = 1, neg = -1, z = 1,
                 override = "zero")

  # should produce no warnings
  perform_checks(sample_rules, pos = 1, neg = -1, z = 0)
})

test_that("Messages are created for missing parameters", {
  sample_rules <- list(
    positive = function(pos, ...) {
      ifelse(pos > 0, "", "Not positive")
    },
    negative = function(neg, ...) {
      ifelse(neg < 0, "", "Not negative")
    },
    zero = function(z, ...) {
      ifelse(z == 0, "", "Not zero")
    },
    neg_or_zero = function(neg, z, ...) {
      ""
    }
  )

  expect_message(perform_checks(sample_rules, pos = NULL, neg = -1, z = 0),
                 regexp = "`positive`.+`pos`",
                 all = TRUE)
  expect_message(perform_checks(sample_rules, pos = 1, neg = NULL, z = 0),
                 regexp = "(`negative`.+`neg`)|(`neg_or_zero`.+`neg`)",
                 all = TRUE)
  # two parameters missing
  expect_message(
    perform_checks(sample_rules, pos = 1, neg = NULL, z = NULL),
    regexp = "(`negative`.+`neg`)|(`zero`.+`z`)|(`neg_or_zero`.+`neg`.+`z`)",
    all = TRUE
  )
})
