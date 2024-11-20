suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

test_that("separate_failure_modes produces expected output", {
  df <- tibble(x = 1, fm = "FAT")
  expect_equal(df, separate_failure_modes(df, fm))

  df <- tibble(x = c(1, 2), fm = c("FGT/FGB/FAT", "FGT,FGB"))
  df1 <- tibble(x = c(1, 1, 1, 2, 2), fm = c("FGT", "FGB", "FAT", "FGT", "FGB"))
  expect_equal(separate_failure_modes(df, fm), df1)

  df <- tibble(x = c(1, 2), fm = c("FGT /FGB / FAT", "FGT, FGB"))
  expect_equal(separate_failure_modes(df, fm), df1)

  df <- tibble(x = c(1, 2), fm = c("FGT|FGB|FAT", "FGT_FGB"))
  expect_equal(separate_failure_modes(df, fm, sep = "[|_]+"), df1)
})

test_that("geom_jitter_failure_mode produces expected errors", {
  dat <- carbon.fabric.2 %>%
    filter(test == "WT") %>%
    mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

  # at least one of color or shape
  gg <- ggplot(dat, aes(x = batch, y = strength)) +
    geom_jitter_failure_mode()
  expect_error(
    print(gg),
    regexp = ".*missing aesthetic.*"
  )
  gg <- dat %>%
    ggplot(aes(x = batch, y = strength)) +
    geom_jitter_failure_mode(
      aes(color = failure_mode))
  expect_no_error(print(gg))
  gg <- dat %>%
    ggplot(aes(x = batch, y = strength)) +
    geom_jitter_failure_mode(
      aes(shape = failure_mode))
  expect_no_error(print(gg))

  # if color and shape specified, both equal
  gg <- dat %>%
    ggplot(aes(x = batch, y = strength)) +
    geom_jitter_failure_mode(
      aes(color = failure_mode, shape = failure_mode))
  expect_no_error(print(gg))

  gg <- dat %>%
    ggplot(aes(x = batch, y = strength)) +
    geom_jitter_failure_mode(
      aes(colour = failure_mode, shape = batch))
  expect_error(
    print(gg),
    ".*both are equal.*"
  )

})

test_that("geom_jitter_failure_mode options work", {
  skip_if_not_installed("vdiffr")

  dat <- carbon.fabric.2 %>%
    filter(test == "WT") %>%
    mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

  set.seed(123)  # since jitter is random, make sure this is reproducible
  vdiffr::expect_doppelganger("geom_jitter_failure_mode_sep1", {
    dat %>%
      mutate(failure_mode = sub("/", "|", failure_mode)) %>%
      ggplot(aes(x = batch, y = strength)) +
      geom_jitter_failure_mode(
        aes(color = failure_mode))
  })

  set.seed(123)  # since jitter is random, make sure this is reproducible
  vdiffr::expect_doppelganger("geom_jitter_failure_mode_sep2", {
    dat %>%
      mutate(failure_mode = sub("/", "|", failure_mode)) %>%
      ggplot(aes(x = batch, y = strength)) +
      geom_jitter_failure_mode(
        aes(color = failure_mode),
        sep = "[|, ]+")
  })

  # the result above should be identical to the one below. Test using
  # the same filename
  set.seed(123)  # since jitter is random, make sure this is reproducible
  vdiffr::expect_doppelganger("geom_jitter_failure_mode_sep2", {
    dat %>%
      ggplot(aes(x = batch, y = strength)) +
      geom_jitter_failure_mode(
        aes(color = failure_mode),
        sep = "[/, ]+")
  })
})

test_that("geom_jitter_failure_mode dopleganger", {
  skip_if_not_installed("vdiffr")

  dat <- carbon.fabric.2 %>%
    filter(test == "WT") %>%
    mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

  set.seed(123)  # since jitter is random, make sure this is reproducible
  vdiffr::expect_doppelganger("geom_jitter_failure_mode_basic", {
    dat %>%
      ggplot(aes(x = batch, y = strength)) +
      geom_jitter_failure_mode(
        aes(color = failure_mode, shape = failure_mode))
  })
})
