suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))


test_that("nested_data_plot-no_grouping", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("nested_data_plot-no_grouping", {
    carbon.fabric.2 %>%
      filter(test == "WT") %>%
      nested_data_plot(strength)
  })
})

test_that("nested_data_plot-single_grouping", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("nested_data_plot-single_grouping", {
    carbon.fabric.2 %>%
      filter(test == "WT" & condition == "RTD") %>%
      nested_data_plot(strength,
                       groups = c(batch))
  })
})

test_that("nested_data_plot-two_groupings", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("nested_data_plot-two_groupings", {
    carbon.fabric.2 %>%
      filter(test == "WT" & condition == "RTD") %>%
      nested_data_plot(strength,
                       groups = c(batch, panel))
  })
})

test_that("nested_data_plot-single_obs_per_group", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("nested_data_plot-single_obs_per_group", {
    carbon.fabric.2 %>%
      filter(test == "WT" & condition == "RTD") %>%
      nested_data_plot(strength,
                       groups = c(batch, panel, strength))
  })
})

test_that("nested_data_plot-color", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("nested_data_plot-color", {
    carbon.fabric.2 %>%
      filter(test == "WT" & condition == "RTD") %>%
      nested_data_plot(strength,
                       groups = c(batch, panel),
                       color = batch)
  })
})

test_that("nested_data_plot-color-and-fill", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("nested_data_plot-color-and-fill", {
    carbon.fabric.2 %>%
      filter(test == "WT") %>%
      nested_data_plot(strength,
                       groups = c(condition, batch, panel),
                       color = batch,
                       fill = condition)
  })
})
