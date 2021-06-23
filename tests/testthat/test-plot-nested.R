suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))


expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}


expect_doppelganger("nested_data_plot-no_grouping", {
  carbon.fabric.2 %>%
  filter(test == "WT") %>%
  nested_data_plot(strength)
})


expect_doppelganger("nested_data_plot-single_grouping", {
  carbon.fabric.2 %>%
    filter(test == "WT" & condition == "RTD") %>%
    nested_data_plot(strength,
                     groups = c(batch))
})


expect_doppelganger("nested_data_plot-two_groupings", {
  carbon.fabric.2 %>%
    filter(test == "WT" & condition == "RTD") %>%
    nested_data_plot(strength,
                     groups = c(batch, panel))
})


expect_doppelganger("nested_data_plot-single_obs_per_group", {
  carbon.fabric.2 %>%
    filter(test == "WT" & condition == "RTD") %>%
    nested_data_plot(strength,
                     groups = c(batch, panel, strength))
})
