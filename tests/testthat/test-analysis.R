context("Analysis")

suppressMessages(library(dplyr))

test_that("Reused observations cause warning", {
  expect_warning({
    carbon.fabric.2 %>%
      start_analysis(test, condition) %>%
      analyze_property(condition == "CTD" & test == "WT",
                       b_basis = basis_normal(
                         strength, batch,
                         override = c("between_batch_variability"))) %>%
      analyze_property(test == "WT",
                       b_basis = basis_pooled_sd(
                         strength, condition, batch,
                         override = c("between_group_variability",
                                      "pooled_variance_equal")))
  },
  "reused")
})

test_that("as.data.frame on pooled data produces correct results", {
  df <- carbon.fabric.2 %>%
    start_analysis(test, condition) %>%
    analyze_property(test == "WT",
                     b_basis_pooled = basis_pooled_sd(
                       strength, condition, batch,
                       override = c("between_group_variability",
                                    "pooled_variance_equal"))) %>%
    as.data.frame()

  # The correct basis values are know and will be added to the data.frame
  # so that we can ensure that the correct one is matched with the correct
  # environment
  df %>%
    mutate(basis_correct = case_when(
      condition == "CTD" ~ 125.0637,
      condition == "ETW" ~ 123.6346,
      condition == "ETW2" ~ 119.8492,
      condition == "RTD" ~ 129.5842,
      TRUE ~ 0
    )) %>%
    mutate(check = expect_equal(basis_correct, b_basis_pooled.basis,
                                tolerance = 0.01))
})

test_that("Extra arguments can be passed to glance by as.data.frame", {
  res <- carbon.fabric.2 %>%
    start_analysis(test, condition) %>%
    analyze_property(test == "WT" & condition == "RTD",
                     b_basis = basis_normal(
                       strength, batch,
                       override = c("between_batch_variability")),
                     acceptance = equiv_mean_extremum(
                       df_qual = ., data_qual = strength,
                       n_sample = 5, alpha = 0.01))

  df_no_args <- res %>%
    as.data.frame()

  df_args <- res %>%
    as.data.frame(include_diagnostics = TRUE)
})

test_that("condition column can be called anything", {
  df <- carbon.fabric.2 %>%
    rename(anything = condition) %>%
    start_analysis(test, anything) %>%
    analyze_property(test == "WT",
                     b_basis_pooled = basis_pooled_sd(
                       strength, anything, batch,
                       override = c("between_group_variability",
                                    "pooled_variance_equal")),
                     ) %>%
    as.data.frame()

  # The correct basis values are know and will be added to the data.frame
  # so that we can ensure that the correct one is matched with the correct
  # environment
  df %>%
    mutate(basis_correct = case_when(
      anything == "CTD" ~ 125.0637,
      anything == "ETW" ~ 123.6346,
      anything == "ETW2" ~ 119.8492,
      anything == "RTD" ~ 129.5842,
      TRUE ~ 0
    )) %>%
    mutate(check = expect_equal(basis_correct, b_basis_pooled.basis,
                                tolerance = 0.01))
})

test_that("Non-pooled functions called on multi-env data cause error", {
  expect_error({
    carbon.fabric.2 %>%
      start_analysis(test, condition) %>%
      analyze_property(test == "WT",
                       basis = basis_normal(
                         strength, batch,
                         override = c("between_batch_variability",
                                      "anderson_darling_normal")
                       ))
  },
  "not pooled"
  )

  special_basis_fun <- function(data = NULL) {
    data %>%
      filter(condition != "CTD") %>%
      basis_pooled_sd(strength, condition, batch,
                      override = c("between_batch_variability",
                                   "between_group_variability",
                                   "pooled_variance_equal",
                                   "anderson_darling_normal"))
  }

  expect_error({
    carbon.fabric.2 %>%
      start_analysis(test, condition) %>%
      analyze_property(test == "WT",
                       basis = special_basis_fun())
  },
  "not pooled across all environments")
})

# TODO: Test un-named slots
test_that("Unnamed slots are valid", {
  res <- carbon.fabric.2 %>%
    start_analysis(test, condition) %>%
    analyze_property(test == "WT" & condition == "RTD",
                     basis_normal(strength, batch,
                                  override = c("between_batch_variability")))

  expect_equal(names(res$results[[1]]$slots), "basis_normal")
})
