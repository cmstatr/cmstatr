context("Anderson-Darling k-Sample")

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

test_that("ADK test match ASAP", {
  # carbon.fabric %>%
  #   filter(test == "WT")
  # ADK(RTD) = 0.456 / same pop
  # ADK(ETW) = 1.604 / same pop
  # ADK(CTD) = 1.778 / same pop
  # ADC(0.05) = 1.917
  # ADC(0.025) = 2.217
  # ADC(0.01) = 2.613

  # Carbon FT
  # ADK(RTD) = 0.777 / same pop
  # ADK(ETW) = 1.355 / same pop
  # ADK(CTD) = 1.432 / same pop
  # ADC(0.05) = 1.917
  # ADC(0.025) = 2.217
  # ADC(0.01) = 2.613

  # Carbon WC
  # ADK(RTD) = 0.384 / same pop
  # ADK(ETW) = 0.723 / same pop
  # ADK(CTD) = 1.145 / same pop
  # ADC(0.05) = 1.917
  # ADC(0.025) = 2.217
  # ADC(0.01) = 2.613

  # Carbon WT
  # ADK(RTD) = 0.865 / same pop
  # ADK(ETW) = 0.934 / same pop
  # ADK(CTD) = 0.501 / same pop
  # ADC(0.05) = 1.917
  # ADC(0.025) = 2.217
  # ADC(0.01) = 2.613
})
