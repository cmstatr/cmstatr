library(tidyverse)

generate_fake_data <- function(seed = 123) {
  set.seed(seed)

  generate_test <- function(n, distribution, x_bar, cv, test, condition, batch) {
    s <- x_bar * cv / 100
    if (distribution == "normal") {
      strength <- rnorm(n, x_bar, s)
    } else if (distribution == "weibull") {
      k <- (s / x_bar) ^ -1.086
      lambda <- x_bar / gamma(1 + 1 / k)
      strength <- rweibull(n, k, lambda)
    }

    return(data.frame(
      id = paste0(test, "-", condition, "-", batch, "-", 1:n),
      test = test,
      condition = condition,
      batch = batch,
      strength = strength,
      stringsAsFactors = FALSE
    ))
  }

  generate_test_mult_batch <- function(n, distribution, x_bar, cv, test, condition, n_batch) {
    bind_rows(
      lapply(1:n_batch, function(b) {
        generate_test(n, distribution, x_bar, cv, test, condition, b)
      })
    )
  }

  bind_rows(
    generate_test_mult_batch(6, "normal", 141, 4.5, "WT", "RTD", 3),
    generate_test_mult_batch(6, "normal", 135, 3.5, "WT", "ETW", 3),
    generate_test_mult_batch(6, "normal", 137, 4.8, "WT", "CTD", 3),

    generate_test_mult_batch(6, "normal", 128, 5.9, "FT", "RTD", 3),
    generate_test_mult_batch(6, "normal", 117, 5.4, "FT", "ETW", 3),
    generate_test_mult_batch(6, "normal", 126, 4.2, "FT", "CTD", 3),

    generate_test_mult_batch(6, "normal", 99, 5.6, "WC", "RTD", 3),
    generate_test_mult_batch(6, "normal", 65, 7.2, "WC", "ETW", 3),
    generate_test_mult_batch(6, "normal", 105, 9.3, "WC", "CTD", 3),

    generate_test_mult_batch(6, "normal", 89, 7.0, "FC", "RTD", 3),
    generate_test_mult_batch(6, "normal", 58, 4.0, "FC", "ETW", 3),
    generate_test_mult_batch(6, "normal", 96, 7.9, "FC", "CTD", 3)
  )
}

carbon.fabric <- generate_fake_data()
devtools::use_data(carbon.fabric)
