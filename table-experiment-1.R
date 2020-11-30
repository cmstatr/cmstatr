library(rlang)
library(dplyr)
library(tidyr)
library(flextable)


res <- carbon.fabric.2 %>%
  start_analysis(test, condition) %>%
  analyze_property(condition == "CTD" & test == "WT",
                   # discard_obs(),
                   `B Basis` = basis_normal(strength, batch),
                   a_basis = basis_lognormal(strength, batch, p = 0.99, conf = 0.95, override = c("between_batch_variability")),
                   n = nrow()) %>%
  analyze_property(condition == "CTD" & test == "FC",
                   b_basis = basis_normal(strength, batch, override = c("between_batch_variability"))) %>%
  analyze_property(test == "WT",
                   b_basis_pooled = basis_pooled_sd(strength, condition, batch, override = c("between_group_variability", "pooled_variance_equal"))) %>%
  analyze_property(condition == "RTD" & test == "FC",
                   b_basis = basis_normal(strength, batch))

table_format_fcn <- list(
  "basis" = function(x, condition) {
    if (is.data.frame(x$basis)) {
        return(c(x$basis$value[x$basis$group == condition], x$distribution))
    }
    return(c(x$basis, x$distribution))
  },
  "integer" = function(x, condition) as.character(x)
)

footnote_obj <- function(test, slot, i, condition, type, text) {
  res <- list()
  res[["test"]] <- test
  res[["slot"]] <- slot
  res[["i"]] <- i
  res[["condition"]] <- condition
  res[["type"]] <- type
  res[["text"]] <- text
  res
}

footnote_fcn <- list(
  "basis" = function(test, slot, x, c) {
    append(
      lapply(x$override, function(ov) {
        footnote_obj(
          test = test,
          slot = slot,
          i = 2,
          condition = c,
          type = "override",
          text = paste0("Test ", ov, " overriden")
        )
      }),
      lapply(x$diagnostic_failures, function(df) {
        footnote_obj(
          test = test,
          slot = slot,
          i = 2,
          condition = c,
          type = "failure",
          text = paste0("Test ", df, " failed")
        )
      })
    )
  },
  "integer" = function(test, slot, x, c) {
    c()
  }
)

# TODO: Add footnotes

# Columns:
# - Test (group by this)
# - Variable
# - Condition 1....

footnote_list <- list()

rows <- bind_rows(lapply(res$properties, function(p) {
  bind_rows(lapply(
    seq_along(p$slots),
    function(i_slot) {
      s <- p$slots[[i_slot]]

      cur_class <- class(s)
      if (!cur_class %in% names(table_format_fcn)) {
        stop(paste0("No table formatter defined for class `", cur_class, "`"))
      }
      bind_rows(lapply(
        p$condition,
        function(c) {
          cur_cell_value <- table_format_fcn[[cur_class]](s, c)
          slot_name <- names(p$slots)[i_slot]
          footnote_list <<- append(footnote_list, footnote_fcn[[cur_class]](p$test, slot_name, s, c))
          tibble(
            test = p$test,
            condition = c,
            slot = slot_name,
            i = seq_along(cur_cell_value),
            value = cur_cell_value
          )
        }
      ))
    }
  ))
}))

rows_wider <- rows %>%
  pivot_wider(names_from = condition, values_from = value)

table <- rows_wider %>%
  flextable(col_keys = names(rows_wider)[names(rows_wider) != "i"]) %>%
  theme_box()

table

table %>%
  footnote(
    i = c(~ test == "FC" & slot == "b_basis" & i == 2),
    j = c("CTD", "RTD"),
    as_paragraph("test"),
    ref_symbols = "1"
  ) #%>%
  # footnote(
  #   i = ~ test == "WT" & slot == "b_basis_pooled" & i == 1,
  #   j = 3:6,
  #   as_paragraph("test1"),
  #   ref_symbols = "2"
  # ) %>%
  # footnote(
  #   i = ~ test == "WT" & slot == "b_basis_pooled" & i == 2,
  #   j = 3:6,
  #   as_paragraph("test2"),
  #   ref_symbols = "3"
  # )

footnote_text <- list()
for (fn in footnote_list) {
  if (!fn$text %in% footnote_text) {
    footnote_text <- append(footnote_text, fn$text)
  }
  fn_number <- match(fn$text, footnote_text)
  table <- table %>%
    footnote(
      i = ~ test == fn$test & slot == fn$slot & i == fn$i,
      j = fn$condition,
      as_paragraph(fn$text),
      ref_symbols = fn_number
    )
}
table


for (p in res$properties) {
  if (length(p$condition) > 1) {
    # Pooled
    for (i_slot in seq_along(p$slots)) {
      slot_name <- names(p$slots)[i_slot]
      table <- table %>%
        merge_h(
          i = ~ test == p$test & slot == slot_name
        )
    }
  }
}

table

table %>%
  set_header_labels(slot = "") %>%
  merge_v(j = ~ test + slot) %>%
  align(align = "center") %>%
  fix_border_issues()

# - Order of test (should match the order that they were first added in)
# - multi-result slots (acceptance, etc.)
# - Column span for pooled distribution label
# - precision of numbers
# - footnotes
# - Row span for tests
# - Add support for sub-groups

# gt doesn't seem to allow the merging behavior we want



