library(rlang)
library(dplyr)
library(tidyr)
library(flextable)
library(data.table)


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
        return(list(
          x$basis$value[x$basis$group == condition],
          x$distribution
        ))
    }
    return(list(x$basis, x$distribution))
  },
  "integer" = function(x, condition) list(x)
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
    list()
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
          cur_cell_value <- lapply(cur_cell_value, function(v) {
            if (is.integer(v)) {
              as.character(v)
            } else if (is.numeric(v)) {
              sprintf(v, fmt = "%.2f")
            } else{
              v
            }
          })
          cur_cell_value <- unlist(cur_cell_value)
          print(cur_cell_value)
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

table <- table %>%
  footnote(
    i = c(1, 6),
    j = c(3, 4),
    as_paragraph("Failed Anderson-Darling Normality Test"),
    ref_symbols = "a"
  ) #%>%
# footnote(
#   i = ~ test == "FC" & slot == "b_basis" & i == 1,
#   j = "RTD",
#   as_paragraph("Failed Anderson-Darling Normality Test"),
#   ref_symbols = "a"
# ) #%>%
# # footnote(
# #   i = ~ test == "WT" & slot == "b_basis_pooled" & i == 2,
# #   j = 3:6,
# #   as_paragraph("test2"),
# #   ref_symbols = "3"
# # )

footnote_text <- list()
for (fn in footnote_list) {
  if (!fn$text %in% footnote_text) {
    # This is a new footnote text
    footnote_text <- append(footnote_text, fn$text)
    table <- footnote(
      table,
      i = ~ test == fn$test & slot == fn$slot & i == fn$i,
      j = fn$condition,
      as_paragraph(fn$text),
      ref_symbols = length(footnote_text)
    )
  } else {
    # A footnote with the same text was already added
    # We'll piggyback on that footnote
    fn_number <- match(fn$text, footnote_text)
    table <- footnote(
      table,
      i = ~ test == fn$test & slot == fn$slot & i == fn$i,
      j = fn$condition,
      as_paragraph(fn_number),
      inline = TRUE
    )
  }
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



