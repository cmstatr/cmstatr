library(cmstatr)
library(tidyverse)

dat <- carbon.fabric.2 %>%
  filter(test == "WT") %>%
  mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

x_vals <- 120:150

dat %>%
  group_by(condition) %>%
  summarise(mean = mean(strength), sd = sd(strength)) %>%
  column_to_rownames("condition") %>%
  pmap(function(condition, mean, sd) {
    dnorm(x_vals, mean, sd)
  }) %>%
  map2(
    levels(dat$condition),
    function(y, c) {
      data.frame(condition = c, strength = x_vals, density = y, stringsAsFactors = FALSE)
    }) %>%
  bind_rows() %>%
  ggplot(aes(y = density, x = strength, color = condition)) +
  geom_path() +
  theme_test() +
  ylab("") +
  xlab("") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  ggsave("density.svg", width = 1, height = 1, bg = "transparent")
