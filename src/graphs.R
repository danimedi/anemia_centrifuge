library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

dat <- read_rds(here("data/database.rds"))

# Differences between the hematocrit values obtained by centrifuge or runrun
dat_plot <- dat %>% pivot_wider(names_from = method, values_from = hematocrit)

blind_operator_names <- function(x) {
  n <- as.integer(as.factor(x))
  paste("operador", n)
}
dat_plot <- dat_plot %>% mutate(operator = blind_operator_names(operator))

# Reorder the hematocrit values
dat_plot <- dat_plot %>%
  arrange(centrifuge) %>%
  mutate(id = factor(id, levels = id))

# Set the output directory
dir <- here("results/graphs")

base_diff_plot <- ggplot(dat_plot) +
  geom_point(aes(id, runrun), color = "blue") +
  geom_point(aes(id, centrifuge), color = "red") +
  geom_linerange(aes(id, ymax = runrun, ymin = centrifuge))

base_diff_plot +
  theme(
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Muestras",
    y = "Valor de hematocrito"
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 10))
ggsave(here(dir, "all_hematocrit_values.png"))

base_diff_plot +
  facet_wrap(vars(operator)) +
  theme(
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Muestras",
    y = "Valor de hematocrito"
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 10))
  
ggsave(here(dir, "difference_between_operators.png"))

base_diff_plot +
  facet_wrap(vars(device))
ggsave(here(dir, "difference_between_devices.png"))

dat_plot <- dat_plot %>% mutate(diff = runrun - centrifuge)
ggplot(dat_plot) +
  geom_col(aes(id, diff)) +
  facet_wrap(vars(as.factor(time_centrifuge)))
ggsave(here(dir, "difference_between_time.png"))
