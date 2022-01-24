library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

dat <- read_rds(here("data/database.rds"))

# Exploratory analysis -------------

# Differences between the hematocrit values obtained by centrifuge or runrun
dat_plot <- dat %>% pivot_wider(names_from = method, values_from = hematocrit)

base_diff_plot <- ggplot(dat_plot) +
  geom_point(aes(id, runrun), color = "blue") +
  geom_point(aes(id, centrifuge), color = "red") +
  geom_linerange(aes(id, ymax = runrun, ymin = centrifuge))

base_diff_plot +
  facet_wrap(vars(operator))

base_diff_plot +
  facet_wrap(vars(device))
  

dat_plot <- dat_plot %>% mutate(diff = runrun - centrifuge)
ggplot(dat_plot) +
  geom_col(aes(id, diff)) +
  facet_wrap(vars(as.factor(time_centrifuge)))

