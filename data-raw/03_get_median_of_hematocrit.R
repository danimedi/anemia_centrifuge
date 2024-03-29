library(here)
library(dplyr)
library(tidyr)
library(readr)

# Read
dat <- read_csv(here("data-raw/hematocrit_data", "hematocrit_full_data.csv"))

dat <- dat %>% select(id, original, starts_with("hematocrit"))

res <- dat %>%
  pivot_longer(starts_with("hematocrit")) %>%
  group_by(id, original) %>%
  summarize(
    hematocrit = median(value, na.rm = TRUE),
    count_na = sum(is.na(value))
  ) %>%
  filter(count_na <= 2) %>%  # Remove values that were only determined by one person
  select(-count_na)

# Write
write_csv(res, here("data-raw/hematocrit_values.csv"))
