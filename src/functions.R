var_summary <- function(.data, var) {
  .data %>%
    summarize(
      min = min({{ var }}, na.rm = TRUE),
      q1 = quantile({{ var }}, 0.25, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      mean = mean({{ var }}, na.rm = TRUE),
      q3 = quantile({{ var }}, 0.75, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE)
    )
}

var_mean_sd <- function(.data, var) {
  .data %>%
    summarize(
      mean = mean({{ var }}, na.rm = TRUE),
      sd = sd({{ var }}, na.rm = TRUE),
    )
}