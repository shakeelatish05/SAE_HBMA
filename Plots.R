##Plot: Bias (x) vs RMSE (y)
library(ggplot2)

plot_df <- bind_rows(res_list)

ggplot(plot_df, aes(x = Bias, y = RMSE, color = method)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Bias–RMSE Trade-off Across Estimators",
    x = "Bias",
    y = "RMSE",
    color = "Method"
  )

####Coverage Plot
coverage_df <- plot_df %>%
  group_by(method) %>%
  summarise(Coverage = mean(Coverage, na.rm = TRUE))

ggplot(coverage_df, aes(x = method, y = Coverage)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Coverage Probability by Method",
    y = "Coverage",
    x = ""
  )
#####Average CI Length vs Coverage (Efficiency–Validity Trade-off)
ci_df <- plot_df %>%
  filter(!is.na(Avg_CI_Length))

ggplot(ci_df, aes(x = Avg_CI_Length, y = Coverage, color = method)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Efficiency vs Validity of Interval Estimates",
    x = "Average CI Length",
    y = "Coverage"
  )
###Scenario-wise Performance (Faceted plot)
ggplot(plot_df, aes(x = method, y = RMSE, fill = method)) +
  geom_boxplot() +
  facet_wrap(~ scenario, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "RMSE Across Scenarios",
    x = "",
    y = "RMSE"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###Area-level True vs Estimated Means (Diagnostic plot)
area_df <- data.frame(
  true   = true_means,
  Direct = direct,
  HB1    = HB1$mean,
  HBMA   = HBMA$mean
) %>%
  pivot_longer(-true, names_to = "method", values_to = "estimate")

ggplot(area_df, aes(x = true, y = estimate, color = method)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "True vs Estimated Area Means",
    x = "True Area Mean",
    y = "Estimated Area Mean"
  )

