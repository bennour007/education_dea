# Analysis 1: Summary Statistics of Efficiency Scores
summary_stats <- dea_out_1 %>%
  mutate(efficiency_scores = map(dea_out, ~.$eff)) %>%
  summarise(
    mean_efficiency = mean(unlist(efficiency_scores)),
    sd_efficiency = sd(unlist(efficiency_scores)),
    min_efficiency = min(unlist(efficiency_scores)),
    max_efficiency = max(unlist(efficiency_scores))
  )

print(summary_stats)

# Analysis 2: Distribution of Efficiency Scores
dea_out_1 %>%
  mutate(efficiency_scores = map(dea_out, ~.$eff)) %>%
  unnest(efficiency_scores) %>%
  ggplot(aes(x = efficiency_scores)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "skyblue") +
  facet_wrap(~CNT) +
  labs(title = "Distribution of Efficiency Scores by Country",
       x = "Efficiency Score",
       y = "Frequency")

dea_by_quartile <- dea_out_1 %>% 
  mutate(
    eff_scores =  map(dea_out, ~pluck(.x, "eff"))
  ) %>% 
  unnest(eff_scores) %>% 
  mutate(
    full_eff = if_else(eff_scores == 1, 1, 0),
    quartiles = cut(eff_scores,
                               breaks = quantile(eff_scores, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                               labels = c('Q1', 'Q2', 'Q3', 'Q4'),
                               include.lowest = TRUE)
  )

dea_by_quartile %>% 
  filter(full_eff != 1) %>% 
  ggplot(aes(x = eff_scores)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "skyblue") +
  facet_grid(quartiles ~ CNT) +
  labs(title = "Distribution of Efficiency Scores by Country and Quartile\nFor non Efficient Schools",
       x = "Efficiency Score",
       y = "Frequency")


dea_by_quartile %>% 
  filter(full_eff != 1) %>% 
  ggplot(aes(x = quartiles, y = eff_scores)) +
  geom_boxplot() +
  facet_grid( ~ CNT) +
  labs(title = "Distribution of Efficiency Scores by Country and Quartile\nFor non Efficient Schools",
       x = "Efficiency Score",
       y = "Frequency")

dea_by_quartile %>% 
  mutate(
    quartiles_ineff = if_else(full_eff == 1, "FE", quartiles)
  ) %>% 
  count(quartiles_ineff) %>% 
  ggplot() +
  geom_col(
    aes(
      x = n, 
      y = CNT,
      .group = CNT,
      fill = quartiles_ineff
    ), position = "dodge"
  ) +
  labs(title = "Number of Fully Efficient (FE) and Quartile distribution\nof Inefficient Schools by Country",
       x = "Number of Schools",
       y = "Country")
