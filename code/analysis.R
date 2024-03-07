# Analysis 1: Calculate and print summary statistics (mean, standard deviation, min, max) for efficiency scores.
summary_stats <- dea_out_1 %>%
  mutate(efficiency_scores = map(dea_out, ~.$eff)) %>%
  summarise(
    mean_efficiency = mean(unlist(efficiency_scores)),
    sd_efficiency = sd(unlist(efficiency_scores)),
    min_efficiency = min(unlist(efficiency_scores)),
    max_efficiency = max(unlist(efficiency_scores))
  )
print(summary_stats)

# Analysis 2: Plot distribution of efficiency scores for each country.
dea_out_1 %>%
  mutate(efficiency_scores = map(dea_out, ~.$eff)) %>%
  unnest(efficiency_scores) %>%
  ggplot(aes(x = efficiency_scores)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "skyblue") +
  facet_wrap(~CNT) +
  labs(title = "Distribution of Efficiency Scores by Country",
       subtitle = "Including Fully efficient schools",
       x = "Efficiency Score",
       y = "Frequency")

# Extract efficiency scores, compute quartiles, and mark fully efficient schools.
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

# Plot histogram of efficiency scores by country and quartile for non-fully efficient schools.
dea_by_quartile %>% 
  filter(full_eff != 1) %>% 
  ggplot(aes(x = eff_scores)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "skyblue") +
  facet_grid(quartiles ~ CNT) +
  labs(title = "Distribution of Efficiency Scores by Country and Quartile\nFor non Efficient Schools",
       x = "Efficiency Score",
       y = "Frequency")

# Plot boxplot of efficiency scores by country and quartile for non-fully efficient schools.
dea_by_quartile %>% 
  filter(full_eff != 1) %>% 
  ggplot(aes(x = quartiles, y = eff_scores)) +
  geom_boxplot() +
  facet_grid(~ CNT) +
  labs(title = "Distribution of Efficiency Scores by Country and Quartile\nFor non Efficient Schools",
       x = "Quartile",
       y = "Efficiency Score")

# Calculate and display count and percentage of schools in each quartile, including fully efficient ones.
dea_by_quartile %>% 
  mutate(
    quartiles_ineff = if_else(full_eff == 1, "FE", quartiles)
  ) %>% 
  count(quartiles_ineff) %>% 
  mutate(
    p = case_when(
      CNT == 'CZE' ~ (n / 417)*100,
      CNT == 'HUN' ~ (n / 254)*100,
      CNT == 'POL' ~ (n / 238)*100,
      CNT == 'SVK' ~ (n / 271)*100
    ),
    p = glue::glue('({round(p, 2)}%)')
  ) %>% 
  unite(p, c('n','p'), sep = '  ') %>% 
  pivot_wider(names_from = quartiles_ineff, values_from = p)

# Create and plot a stacked bar chart showing the percentage distribution of schools by efficiency quartile and country.
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
    ), position = "fill"
  ) +
  labs(title = "Percentage of Fully Efficient (FE) and Quartile distribution\nof Inefficient Schools by Country",
       x = "Percentage",
       y = "Country")


################################################################################
## Analysis of Multipliers 



# Function to calculate the number of zero and non-zero values in each column
count_zeros_and_nonzeros <- function(matrix) {
  # Count non-zero values
  non_zero_counts <- colSums(matrix != 0)
  # Count zero values
  zero_counts <- colSums(matrix == 0)
  # Combine into a data frame for easier viewing
  # Count non-zero values
  non_zero_avg <- colMeans(matrix != 0)
  # Count zero values
  zero_avg <- colMeans(matrix == 0)
  
  return(data.frame(non_zero_counts, non_zero_avg, zero_counts, zero_avg))
}

# Apply the function to the 'multiplier_weights' for each country
# Replace 'multiplier_weights' with 'vy' or any other matrix you need to analyze
frequency_results <- dea_out_1 %>%
  mutate(
    frequency_counts = map(multiplier_weights, count_zeros_and_nonzeros)
  )

subjects <- c('science', 'reading', 'math')


frequency_results %>% 
  unnest(frequency_counts) %>% 
  mutate(output = subjects) %>% 
  pivot_longer(c(non_zero_counts,zero_counts), names_to = 'importance1', values_to = 'frequency') %>% 
  # pivot_longer(c(non_zero_avg,zero_avg), names_to = 'importance2', values_to = 'avg') %>%
  ggplot() +
  geom_col(
    aes(x = frequency, y = CNT, fill = output),
    position = 'fill'
  ) +
  # geom_col(
  #   aes(x = avg, y = CNT, fill = output),
  #   position = 'fill'
  # ) +
  facet_grid(~importance1)



frequency_results %>% 
  unnest(frequency_counts) %>% 
  mutate(output = subjects) %>% 
  # pivot_longer(c(non_zero_counts,zero_counts), names_to = 'importance1', values_to = 'frequency') %>% 
  pivot_longer(c(non_zero_avg,zero_avg), names_to = 'importance2', values_to = 'avg') %>% 
  ggplot() +
  geom_col(
    aes(x = avg, y = CNT, fill = output),
    position = 'dodge'
  ) +
  facet_grid(~importance2)




################################################################################
## intensity analysis


# Define a function to identify top 5 DMUs based on intensity weights
get_top_dmus <- function(intensity_matrix) {
  # Convert the matrix to a dataframe for easier handling
  df <- as.data.frame(intensity_matrix)
  # Get the sum of intensity weights for each DMU (column-wise)
  top_dmus <- colSums(df)
  # Sort DMUs by their summed intensity weight and get the names of the top 5
  names(sort(top_dmus, decreasing = TRUE))[1:5]
}

# Apply the function to each country and create a new column with top DMUs
dea_out_1 <- dea_out_1 %>%
  mutate(top_dmus = map(intensity_weights, get_top_dmus))

# Print the top DMUs for each country
dea_out_1 %>%
  select(CNT, top_dmus) %>%
  walk(~print(.x))

################################################################################
## Bootstrap 

# Function to extract necessary information from each boot object
extract_info <- function(boot_item) {
  # Extracting coefficients and their confidence intervals
  beta_hat <- boot_item$beta_hat_hat
  beta_ci_low <- boot_item$beta_ci[, 1]
  beta_ci_high <- boot_item$beta_ci[, 2]
  
  significance <- ifelse(beta_ci_low * beta_ci_high > 0, "*", "")  # '*' if CI does not include 0
  
  # Combine into a data frame
  data.frame(
    Coefficient = beta_hat,
    CI_Low = beta_ci_low,
    CI_High = beta_ci_high,
    Significance = significance
  )
}

# Apply the function to each element of the 'boot' list column and combine
coefficients_table <- dea_out_1 %>%
  pull(boot) %>%
  map_dfr(extract_info, .id = "CNT") %>% 
  rownames_to_column(var = 'variable')

# coefficients_table%>%
#   mutate(Variable = gsub("\\...\\d+$", "", rownames(.)), # Remove country index from variable names
#          Country = sub(".*\\...", "", rownames(.))) %>% # Extract country index as Country
#   select(-rowname) # Remove original rownames

# Pivot wider to make countries as columns
data_wide <- coefficients_table %>%
  pivot_wider(names_from = CNT, values_from = c(Coefficient, CI_Low, CI_High, Significance))

# Now, create the GT table
gt_table <- gt(data_wide) %>%
  tab_spanner(label = "Country 1", columns = vars(Coefficient_1, CI_Low_1, CI_High_1, Significance_1)) %>%
  tab_spanner(label = "Country 2", columns = vars(Coefficient_2, CI_Low_2, CI_High_2, Significance_2)) %>%
  tab_spanner(label = "Country 3", columns = vars(Coefficient_3, CI_Low_3, CI_High_3, Significance_3)) %>%
  tab_spanner(label = "Country 4", columns = vars(Coefficient_4, CI_Low_4, CI_High_4, Significance_4))



