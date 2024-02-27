################################################################################


pacman::p_load(tidyverse, Benchmarking)

data_full <- readr::read_csv('clean_data/clean_united_data.csv')



nested_data <- data_full %>%
  group_by(CNT) %>%
  nest() %>%
  mutate(
    inputs = map(data, ~ {
      df_selected <- .x %>%
        select(
          mean_reading_attitude, 
          mean_science_attitude, 
          mean_math_attitude,
          student_behavior_issue_mean,
          teacher_behavior_issue_mean,
          # resources_issue_mean,
          # staff_issues_mean,
          # total_homework_time,
          # total_class_periods,
          STRATIO,
          SCHSIZE
        )
      mat <- as.matrix(df_selected)
      colnames(mat) <- colnames(df_selected)  # Ensure column names are preserved
      mat
    }),
    outputs = map(data, ~ {
      df_selected <- .x %>%
        select(
          mean_PV_science, 
          mean_PV_reading, 
          mean_PV_math
        )
      mat <- as.matrix(df_selected)
      colnames(mat) <- colnames(df_selected)  # Ensure column names are preserved
      mat
    })
  ) %>%
  select(-data)


dea_out_1 <- nested_data %>% 
  mutate(
    dea_out = map2(
      inputs, outputs, 
      function(x,y){
          d <- dea(X = x, Y = y, RTS = "vrs", ORIENTATION = "out", DUAL = T)
        return(d)
      }
    )
  )

# dea_out_1 %>% 
#   write_rds(here::here("results", "dea_out_1.rds"))



