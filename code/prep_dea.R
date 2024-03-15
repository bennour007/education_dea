################################################################################


pacman::p_load(tidyverse, Benchmarking, rDEA, gt)

# source(here::here('code', 'data_prep.R'))
# rm(list = ls())
# source(here::here('code', 'join.R'))
# rm(list = ls())

data_full <- readr::read_csv('clean_data/clean_united_data.csv')


nested_data <- data_full %>%
  na.omit() %>% 
  group_by(CNT) %>%
  nest() %>%
  mutate(
    inputs = map(data, ~ {
      df_selected <- .x %>%
        select(
          CPERIODS,    # renamed from total_class_periods
          STRATIO      # STRATIO remains unchanged as it's commented out in the rename list
        )
      mat <- as.matrix(df_selected)
      colnames(mat) <- colnames(df_selected)  # Ensure column names are preserved
      mat
    }),
    outputs = map(data, ~ {
      df_selected <- .x %>%
        select(
          SCIENCE,    # renamed from mean_PV_science
          READING,    # renamed from mean_PV_reading
          MATH        # renamed from mean_PV_math
        )
      mat <- as.matrix(df_selected)
      colnames(mat) <- colnames(df_selected)  # Ensure column names are preserved
      mat
    }),
    env = map(data, ~ {
      df_selected <- .x %>%
        select(
          STUBI,    # renamed from student_behavior_issue_mean
          TEABI,    # renamed from teacher_behavior_issue_mean
          RESSI,    # renamed from resources_issue_mean
          STAFI,    # renamed from staff_issues_mean
          COMP,     # renamed from SC011Q01TA
          SCSIZE,   # renamed from SCHSIZE
          LOC,      # renamed from SC001Q01TA
          TOTAT     # TOTAT remains unchanged as it's commented out in the rename list
          # Other variables if needed
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
          d <- Benchmarking::dea(X = x, Y = y, RTS = "vrs", ORIENTATION = "out", DUAL = T)
        return(d)
      }
    ),
    boot = pmap(
      list(inputs, outputs, env),
      function(x,y,z){
        b <- rDEA::dea.env.robust (X = x, Y  = y , Z = z, model = 'output', RTS="variable",
                             L1=100, L2=2000, alpha=0.05)
        return(b)
      }
    )
  )






dea_out_1 <- dea_out_1 %>%
  mutate(
    efficiencies = map(dea_out, ~pluck(.x, "eff")),
    multiplier_weights = map(dea_out, ~pluck(.x, "vy")),
    intensity_weights = map(dea_out, ~pluck(.x, "lambda"))
  )


dea_out_1 %>%
  write_rds(here::here("results", "dea_out_1.rds"))


# nested_data <- data_full %>%
#   na.omit() %>% 
#   group_by(CNT) %>%
#   nest() %>%
#   mutate(
#     inputs = map(data, ~ {
#       df_selected <- .x %>%
#         select(
#           total_class_periods,
#           STRATIO
#           # mean_reading_attitude, 
#           # mean_science_attitude, 
#           # mean_math_attitude,
#         )
#       mat <- as.matrix(df_selected)
#       colnames(mat) <- colnames(df_selected)  # Ensure column names are preserved
#       mat
#     }),
#     outputs = map(data, ~ {
#       df_selected <- .x %>%
#         select(
#           mean_PV_science, 
#           mean_PV_reading, 
#           mean_PV_math
#         )
#       mat <- as.matrix(df_selected)
#       colnames(mat) <- colnames(df_selected)  # Ensure column names are preserved
#       mat
#     }),
#     env = map(data, ~ {
#       df_selected <- .x %>%
#         select(
#           student_behavior_issue_mean,
#           teacher_behavior_issue_mean,
#           resources_issue_mean,
#           staff_issues_mean,
#           # total_homework_time, #I dont think this is necessary
#           # total_class_periods,
#           SC011Q01TA, # number of schools in the area (3 none, 2 one school, 1 two ore more schools)
#           # STRATIO,
#           SCHSIZE,
#           SC001Q01TA, # Community type where the school is located
#           TOTAT # Total number of all teachers at the school
#         )
#       mat <- as.matrix(df_selected)
#       colnames(mat) <- colnames(df_selected)  # Ensure column names are preserved
#       mat
#     })
#     
#   ) %>%
#   select(-data)