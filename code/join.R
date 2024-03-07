stu <- read_csv('clean_data/studens.csv')
sch <- read_csv('clean_data/schools.csv')

school_data_vars <- c(
  "SC001Q01TA", # Community type where the school is located
  "SC011Q01TA", # number of schools in the area (3 none, 2 one school, 1 two ore more schools)
  "SCHSIZE",    # Total number of students enrolled in the school
  "SC002Q01TA", # Total school enrollment - Number of boys
  "SC002Q02TA", # Total school enrollment - Number of girls
  "STRATIO",    # Student-teacher ratio
  "TOTAT"       # Total number of all teachers at the school
)

data_full <- stu %>% 
  group_by(CNTSCHID) %>% 
  summarise(
    across(
      total_class_periods:mean_PV_science,
      mean, na.rm = T
    )
  ) %>% 
  left_join(
    sch
  ) %>% 
  select(
    CNT, CNTSCHID, REGION, STRATUM,
    all_of(school_data_vars), 
    student_behavior_issue_mean ,
    teacher_behavior_issue_mean ,
    resources_issue_mean ,
    staff_issues_mean,
    total_homework_time, 
    total_class_periods, 
    mean_PV_science, 
    mean_PV_reading, 
    mean_PV_math, 
    mean_reading_attitude, 
    mean_science_attitude, 
    mean_math_attitude
  ) 
  # readr::write_csv(data_full,'clean_data/clean_united_data.csv')
