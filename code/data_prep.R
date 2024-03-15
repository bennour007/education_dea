## Import libraries

library(tidyverse)
library(haven)

## Import data

schools <- read_sav('../SCH_QQQ_SPSS/CY08MSP_SCH_QQQ.sav') %>% 
  filter(CNT %in% c('HUN', 'POL', 'CZE', 'SVK'))

students <- read_sav('../STU_QQQ_SPSS/CY08MSP_STU_QQQ.sav')%>% 
  filter(CNT %in% c('HUN', 'POL', 'CZE', 'SVK'))

## Getting labels and searching for the right ones.[focus on the students data first]

labels_students <- sapply(names(students), function(x) attr(students[[x]], "label"))
labels_schools <- sapply(names(schools), function(x) attr(schools[[x]], "label"))


# tibble(var_name = names(labels_students), label = unlist(labels_students, use.names = FALSE)[1:1279]) %>% write_csv('tmp.csv')
# tibble(var_name = names(labels_schools), label = unlist(labels_schools, use.names = FALSE)[1:432]) %>% write_csv('tmp.csv')

#################################################################################
#################################################################################
########################### PREPARE STUDENT DATA CLEAN ##########################
#################################################################################
#################################################################################
## subjects
# Variables organized by subject: Science, Math, Reading
vars <- c(
  # location id
  "CNT", "CNTSCHID", "REGION", "STRATUM",
  # Total number of class periods per week for all subjects, including mathematics
  "ST059Q02JA", 
  # How much time spent on homework in total for all subjects, including subjects not listed above
  "ST296Q04JA", 
  # Plausible Values in Mathematics
  "PV1MATH", "PV2MATH", "PV3MATH", "PV4MATH", "PV5MATH",
  "PV6MATH", "PV7MATH", "PV8MATH", "PV9MATH", "PV10MATH",
  # Plausible Values in Reading
  "PV1READ", "PV2READ", "PV3READ", "PV4READ", "PV5READ",
  "PV6READ", "PV7READ", "PV8READ", "PV9READ", "PV10READ",
  # Plausible Values in Science
  "PV1SCIE", "PV2SCIE", "PV3SCIE", "PV4SCIE", "PV5SCIE",
  "PV6SCIE", "PV7SCIE", "PV8SCIE", "PV9SCIE", "PV10SCIE",
  # Mathematics Attitudes
  "ST268Q01JA", # Agree/disagree: Mathematics is one of my favourite subjects
  "ST268Q04JA", # Agree/disagree: Mathematics is easy for me
  "ST268Q07JA", # Agree/disagree: I want to do well in my mathematics class
  # Science Attitudes
  "ST268Q03JA", # Agree/disagree: Science is one of my favourite subjects
  "ST268Q06JA", # Agree/disagree: Science is easy for me
  "ST268Q09JA", # Agree/disagree: I want to do well in my science class
  # Reading Attitudes
  "ST268Q02JA", # Agree/disagree: Test language is one of my favourite subjects
  "ST268Q05JA", # Agree/disagree: Test language is easy for me
  "ST268Q08JA"  # Agree/disagree: I want to do well in my test language class
  )

tmps  <- c(
  # Plausible Values in Mathematics
  "PV1MATH", "PV2MATH", "PV3MATH", "PV4MATH", "PV5MATH",
  "PV6MATH", "PV7MATH", "PV8MATH", "PV9MATH", "PV10MATH",
  # Plausible Values in Reading
  "PV1READ", "PV2READ", "PV3READ", "PV4READ", "PV5READ",
  "PV6READ", "PV7READ", "PV8READ", "PV9READ", "PV10READ",
  # Plausible Values in Science
  "PV1SCIE", "PV2SCIE", "PV3SCIE", "PV4SCIE", "PV5SCIE",
  "PV6SCIE", "PV7SCIE", "PV8SCIE", "PV9SCIE", "PV10SCIE",
  # Mathematics Attitudes
  "ST268Q01JA", # Agree/disagree: Mathematics is one of my favourite subjects
  "ST268Q04JA", # Agree/disagree: Mathematics is easy for me
  "ST268Q07JA", # Agree/disagree: I want to do well in my mathematics class
  # Science Attitudes
  "ST268Q03JA", # Agree/disagree: Science is one of my favourite subjects
  "ST268Q06JA", # Agree/disagree: Science is easy for me
  "ST268Q09JA", # Agree/disagree: I want to do well in my science class
  # Reading Attitudes
  "ST268Q02JA", # Agree/disagree: Test language is one of my favourite subjects
  "ST268Q05JA", # Agree/disagree: Test language is easy for me
  "ST268Q08JA"  # Agree/disagree: I want to do well in my test language class
)

students %>% 
  select(all_of(vars)) %>% 
  na.omit() %>% 
  mutate(
    mean_math_attitude = rowMeans(select(., ST268Q01JA, ST268Q04JA, ST268Q07JA), na.rm = TRUE),
    mean_science_attitude = rowMeans(select(., ST268Q03JA, ST268Q06JA, ST268Q09JA), na.rm = TRUE),
    mean_reading_attitude = rowMeans(select(., ST268Q02JA, ST268Q05JA, ST268Q08JA), na.rm = TRUE),
    mean_PV_math = rowMeans(select(., ends_with('MATH')), na.rm = TRUE),
    mean_PV_reading = rowMeans(select(., ends_with('READ')), na.rm = TRUE),
    mean_PV_science = rowMeans(select(., ends_with('SCIE')), na.rm = TRUE)
  ) %>% 
  rename(
    total_class_periods = ST059Q02JA, 
    total_homework_time = ST296Q04JA
  ) %>% 
  # select(- all_of(tmps))
  write_csv('clean_data/studens.csv')





  


#################################################################################
#################################################################################
########################### PREPARE SCHOOL DATA CLEAN ###########################
#################################################################################
#################################################################################
  
school_issues <- c(
  # Learning Hindrances Related to Student Behavior
  "SC061Q01TA", # Learning hindered by student truancy
  "SC061Q02TA", # Learning hindered by students skipping classes
  "SC061Q03TA", # Learning hindered by students lacking respect for teachers
  "SC061Q04TA", # Learning hindered by student use of alcohol or illegal drugs
  "SC061Q05TA", # Learning hindered by students intimidating or bullying other students
  # Learning Hindrances Related to Teacher Behavior
  "SC061Q06TA", # Learning hindered by teachers not meeting individual students' needs
  "SC061Q07TA", # Learning hindered by teacher absenteeism
  "SC061Q08TA", # Learning hindered by staff resisting change
  "SC061Q09TA", # Learning hindered by teachers being too strict with students
  "SC061Q10TA", # Learning hindered by teachers not being well prepared for classes
  # Instructional Hindrances Related to Infrastructure and Resources
  ### staff
  "SC017Q01NA", # Instruction hindered by a lack of teaching staff
  "SC017Q02NA", # Instruction hindered by inadequate or poorly qualified teaching staff
  "SC017Q03NA", # Instruction hindered by a lack of assisting staff
  "SC017Q04NA", # Instruction hindered by inadequate or poorly qualified assisting staff
  ### infra
  "SC017Q05NA", # Instruction hindered by a lack of educational material (e.g., textbooks, IT equipment)
  "SC017Q06NA", # Instruction hindered by inadequate or poor-quality educational material
  "SC017Q07NA", # Instruction hindered by a lack of physical infrastructure (e.g., buildings, heating/cooling)
  "SC017Q08NA"  # Instruction hindered by inadequate or poor-quality physical infrastructure
)

school_data_vars <- c(
  "SC001Q01TA", # Community type where the school is located
  "SC011Q01TA", # number of schools in the area (3 none, 2 one school, 1 two ore more schools)
  "SCHSIZE",    # Total number of students enrolled in the school
  "SC002Q01TA", # Total school enrollment - Number of boys
  "SC002Q02TA", # Total school enrollment - Number of girls
  "STRATIO",    # Student-teacher ratio
  "TOTAT"       # Total number of all teachers at the school
)

schools %>% 
  mutate(
    # Calculate the mean for each group of variables
    student_behavior_issue_mean = rowMeans(select(., c("SC061Q01TA", "SC061Q02TA", "SC061Q03TA", "SC061Q04TA", "SC061Q05TA")), na.rm = T),
    teacher_behavior_issue_mean = rowMeans(select(., c("SC061Q06TA", "SC061Q07TA", "SC061Q08TA", "SC061Q09TA", "SC061Q10TA")), na.rm = T),
    resources_issue_mean = rowMeans(select(., c(
      "SC017Q05NA", # Instruction hindered by a lack of educational material (e.g., textbooks, IT equipment)
      "SC017Q06NA", # Instruction hindered by inadequate or poor-quality educational material
      "SC017Q07NA", # Instruction hindered by a lack of physical infrastructure (e.g., buildings, heating/cooling)
      "SC017Q08NA"  # Instruction hindered by inadequate or poor-quality physical infrastructure
    )), na.rm = T),
    staff_issues_mean = rowMeans(select(., c(
      "SC017Q01NA", # Instruction hindered by a lack of teaching staff
      "SC017Q02NA", # Instruction hindered by inadequate or poorly qualified teaching staff
      "SC017Q03NA", # Instruction hindered by a lack of assisting staff
      "SC017Q04NA", # Instruction hindered by inadequate or poorly qualified assisting staff
    )), na.rm = T)
  ) %>% 


# schools %>% 
#   mutate(
#     # Calculate the mean for each group of variables
#     student_behavior_issue_mean = rowMeans(select(., c("SC061Q01TA", "SC061Q02TA", "SC061Q03TA", "SC061Q04TA", "SC061Q05TA")), na.rm = T),
#     teacher_behavior_issue_mean = rowMeans(select(., c("SC061Q06TA", "SC061Q07TA", "SC061Q08TA", "SC061Q09TA", "SC061Q10TA")), na.rm = T),
#     resources_issue_mean = rowMeans(select(., c("SC017Q01NA", "SC017Q02NA", "SC017Q05NA", "SC017Q06NA")), na.rm = T),
#     staff_issues_mean = rowMeans(select(., c("SC017Q03NA", "SC017Q04NA", "SC017Q07NA", "SC017Q08NA")), na.rm = T)
#   ) %>% 
  select(
    CNT, CNTSCHID, REGION, STRATUM,
    all_of(school_data_vars), 
    student_behavior_issue_mean ,
    teacher_behavior_issue_mean ,
    resources_issue_mean ,
    staff_issues_mean,
    all_of(school_issues)
  ) %>% 
  write_csv('clean_data/schools.csv')


