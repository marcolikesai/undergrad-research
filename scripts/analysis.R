library(janitor)
library(dplyr)
library(visdat)
library(psych)
library(tidyr)
library(ggplot2)


data <- read.csv("EAOP A-G Validation Evaluation_April 8, 2024_11.42.csv") %>%
  clean_names() %>%
  mutate_all(~na_if(., "")) %>%
  filter(q1_4 == "1")



# subset hueneme
df_hueneme <- data %>%
  filter(q2_1 == "3")

#grade demographic
hue_grade <- df_hueneme %>%
  mutate(q2_2 = case_when(
    q2_2 == '1' ~ 'Freshman',
    q2_2 == '2' ~ 'Sophomore',
    q2_2 == '3' ~ 'Junior',
    q2_2 == '4' ~ 'Senior',
    TRUE ~ q2_2
  ))

hue_grade <- hue_grade %>%
  count(q2_2) %>%
  mutate(percent = (n / nrow(hue_grade))*100)


# gender demographic
hue_gender <- df_hueneme %>% 
  mutate(q2_4 = case_when(
    q2_4 == '1' ~ 'Male',
    q2_4 == '2' ~ 'Female',
    q2_4 == '3' ~ 'Non-binary/Third Gender',
    q2_4 == '4' ~ 'Other',
    TRUE ~ q2_4
  ))
hue_gender <- hue_gender %>% 
  count(q2_4) %>%
  mutate(percent = (n / nrow(hue_gender))*100)

## higher education interest
df_interest_higher <- df_hueneme %>%
  drop_na(q2_5) %>%
  mutate(row_id = row_number(),
         total_n = n_distinct(row_id)) %>%
  separate_rows(q2_5, sep = ",") %>%
  select(row_id, q2_5,total_n) %>%
  mutate(q2_5 = case_when(
    q2_5 == "1" ~ "California Community Colleges",
    q2_5 == "2" ~ "California State University",
    q2_5 == "3" ~ "University of California",
    q2_5 == "4" ~ "Out of state college or university",
    q2_5 == "5" ~ "Other",
    TRUE ~ q2_5  # If none of the above conditions match, keep the original value
  ))

df_higher_interest <- df_interest_higher %>%
  count(q2_5, total_n) %>%
  mutate(percent = (n / total_n)*100)

## career path interest
df_interest_career <- df_hueneme %>%
  drop_na(q2_6) %>%
  mutate(row_id = row_number(),
         total_n = n_distinct(row_id)) %>%
  separate_rows(q2_6, sep = ",") %>%
  select(row_id, q2_6,total_n) %>%
  mutate(q2_6 = case_when(
    q2_6 == "1" ~ "Education",
    q2_6 == "2" ~ "Computing and engineering",
    q2_6 == "3" ~ "Health",
    q2_6 == "4" ~ "Business and entrepreneurship",
    q2_6 == "5" ~ "Other",
    TRUE ~ q2_6  # If none of the above conditions match, keep the original value
  ))

df_career_interest <- df_interest_career %>%
  count(q2_6, total_n) %>%
  mutate(percent = (n / total_n) * 100)

## which part of admission process they are familiar with
q3_1_separated <- df_hueneme %>%
  drop_na(q3_1) %>%
  mutate(row_id = row_number(),
         total_n = n_distinct(row_id)) %>%
  separate_rows(q3_1, sep = ",") %>%
  select(row_id, q3_1,total_n) %>%
  mutate(q3_1 = case_when(
    q3_1 == "1" ~ "A-G requirements",
    q3_1 == "2" ~ "PIQ/Personal Statements",
    q3_1 == "3" ~ "Financial Aid",
    q3_1 == "4" ~ "AP courses",
    q3_1 == "5" ~ "CC Dual Enrollment",
    q3_1 == "6" ~ "Extracurricular activities",
    q3_1 == "7" ~ "Community service",
    q3_1 == "8" ~ "Other",
    TRUE ~ q3_1  # If none of the above conditions match, keep the original value
  ))

q3_1 <- q3_1_separated %>%
  count(q3_1, total_n) %>%
  mutate(percent = (n / total_n) * 100)

sum(is.na(df_hueneme$q3_1))

## GPA familiar
q3_2_separated <- df_hueneme %>%
  drop_na(q3_2) %>%
  mutate(row_id = row_number(),
         total_n = n_distinct(row_id)) %>%
  separate_rows(q3_2, sep = ",") %>%
  select(row_id, q3_2,total_n) %>%
  mutate(q3_2 = case_when(
    q3_2 == "1" ~ "UC/CSU GPA",
    q3_2 == "2" ~ "5.0 GPA",
    q3_2 == "3" ~ "4.0 GPA",
    q3_2 == "4" ~ "Cal Grant GPA",
    TRUE ~ q3_2  # If none of the above conditions match, keep the original value
  ))

q3_2 <- q3_2_separated %>%
  count(q3_2, total_n) %>%
  mutate(percent = (n / total_n) * 100)
sum(is.na(df_hueneme$q3_2))

## when thinking about attending college...
q5_4_separated <- df_hueneme %>%
  drop_na(q5_4) %>%
  mutate(row_id = row_number(),
         total_n = n_distinct(row_id)) %>%
  separate_rows(q5_4, sep = ",") %>%
  select(row_id, q5_4,total_n) %>%
  mutate(q5_4 = case_when(
    q5_4 == "1" ~ "eager/excited",
    q5_4 == "2" ~ "optimistic",
    q5_4 == "3" ~ "hopeful",
    q5_4 == "4" ~ "challenged",
    q5_4 == "5" ~ "nervous",
    q5_4 == "6" ~ "worried",
    q5_4 == "7" ~ "other",
    TRUE ~ q5_4  # If none of the above conditions match, keep the original value
  ))

q5_4 <- q5_4_separated %>%
  count(q5_4, total_n) %>%
  mutate(percent = (n / total_n) * 100)

## which parts of A-G they want to know more about
df_hue <- data%>%
  filter(q2_1 == "3")

q3_8_separated <- df_hue %>%
  drop_na(q3_8) %>%
  mutate(row_id = row_number(),
         total_n = n_distinct(row_id)) %>%
  separate_rows(q3_8, sep = ",") %>%
  select(row_id, q3_8,total_n) %>%
  mutate(q3_8 = case_when(
    q3_8 == "1" ~ "history",
    q3_8 == "2" ~ "english",
    q3_8 == "3" ~ "math",
    q3_8 == "4" ~ "science",
    q3_8 == "5" ~ "foreign language",
    q3_8 == "6" ~ "visual and performing arts",
    q3_8 == "7" ~ "college prep electives",
    TRUE ~ q3_8  # If none of the above conditions match, keep the original value
  ))

q3_8 <- q3_8_separated %>%
  count(q3_8, total_n) %>%
  mutate(percent = (n / total_n) * 100)

## item statistics for all
item <- data %>% select(q3_4, q3_5, q3_6, q4_2, q4_3, q5_1, q5_2, q5_3) 
counts <- item %>%
  pivot_longer(everything()) %>% 
  count(name,value) %>% 
  mutate(percent = n/nrow(data)*100)

