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

#Subset Pacifica High school
df_pacifica <- data %>% 
  filter(q2_1 == "4")

#grade demographic
pacifica_grade <- df_pacifica %>% 
  mutate(q2_2 = case_when(
    q2_2 == '1' ~ 'Freshman',
    q2_2 == '2' ~ 'Sophomore',
    q2_2 == '3' ~ 'Junior',
    q2_2 == '4' ~ 'Senior',
    TRUE ~ q2_2
  ))

pacifica_grade <- pacifica_grade %>% 
  count(q2_2) %>% 
  mutate(percent = (n / nrow(pacifica_grade))*100)

#Gender Demographic
pacifica_gender <- df_pacifica %>% 
  mutate(q2_4 = case_when(
    q2_4 == '1' ~ 'Male',
    q2_4 == '2' ~ 'Female',
    q2_4 == '3' ~ 'Non-binary/Third Gender',
    q2_4 == '4' ~ 'Other',
    TRUE ~ q2_4
  ))

pacifica_gender <- pacifica_gender %>% 
  count(q2_4) %>% 
  mutate(percent = (n / nrow(pacifica_gender))*100)

#Higher Education Interest
pdf_interest_higher <- df_pacifica %>%
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

pdf_higher_interest <- pdf_interest_higher %>%
  count(q2_5, total_n) %>%
  mutate(percent = (n / total_n)*100)

#Career Path Interest
pdf_interest_career <- df_pacifica %>%
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

pdf_career_interest <- pdf_interest_career %>%
  count(q2_6, total_n) %>%
  mutate(percent = (n / total_n) * 100)

#Which part of admission process they are familiar with
pq3_1_separated <- df_pacifica %>%
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

pq3_1 <- pq3_1_separated %>%
  count(q3_1, total_n) %>%
  mutate(percent = (n / total_n) * 100)

sum(is.na(df_pacifica$q3_1))


#GPA Familiar 
pq3_2_separated <- df_pacifica %>%
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

pq3_2 <- pq3_2_separated %>%
  count(q3_2, total_n) %>%
  mutate(percent = (n / total_n) * 100)
sum(is.na(df_pacifica$q3_2))

## when thinking about attending college...
pq5_4_separated <- df_pacifica %>%
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

pq5_4 <- pq5_4_separated %>%
  count(q5_4, total_n) %>%
  mutate(percent = (n / total_n) * 100)

#Which parts of A-G they want to know more about 
df_pac <- data%>%
  filter(q2_1 == "4")

pq3_8_separated <- df_pac %>%
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

pq3_8 <- pq3_8_separated %>%
  count(q3_8, total_n) %>%
  mutate(percent = (n / total_n) * 100)

## item statistics for all
item <- data %>% select(q3_4, q3_5, q3_6, q4_2, q4_3, q5_1, q5_2, q5_3) 
counts <- item %>%
  pivot_longer(everything()) %>% 
  count(name,value) %>% 
  mutate(percent = n/nrow(data)*100)

item_pacifica <- df_pacifica %>% select(q3_4, q3_5, q3_6, q4_2, q4_3, q5_1, q5_2, q5_3)

counts_pacifica <- item_pacifica %>% 
  pivot_longer(everything()) %>% 
  count(name, value) %>% 
  mutate(percent = n / sum(n) *100)


# Set a common theme for all plots with removed unnecessary decorations
common_theme <- theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())

# Horizontal bar plot for Grade Demographic at Pacifica High School
ggplot(pacifica_grade, aes(x=reorder(q2_2, percent), y=percent, fill=q2_2)) +
  geom_bar(stat="identity", width = 0.5, fill="grey50") +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="Grade Demographic at Pacifica High School",
       y="Percentage of Students", x="Grade Level") +
  common_theme

# Horizontal bar plot for Gender Demographic
ggplot(pacifica_gender, aes(x=reorder(q2_4, percent), y=percent, fill=q2_4)) +
  geom_bar(stat="identity", width = 0.5, fill="grey50") +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="Gender Demographic at Pacifica High School",
       y="Percentage of Students", x="Gender") +
  common_theme

# Horizontal bar plot for Higher Education Interest
ggplot(pdf_higher_interest, aes(x=reorder(q2_5, percent), y=percent, fill=q2_5)) +
  geom_bar(stat="identity", width = 0.5, fill="grey50") +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="Higher Education Interest at Pacifica High School",
       y="Percentage of Students", x="Type of Institution") +
  common_theme

# Horizontal bar plot for Career Path Interest
ggplot(pdf_career_interest, aes(x=reorder(q2_6, percent), y=percent, fill=q2_6)) +
  geom_bar(stat="identity", width = 0.5, fill="grey50") +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="Career Path Interest at Pacifica High School",
       y="Percentage of Students", x="Career Path") +
  common_theme

# Set up for the themes and colors for consistent styling
theme_set(theme_minimal() +
            theme(legend.position = "none",
                  axis.text = element_text(color = "black"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.ticks = element_blank()))

# Visualizations for various survey responses
# Familiarity with Parts of the Admission Process
ggplot(pq3_1, aes(x=reorder(q3_1, -percent), y=percent)) +
  geom_bar(stat="identity", fill="grey50", width=0.7) +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="Parts of the Admission Process Students Are Familiar With for Pacifica High School",
       x="Topic", y="Percentage of Students")

# Familiarity with Types of GPA
ggplot(pq3_2, aes(x=reorder(q3_2, -percent), y=percent)) +
  geom_bar(stat="identity", fill="grey50", width=0.7) +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="Types of GPA Students Are Familiar With For Pacifica High school",
       x="Types of GPA", y="Percentage of Students")

# Feelings When Thinking About Attending College
ggplot(pq5_4, aes(x=reorder(q5_4, -percent), y=percent)) +
  geom_bar(stat="identity", fill="grey50", width=0.7) +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="Feelings When Thinking About Attending College for Pacifica High School",
       x="Feelings", y="Percentage of Students")

# Parts of A-G Requirements Students Want to Know More About
ggplot(pq3_8, aes(x=reorder(q3_8, -percent), y=percent)) +
  geom_bar(stat="identity", fill="grey50", width=0.7) +
  coord_flip() +
  geom_text(aes(label=sprintf("%.1f%%", percent)), hjust=-0.1, size=2.5) +
  labs(title="A-G Requirements That Students Want to Know More About for Pacifica High School",
       x="Subjects", y="Percentage of Students")

# Execute these plotting commands in your R environment to generate the visuals.

