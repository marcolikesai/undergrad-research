library(tidyverse)
library(janitor)

rm(list = ls())

df_1 <- read_csv("EAOP A-G Validation Evaluation_6_5_24.csv") %>%
  clean_names() %>%
  select(18:125) %>%
  mutate(q2_1 = case_when(
    q2_1 == "1" ~ "Channel Islands High School",
    q2_1 == "3" ~ "Hueneme High School",
    q2_1 == "4" ~ "Pacifica High School",
    q2_1 == "6" ~ "Rio Mesa High School",
    TRUE ~ q2_1  # Keep unchanged if not matched
  )) %>%
  mutate(q2_4 = case_when(
    q2_4 == "1" ~ "Male",
    q2_4 == "2" ~ "Female",
    q2_4 == "3" ~ "Non-binary/third gender",
    q2_4 == "4" ~ "Other",
    TRUE ~ q2_4  # Keep unchanged if not matched
  )) %>%
  filter((q1_4 == "1" | q1_3 == "1"),
         q2_1 != "10")
table(df_1$q2_1)

tc_colors <- c("darkgoldenrod1",  "royalblue4",  "darkcyan", "lightcoral",  "burlywood4", "goldenrod3", "steelblue4", "lightcyan2", "lightpink2", "navajowhite3", "steelblue3", "goldenrod2", "lightcyan1", "thistle2", "antiquewhite3")

df_1 %>%
  drop_na(q2_1) %>%
  count(q2_1) %>%
  arrange(n) %>%
  ggplot(aes(x = reorder(q2_1, n), y = n, label = n)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -.1, fontface = "bold"
  ) +  # Add text labels for counts
  labs(
    title = "Student respondents by school",
    x = "College site",
    y = "Number of Students"
  ) +
  scale_fill_manual(values = tc_colors) +  # Set custom fill colors
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 14, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  )+
  coord_flip()

df_1 %>%
  drop_na(q2_1) %>%
  count(q2_1, q2_4) %>%
  arrange(n) %>%
  ggplot(aes(x = reorder(q2_1, n), y = n, label = n, fill = q2_4)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -.1, fontface = "bold"
  ) +  # Add text labels for counts
  labs(
    title = "Student respondents by school",
    x = "College site",
    y = "Number of Students",
    fill = "Gender"
  ) +
  scale_fill_manual(values = tc_colors) +  # Set custom fill colors
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 14, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  )+
  coord_flip()

df_1 %>%
  drop_na(q2_2) %>%
  count(q2_2, q2_1) %>%
  arrange(n) %>%
  mutate(q2_2 = case_when(
    q2_2 == 1 ~ "Freshman",
    q2_2 == 2 ~ "Sophomore",
    q2_2 == 3 ~ "Junior",
    q2_2 == 4 ~ "Senior",
    TRUE ~ as.character(q2_2)  # Keep unchanged if not matched
  )) %>%
  ggplot(aes(x = reorder(q2_1, n), y = n, label = n, fill =q2_2)) +  # Change fill aesthetic to q2_1
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student respondents by grade level",
    x = "Response",
    y = "Number of Students",
    fill = "College site"
  ) +
  scale_fill_manual(values = tc_colors) +  # Set custom fill colors
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )+
  coord_flip()

#q2_5

df_1_q2_5 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q2_5"),
    names_to = "q2_5",
    values_to = "response_q2_5",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q2_5, response_q2_5, everything()) %>% # Adjust as needed to include additional columns if necessary
  
  filter(q2_5 != "q2_5_5_text") %>%
  mutate(q2_5 = case_when(
    q2_5 == "q2_5_1" ~ "California community colleges",
    q2_5 == "q2_5_2" ~ "California State Universities",
    q2_5 == "q2_5_3" ~ "University of California",
    q2_5 == "q2_5_4" ~ "Out of state college or university",
    q2_5 == "q2_5_5" ~ "Other",
    TRUE ~ q2_5  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q2_5 %>%
  drop_na(q2_1) %>%
  count(q2_1, q2_5) %>%
  arrange(n)

# Plot q2_5 by q2_1
ggplot(counts, aes(x = reorder(q2_1, n), y = n, fill = q2_5)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student Interest in Higher Education Systems by School",
    x = "College site",
    y = "Number of Students",
    fill = "Higher Education System"
  ) +
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) +
  coord_flip()+
  
  scale_fill_manual(values = tc_colors)

#q3_1
df_1_q3_1 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_1"),
    names_to = "q3_1",
    values_to = "response_q3_1",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q3_1, response_q3_1, everything()) %>% # Adjust as needed to include additional columns if necessary
  
  filter(q3_1 != "q3_1_5_text") %>%
  mutate(q3_1 = case_when(
    q3_1 == "q3_1_1" ~ "A-G requirements",
    q3_1 == "q3_1_2" ~ "Personal insight questions/ personal statements",
    q3_1 == "q3_1_3" ~ "Financial aid (FAFSA, CalGrant, Pell grant, etc.)",
    q3_1 == "q3_1_4" ~ "AP courses",
    q3_1 == "q3_1_5" ~ "Community college dual enrollment",
    q3_1 == "q3_1_6" ~ "Extra curricular activies",
    q3_1 == "q3_1_7" ~ "Community service",
    q3_1 == "q3_1_8" ~ "Other",
    TRUE ~ q3_1  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q3_1 %>%
  filter(q3_1 != "q3_1_8_text") %>%
  drop_na(q2_1) %>%
  count(q2_1, q3_1) %>%
  arrange(n)

ggplot(counts, aes(x = reorder(q2_1, n), y = n, 
fill = stringr::str_wrap(q3_1, width = 20))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Parts of the admission process students are familiar with by college site",
    x = "College site",
    y = "Number of Students",
    fill = "Admission topic"
  ) +
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)

#q3_2
df_1_q3_2 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_2"),
    names_to = "q3_2",
    values_to = "response_q3_2",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q3_2, response_q3_2, everything()) %>% # Adjust as needed to include additional columns if necessary
  
  filter(q3_2 != "q3_2_5_text") %>%
  mutate(q3_2 = case_when(
    q3_2 == "q3_2_1" ~ "UC/CSU GPA",
    q3_2 == "q3_2_2" ~ "5.0 GPA",
    q3_2 == "q3_2_3" ~ "4.0 GPA",
    q3_2 == "q3_2_4" ~ "Cal Grant GPA",
    TRUE ~ q3_2  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q3_2 %>%
  filter(q3_2 != "q3_2_8_text") %>%
  drop_na(q2_1) %>%
  count(q2_1, q3_2) %>%
  arrange(n)

ggplot(counts, aes(x = reorder(q2_1, n), y = n, fill = stringr::str_wrap(q3_2, width = 20))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "GPA types students are familiar with by college site",
    x = "College site",
    y = "Number of Students",
    fill = "GPA type"
  ) +
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)

#q3_3
df_1 %>%
  drop_na(q3_3, q2_1) %>%
  count(q3_3, q2_1) %>%
  complete(q3_3, q2_1, fill = list(n = 0)) %>%
  arrange(n) %>%
  mutate(
    q3_3 = factor(q3_3, levels = c(1, 2, 3, 4, 5),
                  labels = c("Strongly disagree", "Somewhat disagree", 
                             "Neither agree nor disagree", "Somewhat agree", 
                             "Strongly agree"))
  ) %>%
  ggplot(aes(x = reorder(q2_1, n), y = n, label = n, fill = q3_3)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student familiarity with UC/CSU GPA calculation method by college site",
    x = "Student rating",
    y = "Number of Students",
    fill = "College site"
  ) +
  scale_fill_manual(values = tc_colors) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip()

#q3_4
df_1 %>%
  drop_na(q3_4, q2_1) %>%
  count(q3_4, q2_1) %>%
  complete(q3_4, q2_1, fill = list(n = 0)) %>%
  arrange(n) %>%
  mutate(
    q3_4 = factor(q3_4, levels = c(1, 2, 3, 4, 5),
                  labels = c("Strongly disagree", "Somewhat disagree", 
                             "Neither agree nor disagree", "Somewhat agree", 
                             "Strongly agree"))
  ) %>%
  ggplot(aes(x = reorder(q2_1, n), y = n, label = n, fill = q3_4)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student familiarity with GPA calculation method by college site",
    x = "College site",
    y = "Number of Students",
    fill = "Student rating"
  ) +
  scale_fill_manual(values = tc_colors) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip()

#q3_6
df_1 %>%
  drop_na(q3_6, q2_1) %>%
  count(q3_6, q2_1) %>%
  complete(q3_6, q2_1, fill = list(n = 0)) %>%
  arrange(n) %>%
  mutate(
    q3_6 = factor(q3_6, levels = c(1, 2, 3, 4, 5),
                  labels = c("Strongly disagree", "Somewhat disagree", 
                             "Neither agree nor disagree", "Somewhat agree", 
                             "Strongly agree"))
  ) %>%
  ggplot(aes(x = reorder(q2_1, n), y = n, label = n, fill =q3_6)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student rating of familiarity of personal A-G status by college site",
    x = "College site",
    y = "Number of Students",
    fill = "Student rating"
  ) +
  scale_fill_manual(values = tc_colors) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip()

#q3_7
df_1_q3_7 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_7"),
    names_to = "q3_7",
    values_to = "response_q3_7",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q3_7, response_q3_7, everything()) %>% # Adjust as needed to include additional columns if necessary
  
  filter(q3_7 != "q3_7_5_text") %>%
  mutate(q3_7 = case_when(
    q3_7 == "q3_7_1" ~ "History",
    q3_7 == "q3_7_2" ~ "English",
    q3_7 == "q3_7_3" ~ "Math",
    q3_7 == "q3_7_4" ~ "Science",
    q3_7 == "q3_7_5" ~ "Language other than English",
    q3_7 == "q3_7_6" ~ "Visual and performing arts",
    q3_7 == "q3_7_7" ~ "College preparatory elective",
    TRUE ~ q3_7  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q3_7 %>%
  filter(q3_7 != "q3_7_8_text") %>%
  drop_na(q2_1) %>%
  count(q2_1, q3_7) %>%
  arrange(n)

ggplot(counts, aes(x = reorder(q2_1, n), y = n, fill = stringr::str_wrap(q3_7, width = 20))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "A-G requirements students felt familiar with by college site",
    x = "College site",
    y = "Number of Students",
    fill = "A-G requirement"
  ) +
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)

#q3_8
df_1_q3_8 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_8"),
    names_to = "q3_8",
    values_to = "response_q3_8",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q3_8, response_q3_8, everything()) %>% # Adjust as needed to include additional columns if necessary
  
  filter(q3_8 != "q3_8_5_text") %>%
  mutate(q3_8 = case_when(
    q3_8 == "q3_8_1" ~ "History",
    q3_8 == "q3_8_2" ~ "English",
    q3_8 == "q3_8_3" ~ "Math",
    q3_8 == "q3_8_4" ~ "Science",
    q3_8 == "q3_8_5" ~ "Language other than English",
    q3_8 == "q3_8_6" ~ "Visual and performing arts",
    q3_8 == "q3_8_7" ~ "College preparatory elective",
    TRUE ~ q3_8  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q3_8 %>%
  filter(q3_8 != "q3_8_8_text") %>%
  drop_na(q2_1) %>%
  count(q2_1, q3_8) %>%
  arrange(n)

ggplot(counts, aes(x = reorder(q2_1, n), y = n, fill = stringr::str_wrap(q3_8, width = 20))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "A-G requirements students wanted more information about by college site",
    x = "College site",
    y = "Number of Students",
    fill = "A-G requirement"
  ) +
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)



#q4_2
df_1 %>%
  drop_na(q4_2, q2_1) %>%
  count(q4_2, q2_1) %>%
  complete(q4_2, q2_1, fill = list(n = 0)) %>%
  arrange(n) %>%
  mutate(
    q4_2 = factor(q4_2, levels = c(1, 2, 3, 4, 5),
                  labels = c("Strongly disagree", "Somewhat disagree", 
                             "Neither agree nor disagree", "Somewhat agree", 
                             "Strongly agree"))
  ) %>%
  ggplot(aes(x = reorder(q2_1, n), y = n, label = n, fill =q4_2)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student understanding of EAOP services by college site",
    x = "College site",
    y = "Number of Students",
    fill = "Student rating"
  ) +
  scale_fill_manual(values = tc_colors) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip()

#q4_3
df_1 %>%
  drop_na(q4_3, q2_1) %>%
  count(q4_3, q2_1) %>%
  complete(q4_3, q2_1, fill = list(n = 0)) %>%
  mutate(q4_3 = case_when(
    q4_3 == 1 ~ "Strongly disagree",
    q4_3 == 2 ~ "Somewhat disagree",
    q4_3 == 3 ~ "Neither agree nor disagree",
    q4_3 == 4 ~ "Somewhat agree",
    q4_3 == 5 ~ "Strongly agree",
    TRUE ~ as.character(q4_3)  # Keep unchanged if not matched
  )) %>%
  mutate(q4_3 = factor(q4_3, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>%  # Set the levels of q4_3
  arrange(n) %>%
  ggplot(aes(x = reorder(q4_3, n), y = n, label = n, fill = q4_3)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student likelihood of reaching out to EAOP when applying to college by college site",
    x = "Student rating",
    y = "Number of Students",
    fill = "College site"
  ) +
  scale_fill_manual(values = tc_colors) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip()



#q4_4
df_1_q4_4 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q4_4"),
    names_to = "q4_4",
    values_to = "response_q4_4",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q4_4, response_q4_4, everything()) %>% # Adjust as needed to include additional columns if necessary
  
  filter(q4_4 != "q4_4_5_text") %>%
  mutate(q4_4 = case_when(
    q4_4 == "q4_4_1" ~ "Academic advising",
    q4_4 == "q4_4_2" ~ "Choosing a college",
    q4_4 == "q4_4_3" ~ "Understanding application requirments",
    q4_4 == "q4_4_4" ~ "Writing a personal response/ essay",
    q4_4 == "q4_4_5" ~ "Choosing a major",
    q4_4 == "q4_4_6" ~ "Financial aid and scholarships",
    q4_4 == "q4_4_7" ~ "Other",
    TRUE ~ q4_4  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q4_4 %>%
  filter(q4_4 != "q4_4_7_text") %>%
  drop_na(q2_1) %>%
  count(q2_1, q4_4) %>%
  arrange(n)

ggplot(counts, aes(x = reorder(q2_1, n), y = n, fill = stringr::str_wrap(q4_4, width = 20))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Number of students who plan to meet with EAOP by topic and college site",
    x = "School",
    y = "Number of Students",
    fill = "Advising topic"
  ) +
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)

#q4_6
df_1_q4_6 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q4_6"),
    names_to = "q4_6",
    values_to = "response_q4_6",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q4_6, response_q4_6, everything()) %>% # Adjust as needed to include additional columns if necessary
  
  filter(q4_6 != "q4_6_5_text") %>%
  mutate(q4_6 = case_when(
    q4_6 == "q4_6_1" ~ "Academic advising",
    q4_6 == "q4_6_2" ~ "Personal insight questions/ personal statements",
    q4_6 == "q4_6_3" ~ "Financial aid (FAFSA, CalGrant, Pell grant, etc.)",
    q4_6 == "q4_6_4" ~ "AP courses",
    q4_6 == "q4_6_5" ~ "Community college dual enrollment",
    q4_6 == "q4_6_6" ~ "Major exploration",
    q4_6 == "q4_6_7" ~ "Career pathway exploration",
    q4_6 == "q4_6_8" ~ "Other",
    TRUE ~ q4_6  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q4_6 %>%
  filter(q4_6 != "q4_6_8_text") %>%
  drop_na(q2_1) %>%
  count(q2_1, q4_6) %>%
  arrange(n)

ggplot(counts, aes(x = reorder(q2_1, n), y = n, fill = stringr::str_wrap(q4_6, width = 20))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Number of students who want more information by topic and college site",
    x = "School",
    y = "Number of Students",
    fill = "Advising topic"
  ) +
  theme_classic(base_size = 12) +  # Apply minimal theme with specified base size
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Customize title appearance
    axis.title.x = element_text(size = 12, face = "bold"),  # Customize x-axis label appearance
    axis.title.y = element_text(size = 12, face = "bold"),  # Customize y-axis label appearance
    axis.text.x = element_text(size = 10),  # Customize x-axis tick label appearance
    axis.text.y = element_text(size = 10),  # Customize y-axis tick label appearance
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)


