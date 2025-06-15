library(tidyverse)
library(readr)  # For read_csv
library(ggplot2)  # For plotting

df_1 <- read_csv("EAOP A-G Validation Evaluation_6_5_24.csv") %>%
  clean_names() %>%
  select(18:125) %>%
  mutate(q2_1 = case_when(
    q2_1 == "1" ~ "Channel Islands High School",
    q2_1 == "4" ~ "Pacifica High School",
    TRUE ~ q2_1  # Keep unchanged if not matched
  )) %>%
  mutate(q2_4 = case_when(
    q2_4 == "1" ~ "Male",
    q2_4 == "2" ~ "Female",
    q2_4 == "3" ~ "Non-binary/third gender",
    q2_4 == "4" ~ "Other",
    TRUE ~ q2_4  # Keep unchanged if not matched
  )) %>%
  filter(q2_1 == "Pacifica High School")

# Define custom colors
tc_colors <- c("darkgoldenrod1", "royalblue4", "darkcyan", "lightcoral", "burlywood4")

df_1_q2_5 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q2_5"),
    names_to = "q2_5",
    values_to = "response_q2_5",
    values_drop_na = TRUE
  ) %>%
  select(q2_4, q2_5, response_q2_5, q2_1) %>%
  filter(q2_1 == "Pacifica High School") %>%
  filter(q2_5 != "q2_5_5_text") %>%
  mutate(q2_5 = case_when(
    q2_5 == "q2_5_1" ~ "California community colleges",
    q2_5 == "q2_5_2" ~ "California State Universities",
    q2_5 == "q2_5_3" ~ "University of California",
    q2_5 == "q2_5_4" ~ "Out of state college or university",
    q2_5 == "q2_5_5" ~ "Other",
    TRUE ~ q2_5
  ))

# Compute counts ensuring both gender and school are included
counts <- df_1_q2_5 %>%
  drop_na(q2_4) %>%
  count(q2_4, q2_5, name = "n") %>%
  arrange(q2_4, q2_5)

# Plot q2_5 by q2_4
ggplot(counts, aes(x = q2_4, y = n, fill = q2_5)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9), 
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student interest in higher education systems by gender",
    x = "Gender",
    y = "Number of students",
    fill = "Higher education system"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)


tc_colors <- c(
  "darkgoldenrod1", "royalblue4", "darkcyan", "lightcoral", "burlywood4", 
  "goldenrod3", "steelblue4", "lightcyan2")

df_1_q3_1 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_1"),
    names_to = "q3_1",
    values_to = "response_q3_1",
    values_drop_na = TRUE
  ) %>%
  select(q2_4, q3_1, response_q3_1, q2_1) %>%
  filter(q2_1 == "Pacifica High School") %>%
  filter(q3_1 != "q3_1_8_text") %>%
  mutate(q3_1 = case_when(
    q3_1 == "q3_1_1" ~ "A-G requirements",
    q3_1 == "q3_1_2" ~ "Personal insight questions/ personal statements",
    q3_1 == "q3_1_3" ~ "Financial aid (FAFSA, CalGrant, Pell grant, etc.)",
    q3_1 == "q3_1_4" ~ "AP courses",
    q3_1 == "q3_1_5" ~ "Community college dual enrollment",
    q3_1 == "q3_1_6" ~ "Extra curricular activities",
    q3_1 == "q3_1_7" ~ "Community service",
    q3_1 == "q3_1_8" ~ "Other",
    TRUE ~ q3_1
  ))

# Compute counts
counts <- df_1_q3_1 %>%
  drop_na(q2_4) %>%
  count(q2_4, q3_1, name = "n") %>%
  arrange(q2_4, q3_1)

# Plot q3_1 by q2_4
ggplot(counts, aes(x = q2_4, y = n, fill = q3_1)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9), 
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Parts of the admission process students are familiar with by gender",
    x = "Gender",
    y = "Number of Students",
    fill = "Admission Topic"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)

# Define custom colors
tc_colors <- c("darkgoldenrod1", "royalblue4", "darkcyan", "lightcoral", "burlywood4")

df_1_q3_2 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_2"),
    names_to = "q3_2",
    values_to = "response_q3_2",
    values_drop_na = TRUE
  ) %>%
  select(q2_3, q3_2, response_q3_2, everything()) %>%
  filter(q3_2 != "q3_2_5_text") %>%
  mutate(q3_2 = case_when(
    q3_2 == "q3_2_1" ~ "UC/CSU GPA",
    q3_2 == "q3_2_2" ~ "5.0 GPA",
    q3_2 == "q3_2_3" ~ "4.0 GPA",
    q3_2 == "q3_2_4" ~ "Cal Grant GPA",
    TRUE ~ q3_2  # Keep unchanged if not matched
  )) %>%
  filter(q2_1 == "Pacifica High School")

# Compute counts
counts <- df_1_q3_2 %>%
  drop_na(q2_4) %>%
  count(q2_4, q3_2, name = "n") %>%
  arrange(q2_4, q3_2)

# Plot q3_2 by q2_4
ggplot(counts, aes(x = q2_4, y = n, fill = q3_2)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9), 
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "GPA types students are familiar with by gender",
    x = "Gender",
    y = "Number of Students",
    fill = "GPA Type"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)


# Define custom colors for the fill aesthetic
tc_colors <- c("Strongly disagree" = "darkgoldenrod1", "Somewhat disagree" = "royalblue4", 
               "Neither agree nor disagree" = "darkcyan", "Somewhat agree" = "lightcoral",
               "Strongly agree" = "burlywood4")

# Filter data for Pacifica High School and process it for plotting
df_1_q3_4 <- df_1 %>%
  drop_na(q3_4, q2_1) %>%
  filter(q2_1 == "Pacifica High School") %>%
  count(q3_4, q2_4) %>%
  complete(q3_4, q2_4, fill = list(n = 0)) %>%
  arrange(q2_4, q3_4) %>%
  mutate(
    q3_4 = factor(q3_4, levels = c(1, 2, 3, 4, 5),
                  labels = c("Strongly disagree", "Somewhat disagree", 
                             "Neither agree nor disagree", "Somewhat agree", 
                             "Strongly agree"))
  )

# Generate the plot
ggplot(df_1_q3_4, aes(x = q2_4, y = n, fill = q3_4)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")),
    position = position_dodge(width = 0.9),
    size = 3, vjust = -0.5, fontface = "bold"
  ) +
  labs(
    title = "Student familiarity with GPA calculation method by gender",
    x = "Gender",
    y = "Number of Students",
    fill = "Student rating"
  ) +
  scale_fill_manual(values = tc_colors) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip()



# Assuming df_1 is already loaded and cleaned
df_1 %>%
  drop_na(q3_6, q2_1) %>%
  filter(q2_1 == "Pacifica High School") %>% 
  count(q3_6, q2_4) %>%
  complete(q3_6, q2_4, fill = list(n = 0)) %>%
  mutate(
    q3_6 = factor(q3_6, levels = c(1, 2, 3, 4, 5),
                  labels = c("Strongly disagree", "Somewhat disagree", 
                             "Neither agree nor disagree", "Somewhat agree", 
                             "Strongly agree"))
  ) %>%
  ggplot(aes(x = q2_4, y = n, label = n, fill = q3_6)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student rating of familiarity of personal A-G status by gender",
    x = "Gender",
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


tc_colors <- c(
  "darkgoldenrod1", "royalblue4", "darkcyan", "lightcoral", "burlywood4", 
  "goldenrod3", "steelblue4")

df_1_q3_7 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_7"),
    names_to = "q3_7",
    values_to = "response_q3_7",
    values_drop_na = TRUE
  ) %>%
  select(q2_4, q3_7, response_q3_7, q2_1) %>% # Include gender and school columns
  filter(q2_1 == "Pacifica High School") %>%
  filter(q3_7 != "q3_7_8_text") %>% # Exclude any text responses
  mutate(q3_7 = case_when(
    q3_7 == "q3_7_1" ~ "History",
    q3_7 == "q3_7_2" ~ "English",
    q3_7 == "q3_7_3" ~ "Math",
    q3_7 == "q3_7_4" ~ "Science",
    q3_7 == "q3_7_5" ~ "Language other than English",
    q3_7 == "q3_7_6" ~ "Visual and performing arts",
    q3_7 == "q3_7_7" ~ "College preparatory elective",
    TRUE ~ q3_7  # Keep unchanged if not matched
  )) %>%
  count(q2_4, q3_7, name = "n") %>%
  arrange(q2_4, q3_7)

# Plot q3_7 by q2_4
ggplot(df_1_q3_7, aes(x = q2_4, y = n, fill = q3_7)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),
    size = 3, vjust = -0.5, fontface = "bold"
  ) +
  labs(
    title = "A-G requirements students felt familiar with by gender",
    x = "Gender",
    y = "Number of Students",
    fill = "A-G requirement"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)

df_1_q3_8 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q3_8"),
    names_to = "q3_8",
    values_to = "response_q3_8",
    values_drop_na = TRUE
  ) %>%
  select(q2_1, q2_4, q3_8, response_q3_8) %>%  # Ensure q2_1 is selected here
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
  )) %>%
  filter(q2_1 == "Pacifica High School")  # Ensure this filter is correctly placed after `q2_1` is available

# Compute counts
counts <- df_1_q3_8 %>%
  drop_na(q2_4, q3_8) %>%
  count(q2_4, q3_8, name = "n") %>%
  arrange(q2_4, q3_8)

# Generate the plot
ggplot(counts, aes(x = q2_4, y = n, fill = q3_8)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9), 
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "A-G requirements students wanted more information about by gender",
    x = "Gender",
    y = "Number of Students",
    fill = "A-G requirement"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_flip() +
  scale_fill_manual(values = tc_colors)

df_1 %>%
  drop_na(q4_2, q2_4) %>%
  filter(q2_1 == "Pacifica High School") %>% 
  count(q4_2, q2_4) %>%
  complete(q4_2, q2_4, fill = list(n = 0)) %>%
  arrange(n) %>%
  mutate(
    q4_2 = factor(q4_2, levels = c(1, 2, 3, 4, 5),
                  labels = c("Strongly disagree", "Somewhat disagree", 
                             "Neither agree nor disagree", "Somewhat agree", 
                             "Strongly agree"))
  ) %>%
  ggplot(aes(x = q2_4, y = n, label = n, fill = q4_2)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student understanding of EAOP services by gender",
    x = "Gender",
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

df_1 %>%
  drop_na(q4_3, q2_4) %>%
  filter(q2_1 == "Pacifica High School") %>%
  count(q4_3, q2_4) %>%
  complete(q4_3, q2_4, fill = list(n = 0)) %>%
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
  ggplot(aes(x = q2_4, y = n, label = n, fill = q4_3)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = as.integer(n)),
    position = position_dodge(width = 0.9),
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Student likelihood of reaching out to EAOP when applying to college by gender",
    x = "Gender",
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

df_1_q4_4 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q4_4"),
    names_to = "q4_4",
    values_to = "response_q4_4",
    values_drop_na = TRUE
  ) %>%
  select(q2_1, q2_4, q4_4, response_q4_4) %>%
  filter(q4_4 != "q4_4_5_text", q2_1 == "Pacifica High School") %>%
  mutate(q4_4 = case_when(
    q4_4 == "q4_4_1" ~ "Academic advising",
    q4_4 == "q4_4_2" ~ "Choosing a college",
    q4_4 == "q4_4_3" ~ "Understanding application requirements",
    q4_4 == "q4_4_4" ~ "Writing a personal response/ essay",
    q4_4 == "q4_4_5" ~ "Choosing a major",
    q4_4 == "q4_4_6" ~ "Financial aid and scholarships",
    q4_4 == "q4_4_7" ~ "Other",
    TRUE ~ q4_4  # Keep unchanged if not matched
  ))

# Compute counts
counts <- df_1_q4_4 %>%
  drop_na(q2_4) %>%
  count(q2_4, q4_4) %>%
  arrange(q2_4, q4_4)

# Plot
ggplot(counts, aes(x = q2_4, y = n, fill = stringr::str_wrap(q4_4, width = 20))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_text(
    aes(label = ifelse(n > 0, as.integer(n), "")), 
    position = position_dodge(width = 0.9),  # Dodge the text labels within the bars
    size = 3, hjust = -0.1, fontface = "bold"
  ) +
  labs(
    title = "Number of students who plan to meet with EAOP by topic and gender",
    x = "Gender",
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


df_1_q4_6 <- df_1 %>%
  pivot_longer(
    cols = starts_with("q4_6"),
    names_to = "q4_6",
    values_to = "response_q4_6",
    values_drop_na = TRUE
  ) %>%
  filter(q2_1 == "Pacifica High School", q4_6 != "q4_6_5_text") %>%
  mutate(q4_6 = case_when(
    q4_6 == "q4_6_1" ~ "Academic advising",
    q4_6 == "q4_6_2" ~ "Personal insight questions/ personal statements",
    q4_6 == "q4_6_3" ~ "Financial aid (FAFSA, CalGrant, Pell grant, etc.)",
    q4_6 == "q4_6_4" ~ "AP courses",
    q4_6 == "q4_6_5" ~ "Community college dual enrollment",
    q4_6 == "q4_6_6" ~ "Major exploration",
    q4_6 == "q4_6_7" ~ "Career pathway exploration",
    q4_6 == "q4_6_8" ~ "Other",
    TRUE ~ q4_6
  )) %>%
  count(q2_4, q4_6, name = "n") %>%
  mutate(q2_4 = factor(q2_4, levels = c("Female", "Male", "Other")))

# Define the color mapping for advising topics
tc_colors <- c(
  "Academic advising" = "darkgoldenrod1", 
  "Personal insight questions/ personal statements" = "lightcyan2", 
  "Financial aid (FAFSA, CalGrant, Pell grant, etc.)" = "burlywood4", 
  "AP courses" = "royalblue4", 
  "Community college dual enrollment" = "lightcoral", 
  "Major exploration" = "goldenrod3", 
  "Career pathway exploration" = "darkcyan", 
  "Other" = "steelblue4"
)

# Plot
ggplot(df_1_q4_6, aes(x = q2_4, y = n, fill = q4_6)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = tc_colors) +
  labs(
    title = "Number of students who want more information by topic and gender",
    x = "Gender",
    y = "Number of Students",
    fill = "Advising Topic"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14), # Bold and slightly larger title
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title.align = 0.5,
) +
  coord_flip()