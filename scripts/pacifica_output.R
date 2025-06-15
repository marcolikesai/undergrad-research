# Load necessary libraries
library(tm)
library(topicmodels)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidytext)

# Read your CSV file
data <- read.csv("ED199RA/pacifica_output (coded).xlsx - Coded - Marco.csv", stringsAsFactors = FALSE) %>%
  clean_names()

# Assuming 'Sentence' column contains the text data
documents <- data$sentence

# Create a corpus
corpus <- Corpus(VectorSource(documents))

# Text Preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Add custom stopwords if necessary
# custom_stopwords <- c("your", "custom", "stop", "words")
# corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Convert DTM to matrix and get word counts
matrix <- as.matrix(dtm)
word_counts <- colSums(matrix)
word_counts_df <- data.frame(word = names(word_counts), count = word_counts)
word_counts_df <- word_counts_df[order(-word_counts_df$count),]

# Visualize top words
ggplot(word_counts_df[1:20, ], aes(x = reorder(word, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 2.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 20 words in the text data (excluding stop words)",
       x = "Words",
       y = "Count")

# ... (previous code remains the same)

# Filter out the empty documents
corpus <- tm_map(corpus, PlainTextDocument)  # Ensure all transformations are applied

# This function checks if a document is empty
isEmptyDocument <- function(doc) {
  all(str_squish(as.character(doc)) == "")
}

# Filter out empty documents from the corpus
corpus <- corpus[!sapply(corpus, isEmptyDocument)]

# Make sure there are no empty documents before creating the DTM
if (length(corpus) == 0) {
  stop("All documents were removed after preprocessing.")
}

# Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Check if there are any empty rows/documents in the DTM
if (any(rowSums(as.matrix(dtm)) == 0)) {
  dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
}



# Build LDA Model
lda_model <- LDA(dtm, k = 10, control = list(seed = 123))
topics <- tidy(lda_model, matrix = "beta")

# Visualize topics
topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5, size = 10, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    strip.text.x = element_text(size = 10)
    ) +
  labs(title = "Topic model of your dataset",
       x = "",
       y = "beta")

