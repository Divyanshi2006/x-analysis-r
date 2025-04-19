library(dplyr)
library(tidytext)
library(ggplot2)
library(textclean)
library(readr)

tweets <- read_csv("twitter_dataset.csv")


# Clean text: remove HTML and URLs
tweets$Text <- replace_html(tweets$Text)
tweets$Text <- replace_url(tweets$Text)
tweets$Text <- gsub("[^[:alnum:] ]", "", tweets$Text)

# Tokenize and remove stopwords
data("stop_words")

tidy_tweets <- tweets %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words, by = "word")


# Load Bing lexicon
bing <- get_sentiments("bing")

# Join lexicon with tokenized words
sentiment_scores <- tidy_tweets %>%
  inner_join(bing, by = "word") %>%
  count(word, sentiment, sort = TRUE)

# Count total sentiment polarity
total_sentiment <- sentiment_scores %>%
  count(sentiment)

# Visualize sentiment
ggplot(total_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Distribution", x = "Sentiment", y = "Word Count")


# Top sentiment words
top_words <- sentiment_scores %>%
  group_by(sentiment) %>%
  top_n(10, n)

# Plot
ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Sentiment Words", x = "Words", y = "Frequency")

