#Start of the Script

#Loading the required packages and the Data

library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)
library(lubridate)

load(file="climate_text.rda")

###Exploring the Data
climate_text

head(climate_text)

str(climate_text)

#Tidying the Data and performing a more EDA

tidy_tv <- climate_text %>%
  # Transforming the non-tidy text data to tidy text data
  unnest_tokens(word,text)

tidy_tv

# #Question :what words are most common when discussing climate change 
# on TV news, as well as the total number of words from each station.

tidy_tv %>% 
  anti_join(stop_words) %>%
  # Count by word with sort = TRUE
  count(word, sort = TRUE)

tidy_tv %>%
  # Count by station
  count(station) %>%
  # Rename the new column station_total
  rename(station_total = n)

#Common words include climate,change,warmig..

###Performing Sentiment Analysis

tv_sentiment <- tidy_tv %>% 
  # Group by station
  group_by(station) %>% 
  # Define a new column station_total
  mutate(station_total = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis with the NRC lexicon
  inner_join(get_sentiments("nrc"))

tv_sentiment

###Question Which stations use the most negative words?
tv_sentiment %>% 
  count(station, sentiment, station_total) %>%
  mutate(percent = n / station_total) %>%
  # Filter only for negative words
  filter(sentiment == "negative") %>%
  # Arrange by percent
  arrange(percent)

###Question Which stations use the most positive words?
tv_sentiment %>% 
  count(station, sentiment, station_total) %>%
  mutate(percent = n / station_total) %>%
  filter(sentiment == "positive") %>%
  arrange(percent)    

# Comments: MSNBC used a low proportion of negative words 
# but a high proportion of positive words, 
# the reverse is true of FOX News, and CNN is middle of the pack.**

###Question : Which words contribute to the sentiment scores?
tv_sentiment %>%
  # Count by word and sentiment
  count(word, sentiment) %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 words for each sentiment ordered by n
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

# ###Which negative words did each station use when talking 
# about climate change on the air?

tv_sentiment %>%
  # Filter for only negative words
  filter(sentiment=="negative") %>%
  # Count by word and station
  count(word,station) %>%
  # Group by station
  group_by(station) %>%
  # Take the top 10 words for each station
  top_n(10,n) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, station, sep = "__"), n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word,n,fill=station)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ station, nrow = 2, scales = "free") +
  coord_flip()

###Question : Are TV news stations using more negative words as time passes? 

sentiment_by_time <- tidy_tv %>%
  # Define a new column using floor_date()
  mutate(date = floor_date(show_date, unit = "6 months")) %>%
  # Group by date
  group_by(date) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis using the NRC lexicon
  inner_join(get_sentiments("nrc"))

sentiment_by_time %>%
  # Filter for positive and negative words
  filter(sentiment %in% c("positive", "negative")) %>%
  # Count by date, sentiment, and total_words
  count(date, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  # Set up the plot with aes()
  ggplot(aes(date, percent, color = sentiment)) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)

###Word Changes Over Time

tidy_tv %>%
  # Define a new column that rounds each date to the nearest 1 month
  mutate(date = floor_date(show_date, unit = "1 month")) %>%
  filter(word %in% c("threat", "hoax", "denier",
                     "real", "warming", "hurricane")) %>%
  # Count by date and word
  count(date, word) %>%
  ungroup() %>%
  # Set up your plot with aes()
  ggplot(aes(date, n, color = word)) +
  # Make facets by word
  facet_wrap(~word) +
  geom_line(size = 1.5, show.legend = FALSE) +
  expand_limits(y = 0)
