library(readxl) # install if needed
library(dplyr) # install if needed
library(tidytext) # install if needed
library(stringr) #install if needed
library(tidyverse)

music <- read_excel ("music.xlsx")
head(music)
music1 <- subset(music, music$lang == "en")
to_remove <- "https?://[^\\s]+|&amp;|&lt;|&gt;|\bRT\\b"
tidy_music <- music1 %>% mutate(text = str_replace_all(text, to_remove, "")) %>% unnest_tokens(word, text, token="tweets")

#Tokenization: *unnest_tokens* splits the text of each tweet into individual words. Usually, when splitting text into words, you would use *token = "words"*. Here, we used *token = "tweets"*, which is a variant that retains hashtag # and @ symbols. 
#Note that *unnest_tokens* also converts text to lower case by default.

#We can have a look at the cleaned data:

head(tidy_music)

#Let's have a look at the most popular terms at this point

word_count <- tidy_music %>%
  count(word, sort = TRUE) %>%
  arrange(-n)
head(word_count)

#Let's filter out frequently used stop words     

View(stop_words)
tidy_music <- tidy_music %>%
  anti_join(stop_words, by = "word")

#Let's have another look at the most popular terms, using the same code like before

word_count <- tidy_music %>%
  count(word, sort = TRUE) %>%
  arrange(-n)
head(word_count)

words_to_remove <- c("@harrystyles","night","rt","building","ðÿ\u008f", "ðÿz¸")

tidy_music <- filter(tidy_music, ! word %in% words_to_remove)

#Let's have another look at the most popular terms, using the same code like before

word_count <- tidy_music %>%
  count(word, sort = TRUE) %>%
  arrange(-n)
head(word_count)

#3. Visualize the most commonly used words using by creating a word cloud of the data

install.packages("wordcloud")
library(wordcloud) #install if needed
tidy_music %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,scale = c(2,0.5)))

positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

#Add sentiment

tidy_music_positive <- tidy_music %>%
  semi_join(positive) %>%
  count(word, sort = TRUE)
head(tidy_music_positive)

tidy_music_negative <- tidy_music %>%
  semi_join(negative) %>%
  count(word, sort = TRUE)
head(tidy_music_negative)

#4. What are the most positive and negative tweets in the data?

#Sentiment using the Nrc lexicon *

install.packages("syuzhet")
library(syuzhet) #install if needed

sentiment <- get_nrc_sentiment(music$text) # this one takes several minutes

music_sentiment <- cbind(music,sentiment)



