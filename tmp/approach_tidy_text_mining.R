

library(tidytext)
library(dplyr)

# Connect to file
con <- file("data/sample_en_US_twitter.txt", "r") 
textVector <- readLines(con)

# -------------------------------------------------------
# tokenization and anaylizing word frequency
# -------------------------------------------------------

text_df <- data.frame(line = 1: length(textVector), text = textVector, stringsAsFactors = F)

words <- text_df %>% unnest_tokens(word, text)

# Counting function from dplyr on words to get a sorted list of most used words
words %>% count(word, sort=TRUE)


# Removing "STOp WORDS"
# Though stop words usually refer to the most common words in a language

words <- words %>% anti_join(stop_words)  # stop_words comes from tidytext

words %>% count(word, sort=TRUE)


library(ggplot2)

words %>% 
      count(word, sort=TRUE) %>%
      filter(n > 10) %>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word,n)) +
      geom_bar(stat="identity") +
      xlab(NULL) + coord_flip()


# -----------------------------------------------------
# word sentiments with "sentiments" dataset
# -----------------------------------------------------

library(tidytext)

sentiments

get_sentiments("nrc") # other options ""bing, "afinn"


# ---------------------------

unique(get_sentiments("nrc")$sentiment)

# ------------------------------
# getting the positve words
# -----------------------------
positive_words <- get_sentiments("nrc") %>% filter(sentiment == "positive")


words %>% inner_join(positive_words) %>% count(word, sort=TRUE)


# -----------------------------
# Classifiying a sentence by score
# -----------------------------

scored_words <- get_sentiments("afinn")
words %>% 
      inner_join(scored_words) %>%
      group_by(line) %>%
      summarize(sentiment_level = sum(score))



# -----------------------------
# WordClowd
# -----------------------------

library(tidytext)
library(dplyr)
library(wordcloud)

#words <- words %>% anti_join(stop_words)  # stop_words comes from tidytext

words %>% count(word, sort=TRUE) %>% with(wordcloud(word,n,max.words=100))


# ------------------------------------
#  Analysing word relationship
# ------------------------------------

library(tidytext)
library(dplyr)
library(tidyr)

words <- text_df %>% unnest_tokens(bigram, text, token="ngrams", n=2)
head(words)

bigrams <- words %>% count(bigram, sort=T)

# Separate into 2 words and filter out the combinations which don't contain stop_words
separated_gigrams <- bigrams %>% separate(bigram, c("word1", "word2"), sep=" ")
head(separated_gigrams)

bigrams_filtered <- separated_gigrams %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word)
head(bigrams_filtered)

bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
head(bigrams_united)





