# setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE

# setwd("C:/Users/Grund/Documents/GitHub/DataScience_CapStone") # HOME


# ------------------------------------------
# Code that always needs to run
# ------------------------------------------

library(tidytext)
library(dplyr)
library(ggplot2)


# STEP 1: Setup data

con <- file("data/en_US.blogs.txt", "r") 
blog_file <- readLines(con, encoding="UTF-8")
close(con)

blog_file_df <- data.frame(line = 1:length(blog_file), text = blog_file, stringsAsFactors = F)

rm(blog_file)

# object.size(blog_file_df) / 1000


# STEP 2: Basic Tokenization and visualizing term usage

blog_words <- blog_file_df %>% unnest_tokens(word, text, token="words")

blog_words %>% count(word, sort=TRUE) %>% top_n(10)

blog_words %>% anti_join(stop_words) %>% count(word, sort=TRUE) %>% top_n(20) %>%
  ggplot(aes(word,n)) +
  geom_bar(stat="identity") +
  xlab(NULL) + coord_flip()


# STEP 3 Analyse term frequency (tf) and inverse term frequency (itf)


perBlogWords <- blog_file_df %>% unnest_tokens(word, text, token="words") %>% # tokenization
                      count(line, word, sort=TRUE) %>% ungroup()            # counting words per blog entry

total_blog_words <- perBlogWords %>% group_by(line) %>% summarize(total=sum(n))                # counting total words per blog entry


summary_words <- left_join(perBlogWords, total_blog_words)

rm(perBlogWords)
rm(total_blog_words)

head(summary_words)


freq_by_rank <- summary_words %>% order_by(n) %>% group_by(line) %>% mutate(rank=row_number(), term_freq = n/total)

head(freq_by_rank)

# Zipf's law
freq_by_rank %>% filter(line %in% c(517366,483415)) %>%
  ggplot(aes(rank, term_freq, color=as.factor(line))) +
  geom_line(size=1.2, alpha=0.8) +
  scale_x_log10() +
  scale_y_log10()


summary_words <- summary_words %>% bind_tf_idf(word,line, n)

head(summary_words)
summary_words %>% arrange(line,n) %>% mutate(term_freq=n/total) %>% top_n(20)


